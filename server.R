# loads the required libraries.
# when adding libraries here, REMEMBER to include them as requirements on README.md
library(shiny)
library(PerformanceAnalytics) #required for table ploting in tutorial tab
library(Rsampling)
shinyServer(function(input, output, session) {
            ###########################################
            ### INTERNAL OBJECTS AND INPUT HANDLING ###
            ###########################################
            #### Selection of prebuilt statistic functions
            smean <- function(dataframe){
              mean(dataframe[,as.numeric(input$m1)])
            }
            ssd <- function(dataframe){
              sd(dataframe[,as.numeric(input$m1)])
            }
            meandif <- function(dataframe){
              props <- unlist(tapply(dataframe[,as.numeric(input$m2)], dataframe[,as.numeric(input$m1)], mean))
              props[1] - props[length(props)]
            }
            meandifc <- function(dataframe){
              dif <- dataframe[, as.numeric(input$m2)] - dataframe[, as.numeric(input$m1)]
              mean(dif)
            }
            srow <- function(dataframe) {
              mean(apply(dataframe, 1, sum))
            }
            scol <- function(dataframe) {
              mean(apply(dataframe, 2, sum))
            }
            intercept <- function(dataframe) {
              coef(lm(dataframe[, as.numeric(input$m1)] ~ dataframe[, as.numeric(input$m2)]))[1]
            }
            slope <- function(dataframe) {
              coef(lm(dataframe[, as.numeric(input$m1)] ~ dataframe[, as.numeric(input$m2)]))[2]
            }
            corr <- function(dataframe) {
              cor(dataframe[, as.numeric(input$m1)], dataframe[, as.numeric(input$m2)])
            }
            Fstatistic <- function(dataframe){
              mod <- lm(dataframe[,as.numeric(input$m2)]~ as.factor(dataframe[,as.numeric(input$m1)]))
              anova(mod)[1,4]
            }
            ancova1 <- function(dataframe){
                # Difference between slopes of regressions fitted to two groups 
                slope = function(x) coef(lm(x[,2]~x[,1]))[2]
                props = by(dataframe[,as.numeric(c(input$m1,input$m2))], dataframe[,as.numeric(input$m3)], slope)
                props[1] - props[length(props)]
            }
            ancova2 <- function(dataframe){
                # Difference between intercepts with same slope
                coef(lm(dataframe[, as.numeric(input$m2)] ~ 
                        dataframe[, as.numeric(input$m1)] + 
                        dataframe[, as.numeric(input$m3)]))[3]
            }
            # custom function handler: parses the text in the custom input
            custom <- function(dataframe) {
              input$gocustomstat
              eval(parse(text=isolate(input$customstat)))
            }
            # what columns should be randomized?
            cols <- reactive({ 
              # the "data" column is indicated by m1 for these statistics
              if(input$stat %in% c("smean", "ssd")) 
                return(as.numeric(input$m1))
              # the "data" column is indicated by m2 for these statistics
              if(input$stat %in% c("meandif","Fstatistic", "slope", "intercept", "corr"))
                return(as.numeric(input$m2))
              # both columns are randomized
              if(input$stat %in% c("meandifc", "ancova1", "ancova2")) 
                return(c(as.numeric(input$m1), as.numeric(input$m2)))
              # all columns should be used
              if(input$stat %in% c("scol", "srow"))
                return(1:ncol(data()))
              # the custom function has a special input box for selecting columns
              if(input$stat == "custom")
                return(eval(parse(text=isolate(input$customcols))))
              # should never reach this point
            })
            stratum <- reactive({
              if (input$stratum == FALSE)
                return (rep(1, nrow(data())))
              #else
              d <- data()
              return (d[, as.numeric(input$stratumc)])
            })
            ### this reactive simply translates the input value into the corresponding function
            # can we tidy this up with something like match.fun?
            statistic <- reactive({
              switch(input$stat,
                     "meandif" = meandif,
                     "meandifc" = meandifc,
                     "smean" = smean,
                     "ssd" = ssd,
                     "srow" = srow,
                     "scol" = scol,
                     "intercept" = intercept,
                     "slope" = slope,
                     "corr" = corr,
                     "Fstatistic" = Fstatistic,
                     "ancova1" = ancova1,
                     "ancova2" = ancova2,
                     "custom" = custom
                     )
            })
            ### the statistic applied to the original data
            svalue <- reactive({
              f <- statistic()
              f(data())
            })
            ### reads the CSV uploaded by the data selector
            csvfile <- reactive({
              if(is.null(input$file)) return (data.frame());
              read.csv(input$file$datapath, header=input$header, sep=input$sep, quote=input$quote, dec=input$dec)
            })
            ### translates the input value for data source into the corresponding R object
            ### in the case "upload file", it reads the csv using the csvfile() reactive
            data <- reactive({
              switch(input$datasource,
                     "embauba" = embauba,
                     "azteca" = azteca,
                     "peucetia" = peucetia,
                     "rhyzophora" = rhyzophora,
                     "upload file" = csvfile())
            })
            ### calculates the distribution of the statistic of interest using Rsampling
            ### several of its arguments are isolate()'d, meaning that changing them will
            ### trigger a recalculation of the statistic (for performance reasons)
            # As of Rsampling-shiny 1.3, the distribution() reactive was replaced by the
            # vals reactive. Now vals$distribution should be used on all contexts in 
            # which distribution() was previously used, EXCEPT inside the plotting function,
            # where vals$x represent the distribution as it is being animated
            # vals$run controls whether the distribution() has run succesfully
            vals<-reactiveValues(run = FALSE)
            distribution <- observe({
              vals$run <- FALSE
              # triggers the calculations when the "Update graph" is pressed
              if (input$go == 0) { #no sampling was done yet, initializes vals with "zeroes"
                vals$iter <- 1; vals$distribution <- 0; vals$seqsim <- c()
                return(0);
              }
              # EVERYTHING ELSE is isolated:
              isolate({
              # traps NA, NaN, NULL, Infin the statistic applied over the original data
                if ((is.null(svalue())) || (is.na(svalue()) | is.nan(svalue()) | !is.finite(svalue()))) {
                  vals$iter <- 1; vals$distribution <- 0; vals$seqsim <- c()
                  return (0);
                }
                type = switch(isolate(input$type),
                              "Normal" = "normal_rand",
                              "Rows as units" = "rows_as_units",
                              "Columns as units" = "columns_as_units",
                              "Within rows" = "within_rows",
                              "Within columns" = "within_columns"
                              )
             # sets up a new shiny progress bar and callback function
                progress <- shiny::Progress$new(max=100)
                on.exit(progress$close())
                progress$set(message = tr("Sampling..."), value = 0)
                pupdate <- function(x) 
                  progress$set(value = x * progress$getMax(), 
                               detail=paste0(round(progress$getValue()), "%"))
                vals$iter <- 1
                vals$total_iterations <- input$ntrials 
                vals$seqsim <- seq(100, vals$total_iterations, len=100)
                vals$distribution <- Rsampling::Rsampling(type = type, dataframe = data(),
                                       statistics = statistic(), cols = cols(),
                                       stratum = isolate(stratum()),
                                       ntrials = isolate(input$ntrials), 
                                       replace=isolate(input$replace),
                                       progress = pupdate)
                vals$maxcount<-max(hist(vals$distribution, plot=FALSE)$counts)
              })
              vals$run <- TRUE
              # why is resume() called here???
              run_iter$resume()
            })
            run_iter <- observe({
              if (!vals$run) return();
              qry <- parseQueryString(session$clientData$url_search)
              if (is.null(qry$auto) && input$go == 0) {
                isolate({
                  vals$x <- vals$distribution
                })
              } else {
                isolate({
                  vals$x <- vals$distribution[1:(round(vals$seqsim[vals$iter]))]
                  vals$iter <- vals$iter + 1
                })
              }
              if (isolate(vals$iter) < 100) 
                invalidateLater(0, session)
              else 
                return()
            })    
            ###########################################
            ####### OUTPUT GENERATING FUNCTIONS #######
            ###########################################
            output$download <- downloadHandler(
              filename=function() "Rsampling.csv",
              content=function(file) {
                if(!vals$run) stop (tr("Sampling ended with error!"))
                write.csv(vals$distribution, file)
              }
            )
            # displays a warning in the case svalue() is not a single number
            output$svaluewarning <- renderText({
              s <- isolate(try(svalue(), silent=TRUE))
              input$gocustomstat
              input$stat
              if(!is.null(s) && length(s) > 1)
                return(tr("WARNING, the statistic function should return a single number."))
              return("")
            })
            output$needinstall <- reactive({
              if(packageDescription("Rsampling")$Version != "0.0.0.3") return ("incompatible")
              else return ("ok")
            })
            # see: http://stackoverflow.com/questions/19686581/make-conditionalpanel-depend-on-files-uploaded-with-fileinput
            outputOptions(output, 'needinstall', suspendWhenHidden=FALSE)
            ### simple table display to see the contents of the data selected
            output$view <- renderTable({
              data()
            })
            ### main plot of the program: generates a histogram of distribution()
            output$distPlot <- renderPlot({
              # Traps errors
              if (input$go == 0 | !is.numeric(vals$x)) {
                plot(0,0, type='n',xlab="", ylab="", main=tr("Run the resampling to see the graphs"));
                return();
              }
              if (! vals$run)
                  stop(tr("Distribution calculation stopped with error!"))
              Rsampling::dplot(dist = vals$x, svalue =  isolate(svalue()), pside= input$pside, 
                   extreme = input$extreme, vline = TRUE, rejection = input$rejection, ylim=c(0,vals$maxcount),
                             main=tr("distplot_title"), xlab=tr("Statistic of interest"), ylab=tr("Frequency"))
            })
            ### simply displays the statistic of interest
            output$stat <- renderText({
              # to avoid weird things when length > 1
              s <- paste(round(svalue(), 3), collapse = " ")
              paste(tr("Statistic of interest"),": ", s, "\n", sep="")
            })
            ### simply displays the "p-value"
            output$p <- renderText({
              if (! vals$run) return (tr("no available p-value yet..."))
              side <- switch(input$pside, "Two sided" = "twosided", "onesided")
              p <- switch(input$pside, 
                          "Two sided" = abs(vals$distribution) >= abs(svalue()),
                          "Greater" = vals$distribution >= svalue(),
                          "Lesser" = vals$distribution <= svalue()
                          )
              p <- round(sum(p) / length(vals$distribution),3)
              paste(tr(side), p)
            })
            ### Updates the values in the dropdowns for column selection
            observe({
              # Check to see if there is any data (may fail during file upload)
              d <- tryCatch(data(), error=function(cond) return(data.frame()))
              if(!ncol(d)) return();
              cols <- 1:length(colnames(d))
              names(cols) <- colnames(d)
              # Please see ?switch for the syntax below
              label1 <- switch(input$stat,
                               'smean'=,'ssd'= tr("Variable column:"), 
                               'intercept'=,'slope'=,'corr'=,'ancova1'=,'ancova2'=tr("Dependent variable column:"),
                               'meandif'=, 'Fstatistic'=tr("Categorical variable column:"),
                               'meandifc'= tr("Before treatment:"))
              label2 <- switch(input$stat, 
                               'intercept'=,'slope'=,'corr'=,'ancova1'=,'ancova2'=tr("Dependent variable column:"),
                               'meandif'=, 'Fstatistic'=tr("Numerical variable column:"),
                               'meandifc'= tr("After treatment:"))
              label3 <- switch(input$stat,
                               'ancova1'=, 'ancova2'=tr("Categorical variable column:"))
              updateSelectInput(session, "m1", choices = cols, label = label1)
              updateSelectInput(session, "m2", choices = cols, selected=2, label = label2)
              updateSelectInput(session, "m3", choices = cols, selected=3, label = label3)
              updateSelectInput(session, "stratumc", choices = cols)
          })
          session$onSessionEnded(function() {
          run_iter$suspend()
          })
          ###########################################
          ####### FUNCTIONS USED IN TUTORIAL #######
          ###########################################
          ###########################################################################
          values <- reactiveValues()
          #Object where the statistics from the randomized
          ## data sets are stored
          values$saveDist <- list()
          #Clear histrogram when button is pressed
          ##Tab 1 - mangrove trees
          observeEvent(input$clear1, {
            isolate(values$saveDist <- list())
          })
          ##Tab 2 - balanced mangrove trees
          observeEvent(input$clear2, {
            isolate(values$saveDist <- list())
          })
          ##Tab 3 - protective ants
          observeEvent(input$clear3, {
            isolate(values$saveDist <- list())
          })
          #Reset values when the user goes to a different tab
          observeEvent(input$tabEvent, {
            isolate(values$saveDist <- list())
            isolate(values$orig <- data.frame())
            updateSelectInput(session, "dataset", selected='Original')
            updateSelectInput(session, "dataset2", selected='Original')
            updateSelectInput(session, "dataset3", selected='Original')
          })
          favoriteColor <- "#428bca" #blue 
          tablePlot <- function(dataframe, is.randomizedSet){
            nrows <- dim(dataframe)[1]          
            if(is.randomizedSet) {
              colors <- matrix(ifelse(unlist(dataframe) == unlist(values$orig), "black", favoriteColor),nrow=nrows)
            } else {
              colors <- matrix(rep("black",dim(dataframe)[1]*dim(dataframe)[2]),nrow=nrows,byrow=TRUE)
            }
            title <- ifelse(is.randomizedSet,
                            tr("Randomized data"),
                               tr("Original data"))
            xmin <- par("usr")[1]
            xmax <- par("usr")[2]
            ymin <- par("usr")[3]
            ymax <- par("usr")[4]
            xsize <- xmax-xmin
            ysize <- ymax-ymin
            textplot(dataframe,col.data=colors,show.rownames=TRUE,show.colnames=TRUE,
                     col.rownames= "grey90", wrap=TRUE, wrap.colnames = 2, cmar = 1,
                     rmar = 0.05*(ysize/nrows), cex = 18*(ysize/nrows))
            text((xmin+xmax)/2, ysize-(0.06*ysize), labels=title, cex=1.2, font=2)
            rect(xmin, ymin ,xmax, ysize-(0.08*ysize), xpd=TRUE) 

          }
          distPlot <- function(xmin,xmax,binsize){
            par(mar = c(4,3,2,0))
            Rsampling::dplot(dist = as.numeric(values$saveDist), svalue = values$origStat, 
                             extreme = FALSE, vline = TRUE, rejection = FALSE, 
                             breaks = seq(xmin,xmax,binsize), xlim=c(xmin,xmax), ylim=c(0,30),
                             main=tr("distplot_title"), xlab=tr("Statistic of interest"), ylab=tr("Frequency")) 
          }
          #################################################################
          ###Mangrove trees
          mangBoxPlot <- function(dataframe,stat,is.randomizedSet){
            par(mar = c(4,4,2,2))
            title <- ifelse(is.randomizedSet,
                            tr("Randomized data"),
                            tr("Original data"))
            color <- ifelse(is.randomizedSet,favoriteColor,"grey")
            textcolor <- ifelse(is.randomizedSet,"black","red")
            boxplot(root ~ soil.instability, data=dataframe, type="n",
                    main=title, col=color,
                    xlab=tr("Soil instability"), ylab=tr("area covered by aerial root (m2)"))             
            text(2,33,paste(tr("meandiff="),round(stat,4)),col=textcolor)
          }
          #Randomization output
          randomizedMang <- reactive({
            input$mangRand # triggers the calculations when the "Do it again!" button is pressed
            Rsampling::normal_rand(dataframe = rhyzophora, cols = 3)
          })
          # Display data set as a plot
          ## Note that the original data set is stored in this block
          output$mangTable <- renderPlot({
            data <- rhyzophora[,c(1,3)]
            data$root <- round(data$root,3)
            isolate(values$orig <- data)
            tablePlot(values$orig, is.randomizedSet=FALSE)     
          })         
          output$mangTableRandom <- renderPlot({
            thisDf <- randomizedMang()   
            data <- thisDf[,c(1,3)]
            data$root <- round(data$root,3)      
            tablePlot(data, is.randomizedSet=TRUE)    
          })
          # Specific plots for this dataset
          ## Note that the statistic of interest for the original data set is stored in this block
          output$mangPlot <- renderPlot({
            isolate(values$origStat <- mean(rhyzophora[ which(rhyzophora$soil.instability=='high'),]$root)-
                      mean(rhyzophora[ which(rhyzophora$soil.instability=='medium'),]$root))
            mangBoxPlot(rhyzophora,values$origStat,is.randomizedSet=FALSE)
          })          
          output$mangPlotRandom <- renderPlot({
            thisDf <- randomizedMang()
            thisStat <- mean(thisDf[ which(thisDf$soil.instability=="high"),]$root)-
             mean(thisDf[ which(thisDf$soil.instability=="medium"),]$root)
             isolate(values$saveDist <- rbind(values$saveDist, thisStat))
            mangBoxPlot(thisDf,thisStat,is.randomizedSet=TRUE)
          })
          # Histrogram of statistics from the the randomized data sets
          output$distPlotMang <- renderPlot({
            distPlot(xmin=-15,xmax=15,binsize=2)              
          })
          ######################################################################
          ###Balanced mangrove trees          
          rhyzScatterPlot <- function(dataframe,stat,is.randomizedSet){
            par(mar = c(4,4,2,2))
            title <- ifelse(is.randomizedSet,
                            tr("Randomized data"),
                            tr("Original data"))
            color <- ifelse(is.randomizedSet,favoriteColor,"grey")
            textcolor <- ifelse(is.randomizedSet,"black","red")
            plot(n.roots ~ canopy.trunk, data=dataframe, pch=19,
                 main=title, col=color,
                 xlab=tr("canopy area / trunk area"), ylab=tr("number of roots"))             
            abline(lm(n.roots ~ canopy.trunk, data=dataframe))
            text(3000,150,paste(tr("slope ="),round(stat,4)),col=textcolor)
          }
          #Randomization output
          randomizedRhyz <- reactive({
            input$rhyzRand # triggers the calculations when the "Do it again!" button is pressed
            Rsampling::normal_rand(dataframe = rhyzophora, cols = 4)
          })   
          # Display data set as a plot
          output$rhyzTable <- renderPlot({
            data <- rhyzophora[,c(2,4)]
            data$canopy.trunk <- round(data$canopy.trunk,3)
            isolate(values$orig <- data)            
            tablePlot(values$orig, is.randomizedSet=FALSE)     
          })
          output$rhyzTableRandom <- renderPlot({
            thisDf <- randomizedRhyz()   
            data <- thisDf[,c(2,4)]
            data$canopy.trunk <- round(data$canopy.trunk,3)    
            tablePlot(data, is.randomizedSet=TRUE)    
          })       
          # Specific plots for this dataset
          output$rhyzPlot <- renderPlot({
            isolate(values$origStat <- coef(lm(n.roots ~ canopy.trunk, data=rhyzophora))[2])
            rhyzScatterPlot(rhyzophora,values$origStat,is.randomizedSet=FALSE)
          })
          output$rhyzPlotRandom <- renderPlot({
            thisDf <- randomizedRhyz()
            thisStat <- coef(lm(n.roots ~ canopy.trunk, data=thisDf))[2]
            isolate(values$saveDist <- rbind(values$saveDist, thisStat))
            rhyzScatterPlot(thisDf,thisStat,is.randomizedSet=TRUE)
          })
          # Histrogram of statistics from the the randomized data sets
          output$distPlotRhyz <- renderPlot({
            distPlot(xmin=-0.04,xmax=0.04,binsize=0.005)             
          })      
          ###########################################################################
          ###Azteca
          aztPairedPlot <- function(dataframe,stat,is.randomizedSet){
            par(mar = c(4,4,2,2))
            title <- ifelse(is.randomizedSet,
                            tr("Randomized data"),
                               tr("Original data"))
            color <- ifelse(is.randomizedSet,favoriteColor,"grey")
            textcolor <- ifelse(is.randomizedSet,"black","red")
            splot(dataframe$extract.new,dataframe$extract.old, col.dif = c(color, color),
                  main=tr("Original data"), pch=19,
                  xlab=tr("Treatment"), ylab=tr("Number of recruited ants"), xaxt='n')
            mtext(tr("Extract of new leaves"),1, at=1, line=1.5)
            mtext(tr("Extract of old leaves"),1, at=2, line=1.5)
            text(1.5,46,paste(tr("meandiff="),
                              round(stat,4)),col=textcolor)            
          }
          #Randomization output
          randomizedAzt <- reactive({
            input$aztRand # triggers the calculations when the "Do it again!" button is pressed
            Rsampling::within_rows(dataframe = azteca, cols = c(2,3))
          })
          ### Display data set as a plot
          output$aztTable <- renderPlot({
            data <- azteca
            isolate(values$orig <- data)            
            tablePlot(values$orig, is.randomizedSet=FALSE)     
          })
          output$aztTableRandom <- renderPlot({
            thisDf <- randomizedAzt()
            tablePlot(thisDf, is.randomizedSet=TRUE) 
          })
          ### Specific plots for this dataset          
          output$aztPlot <- renderPlot({
            isolate(values$origStat <- mean(azteca$extract.new)-mean(azteca$extract.old))
            aztPairedPlot(azteca,values$origStat,is.randomizedSet=FALSE)
          })
          output$aztPlotRandom <- renderPlot({
            thisDf <- randomizedAzt()
            thisStat <- mean(thisDf$extract.new)-mean(thisDf$extract.old)
            isolate(values$saveDist <- rbind(values$saveDist, thisStat))
            aztPairedPlot(thisDf,thisStat,is.randomizedSet=TRUE)
          })
          ### Histrogram of statistics from the the randomized data sets
          output$distPlotAzt <- renderPlot({
            distPlot(xmin=-8,xmax=8,binsize=1)          
          })
})
