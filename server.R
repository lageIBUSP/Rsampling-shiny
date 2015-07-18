library(shiny)
library(gridExtra)
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
            # custom function handler: parses the text in the custom input
            custom <- function(dataframe) {
              eval(parse(text=input$customstat))
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
              if(input$stat == "meandifc") 
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
            vals<-reactiveValues()
            distribution <- observe({
              input$go # triggers the calculations when the "Update graph" is pressed
              # traps NA, NaN, NULL, Infin the statistic applied over the original data
              if ((is.null(svalue())) || (is.na(svalue()) | is.nan(svalue()) | !is.finite(svalue())))
                return (0);
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
              progress$set(message = "Sampling...", value = 0)
              pupdate <- function(x) 
                progress$set(value = x * progress$getMax(), 
                             detail=paste0(round(progress$getValue()), "%"))
              isolate({
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
              run_iter$resume()
            })
            run_iter <- observe({
              qry <- parseQueryString(session$clientData$url_search)
              if (input$go == 0 & is.null(qry$auto)) {
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
                write.csv(vals$distribution, file)
              }
            )
            # displays a warning in the case svalue() is not a single number
            output$svaluewarning <- renderText({
              s <- isolate(try(svalue(), silent=TRUE))
              input$gocustomstat
              input$stat
              if(!is.null(s) && length(s) > 1)
                return("WARNING, the statistic function should return a single number.")
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
              if (length(vals$distribution) == 1)
                  stop("Distribution calculation stopped with error!")
               Rsampling::dplot(dist = vals$x, svalue =  svalue(), pside= input$pside, 
                   extreme = input$extreme, vline = TRUE, rejection = input$rejection, ylim=c(0,vals$maxcount))
            })
            ### simply displays the statistic of interest
            output$stat <- renderText({
	            c(input$m1, input$m2)
              input$gocustomstat
              input$stat
              # to avoid weird things when length > 1
              s <- paste(round(isolate(svalue()), 3), collapse = " ")
              paste("Statistic of interest: ", s, "\n", sep="")
            })
            ### simply displays the "p-value"
            output$p <- renderText({
              side <- switch(input$pside, "Two sided" = "(two sided)", "(one sided)")
              p <- switch(input$pside, 
                          "Two sided" = abs(vals$distribution) >= abs(svalue()),
                          "Greater" = vals$distribution >= svalue(),
                          "Lesser" = vals$distribution <= svalue()
                          )
              p <- round(sum(p) / length(vals$distribution),3)
              paste(side, "p-value:", p)
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
                               'smean'=,'ssd'= "Variable column: ", 
                               'intercept'=,'slope'=,'corr'="Dependent variable column: ",
                               'meandif'=, 'Fstatistic'="Categorical variable column: ",
                               'meandifc'= "Before treatment")
              label2 <- switch(input$stat, 
                               'intercept'=,'slope'=,'corr'="Independent variable column: ",
                               'meandif'=, 'Fstatistic'="Numerical variable column: ",
                               'meandifc'= "After treatment: ")
              updateSelectInput(session, "m1", choices = cols, label = label1)
              updateSelectInput(session, "m2", choices = cols, selected=2, label = label2)
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
          values$saveDist <- list()
          observeEvent(input$clear1, {
            isolate(values$saveDist <- list())
          })
          observeEvent(input$clear2, {
            isolate(values$saveDist <- list())
          })
          observeEvent(input$clear3, {
            isolate(values$saveDist <- list())
          })
          observeEvent(input$tabEvent, {
            isolate(values$saveDist <- list())
          })
          #################################################################
          ###Mangrove trees
          mangStat <- mean(rhyzophora[ which(rhyzophora$soil.instability=='high'),]$root)-
            mean(rhyzophora[ which(rhyzophora$soil.instability=='medium'),]$root)
          ### Specific plots for this dataset
          output$mangPlot <- renderPlot({
            par(mar = c(4,4,2,2))
            boxplot(root ~ soil.instability, data=rhyzophora, type="n",
                    main="Original data",
                    xlab="Soil instability", ylab="area covered by aerial root (m2)")             
            text(2,33,paste("mean difference = \n",round(mangStat,4)),col="red")
          })
          output$mangPlotRandom <- renderPlot({
            thisSet <- randomizedMang()
            thisDf <- data.frame(matrix(unlist(thisSet), nrow=24))
            names(thisDf) <- c("si","canopy.trunk","root","n.roots")
            thisStat <- mean(thisDf[ which(thisDf$si==1),]$root)-
              mean(thisDf[ which(thisDf$si==2),]$root)
            isolate(values$saveDist <- rbind(values$saveDist, thisStat))
            par(mar = c(4,4,2,2))
            boxplot(root ~ si, data=thisDf, type="n",
                    main="Randomized data", col="#428bca",
                    xlab="Soil instability", ylab="area covered by aerial root (m2)")             
            text(2,33,paste("mean difference = \n",round(thisStat,4)))
          })
          ### Display data set as a plot
          output$mangTable <- renderPlot({
            data <- rhyzophora[,c(1,3)]
            data$root <- round(data$root,3)
            tbl <- tableGrob(data,
                             cols = c("soil.instability", 
                                      "roots"),
                             show.rownames =FALSE,
                             gpar.corefill = gpar(fill = "grey80", col = "white"))
            grid.draw(tbl)
          },height=500)
          output$mangTableRandom <- renderPlot({
            thisSet <- randomizedMang()
            thisDf <- data.frame(matrix(unlist(thisSet), nrow=24))
            names(thisDf) <- c("si","canopy.trunk","root","n.roots")
            thisDf$si[thisDf$si == 1] <- "high"
            thisDf$si[thisDf$si == 2] <- "medium"
            data <- thisDf[,c(1,3)]
            data$root <- round(data$root,3)   
            tbl <- tableGrob(data,
                             cols = c("soil.instability", 
                                      "roots"),
                             show.rownames =FALSE,
                             gpar.corefill = gpar(fill = "#428bca", col = "white"))
            grid.draw(tbl)
          },height=500)
          #Randomization output
          randomizedMang <- reactive({
            input$mangRand # triggers the calculations when the "..." is pressed
            Rsampling(type = "normal_rand", dataframe = rhyzophora,
                      statistics = function(dataframe) return(dataframe), cols = 3,
                      ntrials = 1)
          })
          ### main plot of the program: generates a histogram of distribution()
          output$distPlotMang <- renderPlot({
            par(mar = c(4,3,2,0))
            Rsampling::dplot(dist = as.numeric(values$saveDist), svalue = mangStat, 
                             extreme = FALSE, vline = TRUE, rejection = FALSE, 
                             breaks = seq(-15,15,2), xlim=c(-15,15))                
          })
          ######################################################################
          ###More mangrove trees
          rhyzSlope <- coef(lm(n.roots ~ canopy.trunk, data=rhyzophora))[2]
          ### Specific plots for this dataset
          output$rhyzPlot <- renderPlot({
            par(mar = c(4,4,2,2))
            plot(n.roots ~ canopy.trunk, data=rhyzophora, pch=19,
                 main="Original data",
                 xlab="canopy area / trunk area", ylab="number of roots")             
            abline(lm(n.roots ~ canopy.trunk, data=rhyzophora))
            text(3000,150,paste("slope =",round(rhyzSlope,4)),col="red")
          })
          output$rhyzPlotRandom <- renderPlot({
            thisSet <- randomizedRhyz()
            thisDf <- data.frame(matrix(unlist(thisSet), nrow=24))
            names(thisDf) <- c("si","canopy.trunk","root","n.roots")
            thisSlope <- coef(lm(n.roots ~ canopy.trunk, data=thisDf))[2]
            isolate(values$saveDist <- rbind(values$saveDist, thisSlope))
            par(mar = c(4,4,2,2))
            plot(n.roots ~ canopy.trunk, data=thisDf, pch=19,
                 main="Randomized data", col="#428bca",
                 xlab="canopy area / trunk area", ylab="number of roots")
            abline(lm(n.roots ~ canopy.trunk, data=thisDf))
            text(3000,150,paste("slope =",round(thisSlope,4)))
          })
          output$rhyzTable <- renderPlot({
            data <- rhyzophora[,c(2,4)]
            data$canopy.trunk <- round(data$canopy.trunk,2)
            tbl <- tableGrob(data,
                             cols = c("canopy.trunk", 
                                      "n.roots"),
                             show.rownames =FALSE,
                             gpar.corefill = gpar(fill = "grey80", col = "white"))
            grid.draw(tbl)
          },height=500)
          output$rhyzTableRandom <- renderPlot({
            thisSet <- randomizedRhyz()
            thisDf <- data.frame(matrix(unlist(thisSet), nrow=24))
            names(thisDf) <- c("si","canopy.trunk","root","n.roots")
            data <- thisDf[,c(2,4)]
            data$canopy.trunk <- round(data$canopy.trunk,2)   
            tbl <- tableGrob(data,
                             cols = c("canopy.trunk", 
                                      "n.roots"),
                             show.rownames =FALSE,                               
                             gpar.corefill = gpar(fill = "#428bca", col = "white"))
            grid.draw(tbl)
          },height=500)
          #Randomization output
          randomizedRhyz <- reactive({
            input$rhyzRand # triggers the calculations when the "..." is pressed
            Rsampling(type = "normal_rand", dataframe = rhyzophora,
                      statistics = function(dataframe) return(dataframe), cols = 4,
                      ntrials = 1)
          })
          ### main plot of the program: generates a histogram of distribution()
          output$distPlotRhyz <- renderPlot({
            par(mar = c(4,3,2,0))
            Rsampling::dplot(dist = as.numeric(values$saveDist), svalue = rhyzSlope, 
                             extreme = FALSE, vline = TRUE, rejection = FALSE, 
                             breaks = seq(-0.04,0.04,0.005), xlim=c(-0.04,0.04))                
          })      
          ###########################################################################
          ###Azteca
          ### Specific plots for this dataset
          aztStat <- mean(azteca$extract.new)-mean(azteca$extract.old)
          output$aztPlot <- renderPlot({
            par(mar = c(4,4,2,2))
            plot(c(azteca$extract.new[1], azteca$extract.old[1]), type="o",
                 main="Original data", ylim = c(0,60), xlim=c(0.9,2.1), pch=19,
                 xlab="Treatment", ylab="Number of recruited ants", xaxt='n')
            for(i in 2:dim(azteca)[1])
            {
              points(c(azteca$extract.new[i], azteca$extract.old[i]), pch=19,type="o")
            }
            mtext("Extract of \n new leaves",1, at=1, line=1.5)
            mtext("Extract of \n old leaves",1, at=2, line=1.5)
            text(1.8,52,paste("mean difference = \n",
                              round(aztStat,4)),col="red")
          })
          output$aztPlotRandom <- renderPlot({
            thisSet <- randomizedAzt()
            thisDf <- data.frame(matrix(unlist(thisSet), nrow=21))
            names(thisDf) <- c("plant","extract.new","extract.old")
            thisStat <- mean(thisDf$extract.new)-mean(thisDf$extract.old)
            isolate(values$saveDist <- rbind(values$saveDist, thisStat))
            par(mar = c(4,4,2,2))
            plot(c(thisDf$extract.new[1], thisDf$extract.old[1]), type="o",
                 main="Randomized data", ylim = c(0,60), xlim=c(0.9,2.1), pch=19,
                 col="#428bca",
                 xlab=tr("treatment",lg), ylab="Number of recruited ants", xaxt='n')
            for(i in 2:dim(azteca)[1])
            {
              points(c(thisDf$extract.new[i], thisDf$extract.old[i]), pch=19,type="o",
                     col="#428bca")
            }
            mtext("Extract of \n new leaves",1, at=1, line=1.5)
            mtext("Extract of \n old leaves",1, at=2, line=1.5)
            text(1.8,52,paste("mean difference = \n",
                              round(thisStat,4)))
          })
          ### Display data set as a plot
          output$aztTable <- renderPlot({
            data <- azteca
            tbl <- tableGrob(data,
                             cols = c("plant", 
                                      "extract.new",
                                      "extract.old"),
                             show.rownames =FALSE,
                             gpar.corefill = gpar(fill = "grey80", col = "white"))
            grid.draw(tbl)
          },height=500)
          output$aztTableRandom <- renderPlot({
            thisSet <- randomizedAzt()
            thisDf <- data.frame(matrix(unlist(thisSet), nrow=21))
            data <- thisDf
            tbl <- tableGrob(data,
                             cols = c("plant", 
                                      "extract.new",
                                      "extract.old"),
                             show.rownames =FALSE,
                             gpar.corefill = gpar(fill = "#428bca", col = "white"))
            grid.draw(tbl)
          },height=500)
          #Randomization output
          randomizedAzt <- reactive({
            input$aztRand # triggers the calculations when the "Do it again!" button is pressed
            Rsampling(type = "within_rows", dataframe = azteca,
                      statistics = function(dataframe) return(dataframe), cols = c(2,3),
                      ntrials = 1)
          })
          ### main plot of the program: generates a histogram of distribution()
          output$distPlotAzt <- renderPlot({
            par(mar = c(4,3,2,0))
            Rsampling::dplot(dist = as.numeric(values$saveDist), svalue = aztStat, 
                             extreme = FALSE, vline = TRUE, rejection = FALSE, 
                             breaks = seq(-8,8,1), xlim=c(-8,8))                
          })          
})
