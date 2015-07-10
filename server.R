library(shiny)
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
            distribution <- reactive({
              input$go # triggers the calculations when the "Update graph" is pressed
              # traps NA, NaN, NULL, Infin the statistic applied over the original data
              if ((is.null(svalue())) || (is.na(svalue()) | is.nan(svalue()) | !is.finite(svalue())))
                return (0);
              type = switch(isolate(input$type),
                            "Normal shuffle" = "normal_rand",
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
              Rsampling(type = type, dataframe = data(),
                        statistics = statistic(), cols = cols(),
                        stratum = isolate(stratum()),
                        ntrials = isolate(input$ntrials), 
                        replace=isolate(input$replace),
                        progress = pupdate)
            })
            ###########################################
            ####### OUTPUT GENERATING FUNCTIONS #######
            ###########################################
            output$download <- downloadHandler(
              filename=function() "Rsampling.csv",
              content=function(file) {
              write.csv(distribution(), file)
            })
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
              if(! require(Rsampling)) return ("notinstalled")
              else if(packageDescription("Rsampling")$Version != "0.0.0.3") return ("incompatible")
              else return ("ok")
            })
            # see: http://stackoverflow.com/questions/19686581/make-conditionalpanel-depend-on-files-uploaded-with-fileinput
            outputOptions(output, 'needinstall', suspendWhenHidden=FALSE)
            ### simple table display to see the contents of the data selected
            output$view <- renderTable({
              head(data(), 15)
            })
            ### main plot of the program: generates a histogram of distribution()
            output$distPlot <- renderPlot({
              mydist <- distribution()
              # Traps errors
              if (length(mydist) == 1)
                stop("Distribution calculation stopped with error!")
              Rsampling::dplot(dist = mydist, svalue = svalue(), pside= input$pside, 
                    extreme = input$extreme, vline = TRUE, rejection = input$rejection)
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
                          "Two sided" = abs(distribution()) >= abs(svalue()),
                          "Greater" = distribution() >= svalue(),
                          "Lesser" = distribution() <= svalue()
                          )
              p <- round(sum(p) / length(distribution()),3)
              paste(side, "p-value:", p)
            })
            ### Updates the values in the dropdowns for column selection
            observe({
              # Check to see if there is any data (may fail during file upload 
              # or if Rsampling is not installed)
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
})
