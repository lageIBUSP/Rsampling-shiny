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
              props <- unlist(tapply(dataframe[,as.numeric(input$s2)], dataframe[,as.numeric(input$s1)], mean))
              props[1] - props[length(props)]
            }
            meandifc <- function(dataframe){
              dif <- dataframe[, as.numeric(input$d2)] - dataframe[, as.numeric(input$d1)]
              mean(dif)
            }
            srow <- function(dataframe) {
              mean(apply(dataframe, 1, sum))
            }
            scol <- function(dataframe) {
              mean(apply(dataframe, 2, sum))
            }
            intercept <- function(dataframe) {
              coef(lm(dataframe[, as.numeric(input$r1)] ~ dataframe[, as.numeric(input$r2)]))[1]
            }
            slope <- function(dataframe) {
              coef(lm(dataframe[, as.numeric(input$r1)] ~ dataframe[, as.numeric(input$r2)]))[2]
            }
            corr <- function(dataframe) {
              cor(dataframe[, as.numeric(input$r1)], dataframe[, as.numeric(input$r2)])
            }
            # accessory for the custom function handler 
            # (for a slight performance improvement over parsing everytime)
            pcustom <- reactive({
              input$gocustomstat
              parse(text=isolate(input$customstat))
            })
            custom <- function(my.df) {
              dataframe <- as.data.frame(my.df)
              # Removes the spurious stratum column included by Rsampling routines
              if (colnames(dataframe)[1] == "stratum")
                dataframe <- dataframe[,-1]
              eval(pcustom())
            }
            # what columns should be randomized?
            cols <- reactive({
              if(input$stat %in% c("smean", "ssd")) # the "data" column is indicated by m1
                return(as.numeric(input$m1))
              if(input$stat == "meandif") # the "data" column is indicated by s2
                return(as.numeric(input$s2))
              if(input$stat == "meandifc") # the before and after columns are d1 and d2
                return(c(as.numeric(input$d1), as.numeric(input$d2)))
              if(input$stat %in% c("scol", "srow", "custom")) # all columns should be used
                return(1:ncol(data()))
              if(input$stat %in% c("slope", "intercept", "corr")) # the independent variable is r1
                return(as.numeric(input$r1))
              #else?
              return(NULL)
            })
            stratum <- reactive({
              if (input$stratum == FALSE)
                return (rep(1, nrow(data())))
              #else
              d <- data()
              return (d[, as.numeric(input$stratumc)])
            })
            ### this reactive simply translates the input value into the corresponding function
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
              Rsampling(type = type, dataframe = data(),
                        statistics = statistic(), cols = cols(),
                        stratum = isolate(stratum()),
                        ntrials = isolate(input$ntrials), 
                        replace=isolate(input$replace))
            })
            ###########################################
            ####### OUTPUT GENERATING FUNCTIONS #######
            ###########################################
            ### tries to install the Rsampling package
            output$pkginstall <- renderText({
              # runs when the install button is pressed
              if (input$installbutton > 0) {
                if(!require(devtools))
                   install.packages("devtools")
                library(devtools)
                install_github(repo = 'lageIBUSP/Rsampling')
                if(require(Rsampling))
                  return("Installation complete!")
                else
                  return("Installation error!")
              }
            })
            # displays a warning in the case svalue() is not a single number
            output$svaluewarning <- renderText({
              if(!is.null(svalue()) && length(svalue()) > 1)
                return("WARNING, the statistic function should return a single number.")
              return("")
            })
            output$needinstall <- reactive({
              ! require(Rsampling)
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
              dplot(dist = mydist, svalue = svalue(), pside= input$pside, 
                    extreme = input$extreme, vline = TRUE, rejection = input$rejection)
            })
            ### simply displays the statistic of interest
            output$stat <- renderText({
              s <- paste(round(svalue(), 3), collapse = " ") # to avoid weird things when length > 1
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
              # Check to see if there is any data (may fail during file upload)
              if(!ncol(data())) return();
              cols <- 1:length(colnames(data()))
              names(cols) <- colnames(data())
              updateSelectInput(session, "m1", choices = cols)
              updateSelectInput(session, "r1", choices = cols)
              updateSelectInput(session, "r2", choices = cols, selected=2)
              updateSelectInput(session, "s1", choices = cols)
              updateSelectInput(session, "s2", choices = cols, selected=2)
              updateSelectInput(session, "d1", choices = cols)
              updateSelectInput(session, "d2", choices = cols, selected=2)
              updateSelectInput(session, "stratumc", choices = cols)
            })
})
