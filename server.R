library(shiny)
library(Rsampling)
shinyServer(function(input, output) {
	###########################################
	### INTERNAL OBJECTS AND INPUT HANDLING ###
	###########################################
	#### Selection of prebuilt statistic functions
	meandif <- function(dataframe){
		props <- unlist(tapply(dataframe[,input$s2], dataframe[,input$s1], mean))
		props[1] - props[length(props)]
	}
	meandifc <- function(dataframe){
		dif <- dataframe[, input$d2] - dataframe[, input$d1]
		mean(dif)
	}
	# what columns should be randomized?
	cols <- reactive({
		if(input$stat == "meandif") # the "data" column is indicated by s2
			return(input$s2)
		if(input$stat == "meandifc") # the before and after columns are d1 and d2
			return(c(input$d1, input$d2))
		#else?
		  return(NULL)
	})
	### this reactive simply translates the input value into the corresponding function
	statistic <- reactive({
		switch(input$stat,
					 "meandif" = meandif,
					 "meandifc" = meandifc
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
		read.csv(input$file$datapath, header=input$header)
	})
	### translates the input value for data source into the corresponding R object
	### in the case "custom", it reads the csv using the csvfile() reactive
	data <- reactive({
		switch(input$datasource,
					 "embauba" = embauba,
					 "azteca" = azteca,
					 "peucetia" = peucetia,
					 "rhyzophora" = rhyzophora,
					 "custom" = csvfile())
	})
	### calculates the distribution of the statistic of interest using Rsampling
	### several of its arguments are isolate()'d, meaning that changing them will
	### trigger a recalculation of the statistic (for performance reasons)
	distribution <- reactive({
		input$go # triggers the calculations when the "Update graph" is pressed
		type = switch(isolate(input$type),
									"Normal shuffle" = "normal_rand",
									"Rows as units" = "rows_as_units",
									"Columns as units" = "columns_as_units",
									"Within rows" = "within_rows",
									"Within columns" = "within_columns"
									)
		Rsampling(type = type, dataframe = data(),
							 statistics = statistic(), cols = cols(),
							 ntrials = isolate(input$ntrials), 
							 replace=isolate(input$replace))
	})
	###########################################
	####### OUTPUT GENERATING FUNCTIONS #######
	###########################################
	### simple table display to see the contents of the data selected
	output$view <- renderTable({
		head(data(), 15)
	})
	### main plot of the program: generates a histogram of distribution()
  output$distPlot <- renderPlot({
		mydist <- distribution()
		# what should be the xlim?
		maxx <- max(abs(mydist))
		line <- svalue(); if(abs(line) > maxx) maxx = abs(line); 
		# draws the histogram
		oh <- hist(mydist, xlim=1.1*c(-maxx, maxx), main = "Distribution of the statistic of interest", col="skyblue", border="white", xlab="Statistic of interest")
		# adds the extreme values in orange
		mydist <- mydist[abs(mydist) >= abs(line)]
		if(length(mydist)>0) 
			hist(mydist, xlim=1.1*c(-maxx,maxx), col="orange1", border="white", 
					 add=TRUE, breaks = oh$breaks)
		# vertical line with the original statistic
		abline(v = line, lty=2, col="red")
	})
	### simply displays the statistic of interest
	output$stat <- renderText({
		paste("Statistic of interest: ", round(svalue(),3),"\n", sep="")
	})
	### simply displays the "p-value"
	output$p <- renderText({
		paste("(two sided) p-value: ", round(sum(abs(distribution()) >= abs(svalue())) / length(distribution()),3), "\n", sep="")
	})
})
