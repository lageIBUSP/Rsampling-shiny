library(shiny)
library(Rsampling)
shinyServer(function(input, output) {
	meandif <- function(dataframe){
		props <- unlist(tapply(dataframe[,input$s2], dataframe[,input$s1], mean))
		props[1] - props[length(props)]
	}
	statistic <- reactive({
		switch(input$stat,
					 "meandif" = meandif
					)
	})
	svalue <- reactive({
		f <- statistic()
		f(data())
	})
	csvfile <- reactive({
		if(is.null(input$file)) return (data.frame());
		read.csv(input$file$datapath, header=input$header)
	})
	data <- reactive({
		switch(input$datasource,
					 "embauba" = embauba,
					 "azteca" = azteca,
					 "peucetia" = peucetia,
					 "rhyzophora" = rhyzophora,
					 "custom" = csvfile())
	})
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
							 statistics = statistic(), cols = 2, 
							 ntrials = isolate(input$ntrials), 
							 replace=isolate(input$replace))
	})
	output$view <- renderTable({
		head(data(), 15)
	})
  output$distPlot <- renderPlot({
		mydist <- distribution()
		maxx <- max(abs(mydist))
		line <- svalue(); if(abs(line) > maxx) maxx = abs(line); 
		oh <- hist(mydist, xlim=1.1*c(-maxx, maxx), main = "Distribution of the statistic of interest", col="skyblue", border="white", xlab="Statistic of interest")
		# adds the extreme values in orange
		mydist <- mydist[abs(mydist) >= abs(line)]
		if(length(mydist)>0) 
			hist(mydist, xlim=1.1*c(-maxx,maxx), col="orange1", border="white", 
					 add=TRUE, breaks = oh$breaks)
		abline(v = line, lty=2, col="red")
	})
	output$stat <- renderText({
		paste("Statistic of interest: ", round(svalue(),3),"\n", sep="")
	})
	output$p <- renderText({
		paste("(two sided) p-value: ", round(sum(abs(distribution()) >= abs(svalue())) / length(distribution()),3), "\n", sep="")
	})
})
