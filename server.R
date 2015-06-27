library(shiny)
library(Rsampling)
shinyServer(function(input, output) {
	emb.ei <- function(dataframe){
		props <- unlist(tapply(dataframe[,2], dataframe[,1], mean))
		props[1] - props[length(props)]
	}
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
		type = switch(input$type,
									"Normal shuffle" = "normal_rand",
									"Rows as units" = "rows_as_units",
									"Columns as units" = "columns_as_units",
									"Within rows" = "within_rows",
									"Within columns" = "within_columns"
									)
		Rsampling(type = type, dataframe = data(),
							 statistics = emb.ei, cols = 2, ntrials = input$ntrials, replace=input$replace)
	})
	output$view <- renderTable({
		head(data(), 15)
	})
  output$distPlot <- renderPlot({
		maxx <- max(abs(distribution()))
		line <- emb.ei(data()); if(abs(line) > maxx) maxx = abs(line); 
		hist(distribution(), xlim=1.1*c(-maxx, maxx), main = "Statistic of interest", col="skyblue", border="white")
		abline(v = emb.ei(data()), lty=2, col="red")
	})
	output$stat <- renderText({
		paste("Statistic of interest: ", round(emb.ei(data()),3),"\n", sep="")
	})
	output$p <- renderText({
		paste("(two sided) p-value: ", round(sum(abs(distribution()) >= abs(emb.ei(data()))) / length(distribution()),3), "\n", sep="")
	})
})
