library(shiny)
library(Rsampling)
shinyServer(function(input, output) {
	emb.ei <- function(dataframe){
		props <- tapply(dataframe[,2], dataframe[,1], mean)
		props[[1]] - props[[2]]
	}
	distribution <- reactive({
		type = switch(input$type,
									"Normal shuffle" = "normal_rand",
									"Rows as units" = "rows_as_units",
									"Columns as units" = "columns_as_units",
									"Within rows" = "within_rows",
									"Within columns" = "within_columns"
									)
		Rsampling(type = type, dataframe = embauba,
							 statistics = emb.ei, cols = 2, ntrials = input$ntrials, replace=input$replace)
	})
  output$distPlot <- renderPlot({
		hist(distribution(), xlim=c(-0.5, 0.5), main = "Interest statistic", col="skyblue", border="white")
		abline(v = emb.ei(embauba), lty=2, col="red")
	})
	output$stat <- renderText({
		paste("Interest statistic: ", round(emb.ei(embauba),3),"\n", sep="")
	})
	output$p <- renderText({
		paste("p-value: ", sum(distribution() >= emb.ei(embauba)) / length(distribution()), "\n", sep="")
	})
})
