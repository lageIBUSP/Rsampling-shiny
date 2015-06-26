library(shiny)
library(Rsampling)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Expression that generates a histogram. The expression is
	# wrapped in a call to renderPlot to indicate that:
	#
	#  1) It is "reactive" and therefore should be automatically
	#     re-executed when inputs change
	#  2) Its output type is a plot
  output$distPlot <- renderPlot({
		emb.ei <- function(dataframe){
			props <- tapply(dataframe$with.vines, dataframe$morphotype, mean)
			props[[1]] - props[[2]]
		}
		emb.r <- Rsampling(type = "normal", dataframe = embauba,
											 statistics = emb.ei, cols = 2, ntrials = input$ntrials)
#		x    <- faithful[, 2]  # Old Faithful Geyser data
#		bins <- seq(min(x), max(x), length.out = input$bins + 1)

		# draw the histogram with the specified number of bins
		hist(emb.r, xlim=c(-0.5, 0.5), xlab = "Estatística de interesse", col="skyblue", border="white")
		abline(v = emb.ei(embauba), lty=2, col="red")
	})
})
