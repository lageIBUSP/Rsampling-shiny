library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Rsampling!"),

	# Sidebar with a slider input for the number of bins
	sidebarLayout(
								# another panel for data input??
    sidebarPanel(
			# checkbox: replace?
			selectInput("type", "Randomization type:", 
				choices=c("Normal shuffle", "Within rows", "Whithin columns", 
									"Rows as units", "Columns as units")
				),
		  numericInput("ntrials", "Number of trials:", 100)
			# statistic: from a dropdown or write your own
			# checkbox: simplify?
			),

  # Show a plot of the generated distribution
	  mainPanel(
		  plotOutput("distPlot"),
			h3(textOutput("stat")),
			h3(textOutput("p"))
    )
	)
))
