library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Rsampling!"),

	# Sidebar with a slider input for the number of bins
	sidebarLayout(
    sidebarPanel(
		  sliderInput("ntrials",
									"Number of trials:",
									min = 1000,
									max = 5000,
									value = 1500)
			),

  # Show a plot of the generated distribution
	  mainPanel(
		  plotOutput("distPlot")
    )
	)
))
