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
			checkboxInput("replace", "Replace?"),
		  numericInput("ntrials", "Number of trials:", 100, increment=100),
			# statistic: from a dropdown or write your own
			# checkbox: simplify?

			      helpText("Note: while the data view will show only the specified",
										                "number of observations, the summary will still be based",
																		               "on the full dataset."),
								       
								       submitButton("Update Graph")
			),

  # Show a plot of the generated distribution
	  mainPanel(
			tabsetPanel(type="tabs",
				tabPanel("Help/info",
					h2("Rsampling - resampling statistics in R"),
					h4("powered by ", a("Shiny", href="http://www.rstudio.com/shiny"))
				),
			  tabPanel("Data input", 
          numericInput("dx", "Ahm whatever:", 100)
			  ),
				tabPanel("Graphs",
		      plotOutput("distPlot"),
			    h3(textOutput("stat")),
			    h3(textOutput("p"))
				)
			)
    )
	)
))
