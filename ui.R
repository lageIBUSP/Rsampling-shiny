library(shiny)
shinyUI(fluidPage(
			tabsetPanel(type="tabs",
				tabPanel("Help/info",
					h2("Rsampling - resampling statistics in R"),
					h4("powered by ", a("Shiny", href="http://www.rstudio.com/shiny"))
				),
			  tabPanel("Data input", 
					selectInput("datasource",
											"What is your input data?",
											choices = c("embauba","azteca", "peucetia", "rhyzophora")
										 ),
					submitButton("Go!"),
					tableOutput("view")
			  ),
				tabPanel("Statistics",
	sidebarLayout(
								# another panel for data input??
    sidebarPanel(
			# statistic: from a dropdown or write your own
			selectInput("stat", "Statistic:", choices=c("Mean difference")), # TODO: add more stats??
			helpText("Use one of the preset statistics or write your own."),
			selectInput("type", "Randomization type:", 
				choices=c("Normal shuffle", "Within rows", "Whithin columns", 
									"Rows as units", "Columns as units")
			),
			checkboxInput("replace", "Replace?"),
			helpText("See the help page for details on the different randomization types."),
		  sliderInput("ntrials", "Number of trials:", min=100,max=5000,value=300,step=100),
			submitButton("Update Graph")
			),

  # Show a plot of the generated distribution
	  mainPanel(
		      plotOutput("distPlot"),
			    h3(textOutput("stat")),
			    h3(textOutput("p"))
			)
    )
	)
))
)
