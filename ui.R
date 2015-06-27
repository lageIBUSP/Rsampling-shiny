library(shiny)
shinyUI(fluidPage(
			tabsetPanel(type="tabs",
				tabPanel("Help/info",
					h2("Rsampling - resampling statistics in R"),
					h4("powered by ", a("Shiny", href="http://www.rstudio.com/shiny")),
					includeHTML("help.html")
				),
			  tabPanel("Data input", 
					helpText("Use this tab to select the input data for your analysis. The first options are datasets included in the library, select \"custom\" to upload your own file."),
					selectInput("datasource",
											"What is your input data?",
											choices = c("embauba", "azteca", "peucetia", "rhyzophora", "custom")
										 ),
					conditionalPanel(
						fileInput("file", "Choose CSV file:", accept='.csv'),
						checkboxInput("header", "Header?"),
						helpText("Make sure that the data is correctly interpreted in the display below!", style="color:#f30;"),
						condition="input.datasource	== 'custom'"
													 ),
#					submitButton("Go!"),
					helpText("The first rows of the selected data table are:"),
					tableOutput("view")
			  ),
				tabPanel("Statistics",
	sidebarLayout(
								# another panel for data input??
    sidebarPanel(
			# statistic: from a dropdown or write your own
			selectInput("stat", "Statistic:", choices=c("Mean difference")), # TODO: how to add more stats??
			helpText("Use one of the preset statistics or write your own."),
			selectInput("type", "Randomization type:", 
				choices=c("Normal shuffle", "Within rows", "Whithin columns", 
									"Rows as units", "Columns as units")
			),
			checkboxInput("replace", "Replace?"),
			helpText("See the help page for details on the different randomization types."),
		  sliderInput("ntrials", "Number of trials:", min=100,max=5000,value=300,step=100),
			actionButton("go", "Update Graph")
			),
	  mainPanel(
		      plotOutput("distPlot"),
			    h3(textOutput("stat")),
			    h3(textOutput("p"))
			)
    )
	)
))
)
