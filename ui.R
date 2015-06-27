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
    			helpText("Next, we need to determine what is the function (i.e., the statistic) that will be applied to the data. Use one of the preset statistics or write your own."),
			    selectInput("stat", "Statistic:", choices=c("Mean difference" = "meandif")),
					### Panel for mean diff:
					conditionalPanel(
						helpText("This function splits the data acording to a categorical variable. Then it calculates 
										 the mean for each group, and subtracts one from another. Note that this is designed 
										 to work with only ",em("TWO")," categories!"),
						numericInput("s1", "Categorical variable column: ", 1),
						numericInput("s2", "Numerical variable column: ", 2),
								condition="input.stat== 'meandif'"
					),
					helpText("Below you see the result of this function applied to the original data:"),
			    h3(textOutput("stat"))
				),
				tabPanel("Resampling",
	sidebarLayout(
    sidebarPanel(
			helpText("Here is where we do the randomization!"),
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
					helpText("The graph above shows the distribution of your selected statistic after repeated 
									 randomization from your data. The histograms bins in orange (if any) represent those
									 simulations in which the statistic had a value that's ", em("equal to or more extreme"),
									 " than the statistic calculated on your original data (represented by the dotted red line).
									 This leads to a p-value of:"),
			    h3(textOutput("p"))
			)
    )
	)
))
)
