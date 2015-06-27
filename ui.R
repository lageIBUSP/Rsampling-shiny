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
             ## the next panel only shows for the custom datasource
             conditionalPanel(
               fileInput("file", "Choose CSV file:", accept='.csv'),
               checkboxInput("header", "Header?"),
               helpText("Make sure that the data is correctly interpreted in the display below!", style="color:#f30;"),
               condition="input.datasource	== 'custom'"
             ),
             helpText("The first rows of the selected data table are:"),
             tableOutput("view")
             ),
    tabPanel("Statistics",
             helpText("Next, we need to determine what is the function (i.e., the statistic) that will be applied to the data. Use one of the preset statistics or write your own."),
             selectInput("stat", "Statistic:", 
                         choices=c("Column mean" = "smean",
                                   "Column standard deviation" = "ssd",
                                   "Mean difference between groups" = "meandif",
                                   "Mean difference between columns" = "meandifc",
                                   "Mean sum of rows" = "srow",
                                   "Mean sum of columns" = "scol",
                                   "Regression intercept" = "intercept",
                                   "Regression coefficient" = "slope",
                                   "Correlation between columns" = "corr")),
             ### Panel for smean/ssd:
             conditionalPanel(
               helpText("These functions calculate the correlation coefficient, slope or intercept of
                        a linear correlation analysis between two columns, y ~ ax + b. Here, x is the
                        independent variable, and y is the dependent variable."),
               numericInput("r1", "Dependent variable column: ", 1),
               numericInput("r2", "Independent variable column: ", 1),
               condition="input.stat == 'intercept' || input.stat == 'slope' || input.stat == 'corr'"
             ),
             ### Panel for smean/ssd:
             conditionalPanel(
               helpText("This function calculates the mean or standard deviation of a single data column."),
               numericInput("m1", "Variable column: ", 1),
               condition="input.stat == 'smean' || input.stat == 'ssd'"
             ),
             ### Panel for srow/scol:
             conditionalPanel(
               helpText("This function calculates the sum of every values in a row (or column). Then, it takes
                        the mean of these values."),
               condition="input.stat == 'srow' || input.stat == 'scol'"
             ),
             ### Panel for meandif:
             conditionalPanel(
               helpText("This function splits the data acording to a categorical variable. Then it calculates 
                        the mean for each group, and subtracts one from another. Note that this is designed 
                        to work with only ",em("TWO")," categories!"),
               numericInput("s1", "Categorical variable column: ", 1),
               numericInput("s2", "Numerical variable column: ", 2),
               condition="input.stat == 'meandif'"
             ),
             ### Panel for meandifc:
             conditionalPanel(
               helpText("This function calculates the pairwise difference between two columns in your dataset (i.e.,
                        before and after a treatment is applied). It then averages these differences."),
               numericInput("d1", "Before treatment: ", 1),
               numericInput("d2", "After treatment: ", 2),
               condition="input.stat== 'meandifc'"
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
                          simulations in which the statistic had a value that's ", em("equal 
                          to or more extreme"), " than the statistic calculated on your original data
                          (represented by the dotted red line). This leads to a p-value of:"),
                 h3(textOutput("p"))
               )
             )
    )
  )
))
