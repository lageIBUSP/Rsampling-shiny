library(shiny)
library(shinyBS)
shinyUI(fluidPage(theme= "bootstrap.css",
  tabsetPanel(type="tabs",
    tabPanel("Rsampling",
             h2("Rsampling - resampling statistics in R"),
             h4("powered by ", a("Shiny", href="http://www.rstudio.com/shiny")),
             includeHTML("help.html"),
             h3("Package installation!"),
             conditionalPanel( # for Rsampling install
                p("It seems that you don't have the Rsampling package installed. Please install it
                  following the instructions ", 
                  a("here", href="https://github.com/lageIBUSP/Rsampling"), ", then reload this interface",
                  style="color:#f30"),
#                actionButton("installbutton", "Install!"),
#                conditionalPanel(condition="input.installbutton > 0",
#                  textOutput("pkginstall")
#                ),
                condition="output.needinstall=='notinstalled'"
             ),
             conditionalPanel("output.needinstall=='ok'",
               p("You already have Rsampling installed :)")
             ),
             conditionalPanel("output.needinstall=='incompatible'",
                p("Your version of the Rsampling library seems to be incompatible with this interface
                  version. Please download the latest version of the library ",
                  a("here", href="https://github.com/lageIBUSP/Rsampling"), ", then reload this interface",
                  style="color:#f30")
             )
    ),
    tabPanel("Data input", 
             helpText("Use this tab to select the input data for your analysis. The first options are datasets included in the library, select the \"upload\" option to upload your own file."),
             selectInput("datasource",
                         "What is your input data?",
                         choices = c("embauba", "azteca", "peucetia", "rhyzophora", "upload file")
                         ),
             ## the next panel only shows for the custom datasource
             fluidRow(column(6,conditionalPanel(
               fileInput("file", "Choose CSV file:", accept='.csv'),
               fluidRow(
                 column(3, checkboxInput("header", "Header?")),
                 bsTooltip("header", "First row as header"),
                 column(3,
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t',
                                Space=' '),
                              ',')),
                 column(3,
                 radioButtons('dec', 'Decimal',
                              c(dot ='.',
                                comma=','),
                              '.')),
                 column(3,
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"'))
                 ),
               helpText("Make sure that the data is correctly interpreted in the display below!", style="color:#f30;"),
               condition="input.datasource	== 'upload file'"
             ))),
             tableOutput("view"),
             bsTooltip("view", "These are first rows of the selected data table.", "top")
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
                                   "Correlation between columns" = "corr",
                                   "Custom code" = "custom"),
                         "meandif"),
             ### Panel for custom code:
             conditionalPanel(
               helpText("You are free to write down your own R function to calculate any statistic 
                        over your data! The data is stored as a dataframe with the boring name of 
                        \"dataframe\". Your last line in the code should return a single number, representing
                        the statistic of interest. Some examples of what to use include:"),
               strong("Sum of column 1:"), p(""), code("sum(dataframe[, 1])"), p(""),
               strong("Mean of row 3:"), p(""), code("mean(dataframe[3, ])"), p(""),
               strong("Correlation coefficient between columns 1 and 2:"), p(""),
               code("cor(dataframe[,1], dataframe[,2])"), p(""),
               strong("Sum of the squared residuals of a linear model:"), p(""),
               code("my.lm <- lm(dataframe[,5] ~ dataframe[,4])"), p(""),
               code("my.r <- residuals(my.lm)"), p(""),
               code("sum(my.r^2)"), p(""),
               tags$textarea(id = "customstat", rows=5, cols=40, "return(pi)"),
               actionButton("gocustomstat", "Go!"),
               condition="input.stat == 'custom'"
             ),
             ### Panel for smean/ssd:
             conditionalPanel(
               helpText("These functions calculate the correlation coefficient, slope or intercept of
                        a linear correlation analysis between two columns, y ~ ax + b. Here, x is the
                        independent variable, and y is the dependent variable."),
               selectInput("r1", "Dependent variable column: ", 1),
               selectInput("r2", "Independent variable column: ", 2),
               condition="input.stat == 'intercept' || input.stat == 'slope' || input.stat == 'corr'"
             ),
             ### Panel for smean/ssd:
             conditionalPanel(
               helpText("This function calculates the mean or standard deviation of a single data column."),
               selectInput("m1", "Variable column: ", choices=1),
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
               selectInput("s1", "Categorical variable column: ", 1),
               selectInput("s2", "Numerical variable column: ", 2),
               condition="input.stat == 'meandif'"
             ),
             ### Panel for meandifc:
             conditionalPanel(
               helpText("This function calculates the pairwise difference between two columns in your dataset (i.e.,
                        before and after a treatment is applied). It then averages these differences."),
               selectInput("d1", "Before treatment: ", 1),
               selectInput("d2", "After treatment: ", 2),
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
                 bsTooltip("type", "See the help page for details on the different randomization types."),
                 checkboxInput("replace", "With replacement?"),
                 selectInput("pside", "Alternative:", choices=c("Two sided", "Greater", "Lesser")),
                 bsTooltip("replace", "Check this option if you want all the draws to be made independently (that is, with replacement) from the original data"),
                 bsTooltip("pside", "Use this to select if you want the p-value to be assigned from a two-sided hypothesis (that is, both positive and negative values can be considered extreme), or a one sided test.", "top"),
                 sliderInput("ntrials", "Number of trials:", min=100,max=5000,value=300,step=100),
                 checkboxInput("stratum", "Stratified resampling?"),
                 bsTooltip("stratum", "Check this if you want the randomization to be restricted inside groups of rows defined by a categorical value."),
                 conditionalPanel("input.stratum",
                   selectInput("stratumc", "Stratum variable: ", 1)
                 ),
                 bsTooltip("ntrials", "How many iteractions of sampling should we do?"),
                 fluidRow(column(6, checkboxInput("extreme", "Show extremes?", TRUE)),
                          column(6, checkboxInput("rejection", "Show rejection region?", TRUE))
                         ),
                 actionButton("go", "Update Graph")
               ),
               mainPanel(
                 plotOutput("distPlot"),
                 helpText("The graph above shows the distribution of your selected statistic after repeated 
                          randomization from your data. The histograms bins in orange (if any) represent those
                          simulations in which the statistic had a value that's ", em("equal 
                          to or more extreme"), " than the statistic calculated on your original data
                          (represented by the dotted red line). The gray area delimits the values of the statistics under which the null hypothesis should be accepted with 5% of chance of error. The proportion of simulations with statistics more extreme than the observed (p-value of) is:"),
                 h3(textOutput("p"))
               )
             )
    )
  )
))
