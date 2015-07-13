library(shiny)
library(shinyBS)
shinyUI(fluidPage(theme= "bootstrap.css",
  tabsetPanel(type="tabs", id="tabEvent",
    tabPanel("Rsampling",
             h2("Rsampling - resampling statistics in R"),
             h4("powered by ", a("Shiny", href="http://www.rstudio.com/shiny")),
             includeHTML("help.html"),
             h3("Rsampling version"),
             conditionalPanel("output.needinstall=='ok'",
               p("You already have the right version of Rsampling installed :)")
             ),
             conditionalPanel("output.needinstall=='incompatible'",
                p("Your version of the Rsampling library seems to be incompatible with this interface
                  version. Please download the latest version of the library ",
                  a("here", href="https://github.com/lageIBUSP/Rsampling"), " then reload this interface",
                  style="color:#f30")
             )
    ),
    navbarMenu("Tutorials",
               tabPanel("Mangrove trees",
                        column(4,
                               h4("Mangrove trees and soil stability"),
                               h6("Question"),
                               p("Do mangrove trees in more unstable soil allocate
                                 more biomass in supporting roots?"),
                               h6("Hypothesis"),
                               p("Trees located in less stable soils 
                                 will have more biomass on roots."),
                               h5("In the left there is a plot and table (in blue) for which
                                  the second column has been randomized"),
                               actionButton("mangRand","Do it again!"),
                               actionButton("clear1","Clear histogram"),
                               plotOutput("distPlotMang",height=300),
                               helpText("Each time a new randomized data set is generated, the statistic of interest is 
                                        recalculated and added to this histogram. The statistic calculated on the original data
                                        is shown as a dotted red line. Should the null hypothesis be rejected? Run this exercise in
                                        the following tabs to find out!")
                               ),
                        column(8,
                               fluidRow(
                                 h5("About this data set"),
                                 p("Area covered by aerial roots in mangrove trees sampled in two soil types.")
                               ),
                               fluidRow(
                                 column(3, plotOutput("mangTable")),
                                 column(6, plotOutput("mangPlot",height=300),
                                        plotOutput("mangPlotRandom",height=300)),
                                 column(3, plotOutput("mangTableRandom"))
                               ),
                               fluidRow(
                                 helpText("Source: Prado, A. et al. 2013. Variações na morfologia de sustentação em", em("Rhizophora mangle"), 
                                          "(Rizophoraceae) em diferentes condições de inundação do solo. Curso de campo 
                                          Ecologia da Mata Atlântica (G. Machado, P.I. Prado & A.M.Z. Martini eds.). 
                                          Universidade de São Paulo, São Paulo.", 
                                          tags$a(href="http://ecologia.ib.usp.br/curso/2013/pdf/PO4-2.pdf", "Download pdf!"))
                               )
                               )
                               ),      
               tabPanel("Balanced mangrove trees",
                        column(4,
                               h4("Mangrove trees and torque"),
                               h6("Question"),
                               p("Does the torque caused by canopy weight result in mangrove
                                 trees with more roots?"),
                               helpText(tags$small("What is torque again? Review this concept in",
                                                   tags$a(href="https://en.wikipedia.org/wiki/Torque", "Wikipedia"))),
                               h6("Hypothesis"),
                               p("The higher the torque caused by the canopy, the more roots
                                 a tree wil have."),
                               h5("In the left there is a plot and table (in blue) for which
                                  the second column has been randomized"),
                               actionButton("rhyzRand","Do it again!"),
                               actionButton("clear2","Clear histogram"),
                               plotOutput("distPlotRhyz", height=300),
                               helpText("Each time a new randomized data set is generated, the statistic of interest is 
                                        recalculated and added to this histogram. The statistic calculated on the original data
                                        is shown as a dotted red line. Should the null hypothesis be rejected? Run this exercise in
                                        the following tabs to find out!")
                               ),
                        column(8,
                               fluidRow(
                                 h5("About this data set"),
                                 p("Ratio between canopy and trunk area, both in square meters, and number of aerial roots")
                               ),
                               fluidRow(
                                 column(3, plotOutput("rhyzTable")),
                                 column(6, plotOutput("rhyzPlot",height=300),
                                        plotOutput("rhyzPlotRandom",height=300)),
                                 column(3, plotOutput("rhyzTableRandom"))
                               ),
                               fluidRow(
                                 helpText("Source: Prado, A. et al. 2013. Variações na morfologia de sustentação em", em("Rhizophora mangle"), 
                                          "(Rizophoraceae) em diferentes condições de inundação do solo. Curso de campo 
                                          Ecologia da Mata Atlântica (G. Machado, P.I. Prado & A.M.Z. Martini eds.). 
                                          Universidade de São Paulo, São Paulo.", 
                                          tags$a(href="http://ecologia.ib.usp.br/curso/2013/pdf/PO4-2.pdf", "Download pdf!"))
                               )
                               )
                               ),
               tabPanel("Protective ants",
                        column(4,
                               h4("Protective ants"),
                               h6("Question"),
                               p("Do ants respond more intensely to damage in younger leaves?"),
                               h6("Hypothesis"),
                               p("Damage on young leaves will lead to more recruited ants when compared
                                 to damage on old leaves."),
                               h5("In the left there is a plot and table (in blue) for which
                                  the data has been randomized. How should the randomization be performed in
                                  a paired experimental design?"),
                               actionButton("aztRand","Do it again!"),
                               actionButton("clear3","Clear histogram"),
                               plotOutput("distPlotAzt", height=300),
                               helpText("Each time a new randomized data set is generated, the statistic of interest is 
                                        recalculated and added to this histogram. The statistic calculated on the original data
                                        is shown as a dotted red line. Should the null hypothesis be rejected? Run this exercise in
                                        the following tabs to find out!")
               ),
               column(8,
                      fluidRow(
                        h5("About this data set"),
                        p("The ant colonies live in the hollow trunk of", em("Cecropia"), "and can detect and expel leaf-chewing 
                          insects. To test if this response is more intense in young leaves, drops of extract of smashed
                          young and old leaves were poured in two neighbor leaves of the same plant. After 7 minutes 
                          the number of recruited", em("Azteca"),"ants in each leaf was recorded.")
                      ),
                      fluidRow(
                        column(3, plotOutput("aztTable")),
                        column(6, plotOutput("aztPlot",height=300),
                               plotOutput("aztPlotRandom",height=300)),
                        column(3, plotOutput("aztTableRandom"))
                      ),
                      fluidRow(
                        helpText("Source: Kondrat, H. 2012. Estímulos químicos de folhas novas promovem recrutamento eficiente
                                 de formigas associadas à embaúba Cecropia glaziovi (Urticaceae). Curso de campo Ecologia
                                 da Mata Atlântica (G. Machado; P.I. Prado & A.M.Z. Martini, eds.). Universidade de
                                 São Paulo, São Paulo", 
                                 tags$a(href="http://ecologia.ib.usp.br/curso/2012/PDF/PI-Hebert.pdf", "Download pdf!"))
                      )
               )
      )
    ),    
    tabPanel("Data input", 
             helpText("Use this tab to select the input data for your analysis. The first options are 
                      datasets included in the library, select the \"upload\" option to upload your own 
                      file."),
             selectInput("datasource",
                         "What is your input data?",
                         choices = c("embauba", "azteca", "peucetia", "rhyzophora", "upload file")
                         ),
             ## the next panel only shows for the custom datasource
             fluidRow(column(6,conditionalPanel(
               fileInput("file", "Choose CSV file:", accept='.csv'),
               fluidRow(
                 column(3, checkboxInput("header", "First row as header?")),
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
             tableOutput("view")
             ),
    tabPanel("Statistics",
             helpText("Next, we need to determine what is the function (i.e., the statistic) that will be applied to the data. Use one of the preset statistics or write your own."),
             selectInput("stat", "Statistic:", 
                         choices=c("Column mean" = "smean",
                                   "Column standard deviation" = "ssd",
                             "Mean difference between 2 groups" = "meandif",
                             "Variance ratio (F) for more than 2 groups" = "Fstatistic",
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
               strong("Differences in the slope of linear regressions 
                      applied to different levels of a factor:"),
               code("m1 <- lm(n.roots ~ canopy.trunk, data=dataframe, subset=soil.instability==\"medium\")"),
               p(""), 
               code("m2 <- lm(n.roots ~ canopy.trunk, data=dataframe, subset=soil.instability==\"high\")"),
               p(""),
               code("coef(m1)[[2]] - coef(m2)[[2]]"),p(""),
               tags$textarea(id = "customstat", rows=5, cols=40, "return(pi)"),
               actionButton("gocustomstat", "Go!"),
               condition="input.stat == 'custom'"
             ),
             ### Panels with help text about the selected function
             conditionalPanel("input.stat == 'intercept'",
               helpText("This function calculates intercept of
                        a linear correlation analysis between two columns, y ~ ax + b. Here, x is the
                        independent variable, and y is the dependent variable.")
             ),
             conditionalPanel("input.stat == 'slope'",
               helpText("This function calculates the slope of
                        a linear correlation analysis between two columns, y ~ ax + b. Here, x is the
                        independent variable, and y is the dependent variable.")
             ),
             conditionalPanel("input.stat == 'corr'",
               helpText("This function calculates the correlation coefficient between two columns")
             ),
             conditionalPanel("input.stat == 'smean'",
               helpText("This function calculates the mean of a single data column.")
             ),
             conditionalPanel("input.stat == 'ssd'",
               helpText("This function calculates the standard deviation of a single data column.")
             ),
             conditionalPanel("input.stat == 'srow'",
               helpText("This function calculates the sum of every values in a row. Then, it takes
                        the mean of these values.")
             ),
             conditionalPanel("input.stat == 'scol'",
               helpText("This function calculates the sum of every values in a column. Then, it takes
                        the mean of these values.")
             ),
             conditionalPanel("input.stat == 'meandif'",
               helpText("This function splits the data acording to a categorical variable. Then it calculates 
                        the mean for each group, and subtracts one from another. Note that this is designed 
                        to work with only ",em("TWO")," categories!")
             ),
             conditionalPanel("input.stat == 'Fstatistic'",
               helpText("The variance ratio function splits the data acording to a categorical variable.
                        Then it calculates
                        the ratio of among-group to within-group variances (F).
                        Large differences between means of at least two groups lead to large values of F.")
             ),
             conditionalPanel("input.stat== 'meandifc'",
               helpText("This function calculates the pairwise difference between two columns in 
                        your dataset (i.e., before and after a treatment is applied). It then 
                        averages these differences.")
             ),
             #### Panels for the inputs selectors
             conditionalPanel("input.stat != 'custom' && input.stat != 'srow' && input.stat != 'scol'", 
                              # all other stats have a "column 1"
               selectInput("m1", "Column 1", choices=1) # label and choices will be overriden!
             ),
             conditionalPanel("input.stat == 'slope' || input.stat == 'intercept' || 
                               input.stat == 'corr' || input.stat == 'meandif' ||
                               input.stat == 'Fstatistic' || input.stat == 'meandifc'", 
                              # all the above stats have a "column 2"
               selectInput("m2", "Column 2", choices=2) # label and choices will be overriden!
             ),
             helpText("Below you see the result of this function applied to the original data:"),
             h3(textOutput("stat")),
             # displays a warning in case the statistic is not returning a single number
             h4(textOutput("svaluewarning"), style="color:#f30")
             ),
    tabPanel("Resampling",
             sidebarLayout(
               sidebarPanel(
                 helpText("Here is where we do the randomization!"),
                 selectInput("type", "Randomization type:", 
                             choices=c("Normal", "Within rows", "Within columns", 
                                       "Rows as units", "Columns as units")
                             ),
                 ##Help for each randomization panel
                   conditionalPanel(
                       helpText(
                           "In normal resampling the data is randomized over all cells of the selected columns. If you do not check the 'With replacement' box below the data is permuted over the cells. Otherwise the data from any cell are sampled with replacement and attributed to any other cell."),
                       condition = "input.type == 'Normal'"),
                   conditionalPanel(
                       helpText(
                                "The randomization is done within each row of the data.
                                If you do not check the 'With replacement' box below the values of
                                each row are permuted independently. Otherwise 
                                the values are sampled independently from each row and
                                attributed only to cells of the row they were sampled from."),
                       condition = "input.type == 'Within rows'"),
                   conditionalPanel(
                       helpText(
                                "The randomization is done within each column of the data.
                                If you do not check the 'With replacement' box below the values of
                                each column are permuted independently. Otherwise 
                                the values are sampled independently from each column and
                                attributed only to cells of the column they were sampled from."),
                       condition = "input.type == 'Within columns'"),
                   conditionalPanel(
                       helpText(
                                "Randomizes the placement of rows
                                 in the data table. If you do not check the 'With replacement' box below the
                                 position of rows are permuted. Otherwise
                                 whole rows are sampled with replacement to assemble the randomized data table.
                                 In both cases the position of values within each row is kept."),
                       condition = "input.type == 'Rows as units'"),
                   conditionalPanel(
                       helpText(
                                "Randomizes the placement of columns
                                 in the data table. If you do not check the 'With replacement' box below the
                                 position of columns are permuted. Otherwise
                                 whole columns are sampled with replacement to assemble the randomized data table.
                                 In both cases the position of values within each column is kept."),
                       condition = "input.type == 'Columns as units'"),
                 checkboxInput("replace", "With replacement?"),
                 selectInput("pside", "Alternative hypothesis:", choices=c("Two sided", "Greater", "Lesser")),
                 sliderInput("ntrials", "Number of trials:", min=500,max=10000,value=1000,step=500),
                 checkboxInput("stratum", "Stratified resampling?"),
                 conditionalPanel("input.stratum",
                 helpText("Should the randomization be restricted inside groups of rows?"),
                   selectInput("stratumc", "Stratum variable: ", 1)
                 ),
                 conditionalPanel("input.stat == 'custom'",
                   helpText("Which columns of the data set should be randomized? 
                             R code such as 1:3 or c(1,4,5) are valid values"),
                   textInput("customcols", "Columns", "1")
                 ),
                 fluidRow(column(6, checkboxInput("extreme", "Show extremes?", TRUE)),
                          column(6, checkboxInput("rejection", "Show rejection region?", TRUE))
                         ),
	         fluidRow(column(6, actionButton("go", "Run sampling!")),
			  column(6, downloadButton('download', "Download data"))
			 )
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
