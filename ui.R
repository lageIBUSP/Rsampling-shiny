# loads the required libraries.
# when adding libraries here, REMEMBER to include them as requirements on README.md
library(shiny)
# Translate input widgets
##Statistic
optionsStat <- c("smean","ssd","meandif","Fstatistic","meandifc","srow",
                 "scol","intercept","slope","corr","custom")
names(optionsStat) <- optionsStat
###
shinyUI(fluidPage(theme= "bootstrap.css",
                  tabsetPanel(type="tabs", id="tabEvent",
                              tabPanel("Rsampling",
                                       if(!is.element(lg,colnames(lang)[-1])){
                                         h5(tr("The language selected is not available or was not properly detected. Language was set back to the default.",lg),style="color:#f30")
                                       },
                                       h2(tr("Rsampling - resampling statistics in R",lg)),
                                       h4(tr("powered by ",lg), a("Shiny", href="http://www.rstudio.com/shiny")),
                                       if (lg=="pt") {
                                         includeHTML("help_pt.html")
                                       } else {
                                         includeHTML("help.html")
                                       },
                                       h3(tr("Rsampling version",lg)),
                                       conditionalPanel("output.needinstall=='ok'",
                                                        p(tr("You already have the right version of Rsampling installed :)",lg))
                                       ),
                                       conditionalPanel("output.needinstall=='incompatible'",
                                                        p(tr("Your version of the Rsampling library seems to be incompatible with this interface version. Please download the latest version of the library ",lg),
                                                          a( tr("here",lg), href="https://github.com/lageIBUSP/Rsampling"), tr(" then reload this interface",lg),
                                                          style="color:#f30")
                                       )
                              ),
                              navbarMenu("Tutorial",
                                         tabPanel(tr("Mangrove Trees",lg),
                                                  column(4,
                                                         h4(tr("MangroveTitle",lg)),
                                                         h6(tr("Question",lg)),
                                                         p(tr("Do mangrove trees in more unstable soil allocate more biomass in supporting roots?", lg)),
                                                         h6(tr("Hypothesis",lg)),
                                                         p(tr("Trees located in less stable soils will have more biomass on roots",lg)),
                                                         h6(tr("Statisc of interest",lg)),
                                                         p(tr("Mean difference between soil types",lg)),  
                                                         plotOutput("distPlotMang", height=300),
                                                         h6(tr("Each time a new randomized data set is generated, the statistic of interest is recalculated and added to this histogram. The statistic calculated on the original data is shown as a dotted red line. Should the null hypothesis be rejected? Run this exercise in the following tabs to find out!",lg)),
                                                         actionButton("clear1",tr("Clear histogram",lg))
                                                  ),
                                                  column(8,
                                                         fluidRow(
                                                           h5(tr("About this data set (rhyzophora)",lg)),
                                                           p(tr("Area covered by aerial roots in mangrove trees sampled in two soil types",lg)),
                                                           radioButtons('dataset', label=tr('Data set:',lg), choices = c(tr("Original",lg),tr("Randomized",lg)), 
                                                                        selected = tr("Original",lg), inline = TRUE)
                                                         ),
                                                         fluidRow(                                 
                                                           conditionalPanel(paste("input.dataset == '",tr("Original",lg),"'",sep="") , column(4, plotOutput("mangTable"))),
                                                           conditionalPanel(paste("input.dataset == '",tr("Randomized",lg),"'",sep="") , column(4, plotOutput("mangTableRandom"))),
                                                           conditionalPanel(paste("input.dataset == '",tr("Original",lg),"'",sep="") , column(8, plotOutput("mangPlot",height=300))),
                                                           conditionalPanel(paste("input.dataset == '",tr("Randomized",lg),"'",sep="") , column(8, plotOutput("mangPlotRandom",height=300),
                                                                                                                                                h5(tr("Here the left column of the original data has been randomized. The mean differece between soil types has been calculated for this randomized data set and is shown in the plot above. Hit the button to genetate a new randomized data set",lg)),
                                                                                                                                                actionButton("mangRand",tr("Do it again!",lg))))
                                                         ),
                                                         fluidRow(
                                                           helpText("Source: Prado, A. et al. 2013. Variações na morfologia de sustentação em", em("Rhizophora mangle"), "(Rizophoraceae) em diferentes condições de inundação do solo. Curso de campo Ecologia da Mata Atlântica (G. Machado, P.I. Prado & A.M.Z. Martini eds.). Universidade de São Paulo, São Paulo.", tags$a(href="http://ecologia.ib.usp.br/curso/2013/pdf/PO4-2.pdf", "Download pdf!"))
                                                         )
                                                         
                                                  )
                                         ),
                                         tabPanel(tr("Balanced mangrove trees",lg),
                                                  column(4,
                                                         h4(tr("Mangrove trees and torque",lg)),
                                                         h6(tr("Question",lg)),
                                                         p(tr("Does the torque caused by canopy weight result in mangrove trees with more roots?",lg)),
                                                         helpText(tags$small(tr("What is torque again? Review this concept in",lg), tags$a(href="https://en.wikipedia.org/wiki/Torque", "Wikipedia"))),
                                                         h6(tr("Hypothesis",lg)),
                                                         p(tr("The higher the torque caused by the canopy, the more roots a tree wil have.",lg)),
                                                         h6(tr("Statisc of interest",lg)),
                                                         p(tr("Slope of the regression between the number of roots and the ratio between canopy and trunk area",lg)),  
                                                         plotOutput("distPlotRhyz", height=300),
                                                         h6(tr("Each time a new randomized data set is generated, the statistic of interest is recalculated and added to this histogram. The statistic calculated on the original data is shown as a dotted red line. Should the null hypothesis be rejected? Run this exercise in the following tabs to find out!",lg)),
                                                         actionButton("clear2",tr("Clear histogram",lg))
                                                  ),
                                                  column(8,
                                                         fluidRow(
                                                           h5(tr("About this data set (rhyzophora)",lg)),
                                                           p(tr("Ratio between canopy and trunk area, both in square meters, and number of aerial roots",lg)),
                                                           radioButtons('dataset2', label='Data set:', choices = c("Original","Randomized"), 
                                                                        selected = 'Original', inline = TRUE)
                                                         ),
                                                         fluidRow(                                 
                                                           conditionalPanel("input.dataset2 == 'Original'" , column(4, plotOutput("rhyzTable"))),
                                                           conditionalPanel("input.dataset2 == 'Randomized'" , column(4, plotOutput("rhyzTableRandom"))),
                                                           conditionalPanel("input.dataset2 == 'Original'" , column(8, plotOutput("rhyzPlot",height=300))),
                                                           conditionalPanel("input.dataset2 == 'Randomized'" , column(8, plotOutput("rhyzPlotRandom",height=300),
                                                                                                                      h5(tr("Here the left column of the original data has been randomized. The slope of the linear regression has been calculated for this randomized data set and is shown in the plot above. Hit the button to genetate a new randomized data set",lg)),
                                                                                                                      actionButton("rhyzRand",tr("Do it again!",lg))))
                                                         ),
                                                         fluidRow(
                                                           helpText("Source: Prado, A. et al. 2013. Variações na morfologia de sustentação em", em("Rhizophora mangle"),"(Rizophoraceae) em diferentes condições de inundação do solo. Curso de campo Ecologia da Mata Atlântica (G. Machado, P.I. Prado & A.M.Z. Martini eds.). Universidade de São Paulo, São Paulo.", tags$a(href="http://ecologia.ib.usp.br/curso/2013/pdf/PO4-2.pdf", "Download pdf!"))
                                                         )
                                                  )
                                         ),
                                         tabPanel(tr("Protective ants",lg),
                                                  column(4,
                                                         h4(tr("Protective ants",lg)),
                                                         h6(tr("Question",lg)),
                                                         p(tr("Do ants respond more intensely to damage in younger leaves?",lg)),
                                                         h6(tr("Hypothesis",lg)),
                                                         p(tr("Damage on old leaves will lead to less recruited ants whencompared to damage on young leaves",lg)),
                                                         h6(tr("Statisc of interest",lg)),
                                                         p(tr("Mean difference between treatments",lg)),  
                                                         plotOutput("distPlotAzt", height=300),
                                                         h6(tr("Each time a new randomized data set is generated, the statistic of interest is recalculated and added to this histogram. The statistic calculated on the original data is shown as a dotted red line. Should the null hypothesis be rejected? Run this exercise inthe following tabs to find out!",lg)),
                                                         actionButton("clear3",tr("Clear histogram",lg))
                                                  ),
                                                  column(8,
                                                         fluidRow(
                                                           h5(tr("About this data set (azteca)",lg)),
                                                           p(tr("The ant colonies live in the hollow trunk of",lg), em(tr("Cecropia",lg)), tr(" and can detect and expel leaf-chewing insects. To test if this response is more intense in young leaves, drops of extract of smashed young and old leaves were poured in two neighbor leaves of the same plant. After 7 minutes  the number of recruited",lg), em(tr("Azteca",lg)),tr("ants in each leaf was recorded.",lg)),
                                                           radioButtons('dataset3', label='Data set:', choices = c("Original","Randomized"), 
                                                                        selected = 'Original', inline = TRUE)
                                                         ),
                                                         fluidRow(
                                                           conditionalPanel("input.dataset3 == 'Original'" , column(4, plotOutput("aztTable"))),
                                                           conditionalPanel("input.dataset3 == 'Randomized'" , column(4, plotOutput("aztTableRandom"))),
                                                           conditionalPanel("input.dataset3 == 'Original'" , column(8, plotOutput("aztPlot",height=300))),
                                                           conditionalPanel("input.dataset3 == 'Randomized'" , column(8, plotOutput("aztPlotRandom",height=300),
                                                                                                                      h5(tr("Here the second and third column of the original data have been randomized within each row. The mean differece between treatments is calculated and shown in the plot above. Hit the button to genetate a new randaomized data set",lg)),
                                                                                                                      actionButton("aztRand",tr("Do it again!",lg))))
                                                         ),
                                                         fluidRow(
                                                           helpText("Source: Kondrat, H. 2012. Estímulos químicos de folhas novas promovem recrutamento eficiente de formigas associadas à embaúba Cecropia glaziovi (Urticaceae). Curso de campo Ecologia da Mata Atlântica (G. Machado; P.I. Prado & A.M.Z. Martini, eds.). Universidade de São Paulo, São Paulo", tags$a(href="http://ecologia.ib.usp.br/curso/2012/PDF/PI-Hebert.pdf", "Download pdf!"))
                                                         )
                                                  )
                                         )
                              ),    
                              tabPanel(tr("dataInput",lg),
                                       helpText(tr("help1",lg)),
                                       selectInput("datasource",
                                                   tr("What is your input data?",lg),
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
                                         helpText(tr("Make sure that the data is correctly interpreted in the display below!",lg), style="color:#f30;"),
                                         condition="input.datasource	== 'upload file'"
                                       ))),
                                       tableOutput("view")
                              ),
                              tabPanel(tr("Statistics",lg),
                                       helpText(tr("Next, we need to determine what is the function (i.e., the statistic) that will be applied to the data. Use one of the preset statistics or write your own.",lg)),
                                       selectInput("stat", tr("Statistic:",lg), 
                                                   choices=optionsStat,
                                                   "meandif"),
                                       ### Panel for custom code:
                                       conditionalPanel(
                                         helpText(tr("You are free to write down your own R function to calculate any statistic over your data! The data is stored as a dataframe with the boring name of \"dataframe\". Your last line in the code should return a single number, representing the statistic of interest. Some examples of what to use include:",lg)),
                                         strong(tr("Sum of column 1:",lg)), p(""), code("sum(dataframe[, 1])"), p(""),
                                         strong(tr("Mean of row 3:",lg)), p(""), code("mean(dataframe[3, ])"), p(""),
                                         strong(tr("Correlation coefficient between columns 1 and 2:",lg)), p(""),
                                         code("cor(dataframe[,1], dataframe[,2])"), p(""),
                                         strong(tr("Sum of the squared residuals of a linear model:",lg)), p(""),
                                         code("my.lm <- lm(dataframe[,5] ~ dataframe[,4])"), p(""),
                                         code("my.r <- residuals(my.lm)"), p(""),
                                         code("sum(my.r^2)"), p(""),
                                         strong(tr("Differences in the slope of linear regressions applied to different levels of a factor:",lg)),
                                         code("m1 <- lm(n.roots ~ canopy.trunk, data=dataframe, subset=soil.instability==\"medium\")"),
                                         p(""), 
                                         code("m2 <- lm(n.roots ~ canopy.trunk, data=dataframe, subset=soil.instability==\"high\")"),
                                         p(""),
                                         code("coef(m1)[[2]] - coef(m2)[[2]]"),p(""),
                                         tags$textarea(id = "customstat", rows=5, cols=40, "return(pi)"),
                                         actionButton("gocustomstat", tr("Go!",lg)),
                                         condition="input.stat == 'custom'"
                                       ),
                                       ### Panels with help text about the selected function
                                       conditionalPanel("input.stat == 'intercept'",
                                                        helpText(tr("This function calculates intercept of a linear correlation analysis between two columns, y ~ ax + b. Here, x is the independent variable, and y is the dependent variable.",lg))
                                       ),
                                       conditionalPanel("input.stat == 'slope'",
                                                        helpText(tr("This function calculates the slope of a linear correlation analysis between two columns, y ~ ax + b. Here, x is the independent variable, and y is the dependent variable.",lg))
                                       ),
                                       conditionalPanel("input.stat == 'corr'",
                                                        helpText(tr("This function calculates the correlation coefficient between two columns",lg))
                                       ),
                                       conditionalPanel("input.stat == 'smean'",
                                                        helpText(tr("This function calculates the mean of a single data column.",lg))
                                       ),
                                       conditionalPanel("input.stat == 'ssd'",
                                                        helpText(tr("This function calculates the standard deviation of a single data column.",lg))
                                       ),
                                       conditionalPanel("input.stat == 'srow'",
                                                        helpText(tr("This function calculates the sum of every values in a row. Then, it takes the mean of these values.",lg))
                                       ),
                                       conditionalPanel("input.stat == 'scol'",
                                                        helpText(tr("This function calculates the sum of every values in a column. Then, it takes the mean of these values.",lg))
                                       ),
                                       conditionalPanel("input.stat == 'meandif'",
                                                        helpText(tr("This function splits the data acording to a categorical variable. Then it calculates the mean for each group, and subtracts one from another. Note that this is designed to work with only ",lg),em(tr("TWO",lg)),tr(" categories!",lg))
                                       ),
                                       conditionalPanel("input.stat == 'Fstatistic'",
                                                        helpText(tr("The variance ratio function splits the data acording to a categorical variable. Then it calculates the ratio of among-group to within-group variances (F). Large differences between means of at least two groups lead to large values of F.",lg))
                                       ),
                                       conditionalPanel("input.stat== 'meandifc'",
                                                        helpText(tr("This function calculates the pairwise difference between two columns in your dataset (i.e., before and after a treatment is applied). It then averages these differences.",lg))
                                       ),
                                       #### Panels for the inputs selectors
                                       conditionalPanel("input.stat != 'custom' && input.stat != 'srow' && input.stat != 'scol'", 
                                                        # all other stats have a "column 1"
                                                        selectInput("m1", "Column 1", choices=1) # label and choices will be overriden!
                                       ),
                                       conditionalPanel("input.stat == 'slope' || input.stat == 'intercept' || input.stat == 'corr' || input.stat == 'meandif' || input.stat == 'Fstatistic' || input.stat == 'meandifc'", 
                                                        # all the above stats have a "column 2"
                                                        selectInput("m2", "Column 2", choices=2) # label and choices will be overriden!
                                       ),
                                       helpText(tr("Below you see the result of this function applied to the original data:",lg)),
                                       h3(textOutput("stat")),
                                       # displays a warning in case the statistic is not returning a single number
                                       h4(textOutput("svaluewarning"), style="color:#f30")
                              ),
                              tabPanel("Resampling",
                                       sidebarLayout(
                                         sidebarPanel(
                                           helpText(tr("Here is where we do the randomization!",lg)),
                                           selectInput("type", tr("Randomization type:",lg), 
                                                       choices=c("Normal", "Within rows", "Within columns", "Rows as units", "Columns as units")
                                           ),
                                           ##Help for each randomization panel
                                           conditionalPanel(
                                             helpText(tr("In normal resampling the data is randomized over all cells of the selected columns. If you do not check the 'With replacement' box below the data is permuted over the cells. Otherwise the data from any cell are sampled with replacement and attributed to any other cell.",lg)),
                                             condition = "input.type == 'Normal'"),
                                           conditionalPanel(
                                             helpText(tr("The randomization is done within each row of the data. If you do not check the 'With replacement' box below the values of each row are permuted independently. Otherwise the values are sampled independently from each row and attributed only to cells of the row they were sampled from.",lg)),
                                             condition = "input.type == 'Within rows'"),
                                           conditionalPanel(
                                             helpText(tr("The randomization is done within each column of the data. If you do not check the 'With replacement' box below the values of each column are permuted independently. Otherwise the values are sampled independently from each column and attributed only to cells of the column they were sampled from.",lg)),
                                             condition = "input.type == 'Within columns'"),
                                           conditionalPanel(
                                             helpText(tr("Randomizes the placement of rows in the data table. If you do not check the 'With replacement' box below the position of rows are permuted. Otherwise whole rows are sampled with replacement to assemble the randomized data table. In both cases the position of values within each row is kept.",lg)),
                                             condition = "input.type == 'Rows as units'"),
                                           conditionalPanel(
                                             helpText(tr("Randomizes the placement of columns in the data table. If you do not check the 'With replacement' box below the position of columns are permuted. Otherwise whole columns are sampled with replacement to assemble the randomized data table. In both cases the position of values within each column is kept.",lg)),
                                             condition = "input.type == 'Columns as units'"),
                                           ##bsTooltip("type", "See the help page for details on the different randomization types."),
                                           checkboxInput("replace", tr("With replacement?",lg)),
                                           selectInput("pside", tr("Alternative:",lg), choices=c("Two sided", "Greater", "Lesser")),
                                           bsTooltip("replace", tr("Check this option if you want all the draws to be made independently (that is, with replacement) from the original data",lg)),
                                           bsTooltip("pside", tr("Use this to select if you want the p-value to be assigned from a two-sided hypothesis (that is, both positive and negative values can be considered extreme), or a one sided test.",lg), "top"),
                                           sliderInput("ntrials", tr("Number of trials:",lg), min=500,max=10000,value=1000,step=500),
                                           checkboxInput("stratum", tr("Stratified resampling?",lg)),
                                           bsTooltip("stratum", tr("Check this if you want the randomization to be restricted inside groups of rows defined by a categorical value.",lg)),
                                           conditionalPanel("input.stratum",
                                                            selectInput("stratumc", tr("Stratum variable: ",lg), 1)
                                           ),
                                           conditionalPanel("input.stat == 'custom'",
                                                            helpText(tr("Which columns of the data set should be randomized? This input will be parsed as R code, so 1:3 or c(1,4,5) are valid values",lg)),
                                                            textInput("customcols", "Columns", "1")
                                           ),
                                           bsTooltip("ntrials", tr("How many iteractions of sampling should we do?",lg)),
                                           fluidRow(column(6, checkboxInput("extreme", tr("Show extremes?",lg), TRUE)),
                                                    column(6, checkboxInput("rejection", tr("Show rejection region?",lg), TRUE))
                                           ),
                                           fluidRow(column(6, actionButton("go", tr("Update Graph",lg))),
                                                    column(6, downloadButton('download', tr("Download data",lg)))
                                           )
                                         ),
                                         mainPanel(
                                           plotOutput("distPlot"),
                                           helpText(tr("The graph above shows the distribution of your selected statistic after repeated randomization from your data. The histograms bins in orange (if any) represent those simulations in which the statistic had a value that's ",lg), em(tr("equal to or more extreme",lg)), tr(" than the statistic calculated on your original data (represented by the dotted red line). The gray area delimits the values of the statistics under which the null hypothesis should be accepted with 5% of chance of error. The proportion of simulations with statistics more extreme than the observed (p-value of) is:",lg)),
                                           h3(textOutput("p"))
                                         )
                                       )
                              )
                  )
))
