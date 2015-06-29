# Rsampling-shiny

[Rsampling] (https://github.com/lageIBUSP/Rsampling) is a free and open source package intended for 
teaching and learning about resampling in statistics. Rsampling-shiny is a web-based graphical user interface
written in [shiny] (http://shiny.rstudio.com).

To run, you need to install [R] (http://www.r-project.org/). Then, launch R and run
```R
install.packages(c("shiny","shinyBS","devtools"))
library(shiny)
runGitHub(repo="andrechalom/Rsampling-shiny")
``` 
and wait for the browser window to open!

## Current features
* Access the Rsampling sample datafiles, or upload your own data in csv format
* Choose between a range of statistics, or write your own R function
* Select your randomization scheme, with or without replacement
* Optional stratified randomization
* Reactive graph showing the distribuion of the statistic of interest and estimated p-value
