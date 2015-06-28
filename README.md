# Rsampling-shiny

Rsampling is a free and open source package intended for teaching and learning about
resampling in statistics.

To run, you need to install [R] (http://www.r-project.org/). Then, launch R and run
```R
install.packages(c("shiny","shinyBS","devtools"))
library(shiny)
runGitHub(repo="Rsampling-shiny", username="andrechalom")
``` 
and wait for the browser window to open!

## Current features
* Access the Rsampling sample datafiles, or upload your own data in csv format
* Choose between a range of statistics, or write your own R function
* Select your randomization scheme, with or without replacement
* Reactive graph showing the distribuion of the statistic of interest and estimated p-value
