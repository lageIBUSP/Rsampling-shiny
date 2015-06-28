# Rsampling-shiny

Rsampling is a free and open source package intended for teaching and learning about
resampling in statistics.

To run, install [R] (http://www.r-project.org/) and the [Rsampling library] (https://github.com/lageIBUSP/Rsampling).

Then, launch R and run
```R
install.packages(c("shiny","devtools"))
library(shiny)
runGitHub(repo="Rsampling-shiny", username="andrechalom")
``` 
and wait for the browser window to open!

## Current features
* Access the Rsampling sample datafiles, or upload your own data in csv format
* Choose between a range of statistics, or write your own R function
* Select your randomization scheme, with or without replacement
* Reactive graph showing the distribuion of the statistic of interest and estimated p-value

## To do list:

* Trap common errors (function returns NA)
* Improve the interface help
* Excel-like interface to input data (like [this?] (https://github.com/AnalytixWare/ShinySky))
* more stats (F-statistics)
* better interface for selecting columns, also remove the redundancies between "s1", "m1", "d1", etc etc
* restricted randomization with a selector for the "stratum"
* add a progress bar
* add a button to install Rsampling from inside the app
* animations!
