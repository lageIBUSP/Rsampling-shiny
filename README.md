# Rsampling-shiny

Rsampling is a free and open source package intended for teaching and learning about
resampling in statistics.

To run, install [R] (http://www.r-project.org/) and the [Rsampling library] (https://github.com/lageIBUSP/Rsampling).

Then, launch R and run
```R
install.package("shiny")
library(shiny)
runGitHub(repo="Rsampling-shiny", username="andrechalom")
``` 

and wait for the browser window to open!

## To do list:

* Improve the interface help
* Excel-like interface to input data (like [this?] (https://github.com/AnalytixWare/ShinySky)
* custom code for statistic of interest, more stats (aov, cor, diff, ???, enter custom code)
* better interface for selecting columns, and add a selector for the "stratum"
* add a progress bar
* add a button to install Rsampling from inside the app
* animations!
