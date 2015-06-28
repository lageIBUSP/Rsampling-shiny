# Rsampling-shiny

Rsampling is a free and open source package intended for teaching and learning about
resampling in statistics.

To run, you need to install [R] (http://www.r-project.org/). Then, launch R and run
```R
install.package("shiny")
library(shiny)
runGitHub(repo="Rsampling-shiny", username="andrechalom")
``` 

and wait for the browser window to open!

## To do list:

* Improve the interface help
* Excel-like interface to input data (like [this?] (https://github.com/AnalytixWare/ShinySky))
* more stats (F-statistics)
* better interface for selecting columns, also remove the redundancies between "s1", "m1", "d1", etc etc
* restricted randomization with a selector for the "stratum"
* add a progress bar
* progress bar for the Rsampling installation
* animations!
