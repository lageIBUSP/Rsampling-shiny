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
* Excel-like interface to input data
* Upload file
* checkboxes for selecting columns
* selection of statistic of interest (aov, cor, diff, ???, enter custom code)
* add a progress bar
* add a button to install Rsampling from inside the app
