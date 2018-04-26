# Rsampling-shiny

[Rsampling](https://github.com/lageIBUSP/Rsampling) is a free and open source package intended for 
teaching and learning about statistics using randomization routines. 
Rsampling-shiny is a web-based graphical user interface
written in [shiny](http://shiny.rstudio.com).

To run, you need to install [R](http://www.r-project.org/) and the [Rsampling](https://github.com/lageIBUSP/Rsampling),
shiny and PerformanceAnalytics libraries. In Windows, note that you need at least R version 3.2, so please upgrade
if you are running an older version.

To install these dependencies, open R and run:

```R
install.packages(c("Rsampling", "shiny", "PerformanceAnalytics"))
```

![ ](www/chicken.png?raw=true)

## Running offline

If you need to be able to use Rsampling-shiny without Internet access, download the [latest stable version]
(https://github.com/lageIBUSP/Rsampling-shiny/releases), 
and extract it to a folder. Then, open R (with the required packages installed, see above) and run

```R
library(shiny)
runApp("<path to Rsampling-shiny>")
```
Remember that you need to install the packages listes above while you have internet access!

## Web version
You can run Rsampling shiny from github! Just open R and run:

```R
library(shiny)
runGitHub(repo="lageIBUSP/Rsampling-shiny")
``` 
and wait for the browser window to open!

## Languages
The app is currently available in English (default) and Portuguese (experimental).
To use the version in Portuguese, run the following command 
before running the app (`runGitHub(...)` or `runApp(...)`)
```R
language <- "pt"
```

## Current features
* Access the Rsampling sample datafiles, or upload your own data in csv format
* Choose between a range of statistics, or write your own R function
* Select your randomization scheme, with or without replacement
* Optional stratified randomization
* Reactive graph showing the distribuion of the statistic of interest and estimated p-value

## Bugs and issues

Please report any issue or suggestion at https://github.com/lageIBUSP/Rsampling-shiny/issues.
