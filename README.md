[![Travis build status](https://travis-ci.org/ryantimpe/rspivot.svg?branch=master)](https://travis-ci.org/ryantimpe/rspivot)

## Introduction

rspivot is a Shiny gadget for RStudio for viewing data frames.

The `View()` function in RStudio displays data frames and tibbles as they are stored in the R environment: flat files of many rows and columns. Though `View()` provides the ability to filter series using a single parameter or value range, it lacks many tools necessary for properly examing output. `rspivot()` provides an alternative to `View()` by viewing data frames and tibbles as interactive pivot tables.

Key features of `rspivot()` include:

* Row, nested row, and column selection - the table summarizes over data values not explicitly shown in the table
* Summarizing data using sum, mean, count, and other common functions
* Marginal totals for rows, nested rows, and columns
* Data transformations including %Growth, %Shares, and differences
* Resuming the selected filters and data metrics using the save feature

Although `rspivot()` provides some calculations, like the `View()` function, `rspivot()` should be used to examine and review the results of analysis. For this reason, the goal is to keep the functionality of the gadget to a minimum, encouraging the user to use [tidyverse](https://github.com/tidyverse/tidyverse) for the bulk of analysis.

See my [GitHub](https://github.com/ryantimpe/rspivot) for the script and [GitHub.io](https://ryantimpe.github.io/rspivot/) for latest updates and examples. 

## Installation

This package requires RStudio and is built using the [tidyverse](https://github.com/tidyverse/tidyverse) and [shiny](http://shiny.rstudio.com/articles/gadgets.html) packages. 

rspivot is not yet on CRAN, but you can install the latest development version from GitHub.

``` r
#install.packages(devtools)
devtools::install_github("ryantimpe/rspivot")
```

## Built With

rspivot uses [Shiny](http://shiny.rstudio.com/articles/gadgets.html) to control a hierarchy of [tidyverse](https://github.com/tidyverse/tidyverse) commands to filter, mutate, and summarize a data frame. The output of those functions is displayed using an [rhandsontable](https://github.com/jrowen/rhandsontable) table and [ggplot2](http://ggplot2.org/) charts.

## Versioning

Version 0.1.0 is the initial public release of this package. Only available on GitHub.

See the [tags on this repository](https://github.com/ryantimpe/rspivot/tags). 

## Authors

* **Ryan Timpe** - [ryantimpe](https://github.com/ryantimpe)

See also the list of [contributors](https://github.com/ryantimpe/rspivot/contributors) who participated in this project.

## License

GPL-3


