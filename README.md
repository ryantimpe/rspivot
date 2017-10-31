## Introduction

rspivot is a Shiny gadget addin for RStudio. rspivot provides an alternative to the `View()` function, allowing examination of a data frame as a pivot table rather than flat file.

The data is presented as columns and nested rows, summing over all data values not explicitly shown in the data table. Additionally, the user can filter each column in the data frame to specific elements to examine subtotals. Row totals or subtotals are shown in last column on the right.

Row, column, and filter selections can be saved as a function call for loading the same view with updated data. This function can be saved to the clipboard or printed directly in an open script.

In the "Data Options" tab, the user can add growth rate or share metrics to the data, with the ability to nest those metrics with the original data.

See my [GitHub](https://github.com/ryantimpe/rspivot) for the script and latest updates. 

### Prerequisites

This package requires RStudio is built using the [tidyverse](https://github.com/tidyverse/tidyverse) and [shiny](http://shiny.rstudio.com/articles/gadgets.html) packages. 


### Installing

``` r
install.packages(devtools)
devtools::install_github("ryantimpe/rspivot")
```
## Built With

* [R](https://www.r-project.org/) - R
* [Shiny](http://shiny.rstudio.com/articles/gadgets.html) - Shiny Gadgets
* [tidyverse](https://github.com/tidyverse/tidyverse) - Tidyverse packages

## Versioning

For the versions available, see the [tags on this repository](https://github.com/ryantimpe/rspivot/tags). 

## Authors

* **Ryan Timpe** - *Initial work* - [ryantimpe](https://github.com/ryantimpe)

See also the list of [contributors](https://github.com/ryantimpe/rspivot/contributors) who participated in this project.

## License

No license.


