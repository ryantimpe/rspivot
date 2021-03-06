% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quicktibble.R
\name{quicktibble}
\alias{quicktibble}
\title{RStudio addin to quickly build small tibbles from an existing data frame or array.}
\usage{
quicktibble(.data = NULL)
}
\arguments{
\item{.data}{Optional data to initialize the new table. If a data frame, then the selection will default to this data frame. If an array, then the input will be the array.}
}
\value{
Text sting that can be read as a tribble in R
}
\description{
RStudio addin to quickly build small tibbles from an existing data frame or array.
Useful with dplyr::left_join().
}
