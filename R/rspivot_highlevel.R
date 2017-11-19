#' \code{rspivot} specifications to clean up R CMD Check. No need to use otherwise.
#'
#' @docType package
#' @name rspivot_highlevel
#' @importFrom dplyr %>%

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

utils::globalVariables(c("ValueNames", "value", 
                         "Metric_calc", "Values", "Growth", "Difference", "Shares",
                         "*Total*",
                         "dim_x", "dim_y"))