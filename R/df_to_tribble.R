#' Convert a reactive data frame in Shiny to a tribble for editor
#'
#' Convert a reactive data frame in Shiny to a tribble for editor
#' Mostly for use inside Shiny RStudio Addins
#'
#' @param .data A data frame to convert
#' @param .name Optional name for tribble output
#' @return Text sting that can be read as a tribble in R
#' @export

df_to_tribble <- function(.data, .name = NULL){
  dat <- .data %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    #Add single quotes to each character...
    dplyr::mutate_if(is.character, dplyr::funs(ifelse(is.na(.), NA, paste0("'", ., "'")))) %>%
    dplyr::mutate_all(dplyr::funs(ifelse(is.na(.), "", .)))

  #Width of each column
  op_widths <- 1:ncol(dat) %>%
    purrr::map_int(function(i){
      max(max(nchar(dat[, i])), nchar(names(dat)[i]))
    })

  #Header
  op_names <- 1:ncol(dat) %>%
    purrr::map_chr(function(i){
      leading_spaces <- strrep(" ", op_widths[i] - nchar(names(dat)[i]) + 1)
      paste0(leading_spaces, "~", names(dat)[i])
    }) %>%
    paste(collapse = ",")

  #Rows
  op_rows <- 1:nrow(dat) %>%
    purrr::map_chr(function(rw){

      dat.row <- dat[rw, ]

      1:ncol(dat.row) %>%
        purrr::map_chr(function(i){
          leading_spaces <- strrep(" ", op_widths[i] - nchar(dat.row[i]) + 2)
          paste0(leading_spaces, dat.row[i])
        }) %>%
        paste(collapse = ",") %>%
        stringr::str_replace_all("  ,", "NA,")
    }) %>%
    paste(collapse = ", \n")

  #Tribble
  op_trib <- paste("tibble::tribble(",
                   paste0(op_names, ","),
                   op_rows,
                   ")\n\n",
                   sep = "\n"
                   )

  if(!is.null(.name)){
    op_trib <- paste(
      make.names(.name), "<-", op_trib
    )
  }

  return(op_trib)
}


