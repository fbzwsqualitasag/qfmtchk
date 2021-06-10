#' ---
#' title: Improvement Functions for Format Specification
#' ---
#'
#' @title Improve Format Specification
#'
#' @description
#' The improvement consists of removing rows from the fmt tibble, changing
#' the name of columns or of adding more rows to the fmt tibble.
#'
#' @param ptbl_fmt original format tibble which needs to be improved
#' @return tbl_imp_fmt improved format tibble
#'
#' @examples
#' \dontrun{
#'  improve_fmt(ptbl_fmt = tbl_gal_fmt, pvec_del_row_idx = c(4:6))
#' }
#'
#' @export improve_fmt
improve_fmt <- function(ptbl_fmt,
                        pvec_del_row_idx = NULL,
                        ptbl_col_rename = NULL){
  # intialise result with original version, which makes the result the
  #  same as the input, if no changes are specified.
  tbl_imp_fmt <- ptbl_fmt
  n_nr_rec <- nrow(tbl_imp_fmt)
  # check whether indices of rows must be specified
  if (is.null(pvec_del_row_idx)){
    answer <- "y"
    while (answer != ""){
      cat("Column Idx   | Column Name  |  Column Width\n")
      cat("=============|==============|==============\n")
      for (idx in 1:n_nr_rec){
        cat(idx, " | ", tbl_imp_fmt$ColName[idx], " | ", tbl_imp_fmt$NrPos[idx], "\n")
      }
      answer <- readline(prompt = " * Enter indices of rows to be deleted: ")
      n_del_idx <- as.integer(unlist(strsplit(answer, split = ",")))
      tbl_imp_fmt <- dplyr::slice(tbl_imp_fmt, -n_del_idx)
      cat(" ** Deleted row: ", n_del_idx, "\n")
      n_nr_rec <- nrow(tbl_imp_fmt)
    }
  } else {
    tbl_imp_fmt <- dplyr::slice(tbl_imp_fmt, -pvec_del_row_idx)
  }

  # check whether the tibble with the column names to be changed is null
  if (is.null(ptbl_col_rename)){

  } else {

  }


  # return result
  return(tbl_imp_fmt)

}
