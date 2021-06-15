#'
#' @title Improve Format Specification
#'
#' @description
#' The improvement consists of removing rows from the fmt tibble, changing
#' the name of columns or of adding more rows to the fmt tibble.
#'
#' @details
#' The vector of row indices for the rows to be deleted can be specified with
#' a simple numberic vector. The list of column names to be renamed can be
#' specified with a tibble (ptbl_col_rename) with a column RowIndex and a
#' second column NewName where the first gives the row index where the column
#' name should be renamed and the second column contains the new column
#' name.
#'
#' @param ptbl_fmt original format tibble which needs to be improved
#' @param pvec_del_row_idx vector of row indices with rows to be deleted
#' @param ptbl_col_rename tibble with row indices and new column names
#' @param ptbl_fmt_add tibble with fmt columns to be added
#' @return tbl_imp_fmt improved format tibble
#'
#' @examples
#' \dontrun{
#' improve_fmt(ptbl_fmt = tbl_gal_fmt, pvec_del_row_idx = c(4:6))
#' improve_fmt(ptbl_fmt = tbl_gal_fmt,
#'             pvec_del_row_idx = 4:6,
#'             ptbl_col_rename = tibble::tibble(RowIndex = c(3,6,9),
#'                                              NewName=c("BlutanteilAnimal",
#'                                                        "BlutanteilVater",
#'                                                        "Traechtigkeitsdauer")))
#' }
#'
#' @export improve_fmt
improve_fmt <- function(ptbl_fmt,
                        pvec_del_row_idx = NULL,
                        ptbl_col_rename  = NULL,
                        ptbl_fmt_add     = NULL){
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
    answer <- "y"
    n_nr_rec <- nrow(tbl_imp_fmt)
    while (answer != ""){
      cat("Column Idx   | Column Name  |  Column Width\n")
      cat("=============|==============|==============\n")
      for (idx in 1:n_nr_rec){
        cat(idx, " | ", tbl_imp_fmt$ColName[idx], " | ", tbl_imp_fmt$NrPos[idx], "\n")
      }
      answer <- readline(prompt = " * Enter row index for which column name should be changed: ")
      n_rename_idx <- as.integer(answer)
      # get the new column line
      s_new_col_name <- readline(prompt = paste0(" ** Old name: ", tbl_imp_fmt$ColName[n_rename_idx], " -- new name: ", collapse = ""))
      tbl_imp_fmt$ColName[n_rename_idx] <- s_new_col_name
    }
  } else {
    n_nr_row_rename <- nrow(ptbl_col_rename)
    for (idx in 1:n_nr_row_rename){
      tbl_imp_fmt$ColName[ptbl_col_rename$RowIndex[idx]] <- ptbl_col_rename$NewName[idx]
    }
  }

  # check whether we have to add fmt columns
  if (!is.null(ptbl_fmt_add)){
    tbl_imp_fmt <- dplyr::bind_rows(tbl_imp_fmt, ptbl_fmt_add)
  }

  # return result
  return(tbl_imp_fmt)

}
