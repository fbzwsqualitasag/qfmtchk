#'
#' @title Delete Row from Format Tibble
#'
#' @description
#' Given a tibble with format information, a given number of rows
#' are deleted. After the deletion, the start and the end positions
#' must be re-computed
#'
#' @param ptbl_imp_fmt given format tibble
#' @param pvec_del_row_idx row indices for rows to be deleted
#' @return tbl_imp_fmt format tibble without rows to be delted
#'
#' @export delete_row
delete_row <- function(ptbl_imp_fmt, pvec_del_row_idx) {
  # intialise result with original version, which makes the result the
  #  same as the input, if no changes are specified.
  tbl_imp_fmt <- ptbl_imp_fmt
  n_nr_rec <- nrow(tbl_imp_fmt)

  # check whether indices of rows must be specified
  if (is.null(pvec_del_row_idx)){
    answer <- "y"
    while (answer != ""){
      cat("Column Idx   | Column Name  |  Column Width\n")
      cat("=============|==============|==============\n")
      for (idx in 1:n_nr_rec){
        cat(idx, " | ", tbl_imp_fmt$ColName[idx], " | ", tbl_imp_fmt$ColWidth[idx], "\n")
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

  # recompute positions
  tbl_imp_fmt <- compute_positions(ptbl_fmt = tbl_imp_fmt)

  # return result
  return(tbl_imp_fmt)
}


#' Rename Format Check Variables
#'
#' @description
#' For a given format tibble certain column variable names can be re-named.
#'
#' @param ptbl_imp_fmt format tibble with old variable names
#' @param ptbl_col_rename tibble containing indices and new names
#'
#' @return tbl_imp_fmt format tibble with new names
#' @export rename_variable
rename_variable <- function(ptbl_imp_fmt, ptbl_col_rename) {
  tbl_imp_fmt <- ptbl_imp_fmt
  # check whether the tibble with the column names to be changed is null
  if (is.null(ptbl_col_rename)){
    answer <- "y"
    n_nr_rec <- nrow(tbl_imp_fmt)
    while (answer != ""){
      cat("Column Idx   | Column Name  |  Column Width\n")
      cat("=============|==============|==============\n")
      for (idx in 1:n_nr_rec){
        cat(idx, " | ", tbl_imp_fmt$ColName[idx], " | ", tbl_imp_fmt$ColWidth[idx], "\n")
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
  return(tbl_imp_fmt)
}
