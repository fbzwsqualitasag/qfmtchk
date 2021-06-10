#' ---
#' title: Output FMT Input Files
#' date:  2021-06-10
#' ---
#'
#' @title Write FMT Info To File
#'
#' @description
#' The tibble with the fmt information consisting of column names and
#' positions is taken and converted into an input file for the chckfwf
#' program.
#'
#' @param ps_fmt_outfile
#' @param ptbl_fmt
#'
#' @examples
#' \dontrun{
#' output_fmt(ps_fmt_outfile = "gal_check.fmt", ptbl_fmt = tbl_gal_fmt)
#' }
#'
#' @export output_fmt
output_fmt <- function(ps_fmt_outfile, ptbl_fmt){
  # check whether output file already exists, if yes delete it
  if (file.exists(ps_fmt_outfile)) {
    cat(" * FOUND output FMT File: ", ps_fmt_outfile, " ==> delete it ...\n")
    unlink(ps_fmt_outfile)
  }
  # compute the cumulative positions in ptbl_fmt
  tbl_fmt <- dplyr::bind_cols(ptbl_fmt, tibble::tibble(CumNrPos = cumsum(ptbl_fmt$NrPos)))
  # number of records in fmt-tibble
  n_nr_fmt_rec <- nrow(tbl_fmt)
  # start with first line outside of loop
  cat("# ", tbl_fmt$ColName[1], "\n", sep = "", file = ps_fmt_outfile, append = TRUE)
  cat("[1-", tbl_fmt$CumNrPos[1], "]\n", sep = "", file = ps_fmt_outfile, append = TRUE)
  cat("data_required=True \n\n", sep = "", file = ps_fmt_outfile, append = TRUE)
  if (n_nr_fmt_rec > 1){
    for (idx in 2:n_nr_fmt_rec){
      cat("# ", tbl_fmt$ColName[idx], "\n", sep = "", file = ps_fmt_outfile, append = TRUE)
      cat("[", tbl_fmt$CumNrPos[idx-1]+1, "-", tbl_fmt$CumNrPos[idx], "]\n", sep = "", file = ps_fmt_outfile, append = TRUE)
      cat("data_required=True \n\n", sep = "", file = ps_fmt_outfile, append = TRUE)
    }

  }
  return(invisible(TRUE))
}

