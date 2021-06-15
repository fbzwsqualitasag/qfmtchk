#'
#' @title Write FMT Info To File
#'
#' @description
#' The tibble with the fmt information consisting of column names and
#' positions is taken and converted into an input file for the chckfwf
#' program.
#'
#' @param ps_fmt_outfile output file for fmt information
#' @param ptbl_fmt tibble containing fmt information
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
  # number of records in fmt-tibble
  n_nr_fmt_rec <- nrow(ptbl_fmt)
  # start with first line outside of loop
  for (idx in 1:n_nr_fmt_rec){
    cat("# ", ptbl_fmt$ColName[idx], "\n", sep = "", file = ps_fmt_outfile, append = TRUE)
    cat("[", ptbl_fmt$StartPos[idx], "-", ptbl_fmt$EndPos[idx], "]\n", sep = "", file = ps_fmt_outfile, append = TRUE)
    cat("data_required=True \n\n", sep = "", file = ps_fmt_outfile, append = TRUE)
  }

  return(invisible(TRUE))
}

