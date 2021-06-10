#' ---
#' title: Extract Format Definition From Export Program
#' date:  2021-10-09
#' ---
#'
#' @title Generic Extraction of Data Format Information
#'
#' @description
#' The format information is extracted from the SQL-Export routine. This
#' routine has to be stored in a textfile whose path is given by the
#' parameter ps_sql_prg_path. In that SQL program all code between the
#' beginning and the end of the specified routine name (ps_sql_exp_routine)
#' is investigated for the pattern given by ps_exp_pattern.
#'
#' @details
#' The format extraction works only, if the export routine is written in
#' a specific way. If these assumptions are not met, the extraction does
#' not work.
#'
#' @param ps_sql_prg_path path to sql program
#' @param ps_sql_exp_routine name of export routine
#' @return tbl_fmt tibble with data column name and the width of the column
#'
extract_generic_fmt <- function(ps_sql_prg_path,
                                ps_sql_exp_routine,
                                ps_exp_pattern){
  # check whether ps_sql_prg_path exists
  if (!file.exists(ps_sql_prg_path)) stop(" *** ERROR: CANNOT FIND ps_sql_prg_path: ", ps_sql_prg_path)
  # read export program
  con_exp_prg <- file(description = ps_sql_prg_path)
  vec_exp_prg <- readLines(con = con_exp_prg)
  close(con = con_exp_prg)
  # search for name of export routine
  n_start_exp_proc <- grep(pattern = paste("PROCEDURE", ps_sql_exp_routine), vec_exp_prg, fixed = TRUE)
  n_end_exp_proc <- grep(pattern = paste("END", ps_sql_exp_routine), vec_exp_prg, fixed = TRUE)
  if (length(n_start_exp_proc) != 1L | length(n_end_exp_proc) != 1L)
    stop(" *** ERROR: CANNOT DETERMINE start and end of export routine")
  # reduce vec_exp_prg to range of export routine
  vec_exp_prg <- vec_exp_prg[n_start_exp_proc:n_end_exp_proc]
  # search for export statement
  vec_exp_idx <- grep(pattern = ps_exp_pattern, vec_exp_prg, fixed = TRUE)
  # reduce to section with export statements
  vec_exp_prg <- vec_exp_prg[vec_exp_idx]
  # remove comment lines
  vec_comm_idx <- grep(pattern = "^--", vec_exp_prg)
  if (length(vec_comm_idx) != 0L){
    vec_exp_prg <- vec_exp_prg[-vec_comm_idx]
  }
  # initialize format result
  tbl_fmt <- NULL
  # extract formats
  for (idx in seq_along(vec_exp_prg)){
    if (length(grep(pattern = "to_char", vec_exp_prg[idx], ignore.case = TRUE)) == 0L){
      n_col_pos_idx <- 2
      if (length(grep(pattern = "PA_SYS", vec_exp_prg[idx], ignore.case = TRUE)) == 0L){
        n_col_name_idx <- 2
      } else {
        n_col_name_idx <- 3
      }

    } else {
      n_col_pos_idx <- 3
      n_col_name_idx <- 3
    }
    n_nr_pos <- as.integer(unlist(strsplit(vec_exp_prg[idx], split = ",", fixed = TRUE))[n_col_pos_idx])
    s_col_name <- unlist(strsplit(vec_exp_prg[idx], split = ",", fixed = TRUE))[1]
    s_col_name <- unlist(strsplit(s_col_name, split = "(", fixed = TRUE))[n_col_name_idx]
    s_col_name <- gsub(pattern = " ", replacement = "", s_col_name, fixed = TRUE)
    s_col_name <- gsub(pattern = ")", replacement = "", s_col_name, fixed = TRUE)

    # collect result
    tbl_cur_res <- tibble::tibble(ColName = s_col_name,
                          NrPos   = n_nr_pos)
    if (is.null(tbl_fmt)){
      tbl_fmt <- tbl_cur_res
    } else {
      tbl_fmt <- dplyr::bind_rows(tbl_fmt, tbl_cur_res)
    }
  }
  # return result
  return(tbl_fmt)
}

#'
#'
#' @title Extract GAL Export Format
#'
#' @description
#' Extraction of format specification of exported GAL data based on SQL export
#' routine. This function is a wrapper which uses the generic export function
#' \code{extract_generic_fmt}.
#'
#' @details
#' Usefulness of this function depends on the format of the SQL export routine.
#' If that format changes, the result might be completely unusable. This function
#' should only be used once to extract the format. Any updates should be done
#' on already existing fmt-input files.
#'
#' @param ps_sql_prg_path path to sql program
#' @param ps_sql_exp_routine name of export routine
#' @param ps_exp_pattern pattern to search for export commands
#' @return tbl_gal_fmt tibble with the column headers and the column width
#'
#' @examples
#' \dontrun{
#' s_sql_prg_path <- system.file("extdata", "zws_gal_pgb.sql", package = "qfmtchk")
#' tbl_gal_fmt <- extract_gal_fmt(ps_sql_prg_path = s_sql_prg_path)
#' }
#'
#' @export extract_gal_fmt
extract_gal_fmt <- function(ps_sql_prg_path,
                            ps_sql_exp_routine = "ExportGAL",
                            ps_exp_pattern = "PA_EXP.sFormat"){
  # call generic export function
  tbl_gal_fmt <- extract_generic_fmt(ps_sql_prg_path = ps_sql_prg_path,
                      ps_sql_exp_routine = ps_sql_exp_routine,
                      ps_exp_pattern = ps_exp_pattern)
  return(tbl_gal_fmt)
}

