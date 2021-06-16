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
  # read sql program
  vec_exp_prg <- read_sql_prg(ps_sql_prg_path    = ps_sql_prg_path,
                              ps_sql_exp_routine = ps_sql_exp_routine,
                              ps_exp_pattern     = ps_exp_pattern)

  # initialize format result
  tbl_fmt <- NULL
  n_start_pos <- 1
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

    # compute column end position
    n_end_pos <- n_start_pos + n_nr_pos - 1
    # collect result
    tbl_cur_res <- tibble::tibble(ColName  = s_col_name,
                                  StartPos = n_start_pos,
                                  EndPos   = n_end_pos)
    # compute start position of next column
    n_start_pos <- n_end_pos + 1
    if (is.null(tbl_fmt)){
      tbl_fmt <- tbl_cur_res
    } else {
      tbl_fmt <- dplyr::bind_rows(tbl_fmt, tbl_cur_res)
    }
  }
  # return result
  return(tbl_fmt)
}

## --- GAL fmt -----------------------------------------------------------------
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
                            ps_exp_pattern     = "PA_EXP.sFormat"){
  # call generic export function
  tbl_gal_fmt <- extract_generic_fmt(ps_sql_prg_path = ps_sql_prg_path,
                      ps_sql_exp_routine = ps_sql_exp_routine,
                      ps_exp_pattern = ps_exp_pattern)
  return(tbl_gal_fmt)
}


## --- KLDAT fmt -----------------------------------------------------------------
#'
#' @title Extract FMT Information For KLDAT
#'
#' @description
#' The format specification used in the SQL program that imports the carcass
#' data into the database are extracted and collected into a tibble where
#' each column in the KLDAT input file is listed with the start position.
#'
#' @param ps_sql_prg_path path to SQL program from where format is extracted
#' @param ps_sql_exp_routine name of export routine in SQL program
#' @param ps_exp_pattern pattern used to find export lines
#' @return tbl_kldat_fmt tibble with extracted format
#'
#' @export extract_kldat_fmt
extract_kldat_fmt <- function(ps_sql_prg_path,
                              ps_sql_exp_routine = "ImportSchlachtdatenTvd",
                              ps_exp_pattern     = "rowSTV.STV"){

  # read SQL statements with format statements
  vec_exp_prg <- read_sql_prg(ps_sql_prg_path    = ps_sql_prg_path,
                              ps_sql_exp_routine = ps_sql_exp_routine,
                              ps_exp_pattern     = ps_exp_pattern)
  # initialise result tibble
  tbl_kldat_fmt <- NULL
  # loop over vec_exp_prg
  for (idx in seq_along(vec_exp_prg)){
    # if not format information is available on the current line, continue
    if (length(grep(pattern = ",", vec_exp_prg[idx], fixed = TRUE)) == 0L) next
    # split current line and get start position
    n_start_pos <- as.integer(unlist(strsplit(vec_exp_prg[idx], ",", fixed = TRUE))[2])
    n_col_width <- unlist(strsplit(vec_exp_prg[idx], ",", fixed = TRUE))[3]
    n_col_width <- as.numeric(unlist(strsplit(n_col_width, ")", fixed = TRUE))[1])
    n_end_pos <- n_start_pos + n_col_width - 1


    # determine column name
    s_col_name <- unlist(strsplit(vec_exp_prg[idx], ",", fixed = TRUE))[1]
    s_col_name <- unlist(strsplit(s_col_name, ':', fixed = TRUE))[1]
    s_col_name <- unlist(strsplit(s_col_name, '.', fixed = TRUE))[2]
    s_col_name <- gsub(pattern = " ", replacement = "", s_col_name)
    # collect results
    # collect result
    tbl_cur_res <- tibble::tibble(ColName  = s_col_name,
                                  StartPos = n_start_pos,
                                  EndPos   = n_end_pos)
    if (is.null(tbl_kldat_fmt)){
      tbl_kldat_fmt <- tbl_cur_res
    } else {
      tbl_kldat_fmt <- dplyr::bind_rows(tbl_kldat_fmt, tbl_cur_res)
    }

  }

  return(tbl_kldat_fmt)
}


