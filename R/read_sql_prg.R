#'
#' @title Read SQL Program Code
#'
#' @description
#' The SQL code of the data export program is read and the part with the
#' export statements is returned. This part is expected to contain the
#' format information.
#'
#' @param ps_sql_prg_path path to sql program to read
#' @param ps_sql_exp_routine name of the routine from where format should be extracted
#' @param ps_exp_pattern pattern which indicate format information
#' @return vec_exp_prg vector with sql statements that contain the format
#'
read_sql_prg <- function(ps_sql_prg_path, ps_sql_exp_routine, ps_exp_pattern) {
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
  return(vec_exp_prg)
}

