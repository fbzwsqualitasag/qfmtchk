#'
#' @title Compute Start and End Positions
#'
#' @description
#' Start and end positions are re-computed based on the width column given in
#' the format tibble ptbl_fmt. Any existing start and end positions are updated.
#'
#' @param ptbl_fmt format tibble for which start and end positions must be computed
#'
#' @return tbl_fmt format tibble with new start and end positions
compute_positions <- function(ptbl_fmt){
  tbl_fmt <- ptbl_fmt
  # recompute positions
  n_new_nr_rec <- nrow(tbl_fmt)
  n_first_start_pos <- tbl_fmt$StartPos[1]
  vec_end_pos <- cumsum(tbl_fmt$ColWidth) + n_first_start_pos - 1
  vec_start_pos <- c(n_first_start_pos, (vec_end_pos[1:(n_new_nr_rec-1)]+1))
  tbl_fmt$StartPos <- vec_start_pos
  tbl_fmt$EndPos <- vec_end_pos

  return(tbl_fmt)
}
