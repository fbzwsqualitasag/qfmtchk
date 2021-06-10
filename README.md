
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qfmtchk

<!-- badges: start -->
<!-- badges: end -->

The website of `qfmtchk` is available at:
<https://fbzwsqualitasag.github.io/qfmtchk/>

The goal of qfmtchk is to provide functionalities related to checks and
verification of input data for the genetic evaluations at Qualitas AG.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fbzwsqualitasag/qfmtchk")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(qfmtchk)
## basic example code
s_sql_prg_path <- system.file("extdata", "zws_gal_pgb.sql", package = "qfmtchk")
tbl_gal_fmt <- extract_gal_fmt(ps_sql_prg_path = s_sql_prg_path)
```

The above produces a tibble with all column names and the width of the
columns. This tibble can be improved and corrected at the users
convenience with the following command

``` r
tbl_gal_fmt <- improve_fmt(ptbl_fmt = tbl_gal_fmt,
                           pvec_del_row_idx = 4:6,
                           ptbl_col_rename = tibble::tibble(RowIndex = c(3,6,9),
                                                            NewName=c("BlutanteilAnimal", 
                                                                      "BlutanteilVater", 
                                                                      "Traechtigkeitsdauer")))
```

The improved fmt-tibble can be written to an fmt-file using

``` r
s_gal_fmt_file <- "gal_check.fmt"
output_fmt(ps_fmt_outfile = s_gal_fmt_file, ptbl_fmt = tbl_gal_fmt)
```

------------------------------------------------------------------------

*Latest Changes: 2021-06-10 17:22:00 (pvr)*
