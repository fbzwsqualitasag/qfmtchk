
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qfmtchk

<!-- badges: start -->
<!-- badges: end -->

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
#> Registered S3 methods overwritten by 'tibble':
#>   method     from  
#>   format.tbl pillar
#>   print.tbl  pillar
```

The above produces a tibble with all column names and the width of the
columns.

------------------------------------------------------------------------

*Latest Changes: 2021-06-10 12:00:43 (pvr)*
