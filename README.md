
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dfsummary

<!-- badges: start -->
<!-- badges: end -->

The goal of dfsummary is to make descriptive summries of datasets, where
any person-sensitive information is automatically masked. In contrast to
other packages, eg the excellent `gtsummary` package, this package does
not aim to provide presentation-ready tables, but instead focuses on
keeping things simple by relying only on the `data.table` package for
efficient summarization. The summarized data is returned as a simple
data.table and the user can then finalize the table themselves using eg
the `gt` or `flextable` package.

## Installation

You can install the development version of dfsummary from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thomas-rasmussen/dfsummary")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dfsummary)
## basic example code
```
