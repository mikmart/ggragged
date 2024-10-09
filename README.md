
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggragged <a href="https://mikmart.github.io/ggragged/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/mikmart/ggragged/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikmart/ggragged/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/mikmart/ggragged/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mikmart/ggragged?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggragged)](https://CRAN.R-project.org/package=ggragged)
<!-- badges: end -->

ggragged extends the faceting system in
[ggplot2](https://ggplot2.tidyverse.org/) to ragged grids—a hybrid
layout between `facet_wrap()` and `facet_grid()`.

- `facet_ragged_rows()` groups panels into rows that can vary in length.
- `facet_ragged_cols()` groups panels into columns that can vary in
  length.

## Installation

Install the current release from
[CRAN](https://cran.r-project.org/package=ggragged):

``` r
install.packages("ggragged")
```

Or the development version from
[GitHub](https://github.com/mikmart/ggragged):

``` r
remotes::install_github("mikmart/ggragged")
```

## Example

Ragged grids can be used to clearly separate nested hierarchies in the
panel layout:

``` r
library(ggplot2)
library(ggragged)

p <- ggplot(mpg, aes(displ, cty)) + geom_point()
p + facet_ragged_rows(vars(drv), vars(cyl))
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
p + facet_ragged_cols(vars(cyl), vars(drv))
```

<img src="man/figures/README-example-2.png" width="100%" />
