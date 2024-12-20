
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tjspack

<!-- badges: start -->
<!-- badges: end -->

The goal of tjspack is to assist in data cleaning for longitudinal study
datasets. Functions within are intended to ensure demographic data
doesn’t change for subjects on different visits, dates visit numbers,
and ages are entered correctly. As well as checking if things are
correctly ordered chronologically

## Installation

You can install the development version of tjspack like so:

``` r
library(devtools)
install_github("tylerserm/tjspack")
```

## Example

This is a basic example which details how one can use the check_age
function. Note that for ID 1, the age is entered incorrectly on the
third visit
