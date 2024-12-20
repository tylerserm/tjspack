
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

``` r
library(tjspack)

example1 <- data.frame(
ID = c(1, 1, 1, 2, 2),
TestDate = as.Date(c("2023-01-01", "2023-01-01", "2023-04-01", "2023-02-01", "2023-02-01")),
DOB = as.Date(c("2017-06-01", "2017-06-01", "2017-06-01", "2017-12-14", "2017-12-14")),
Age = c(5.58, 5.58, 8.83, 5.05, 5.05))

check_age(example, "ID", "TestDate", "DOB", "Age")

example1
```
