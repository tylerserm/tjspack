# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @title My Hello World Function
#'
#'@description Prints 'Hello, thank you for using my package!'
#'
#' @param x The name of the person to say hi to
#'
#' @returns The output from \code{\link{print}}
#' @export
#'
#' @examples
#' hello("Paul")
#' \dontrun{
#' hello("George")
#' }
hello <- function(x) {
  print(paste0("Hello", x, ", thank you for using my package!"))
}
