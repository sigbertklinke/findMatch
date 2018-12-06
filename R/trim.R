#' trim
#'
#' Deletes leading and trailing whitespaces in a string.
#'
#' @param x character vector
#'
#' @references \href{https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace}{Stack overflow: How to trim leading and trailing whitespace?}
#'
#' @return cleaned character vector
#' @export
#'
#' @examples
#' trim (' Hello World ')
trim <- function (x) { gsub("^\\s+|\\s+$", "", x) }
