#' existsVars
#'
#' Checks if all variables in \code{name} exists in the data frame \code{data}.
#'
#' @param name chracter: names of variables
#' @param data data frame
#'
#' @return \code{TRUE} if all names in \code{name} exists in \code{data} otherwise \code{FALSE}
#' @export
#'
#' @examples
existsVars <- function (name, data) { all(name %in% names(data)) }