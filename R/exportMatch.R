#' exportMatch
#'
#' Exports the found matches into a data file for further manual inspection.
#' Sort the matched observations by their respective levensthein distance and delete non-matches from the file.
#'
#' @param match match structure
#' @param file file name
#' @param ... further parameters given to \code{\link[rio]{export}} method oth the library \code{rio}
#'
#' @return a file
#' @importFrom rio export
#' @export
#'
#' @examples
#' #' # create two data sets where the second consists of 50% observations of the first
#' n1 <- n2 <- 500
#' x1 <- generateTestData(n1)
#' x2 <- generateTestData(n2, x1)
#' x1$points <- findInterval(rnorm(n1, mean=10, sd=3), 0:15)-1
#' x2$points <- findInterval(rnorm(n2, mean=10, sd=3), 0:15)-1
#' #
#' match <- findMatch(list(x1,x2), c('code', 'code'))
#' exportMatch(match, 'match.xlsx', overwrite=TRUE)
exportMatch <- function (match, file, ...) {
  match$leven <- cbind(match$leven, rowSums(match$leven))
  export(as.data.frame(match), file, ...)
}
