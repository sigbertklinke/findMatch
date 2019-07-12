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
#' set.seed(0)
#' library("findMatch")
#' # create two data sets where the second consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' #
#' match <- findMatch(x, c('code', 'code'))
#' exportMatch(match, 'match.xlsx', overwrite=TRUE)
exportMatch <- function (match, file, ...) {
  if (!is.null(match[['leven']])) match$leven <- cbind(match$leven, rowSums(match$leven))
  export(as.data.frame(match), file, ...)
}
