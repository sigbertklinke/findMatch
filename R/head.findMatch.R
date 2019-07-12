#' head/tail
#'
#' \code{head} shows the first \code{n} possible matches and \code{tail} shows the last \code{n} possible matches.
#'
#' @param x a list structure with possibly matched observations
#' @param n number of lines to show, defaults to 6
#' @param ... further arguments (unused)
#'
#' @return the first/last n possible matches as data frame
#' @rdname head
#' @export
#' @importFrom utils head
#'
#' @examples
#' set.seed(0)
#' # create two data sets where the second consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' #
#' #
#' match <- findDuplicates(x[[1]], 'code')
#' head(match)
head.findMatch <- function(x, n=6L, ...) {
  if (!is.null(x$leven)) x$leven <- cbind(x$leven, rowSums(x$leven))
  head(as.data.frame(x), n=n)
}

#' @rdname head
#' @export
#' @importFrom utils tail
tail.findMatch <- function(x, n=6L, ...) {
  if (!is.null(x$leven)) x$leven <- cbind(x$leven, rowSums(x$leven))
  tail(as.data.frame(x), n=n)
}
