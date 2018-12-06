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
#' # create a data set
#' x1 <- generateTestData(500)
#' #
#' match <- findDuplicates(x1, 'code')
#' head(match)
head.findMatch <- function(x, n=6L, ...) {
  x$leven <- cbind(x$leven, rowSums(x$leven))
  head(as.data.frame(x), n=n)
}

#' @rdname head
#' @export
#' @importFrom utils tail
tail.findMatch <- function(x, n=6L, ...) {
  x$leven <- cbind(x$leven, rowSums(x$leven))
  tail(as.data.frame(x), n=n)
}
