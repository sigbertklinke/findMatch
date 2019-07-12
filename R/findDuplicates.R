#' findDuplicates
#'
#' Finds possible duplicates in data set.
#'
#' @param data  data frame
#' @param var character: name of variable
#' @param dmax maximal levensthein distance for matching in text variables $l(t_{i1},t{j2]}<dmax$), defaults to \code{3}
#' @param exclude entries to be excluded from the unique values, defaults to \code{c('', '.')}
#' @param ignore.case if FALSE, the uniques values are case sensitive and if TRUE, case is ignored
#'
#' @return a list structure with possibly duplicates
#' @export
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
findDuplicates <- function(data, var, dmax=3, exclude=c("", "."), ignore.case=FALSE) {
  if (! 'data.frame' %in% class(data)) stop("only a data frame is allowed")
  match <- findMatch(data, vars=var, dmax=dmax, exclude=exclude, ignore.case=ignore.case)
  keep  <- match$line[,1]<match$line[,2]
  for (name in names(match)) match[[name]] <- match[[name]][keep,]
  match
}
