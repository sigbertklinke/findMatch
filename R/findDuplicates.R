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
#' # create a data set
#' x1 <- generateTestData(500)
#' #
#' match <- findDuplicates(x1, 'code')
#' head(match)
findDuplicates <- function(data, var, dmax=3, exclude=c("", "."), ignore.case=FALSE) {
  match <- findMatch(data, vars=var, dmax=dmax, exclude=exclude, ignore.case=ignore.case)
  keep  <- match$line[,1]<match$line[,2]
  for (name in names(match)) match[[name]] <- match[[name]][keep,]
  match
}
