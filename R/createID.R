#' createID
#'
#' Creates a variable with the unique values from all entries in \code{vars}.
#'
#' @param data list of data frames
#' @param vars vector of variables. One for each data frame.
#' @param exclude entries to be excluded from the unique values
#' @param ignore.case if FALSE, the uniques values are case sensitive and if TRUE, case is ignored
#'
#' @return a vector with the unique values
#' @export
#'
#' @examples
#' x1 <- generateTestData()
#' x2 <- generateTestData(x=x1)
#' ucode <- createID(list(x1, x2), vars=c('code', 'code'), ignore.case=TRUE)
#' head(ucode)
createID <- function(data, vars=NULL, exclude=c('', '.'), ignore.case = FALSE) {
  idn <- c()
  for (i in seq(length(data))) idn <- c(idn, trim(data[[i]][,vars[i]]))
  if (ignore.case) idn <- toupper(idn)
  idn <- sort(unique(idn))
  idn[!(idn %in% exclude)]
}
