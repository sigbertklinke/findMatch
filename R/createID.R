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
#' set.seed(0)
#' # create two data sets which consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' ucode <- createID(x, vars=c('code', 'code'), ignore.case=TRUE)
#' head(ucode)
createID <- function(data, vars=NULL, exclude=c('', '.'), ignore.case = FALSE) {
  idn <- c()
  for (i in seq(length(data))) idn <- c(idn, trimws(data[[i]][,vars[i]]))
  if (ignore.case) idn <- toupper(idn)
  idn <- sort(unique(idn))
  idn[!(idn %in% exclude)]
}
