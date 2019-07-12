#' deleteDups
#'
#' Deletes duplicate entries according to some variable in the match
#'
#' @param match match structure
#'
#' @return a modified match
#' @export
#' @examples 
#' set.seed(0)
#' # create two data sets which consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' #
#' match <- findMatch(x, c('code', 'code'))
#' summary(match)
#' match <- deleteDups(match)
#' summary(match)
deleteDups <- function(match) {
  res  <- match
  keep <- !duplicated(match$uid)
  for (elem in names(match)) res[[elem]] <- res[[elem]][keep,]
  res
}
