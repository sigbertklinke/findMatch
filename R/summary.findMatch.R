#' summary.findMacth
#'
#' Shows the numer of possible matches and, if any matches available, a contingency table of  
#'
#' @param object  a list structure with possibly matched observations
#' @param ... further parameters (unused)
#'
#' @return summary of possible matches
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
#' match <- findMatch(x, c('code', 'code'))
#' head(match)
#' summary(match)
summary.findMatch <- function (object, ...) {
  if (is.null(object$idn)) {
    t <- apply(object$line, 1, function(v) { t <- as.character(1:length(v)); paste0(t[!is.na(v)], collapse='-') }  )
    cat(sprintf("%0.f matches\n", nrow(object$line)))
    print(table(t))
  } else {
    cat(sprintf("%0.f matches\n", nrow(object$idn)))
    if (nrow(object$idn)) 
      print(table(split(object$leven, c(col(object$leven, as.factor = TRUE)))))
  }
}
