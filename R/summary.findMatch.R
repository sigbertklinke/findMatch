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
#' # create two data sets where the second consists of 50% observations of the first
#' n1 <- n2 <- 500
#' x1 <- generateTestData(n1)
#' x2 <- generateTestData(n2, x1)
#' x1$points <- findInterval(rnorm(n1, mean=10, sd=3), 0:15)-1
#' x2$points <- findInterval(rnorm(n2, mean=10, sd=3), 0:15)-1
#' #
#' match <- findMatch(list(x1,x2), c('code', 'code'))
#' head(match)
#' summary(match)
summary.findMatch <- function (object, ...) {
  cat(sprintf("%0.f matches\n", nrow(object$idn)))
  if (nrow(object$idn)) 
    print(table(split(object$leven, c(col(object$leven, as.factor = TRUE)))))
}
