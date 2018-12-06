#' allIdentical
#'
#' Increases each Levenshtein distances by one if variable entries do not match
#'
#' @param match match structure
#' @param data  list of data frames
#' @param vars vector of variables. One for each data frame.
#' @param name list element name to store original variables
#'
#' @return
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
#' summary(match)
#' match <- allIdentical(match, list(x1,x2), c('sex', 'sex'), 'sex')
#' summary(match)
#' \dontrun{
#' # with %>% operator
#' library('magrittr')
#' match <- findMatch(list(x1,x2), c('code', 'code')) %>%
#'          allIdentical(list(x1,x2), c('sex', 'sex'), 'sex')
#' }
allIdentical <- function(match, data, vars, name=NULL) {
  res   <- match
  dvar1 <- data[[1]][match$line[,1],vars[1]]
  dvars <- matrix(if (is.factor(dvar1)) as.character(dvar1) else dvar1, ncol=1)
  for (i in 2:length(vars)) {
    dvari <- data[[i]][match$line[,i],vars[i]]
    dvars <- cbind(dvars, if (is.factor(dvar1)) as.character(dvari) else dvari)
  }
  d <- apply(dvars, 1, function(v) { length(unique(v))-1 })
  res$leven <- match$leven + d
  if (!is.null(name)) {
    args <- list(match=res, data=data)
    args$name <- vars
    res <- do.call('addVars', args)
  }
  res
}
