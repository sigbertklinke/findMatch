#' allIdentical
#'
#' Increases each Levenshtein distances by one if variable entries do not match
#'
#' @param match match structure
#' @param data  list of data frames
#' @param ... named list of variables (one for each data frame)
#'
#' @return returns a modified match 
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
#' match <- allIdentical(match, list(x1,x2), sex=c('sex', 'sex'))
#' summary(match)
#' \dontrun{
#' # with %>% operator
#' library('magrittr')
#' match <- findMatch(list(x1,x2), c('code', 'code')) %>%
#'          allIdentical(list(x1,x2), sex=c('sex', 'sex'))
#' }
allIdentical <- function(match, data, ...) {#
  #browser()
  args  <- list(...)
  nargs <- names(args)
  res   <- match
  for (i in 1:length(args)) {
    vname <- args[[i]][1]
    if (!existsVars(vname, data[[1]])) stop(sprintf("variable '%s' does not exist in data sets", vname))   
    dvar1 <- data[[1]][match$line[,1],vname]
    dvars <- matrix(if (is.factor(dvar1)) as.character(dvar1) else dvar1, ncol=1)
    for (j in 2:length(data)) {
      vname <- args[[i]][j]
      if (!existsVars(vname, data[[j]])) stop(sprintf("variable '%s' does not exist in data sets", vname))   
      dvari <- data[[j]][match$line[,j],vname]
      dvars <- cbind(dvars, if (is.factor(dvar1)) as.character(dvari) else dvari)
    }
    d <- apply(dvars, 1, function(v) { length(unique(v))-1 })
    res$leven <- match$leven + d
    argsi             <- list(match=res, data=data)
    argsi[[nargs[i]]] <- args[[i]]
    res <- do.call('addVars', argsi)
  }
  res
}
