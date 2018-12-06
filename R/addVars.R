#' addVars
#'
#' Adds further variables for inspection to the match without any check.
#'
#' @param match list structure with possibly matched observations 
#' @param data  list of data frames
#' @param ... names of variables, one for each data frame
#'
#' @return list structure with possibly matched observations with added variables
#' @export
#'
#' @examples
#' #' # create two data sets where the second consists of 50% observations of the first
#' n1 <- n2 <- 500
#' x1 <- generateTestData(n1)
#' x2 <- generateTestData(n2, x1)
#' x1$points <- findInterval(rnorm(n1, mean=10, sd=3), 0:15)-1
#' x2$points <- findInterval(rnorm(n2, mean=10, sd=3), 0:15)-1
#' #
#' match <- findMatch(list(x1,x2), c('code', 'code'))
#' match <- addVars(match, list(x1,x2), c('birthday', 'birthday'))
#' head(match)
#' summary(match)
addVars <- function(match, data, ...) {
  #browser()
  args  <- list(...)
  nargs <- names(args)
  for (i in 1:length(args)) {
    nelem <- nargs[i]
    if (is.null(nargs) || (nchar(nelem)==0)) nelem <- args[[i]][1]
    if (nelem %in% names(match)) stop('duplicate element names are not allowed')
    match[[nelem]] <- data[[1]][match$line[,1],args[[i]][1]]
    for (j in 2:length(data)) match[[nelem]] <- cbind(match[[nelem]], data[[j]][match$line[,j],args[[i]][j]])
  }
  match
} 