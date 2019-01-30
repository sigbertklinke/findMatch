#' addVars
#'
#' Adds further variables for inspection to the match without any check.
#'
#' @param match list structure with possibly matched observations 
#' @param data  list of data frames
#' @param ... named list of variables (one for each data frame)
#'
#' @return list structure with possibly matched observations with added variables
#' @importFrom sjlabelled as_character 
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
#' match <- addVars(match, list(x1,x2), birthday=c('birthday', 'birthday'))
#' head(match)
#' summary(match)
addVars <- function(match, data, ...) {
  args  <- list(...)
  nargs <- names(args)
  for (i in 1:length(args)) {
    nelem <- nargs[i]
    if (is.null(nargs) || (nchar(nelem)==0)) nelem <- args[[i]][1]
    if (nelem %in% names(match)) stop('duplicate element names are not allowed')
    vname <- args[[i]][1]
    if (!existsVars(vname, data[[1]])) stop(sprintf("variable '%s' does not exist in data sets", vname))
    v <- as_character(data[[1]][,vname])
    match[[nelem]] <- v[match$line[,1]]
    for (j in 2:length(data)) {
      vname <- args[[i]][j]
      if (!existsVars(vname, data[[j]])) stop(sprintf("variable '%s' does not exist in data sets", vname))
      v <- as_character(data[[j]][,vname])
      match[[nelem]] <- cbind(match[[nelem]], v[match$line[,j]])
    }
    colnames(match[[nelem]]) <- sprintf('%.0f.%s', 1:length(data), args[[i]])
  }
  match
} 