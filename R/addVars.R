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
#' set.seed(0)
#' # create two data sets which consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' #
#' match <- findMatch(x, c('code', 'code'))
#' match <- addVars(match, x, birthday=c('birthday', 'birthday'))
#' head(match)
#' summary(match)
addVars <- function(match, data, ...) {
  args  <- list(...)
  nargs <- names(args)
  for (i in 1:length(args)) {
    nelem <- nargs[i]
    if (is.null(nargs) || (nchar(nelem)==0)) nelem <- args[[i]][1]
    if (nelem %in% names(match)) stop('duplicate element names are not allowed')
    vars <- if(length(args[[i]])==1) rep(args[[i]], length(data)) else args[[i]] # recycle
    vname <- vars[1]
    if (!existsVars(vname, data[[1]])) stop(sprintf("variable '%s' does not exist in data sets", vname))
    v <- as_character(data[[1]][,vname])
    match[[nelem]] <- v[match$line[,1]]
    for (j in 2:length(data)) {
      vname <- vars[j]
      if (!existsVars(vname, data[[j]])) stop(sprintf("variable '%s' does not exist in data sets", vname))
      v <- as_character(data[[j]][,vname])
      match[[nelem]] <- cbind(match[[nelem]], v[match$line[,j]])
    }
    colnames(match[[nelem]]) <- sprintf('%.0f.%s', 1:length(data), args[[i]])
  }
  match
} 