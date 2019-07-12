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
#' set.seed(0)
#' # create two data sets which consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' #
#' match <- findMatch(x, c('code', 'code'))
#' summary(match)
#' match <- allIdentical(match, x, sex=c('sex', 'sex'))
#' summary(match)
#' \dontrun{
#' # with %>% operator
#' library('magrittr')
#' match <- findMatch(x, c('code', 'code')) %>%
#'          allIdentical(x, sex=c('sex', 'sex'))
#' }
allIdentical <- function(match, data, ...) {#
  #browser()
  args  <- list(...)
  nargs <- names(args)
  res   <- match
  for (i in 1:length(args)) {
    vars <- if(length(args[[i]])==1) rep(args[[i]], length(data)) else args[[i]] # recycle
    vname <- vars[1]
    if (!existsVars(vname, data[[1]])) stop(sprintf("variable '%s' does not exist in data sets", vname))   
    dvar1 <- data[[1]][match$line[,1],vname]
    dvars <- matrix(if (is.factor(dvar1)) as.character(dvar1) else dvar1, ncol=1)
    for (j in 2:length(data)) {
      vname <- vars[j]
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
