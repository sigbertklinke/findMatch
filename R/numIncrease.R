#' numIncrease
#'
#' Checks for two possible matches the intervals $\[t_{i,j}-max_j, t_{i,j}-min_j\]$ intersect.
#' If the condition fails than each Levenshtein distance will be increased by one.
#'
#' @param match match structure
#' @param data  list of data frames
#' @param min minimal in/decrease, defaults to \code{rep(0, length(data))}
#' @param max maximal in/decrease, defaults to \code{0:(length(data)-1)}
#' @param ... named list of variables (one for each data frame)
#'
#' @return updated match structure
#' @export
#'
#' @examples
#' # create two data sets where the second consists of 50% observations of the first
#' n1 <- n2 <- 500
#' x1 <- generateTestData(n1)
#' x2 <- generateTestData(n2, x1)
#' # create ages in years from birthdays
#' today  <- as.Date(Sys.time())
#' x1$age <- as.numeric(trunc(difftime(today, x1$birthday, unit="days")/365))
#' x2$age <- as.numeric(trunc(difftime(today+365, x2$birthday, unit="days")/365))
#' #
#' match <- findMatch(list(x1,x2), c('code', 'code'))
#' summary(match)
#' match <- numIncrease(match, list(x1,x2), age=c('age', 'age'))
#' summary(match)
#' head(match)
#' \dontrun{
#' # with %>% operator
#' library('magrittr')
#' match <- findMatch(list(x1,x2), c('code', 'code')) %>%
#'          numIncrease(list(x1,x2), age=c('age', 'age'), 'age')
#' }
numIncrease <- function (match, data, min=rep(0, length(data)), max=0:(length(data)-1), ...) {
  if (any(min>max)) stop ("It must hold for all entries: min<=max")
  args  <- list(...)
  nargs <- names(args)
  res   <- match
  for (i in 1:length(args)) {
    vname <- args[[i]][1]
    if (!existsVars(vname, data[[1]])) stop(sprintf("variable '%s' does not exist in data sets", vname))  
    dvars <- matrix(data[[1]][match$line[,1],vname], ncol=1)
    for (j in 2:length(data)) {
      vname <- args[[i]][j]
      if (!existsVars(vname, data[[j]])) stop(sprintf("variable '%s' does not exist in data sets", vname))  
      dvars <- cbind(dvars, data[[j]][match$line[,j], vname])
    }
    d <- apply(dvars, 1, function(v) {
      p <- which(!is.na(v))
      if (length(p)<2) return(0) # NA matches always
      mm <- c(v[p[1]]-max[p[1]], v[p[1]]-min[p[1]])
      for (k in 2:length(p)) {
        cmp <- v[p[k]]-max[p[k]]
        if (mm[1]<cmp) mm[1] <- cmp
        cmp <- v[p[k]]-min[p[k]]
        if (mm[2]>cmp) mm[2] <- cmp
      }
      return(mm[1]>mm[2])
    })
    res$leven <- match$leven + d
    argsi             <- list(match=res, data=data)
    argsi[[nargs[i]]] <- args[[i]]
    res <- do.call('addVars', argsi)
  }
  res
}
