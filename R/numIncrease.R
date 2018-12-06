#' numIncrease
#'
#' Checks for two possible matches the intervals $\[t_{i,j}-max_j, t_{i,j}-min_j\]$ intersect.
#' If the condition fails than each Levenshtein distance will be increased by one.
#'
#' @param match match structure
#' @param data  list of data frames
#' @param vars vector of variables. One for each data frame.
#' @param name list element name to store original variables
#' @param min minimal in/decrease, defaults to \code{rep(0, length(data))}
#' @param max maximal in/decrease, defaults to \code{0:(length(data)-1)}
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
#' match <- numIncrease(match, list(x1,x2), c('age', 'age'), 'age')
#' summary(match)
#' head(match)
#' \dontrun{
#' # with %>% operator
#' library('magrittr')
#' match <- findMatch(list(x1,x2), c('code', 'code')) %>%
#'          numIncrease(list(x1,x2), c('age', 'age'), 'age')
#' }
numIncrease <- function (match, data, vars, name=NULL, min=rep(0, length(data)), max=0:(length(data)-1)) {
  if (any(min>max)) stop ("It must hold for all entries: min<=max")
  res   <- match
  dvars <- matrix(data[[1]][match$line[,1],vars[1]], ncol=1)
  for (i in 2:length(vars)) dvars <- cbind(dvars, data[[i]][match$line[,i], vars[i]])
  d <- apply(dvars, 1, function(v) {
    p   <- which(!is.na(v))
    if (length(p)<2) return(0) # NA matches always
    mm <- c(v[p[1]]-max[p[1]], v[p[1]]-min[p[1]])
    for (j in 2:length(p)) {
      cmp <- v[p[j]]-max[p[j]]
      if (mm[1]<cmp) mm[1] <- cmp
      cmp <- v[p[j]]-min[p[j]]
      if (mm[2]>cmp) mm[2] <- cmp
    }
    return(mm[1]>mm[2])
  })
  res$leven <- match$leven + d
  if (!is.null(name)) {
    args <- list(match=res, data=data)
    args$name <- vars
    res <- do.call('addVars', args)
  }
  res
}
