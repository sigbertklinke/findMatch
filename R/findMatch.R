#' findMatch
#'
#' Finds matches between two or more data sets based on a text variable (code or e-mail) based on
#' \href{https://en.wikipedia.org/wiki/Levenshtein_distance}{Levensthein distances}. For a detailed application see the vignette.
#'
#' The result consists of a list with three elements
#' \describe{
#' \item{\code{line}}{a matrix with the line numbers of the matching observations}
#' \item{\code{idn}}{a matrix with the common ID \code{ZDV} and the original text variables in the data sets}
#' \item{\code{leven}}{a matrix with the levenshtein distance between the common ID and the original text variables in the data sets}
#' }
#'
#' @param data  list of data frames
#'
#' @return a list structure with possibly matched observations
#' @export
findMatch <- function(data, ...) { UseMethod("findMatch") }

#' @param vars vector of variables. One for each data frame.
#' @param dmax maximal levensthein distance for matching in text variables $l(t_{i1},t{j2]}<dmax$), defaults to \code{3}
#' @param exclude entries to be excluded from the unique values, defaults to \code{c('', '.')}
#' @param ignore.case if FALSE, the uniques values are case sensitive and if TRUE, case is ignored
#' @param unique.id vector of variables which contain a unique ID over all data sets. If not given then \code{filename:lineno} will be used.
#' @param output number of observation to analyse before a progress information is displayed
#' @param cmpfunc function for comparison of strings of form \code{fun(x, y, ignore.case, ...)} (default: \code{\link[utils]{adist}})
#' @param ... further parameters for cmp
#'
#' @rdname findMatch
#' @return a list structure with possibly matched observations
#' @importFrom utils adist
#' @export
#'
#' @examples
#' set.seed(0)
#' # create two data sets where the second consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' # match by code
#' match <- findMatch(x, c('code', 'code'))
#' head(match)
#' summary(match)
findMatch.default <- function (data, vars, dmax=3, exclude=c("", "."), ignore.case=FALSE, unique.id=NULL, output=50, cmpfunc=NULL, ...) {
  dups <- is.data.frame(data)
  if (dups) {
    idn  <- data[[vars]]
    data <- list(data, data)
    vars <- c(vars, vars)
  } else {
    vars <- if(length(vars)==1) rep(vars, length(data)) else vars # recycle
    idn <- createID(data, vars, exclude=exclude, ignore.case=ignore.case)
  }
  res     <- list(line  = matrix(0, ncol=length(data), nrow=0),
                  uid   = matrix('', ncol=length(data), nrow=0),
                  idn   = matrix('', ncol=1+length(data), nrow=0),
                  leven = matrix(0, ncol=length(data), nrow=0))
  zdv     <- list()
  for (i in seq(length(data))) {
    vname <- vars[i]
    if (!existsVars(vname, data[[i]])) stop(sprintf("variable '%s' does not exist in data sets", vname))   
    zdv[[i]] <- trimws(data[[i]][,vname])
  }
  uid <- list()
  if (is.null(unique.id)) {
    for (i in seq(length(data))) uid[[i]] <- paste0(i, ':', 1:nrow(data[[i]]))
  } else {
    for (i in seq(length(data))) {
      vname <- unique.id[i]
      if (!existsVars(vname, data[[i]])) stop(sprintf("variable '%s' does not exist in data sets", vname))
      uid[[i]] <- trimws(data[[i]][,vname])
    }
  }
  if (anyDuplicated(unlist(uid))) warning("Unique ID contain duplicates")
  if (output<1) output <- length(idn)+1
  cmpargs <- list(...)
  cmpargs$ignore.case <- ignore.case
  for (j in seq(length(idn))) {
    #browser()
    if ((j%%output)==0)
      print(sprintf("%.0f/%.0f", j, length(idn)))
    matches  <- list()
    d        <- rep(0, length(data))
    anymatch <- TRUE
    for (i in seq(length(data))) {
      cmpargs$x <- ifelse(zdv[[i]] %in% exclude, NA, zdv[[i]])
      cmpargs$y <- idn[j]
      di   <- if (is.null(cmpfunc)) do.call('adist', cmpargs) else do.call(cmpfunc, cmpargs)
      d[i] <- min(di, na.rm=TRUE)
      if (d[i]<dmax) {
        matches[[i]] <- which(if(dups) (di<dmax) else (di==d[i]))
        attr(matches[[i]], 'leven') <- di[matches[[i]]]
      } else anymatch <- FALSE
    }
    #browser(expr=(idn[j]=="ab03rec"))
    if (anymatch) {
      grid <- expand.grid(matches)
      nr   <- nrow(grid)
      if (nr>0) {
        #browser(expr = (j==15))
        resi<- list(idn   = matrix('', ncol=1+length(data), nrow=nr),
                    uid   = matrix('', ncol=length(data), nrow=nr),
                    line  = matrix(0, ncol=length(data), nrow=nr),
                    leven = matrix(0, ncol=length(data), nrow=nr))
        resi$idn[,1] <- rep(idn[j], nr)
        for (i in seq(length(data))) {
          resi$idn[,1+i] <- zdv[[i]][grid[[i]]]
          resi$line[,i]  <- grid[[i]]
          resi$uid[,i]   <- uid[[i]][grid[[i]]] 
          resi$leven[,i] <- attr(matches[[i]], 'leven')[match(grid[,i], matches[[i]])]
        }
        res$idn   <- rbind(res$idn, resi$idn)
        res$uid   <- rbind(res$uid, resi$uid)
        res$line  <- rbind(res$line, resi$line)
        res$leven <- rbind(res$leven, resi$leven)
      }
    }
  }
  #browser()
  class(res) <- c('findMatch', 'list')
  colnames(res$idn)   <- sprintf("%.0f.%s",  0:length(data), c ('ZDV', vars))
  colnames(res$line)  <- sprintf("%.0f",  1:length(data))
  colnames(res$leven) <- sprintf("%.0f", 1:length(data))
  colnames(res$uid)   <- sprintf("%.0f",   1:length(data))
  deleteDups(res)
}

