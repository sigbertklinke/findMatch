
#' findMatch
#'
#' Finds matches between two or more data sets based on a text variable (code or e-mail) based on
#' \href{https://en.wikipedia.org/wiki/Levenshtein_distance}{Levensthein distances}.
#'
#' The result consists of a list with three elements
#' \describe{
#' \item{\code{line}}{a matrix with the line numbers of the matching observations}
#' \item{\code{idn}}{a matrix with the common ID \code{ZDV} and the original text variables in the data sets}
#' \item{\code{leven}}{a matrix with the levenshtein distance between the common ID and the original text variables in the data sets}
#' }
#'
#' @param data  list of data frames
#' @param ...  further parameters
#'
#' @return a list structure with possibly matched observations
#' @export
#'
#' @examples
findMatch <- function(data, ...) { UseMethod("findMatch") }

#' @param vars vector of variables. One for each data frame.
#' @param dmax maximal levensthein distance for matching in text variables $l(t_{i1},t{j2]}<dmax$), defaults to \code{3}
#' @param exclude entries to be excluded from the unique values, defaults to \code{c('', '.')}
#' @param ignore.case if FALSE, the uniques values are case sensitive and if TRUE, case is ignored
#'
#' @rdname findMatch
#' @return a list structure with possibly matched observations
#' @importFrom utils adist
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
findMatch.default <- function (data, vars, dmax=3, exclude=c("", "."), ignore.case=FALSE, ...) {
  dups <- is.data.frame(data)
  if (dups) {
    idn  <- data[[vars]]
    data <- list(data, data)
    vars <- c(vars, vars)
  } else {
    idn <- createID(data, vars, exclude=exclude, ignore.case=ignore.case)
  }
  res     <- list(line  = matrix(0, ncol=length(data), nrow=0),
                  idn   = matrix('', ncol=1+length(data), nrow=0),
                  leven = matrix(0, ncol=length(data), nrow=0))
  zdv     <- list()
  for (i in seq(length(data))) zdv[[i]] <- trim(data[[i]][, vars[i]])
  for (j in seq(length(idn))) {
    #browser()
    #if ((j%%25)==0)
    print(sprintf("%.0f/%.0f", j, length(idn)))
    matches  <- list()
    d        <- rep(0, length(data))
    anymatch <- TRUE
    for (i in seq(length(data))) {
      di   <- adist(zdv[[i]], idn[j], ignore.case=ignore.case)
      d[i] <- min(di)
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
        #browser(expr = (nr>1))
        resi<- list(idn   = matrix('', ncol=1+length(data), nrow=nr),
                    line  = matrix(0, ncol=length(data), nrow=nr),
                    leven = matrix(0, ncol=length(data), nrow=nr))
        resi$idn[,1] <- rep(idn[j], nr)
        for (i in seq(length(data))) {
          resi$idn[,1+i] <- zdv[[i]][grid[[i]]]
          resi$line[,i]  <- grid[[i]]
          resi$leven[,i] <- attr(matches[[i]], 'leven')[match(grid[,i], matches[[i]])]
        }
        res$idn   <- rbind(res$idn, resi$idn)
        res$line  <- rbind(res$line, resi$line)
        res$leven <- rbind(res$leven, resi$leven)
      }
    }
  }
  class(res) <- c('findMatch', 'list')
  colnames(res$idn)   <- c('ZDV', vars)
  colnames(res$line)  <- sprintf("line_%.0f", 1:length(data))
  colnames(res$leven) <- sprintf("leven_%.0f", 1:length(data))
  deleteDups(res)
}
