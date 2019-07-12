#' findPanel
#'
#' Based on matches between two files (see \code{\link{findMatch}}) the functions create submatches over \code{n} files.
#' For a detailed application see the vignette.
#'
#' @param data  list of data frames
#' @param matches a double list of matches with elements \code{t1}, \code{t2}, ...
#' @param possible logical: in case of unclear matches should all possible matches be created (default: \code{TRUE})
#'
#' @return matches
#' @export
#'
#' @examples
#' # generate panel data with three time points:
#' # 6 obs only in t1, 
#' # 5 in t1 and t2, 
#' # 8 in t1 and t3, 
#' # 7 in t1, t2 and t3
#' # 4 only in t2
#' # 3 in t2 and t3
#' # 2 only in t3
#' n <- list(c(6, 1), c(5, 1, 2), c(8, 1, 3), c(7, 1, 2, 3), c(4, 2), c(3, 2, 3), c(2, 3))
#' x <- generateTestData(n)
#' # find double matches
#' m12 <- findMatch(x[1:2], 'code') 
#' summary(m12)
#' m23 <- findMatch(x[2:3], 'code') 
#' summary(m23)
#' m13 <- findMatch(x[c(1,3)], 'code') 
#' summary(m13)
#' matches <- list()
#' matches[['t1']][['t2']] <- m12
#' matches[['t1']][['t3']] <- m13
#' matches[['t2']][['t3']] <- m23
#' m123 <- findPanel(x, matches)
#' summary(m123)
#' head(m123)
findPanel <- function (data, matches, possible=TRUE) {
  nd    <- length(data)
  panel <- matrix(NA, nrow=0, ncol=nd)
  leven <- c()
  nd <- length(data)
  for (i in 1:(nd-1)) {
    ti <- sprintf("t%.0f", i)
    for (j in (i+1):nd) {
      tj <- sprintf("t%.0f", j)
      matchfiles <- matches[[ti]][[tj]]
      if (length(matchfiles)) {    
        klen <- 1:length(matchfiles)
        if ('findMatch' %in% class(matchfiles)) klen <- 1
        for (k in klen) {
          browser()
          if (is.character(matchfiles)) {
            filek <- import(matchfiles[k])
          } else {
            if ('findMatch' %in% class(matchfiles)) {
              filek <- matchfiles
            } else {
              filek <- matchfiles[[k]]
            }
            
            filek$leven <- cbind(filek$leven, rowSums(filek$leven))
            filek <- as.data.frame(filek)
          }
          paneli     <- matrix(NA, nrow=nrow(filek), ncol=nd)
          paneli[,i] <- filek[,1]
          paneli[,j] <- filek[,2]
          levenk     <- max(which(startsWith(names(filek), 'leven')),
                            which(startsWith(names(filek), 'dist')))
          leven      <- c(leven, filek[,levenk])  
          panel <- rbind(panel, paneli)
        }
      }
    }
  }
  
  # delete dups
  tf    <- !duplicated(panel)
  panel <- panel[tf,]
  leven <- leven[tf]
  for (i in 1:(nd-1)) {
    for (j in (i+1):nd) {
      cat(sprintf("%.0f-%.0f: %0.f matches\n", i, j, sum(!is.na(panel[,i]) & !is.na(panel[,j]))))
    }
  }
  # merge panel
  panelkey <- apply(panel, 1, function(v) { paste0(v, collapse='-')})
  matches <- matrix(NA, nrow=0, ncol=nd+2)
  
  checked <- rep(0, nrow(panel))
  l <- 1
  ndm <- choose(nd, 2)
  for (i in 1:length(data)) {
    uv <- unique(panel[!checked,i])
    for (uvj in uv) {
      if (!is.na(uvj)) {
        pos  <- match(uvj, panel[,i]) 
        npos <- length(pos)
        repeat {
          for (k in 1:nd) {
            uvk <- unique(panel[pos,k])
            for (m in uvk) {
              if (!is.na(m)) pos <- unique(c(pos, which(panel[,k]==m)))
            }
          }
          if (length(pos)==npos) break
          npos <- length(pos)
        }
        checked[pos] <- l
        panelj <- if (length(pos)>1) panel[pos,] else matrix(panel[pos,], nrow=1)
        #browser(expr=(length(pos)>1))
        if (possible) {
          panelj   <- rbind(panelj, rep(NA, nd))
          matchj   <- lapply(split(panelj, col(panelj)), unique)
          matchj   <- expand.grid(matchj)
          rsmj     <- rowSums(!is.na(matchj))
          matchj   <- matchj[rsmj>1,] 
          rsmj     <- rowSums(!is.na(matchj))
          maxrsnmj <- max(rsmj)
          posmax   <- which(rsmj==maxrsnmj)
          #browser(expr=(l==31))
          if ((length(posmax)==1) & (npos==choose(maxrsnmj,2))) {
            # unique maximum and number of double matches fits
            matchj <- matrix(matchj[posmax,], nrow=1)
          }
        } else {
          matchj <- panelj
        }
        #browser()
        #print(l)
        matchjkey <- apply(matchj, 1, function(v) { paste0(v, collapse='-')})
        inpanel   <- matchjkey %in% panelkey
        if (nrow(matchj)>1) {
          matchj <- cbind(matchj, as.numeric(inpanel), rep(l, nrow(matchj)))
          matchj <- data.matrix(matchj[order(inpanel, decreasing = TRUE),])
        } else {
          matchj <- c(matchj, as.numeric(inpanel), l)
        }
        matches  <- rbind(matches, matchj)
        l <- l+1
      }
    }
  }
  # check inconsistencies
  colnames(matches) <- c(sprintf("t%.0f", 1:nd), "original", "matchno")
  rownames(matches) <- NULL
  matchno <- matches[,ncol(matches)]
  
  dm <- unique(matchno[duplicated(matchno)])
  if (length(dm)) cat("\nProblematic match(es):", paste0(dm, collapse=", "), "\n")
  res <- list(line=matrix(unlist(matches[,1:nd]), ncol=3), match=matrix(unlist(matches[,-(1:nd)]), ncol=2))
  class(res) <- c('findMatch', 'list')
  res
}
