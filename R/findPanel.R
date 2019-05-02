#' findPanel
#'
#' @param data  list of data frames
#' @param matches a double list of matches with elements \code{t1}, \code{t2}, ...
#'
#' @return matches
#' @export
#'
#' @examples
findPanel <- function (data, matches) {
  panel <- matrix(NA, nrow=0, ncol=length(data))
  nd <- length(data)
  for (i in 1:(nd-1)) {
    ti <- sprintf("t%.0f", i)
    for (j in (i+1):nd) {
      tj <- sprintf("t%.0f", j)
      matchfiles <- matches[[ti]][[tj]]
      if (length(matchfiles)) {    
        for (k in 1:length(matchfiles)) {
          filek      <- import(matchfiles[k])
          paneli     <- matrix(NA, nrow=nrow(filek), ncol=length(data))
          paneli[,i] <- filek[,1]
          paneli[,j] <- filek[,2]
          panel <- rbind(panel, paneli)
        }
      }
    }
  }
  # delete dups
  panel  <- panel[!duplicated(panel),]
  # merge panel
  for (j in 1:length(data)) {
    opj <- order(panel[,j])
    panelj <- panel[opj,]
    matchi <- rep(NA, length(data))
    panel  <- matrix(NA, nrow=0, ncol=length(data))
    for (i in 1:nrow(panelj)) {
      tfn <- (matchi==panelj[i,])
      tfn[is.na(tfn)] <- TRUE
      if (all(tfn)) {
        ind <- which(!is.na(panelj[i,])) 
        matchi[ind] <- panelj[i,ind]
      } else {
        panel <- rbind(panel, matchi)
        matchi    <- panelj[i,]
      }
    }  
  }
  rownames(panel) <- NULL
  # check dups
  for (i in 1:ncol(panel)) {
    dups <- duplicated(panel[,i])
    vals <- unique(panel[dups,i])
    vals <- vals[!is.na(vals)]
    if (length(vals)) {
      cat("\nInconsistent match(es):", paste0(vals, collapse=','), "\n\n")
      pos <- which(!is.na(match(panel[,i], vals)))
      print(panel[pos,])
    }
  }
  rownames(panel) <- NULL
  res <- list(line=panel)
  class(res) <- c('findMatch', 'list')
  res
}