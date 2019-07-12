#' makeMerge
#'
#' Creates a a data set consisting of all variables according to the matching results
#'
#' @param data list of data frames
#' @param files list of files which contain the actual matches
#' @param full logical: return full data set (non-matching are observations  are filels with NA) or just matched obs, defaults to \code{FALSE}
#' @param duplicates.ok logical: Are duplicates ok, defaults ton \code{FALSE}
#' @param ... further parameters given to \code{\link[rio]{import}} for importing match data
#'
#' @return a data frame with merged data
#' @export
#'
#' @examples
#' set.seed(0)
#' # create two data sets where the second consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' #
#' match <- findMatch(x, c('code', 'code'))
#' head(match)
#' \dontrun{
#' exportMatch(match, 'match.xlsx')
#' # creates an error since we have duplicate matches
#' data <- makeMerge(x, 'match.xlsx')
#' }
makeMerge <- function(data, files, full=FALSE, duplicates.ok=FALSE, ...) {
  #browser()
  size <- length(data)
#  if (is.null(header)) header <- sprintf("_%.0f", 1:size)
  file <- c() 
  line <- matrix(NA, nrow=0, ncol=size)
  for (i in seq(files)) {
    cat (sprintf("Reading %s\n", files[i]))
    filei <- rio::import(files[i], ...)
    linei <- as.matrix(filei[,1:size])
    line  <- rbind(line, linei)
    file  <- c(file, paste0(files[i], ':', 1:nrow(linei)))
  }
  # delete "perfect" duplicates
  dups <- duplicated(line)
  if (sum(dups)) {
    line <- line[!dups,]
    file <- file[!dups]
  }
  # check for "imperfect" duplicates
  if (!duplicates.ok) {
    browser()
    dups  <- rep(FALSE, nrow(line))
    for (j in 1:size) dups <- dups | duplicated(line[,j], incomparables = NA) | duplicated(line[,j], incomparables = NA, fromLast = TRUE)
    if (any(dups)) {
      line <- line[dups,]
      file <- file[dups]
      order <- c()
      for (j in 1:size) {
        dupj  <- duplicated(line[,j], incomparables = NA) | duplicated(line[,j], incomparables = NA, fromLast = TRUE)
        val   <- sort(unique(line[dupj,j]))
        for (k in val) order <- c(order, which(line[,j]==k))    
      }
      df      <- as.data.frame(line[order,])
      df$file <- file[order]
      print(df)
      stop("Duplicates found")
    }
  }
  #
  line <- apply(line, 1:2, as.numeric)
  if (full) {
    for (i in 1:size) {
      linei <- setdiff(1:nrow(data[[i]]), line[,i])
      line0 <- matrix(NA, nrow=length(linei), ncol=size)
      line0[,i] <- linei
      line <- rbind(line, line0)
    }
  }
  res <- data[[1]][line[,1],]
  for (i in 2:size) {
    res <- cbind(res, data[[i]][line[,i],])
  }
  #
  t <- apply(line, 1, function(v) { t <- as.character(1:length(v)); paste0(t[!is.na(v)], collapse='-') }  )
  cat(sprintf("%0.f matches\n", nrow(line)))
  print(table(t))
  #
  attr(res, "lines") <- line
  res
}
