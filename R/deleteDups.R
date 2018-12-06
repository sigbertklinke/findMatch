deleteDups <- function(match) {
  res  <- match
  keep <- !duplicated(match$line)
  for (elem in names(match)) res[[elem]] <- res[[elem]][keep,]
  res
}
