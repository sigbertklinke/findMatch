#' deleteDups
#'
#' Deletes duplicate entries according to some variable in the match
#'
#' @param match match structure
#'
#' @return a modified match
#' @export
deleteDups <- function(match) {
  res  <- match
  keep <- !duplicated(match$uid)
  for (elem in names(match)) res[[elem]] <- res[[elem]][keep,]
  res
}
