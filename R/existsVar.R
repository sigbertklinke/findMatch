#' existsVars
#'
#' Checks if all variables in \code{name} exists in the data frame \code{data}.
#'
#' @param name chracter: names of variables
#' @param data data frame
#'
#' @return \code{TRUE} if all names in \code{name} exists in \code{data} otherwise \code{FALSE}
#' @export
#'
#' @examples
#' #' set.seed(0)
#' # create two data sets which consists of
#' # 200 obs. only in t1, 200 obs. in t1 and t2 and
#' # 100 obs. only in t2
#' n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
#' x <- generateTestData(n)
#' #
#' existsVars("Geburtstag", x[[1]])
#' existsVars("birthday", x[[2]])
existsVars <- function (name, data) { all(name %in% names(data)) }