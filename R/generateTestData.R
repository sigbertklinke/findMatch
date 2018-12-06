#' generateTestData
#'
#' Generates randomly a test data set of size \code{n}.
#'
#' @details The generated data set contains at least eight variables:
#' \describe{
#' \item{\code{sex}}{sex of respondent (\code{M} male, \code{F} female)}
#' \item{\code{firstname}}{first name of respondent}
#' \item{\code{mothername}}{first name of respondents mother}
#' \item{\code{lastname}}{last name of respondent}
#' \item{\code{email}}{e-mail address of respondent}
#' \item{\code{birthplace}}{birthplace of respondent}
#' \item{\code{birthday}}{birthday of respondent}
#' \item{\code{code}}{code generated from respondents data: 2nd and 3rd letter of mothers first name, day of birth, 3rd and 4th letter of birth city and 1st letter of first name}
#' }
#'
#' The basic data files for the generation can be found in the \code{extdata} directory of the package
#' \describe{
#' \item{\code{firstname_female.txt}}{taken from \href{https://de.wiktionary.org/wiki/Verzeichnis:International/Weibliche_Vornamen}{Wiktionary}}
#' \item{\code{firstname_male.txt}}{taken from \href{https://de.wiktionary.org/wiki/Verzeichnis:International/M\%C3\%A4nnliche_Vornamen}{Wiktionary}}
#' \item{\code{lastname.txt}}{taken from \href{http://wiki-de.genealogy.net/Die_1000_h\%C3\%A4ufigsten_Familiennamen_in_Deutschland}{genealogy.net Wiki}}
#' \item{\code{email_provider.txt}}{taken from \href{https://gist.github.com/tbrianjones/5992856}{T. Brian Jones \code{free_email_provider_domains.txt}}}
#' \item{\code{staedte.txt}}{taken from \href{https://de.wikipedia.org/wiki/Liste_der_St\%C3\%A4dte_in_Deutschland}{Wikipedia list of cities and towns in Germany}}
#' }
#'
#' @param n number of observations for the test data (default: 500)
#' @param x data set to take observations from for simulating longitudinal data
#' @param mortality mortality rate (default: 0.5)
#' @param ... further \strong{named} (artificial) variables to add to the data set
#'
#' @return a data frame
#' @importFrom rio import
#' @export
#'
#' @examples
#' # create two data sets where the second consists of 50% observations of the first
#' n1 <- 500
#' n2 <- 300
#' x1 <- generateTestData(n1)
#' x2 <- generateTestData(n2, x1)
#' x1$points <- findInterval(rnorm(n1, mean=10, sd=3), 0:15)-1
#' head(x1)
#' x2$points <- findInterval(rnorm(n2, mean=10, sd=3), 0:15)-1
#' head(x2)
generateTestData <- function(n=500, x=NULL, mortality=0.5, ...) {
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  #
  args <- list(...)
  if (is.null(args$sex)) {
    args$sex <- sample(factor(c('M', 'F')), size=n, replace=TRUE)
  } else {
     if (is.function(args$sex)) args$sex <- args$sex(n)
  }
  if (is.null(args$firstname)) {
    fnf  <- rio::import(system.file("extdata", "firstname_female.txt", package="findMatch"))
    fnf  <- stringi::stri_trans_general(fnf[,1], "latin-ascii")
    fnm  <- rio::import(system.file("extdata", "firstname_male.txt", package="findMatch"))
    fnm  <- stringi::stri_trans_general(fnm[,1], "latin-ascii")
    args$firstname <- ifelse(as.numeric(args$sex)==1, sample(fnf, size=n, replace=TRUE),
                                                       sample(fnm, size=n, replace=TRUE))
  } else {
    if (is.function(args$firstname)) args$firstname <- args$firstname(n, args$sex)
  }
  if (is.null(args$mothername)) {
    fnf  <- rio::import(system.file("extdata", "firstname_female.txt", package="findMatch"))
    fnf  <- stringi::stri_trans_general(fnf[,1], "latin-ascii")
    args$mothername <- sample(fnf, size=n, replace=TRUE)
  } else {
    if (is.function(args$mothername)) args$mothername <- args$mothername(n)
  }
  if (is.null(args$lastname)) {
    lns  <- rio::import(system.file("extdata", "lastname.txt", package="findMatch"))
    lns  <- stringi::stri_trans_general(lns[,1], "latin-ascii")
    args$lastname <- sample(lns, size=n, replace=TRUE)
  } else {
    if (is.function(args$lastname)) args$lastname  <- args$lastname(n)
  }
  if (is.null(args$email)) {
    eml  <- rio::import(system.file("extdata", "email_provider.txt", package="findMatch"))
    eml  <- stringi::stri_trans_general(eml[,1], "latin-ascii")
    args$email <- paste0(args$firstname, '.', args$lastname, '@', sample(eml, size=n, replace=TRUE))
  } else {
    if (is.function(args$email)) args$email <- args$email(n)
  }
  if (is.null(args$birthplace)) {
    city <- trim(readLines(system.file("extdata", "staedte.txt", package="findMatch")))
    city <- stringi::stri_trans_general(city, "latin-ascii")
    args$birthplace <- sample(city, size=n, replace=TRUE)
  } else {
    if (is.function(args$birthplace)) args$birthplace <- args$birthplace(n)
  }
  if (is.null(args$birthday)) {
    args$birthday <- sample(seq(as.Date('1995/01/01'), as.Date('2000/01/01'), by="day"), size=n, replace=TRUE)
  } else {
    if (is.function(args$birthplace)) args$birthday <- args$birthday(n)
  }
  if (is.null(args$code)) {
    args$code <- paste0(substring(args$mothername, 2,3),
                        format(args$birthday, "%0d"),
                        substring(args$birthplace, 3,4),
                        substring(args$firstname, 1, 1))
  } else {
    if (is.function(args$code)) args$code <- args$code(n)
  }
  if (!is.null(x)) {
    index <- sample(1:nrow(x), size=min(nrow(x), (1-mortality)*n))
    for (arg in names(x)) args[[arg]][seq(index)] <- x[[arg]][index]
  }
  data.frame(args, stringsAsFactors = FALSE)
}


