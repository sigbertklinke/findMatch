% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createID.R
\name{createID}
\alias{createID}
\title{createID}
\usage{
createID(data, vars = NULL, exclude = c("", "."),
  ignore.case = FALSE)
}
\arguments{
\item{data}{list of data frames}

\item{vars}{vector of variables. One for each data frame.}

\item{exclude}{entries to be excluded from the unique values}

\item{ignore.case}{if FALSE, the uniques values are case sensitive and if TRUE, case is ignored}
}
\value{
a vector with the unique values
}
\description{
Creates a variable with the unique values from all entries in \code{vars}.
}
\examples{
set.seed(0)
# create two data sets which consists of
# 200 obs. only in t1, 200 obs. in t1 and t2 and
# 100 obs. only in t2
n <- list(c(200, 1), c(200, 1, 2), c(100, 2))
x <- generateTestData(n)
ucode <- createID(x, vars=c('code', 'code'), ignore.case=TRUE)
head(ucode)
}
