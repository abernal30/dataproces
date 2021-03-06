% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asnum.R
\name{asnum}
\alias{asnum}
\title{Transforming a categorical column into numeric}
\usage{
asnum(x = "Data frame")
}
\arguments{
\item{x}{A data frame}
}
\value{
A Data Frame.
}
\description{
Transforming a categorical column into numeric
}
\examples{
cat <- data.frame(col1 = c("cat1", "cat2", "cat3", "cat2", "cat1", NA), col2 = c(1, 2, 3, 4, 5, 6))
asnum(cat)
}
