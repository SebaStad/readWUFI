% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/substrRight.R
\name{substrRight}
\alias{substrRight}
\title{Subtract n elements from a string x from the right side. Save the subtracted elements.}
\usage{
substrRight(x, n)
}
\arguments{
\item{x}{Chracter. A string of characters}

\item{n}{Numeric. Amount of elements to be subtracted.}
}
\value{
Character. The n first characters of the string x.
}
\description{
Subtract n elements from a string x from the right side. Save the subtracted elements.
}
\examples{
substrRight("WUFI + R = eternal love", 4)
}
