% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetWUFIplusData_vroom.r
\name{f.GetWufiPlusData_vroom}
\alias{f.GetWufiPlusData_vroom}
\title{Read WUFIPlus Data into R.}
\usage{
f.GetWufiPlusData_vroom(v.Path, select.columns = c())
}
\arguments{
\item{v.Path}{Character. Path to file.}

\item{select.columns}{Vector. Which columns should be chosen? Used for timesaving, but readr eliminated that problem.}
}
\value{
Data. Tibble of WUFIPlus Data.
}
\description{
Read WUFIPlus Data into R.
}
