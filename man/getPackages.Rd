% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPackages.R
\name{getPackages}
\alias{getPackages}
\title{getPackages}
\usage{
getPackages(list)
}
\arguments{
\item{list}{a vector of packages to be installed/loaded}
}
\description{
getPackages checks the package library for the package list. For any packages not yet installed, it will install them, and then will load all packages.
}
\examples{
#getPackages(c("tidyr", "dplyr", "ggplot2"))
}

