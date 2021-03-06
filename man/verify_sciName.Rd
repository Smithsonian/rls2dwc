% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verify.R
\name{verify_sciName}
\alias{verify_sciName}
\title{Verify scientific names using the World Register of Marine Species (WoRMS)}
\usage{
verify_sciName(df, dryrun = TRUE)
}
\arguments{
\item{df}{dataframe with scientific names to validate (col should be named scientificname)}

\item{dryrun}{check for unmatched names. Doesn't join to the dataframe.}
}
\value{
dataframe with matches joined to the rows
}
\description{
Verify scientific names using the World Register of Marine Species (WoRMS)
}
