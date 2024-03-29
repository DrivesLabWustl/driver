% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/census.R
\name{parse_fips_code_12}
\alias{parse_fips_code_12}
\title{Parse 12-Digit FIPS Code}
\usage{
parse_fips_code_12(x)
}
\arguments{
\item{x}{12-digit FIPS code to parse}
}
\value{
list containing the 2 digit state, 3 digit county, 6 digit tract, and
1 digit block group codes.
}
\description{
Parse 12-Digit FIPS Code
}
\examples{
parse_fips_code_12("295101022001")
}
