% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sas.R
\name{valid_sas_data_set_name}
\alias{valid_sas_data_set_name}
\title{Assert SAS Data Set Name is Valid}
\usage{
valid_sas_data_set_name(data_set_name)
}
\arguments{
\item{data_set_name}{The data set name to check.}
}
\description{
Assert SAS Data Set Name is Valid
}
\examples{
\dontrun{
valid_sas_data_set_name("good_name_1")

valid_sas_data_set_name("0 bad name")
}

}
\references{
SAS Institute Inc. (1999). Rules for words and names: Names in the SAS
language. Retrieved September 30, 2021, from
\url{https://v8doc.sas.com/sashtml/lgref/z1031056.htm}.
}
