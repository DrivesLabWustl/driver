% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crumbs.R
\name{collate_daily_crumb_metadata}
\alias{collate_daily_crumb_metadata}
\title{Collate Daily Breadcrumb Metadata}
\usage{
collate_daily_crumb_metadata(crumb_directory)
}
\arguments{
\item{crumb_directory}{directory containing the breadcrumb files}
}
\value{
tibble containing \code{file} as full path to each breadcrumb and \code{date}
as extracted from the breadcrumb file name.
}
\description{
Collate Daily Breadcrumb Metadata
}
\examples{
\dontrun{
ris_storage_path("Reports") \%>\%
  collate_daily_crumb_metadata()
}
}
