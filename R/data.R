#' Daily Breadcrumb Headers
#'
#' Breadcrumb files are inconsistently formatted. Known issues include that
#' some are comma delimited and some semicolon, some contain datetimes and some
#' contain dates and times, some use '' and some use '.' as missing, and some
#' included fields are inconsistent from one file to the next - especially the
#' datetimes vs date and time issue. This character vector contains all known
#' breadcrumb file headers and is useful in determining how to read a daily
#' breadcrumb file in `read_daily_crumb()`.
#'
#' @format A tibble with three variables: `class`, `example`, and `header`.
"daily_crumb_headers"

#' Federal Information Processing Standards (FIPS) codes for all states and DC.
#'
#' Federal Information Processing Standards (FIPS) codes for all states and DC.
#'
#' @format A data frame with three variables: `state_name`,
#' `state_fips_code`, and `state_abbreviation`.
"fips_codes_for_states_and_dc"
