#' Retrieve Urban and Rural (p2) Summary File 1 (sf1) Census data from the 2010 decennial census
#'
#' @param state_fips_code Two-digit state code (e.g., "01" == "Alabama").
#'
#' @return a tibble with FIPS codes and P2 variables by tract group for the desired state
#' @export
#'
#' @examples
#' \dontrun{
#' roe_get_census_urban_rural_per_block_group_2010("01")
#' }
#'
#' @details
#' Decennial Census (2010) API Documentation
#' \url{https://www.census.gov/data/developers/data-sets/decennial-census.html}
#'
#' URBAN AND RURAL variables listed in:
#' \url{https://api.census.gov/data/2010/dec/sf1/variables.html}
#' \tabular{llllllll}{
#' \strong{Name} \tab \strong{Label} \tab \strong{Concept} \tab \strong{Required} \tab \strong{Attributes} \tab \strong{Limit} \tab \strong{Predicate Type} \tab \strong{Group} \cr
#' P002001 \tab Total	                                \tab URBAN AND RURAL \tab not required \tab P002001ERR \tab 0 \tab int  \tab P2 \cr
#' P002002 \tab Total!!Urban	                        \tab URBAN AND RURAL \tab not required \tab            \tab 0 \tab int  \tab P2 \cr
#' P002003 \tab Total!!Urban!!Inside urbanized areas  \tab URBAN AND RURAL \tab not required \tab            \tab 0 \tab int  \tab P2 \cr
#' P002004 \tab Total!!Urban!!Inside urban clusters	  \tab URBAN AND RURAL \tab not required \tab            \tab 0 \tab int  \tab P2 \cr
#' P002005 \tab Total!!Rural	                        \tab URBAN AND RURAL \tab not required \tab            \tab 0 \tab int  \tab P2 \cr
#' P002006 \tab Total!!Not defined for this file	    \tab URBAN AND RURAL \tab not required \tab            \tab 0 \tab int  \tab P2
#' }
roe_get_census_urban_rural_per_block_group_2010 <- function(state_fips_code) {
  censusapi::getCensus(
    name = "dec/sf1",
    vintage = 2010,
    key = Sys.getenv("CENSUS_KEY"),
    vars = c("GEO_ID", "NAME", "P002001", "P002002", "P002003", "P002004", "P002005"),
    region = "block group:*",
    regionin = sprintf("state:%s county:*", state_fips_code)
  ) %>%
    dplyr::rename(
      cen_2010_bgrp_state = state,
      cen_2010_bgrp_county = county,
      cen_2010_bgrp_tract = tract,
      cen_2010_bgrp_block_group = block_group,
      cen_2010_bgrp_geo_id = GEO_ID,
      cen_2010_bgrp_name = NAME,
      cen_2010_bgrp_p002001 = P002001,
      cen_2010_bgrp_p002002 = P002002,
      cen_2010_bgrp_p002003 = P002003,
      cen_2010_bgrp_p002004 = P002004,
      cen_2010_bgrp_p002005 = P002005
    )
}
