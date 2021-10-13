#' Retrieve Urban and Rural (p2) Summary File 1 (sf1) Census data
#'
#' @param state_fips_code Two-digit state code (e.g., "01" == "Alabama").
#' @param vintage = Year of dataset, e.g., 2000. This data is only collected during decennial censuses.
#' @param key = Your Census API key, requested using \url{https://api.census.gov/data/key_signup.html}.
#'
#' @return A tibble with FIPS codes and P2 variables by tract group for the desired state.
#' @export
#'
#' @examples
#' \dontrun{
#' roe_get_census_urban_rural_per_block_group_2010("01")
#' }
#'
#' @details
#' Decennial Census API Documentation
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
#' P002005 \tab Total!!Rural	                        \tab URBAN AND RURAL \tab not required \tab            \tab 0 \tab int  \tab P2
#' }
roe_get_census_urban_rural_per_block_group <- function(
  state_fips_code,
  vintage = c("2010", "2000"),
  key = Sys.getenv("CENSUS_KEY")
) {
  vintage <- match.arg(vintage)

  censusapi::getCensus(
    name = "dec/sf1",
    vintage = vintage,
    key = key,
    vars = c("GEO_ID", "NAME", "P002001", "P002002", "P002003", "P002004", "P002005"),
    region = "block group:*",
    regionin = sprintf("state:%s county:*", state_fips_code)
  ) %>%
    dplyr::rename(
      !!paste0("cen_bgrp_state_",     vintage) := state,
      !!paste0("cen_bgrp_county_",    vintage) := county,
      !!paste0("cen_bgrp_tract_",     vintage) := tract,
      !!paste0("cen_bgrp_bgrp_",      vintage) := block_group,
      !!paste0("cen_bgrp_geoid_",     vintage) := GEO_ID,
      !!paste0("cen_bgrp_name_",      vintage) := NAME,
      !!paste0("cen_bgrp_pop_total_", vintage) := P002001,
      !!paste0("cen_bgrp_pop_urban_", vintage) := P002002,
      !!paste0("cen_bgrp_pop_uarea_", vintage) := P002003,
      !!paste0("cen_bgrp_pop_uclst_", vintage) := P002004,
      !!paste0("cen_bgrp_pop_rural_", vintage) := P002005
    )
}