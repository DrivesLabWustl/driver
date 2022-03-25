#' Retrieve Urban and Rural (p2) Summary File 1 (sf1) Census data
#'
#' @param state_fips_code Two-digit state code (e.g., "01" == "Alabama").
#' @param vintage = Year of dataset, e.g., 2000. This data is only collected
#' during decennial censuses.
#' @param key = Your Census API key, requested using
#' <https://api.census.gov/data/key_signup.html>.
#'
#' @return A tibble with FIPS codes and P2 variables by tract group for the
#' desired state.
#' @export
#'
#' @examples
#' \dontrun{
#' ## data for all block groups in Missouri
#' census_block_group_populations("29")
#'
#' ## data for all block groups
#' fips_codes_for_states_and_dc %>%
#'   mutate(data = purrr::map(
#'     state_fips_code,
#'     census_block_group_populations
#'   )) %>%
#'   tidyr::unnest(cols = c(data))
#'
#' ## compute percents urban and rural for each Missouri block group
#' census_block_group_populations("29") %>%
#'   mutate(
#'     cen_bgrp_pct_urban_2010 =
#'       100 * cen_bgrp_pop_urban_2010 / cen_bgrp_pop_total_2010,
#'     cen_bgrp_pct_rural_2010 =
#'       100 * cen_bgrp_pop_rural_2010 / cen_bgrp_pop_total_2010
#'   )
#' }
#'
#' @details
#' Decennial Census API Documentation
#' <https://www.census.gov/data/developers/data-sets/decennial-census.html>
#'
#' URBAN AND RURAL variables listed in:
#' <https://api.census.gov/data/2010/dec/sf1/variables.html>
#' \tabular{llllllll}{
#' **Name** \tab
#' **Label** \tab
#' **Concept** \tab
#' **Required** \tab
#' **Attributes** \tab
#' **Limit** \tab
#' **Predicate Type** \tab
#' **Group** \cr
#' P002001 \tab
#' Total \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' P002001ERR \tab
#' 0 \tab
#' int  \tab
#' P2 \cr
#' P002002 \tab
#' Total!!Urban \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' \tab
#' 0 \tab
#' int  \tab
#' P2 \cr
#' P002003 \tab
#' Total!!Urban!!Inside urbanized areas \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' \tab
#' 0 \tab
#' int  \tab
#' P2 \cr
#' P002004 \tab
#' Total!!Urban!!Inside urban clusters \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' \tab
#' 0 \tab
#' int  \tab
#' P2 \cr
#' P002005 \tab
#' Total!!Rural \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' \tab
#' 0 \tab
#' int  \tab
#' P2
#' }
census_block_group_populations <-
  function(state_fips_code,
           vintage = c("2010", "2000"),
           key = Sys.getenv("CENSUS_KEY")) {
    vintage <- match.arg(vintage)

    censusapi::getCensus(
      name = "dec/sf1",
      vintage = vintage,
      key = key,
      vars = c(
        "GEO_ID",
        "NAME",
        "P002001",
        "P002002",
        "P002003",
        "P002004",
        "P002005"
      ),
      region = "block group:*",
      regionin = sprintf("state:%s county:*", state_fips_code)
    ) %>%
      dplyr::rename(
        !!paste0("cen_bgrp_state_", vintage) := .data$state,
        !!paste0("cen_bgrp_county_", vintage) := .data$county,
        !!paste0("cen_bgrp_tract_", vintage) := .data$tract,
        !!paste0("cen_bgrp_bgrp_", vintage) := .data$block_group,
        !!paste0("cen_bgrp_geoid_", vintage) := .data$GEO_ID,
        !!paste0("cen_bgrp_name_", vintage) := .data$NAME,
        !!paste0("cen_bgrp_pop_total_", vintage) := .data$P002001,
        !!paste0("cen_bgrp_pop_urban_", vintage) := .data$P002002,
        !!paste0("cen_bgrp_pop_uarea_", vintage) := .data$P002003,
        !!paste0("cen_bgrp_pop_uclst_", vintage) := .data$P002004,
        !!paste0("cen_bgrp_pop_rural_", vintage) := .data$P002005
      )
  }

#' Retrieve Urban and Rural (p2) Summary File 1 (sf1) Census data
#'
#' @param vintage = Year of dataset, e.g., 2000. This data is only collected
#' during decennial censuses.
#' @param key = Your Census API key, requested using
#' <https://api.census.gov/data/key_signup.html>.
#'
#' @return A tibble with P2 variables by zip code tabulation area.
#' @export
#'
#' @examples
#' \dontrun{
#' ## data for all zip code tabulation areas
#' census_zipcode_populations()
#'
#' ## compute percents urban and rural for each zip code tabulation area
#' census_zipcode_populations() %>%
#'   mutate(
#'     cen_zcta_pct_urban_2010 =
#'       100 * cen_zcta_pop_urban_2010 / cen_zcta_pop_total_2010,
#'     cen_zcta_pct_rural_2010 =
#'       100 * cen_zcta_pop_rural_2010 / cen_zcta_pop_total_2010
#'   )
#' }
#'
#' @details
#' Decennial Census API Documentation
#' <https://www.census.gov/data/developers/data-sets/decennial-census.html>
#'
#' URBAN AND RURAL variables listed in:
#' <https://api.census.gov/data/2010/dec/sf1/variables.html>
#' \tabular{llllllll}{
#' **Name** \tab
#' **Label** \tab
#' **Concept** \tab
#' **Required** \tab
#' **Attributes** \tab
#' **Limit** \tab
#' **Predicate Type** \tab
#' **Group** \cr
#' P002001 \tab
#' Total \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' P002001ERR \tab
#' 0 \tab
#' int  \tab
#' P2 \cr
#' P002002 \tab
#' Total!!Urban \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' \tab
#' 0 \tab
#' int  \tab
#' P2 \cr
#' P002003 \tab
#' Total!!Urban!!Inside urbanized areas \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' \tab
#' 0 \tab
#' int  \tab
#' P2 \cr
#' P002004 \tab
#' Total!!Urban!!Inside urban clusters \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' \tab
#' 0 \tab
#' int  \tab
#' P2 \cr
#' P002005 \tab
#' Total!!Rural \tab
#' URBAN AND RURAL \tab
#' not required \tab
#' \tab
#' 0 \tab
#' int  \tab
#' P2
#' }
census_zipcode_populations <-
  function(vintage = c("2010", "2000"),
           key = Sys.getenv("CENSUS_KEY")) {
    vintage <- match.arg(vintage)

    censusapi::getCensus(
      name = "dec/sf1",
      vintage = vintage,
      key = key,
      vars = c(
        "GEO_ID",
        "NAME",
        "P002001",
        "P002002",
        "P002003",
        "P002004",
        "P002005"
      ),
      region = "zip code tabulation area:*"
    ) %>%
      dplyr::rename(
        !!paste0("cen_zcta_zcta_", vintage) := .data$zip_code_tabulation_area,
        !!paste0("cen_zcta_geoid_", vintage) := .data$GEO_ID,
        !!paste0("cen_zcta_name_", vintage) := .data$NAME,
        !!paste0("cen_zcta_pop_total_", vintage) := .data$P002001,
        !!paste0("cen_zcta_pop_urban_", vintage) := .data$P002002,
        !!paste0("cen_zcta_pop_uarea_", vintage) := .data$P002003,
        !!paste0("cen_zcta_pop_uclst_", vintage) := .data$P002004,
        !!paste0("cen_zcta_pop_rural_", vintage) := .data$P002005
      )
  }
