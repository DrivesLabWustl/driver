#' Geocode a single address
#'
#' @param street,city,state,postal_code address components
#' @param vintage year of dataset, e.g., 2000.
#' @param key [geocodio](https://geocod.io/) api key
#'
#' @return a [tibble::tibble()] containing geocodio data per the request
#'
#' @export
#'
#' @seealso [rgeocodio::gio_geocode_components()],
#' [geocodio documentation](https://geocod.io/docs/#fields) for more information
#'  on costs/pricing.
census_geocode_components <- function(street,
                                      city,
                                      state,
                                      postal_code,
                                      vintage = c("2010", "2000"),
                                      key = Sys.getenv("GEOCODIO_API_KEY")) {
  vintage <- match.arg(vintage)

  rgeocodio::gio_geocode_components(
    street = street,
    city = city,
    state = state,
    postal_code = postal_code,
    fields = sprintf("census%s", vintage),
    api_key = key
  )
}



#' Parse 12-Digit FIPS Code
#'
#' @param x 12-digit FIPS code to parse
#'
#' @return list containing the 2 digit state, 3 digit county, 6 digit tract, and
#'  1 digit block group codes.
#' @export
#'
#' @examples
#' parse_fips_code_12("295101022001")
parse_fips_code_12 <- function(x) {
  if (nchar(x) != 12) {
    stop("FIPS code is not 12 digits.")
  }

  list(
    state       = stringr::str_extract(x, "\\d{2}(?=\\d{10})"),
    county      = stringr::str_extract(x, "(?<=\\d{2})\\d{3}(?=\\d{7})"),
    tract       = stringr::str_extract(x, "(?<=\\d{5})\\d{6}(?=\\d{1})"),
    block_group = stringr::str_extract(x, "(?<=\\d{11})\\d")
  )
}



#' Retrieve Urban and Rural (p2) Summary File 1 (sf1) Census data
#'
#' @param state_fips_code Two-digit state code (e.g., "01" == "Alabama").
#' @param vintage = Year of dataset, e.g., 2000. This data is only collected
#' during decennial censuses.
#' @param key Your Census API key, requested using
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

    message(
      sprintf(
        "Downloading block group populations for state code %s from %s Census.",
        state_fips_code,
        vintage
      )
    )

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
      ) %>%
      dplyr::mutate(
        !!paste0("cen_bgrp_pct_urban_", vintage) :=
          100 * .data[[paste0("cen_bgrp_pop_urban_", vintage)]] /
            .data[[paste0("cen_bgrp_pop_total_", vintage)]],
        !!paste0("cen_bgrp_pct_rural_", vintage) :=
          100 * .data[[paste0("cen_bgrp_pop_rural_", vintage)]] /
            .data[[paste0("cen_bgrp_pop_total_", vintage)]]
      )
  }



#' Lookup Urban and Rural Data for a Given Census Block Group
#'
#' @param fips_code_12 12-digit FIPS code
#' @param vintage Year of dataset, e.g., 2000. This data is only collected
#' during decennial censuses.
#' @param key Your Census API key, requested using
#' <https://api.census.gov/data/key_signup.html>.
#'
#' @return A tibble with FIPS codes and P2 variables for the given FIPS code.
#' @export
#'
#' @examples
#' \dontrun{
#' ## each new state in a session requires a download from the Census Bureau
#' census_block_group_population("295101022001")
#' census_block_group_population("171635033341")
#'
#' ## looking up FIPS codes for another vintage
#' census_block_group_population("295101022001", vintage = "2000")
#' }
census_block_group_population <- function(fips_code_12,
                                          vintage = c("2010", "2000"),
                                          key = Sys.getenv("CENSUS_KEY")) {
  vintage <- match.arg(vintage)
  fips_code_components <- parse_fips_code_12(fips_code_12)
  cen_bgrp_file <- sprintf("census_block_group_populations_%s.RData", vintage)
  cen_bgrp_path <- file.path(tempdir(), cen_bgrp_file)

  if (!file.exists(cen_bgrp_path)) {
    cen_bgrp_data <- census_block_group_populations(
      fips_code_components$state,
      vintage,
      key
    )
    save(cen_bgrp_data, file = cen_bgrp_path)
  }

  load(cen_bgrp_path)

  downloaded_states <- unique(
    cen_bgrp_data[[sprintf("cen_bgrp_state_%s", vintage)]]
  )
  if (!(fips_code_components$state %in% downloaded_states)) {
    cen_bgrp_data <- dplyr::bind_rows(
      cen_bgrp_data,
      census_block_group_populations(fips_code_components$state, vintage, key)
    )
    save(cen_bgrp_data, file = cen_bgrp_path)
  }

  fips_code_components %>%
    dplyr::as_tibble() %>%
    dplyr::rename(
      !!paste0("cen_bgrp_state_", vintage) := .data[["state"]],
      !!paste0("cen_bgrp_county_", vintage) := .data[["county"]],
      !!paste0("cen_bgrp_tract_", vintage) := .data[["tract"]],
      !!paste0("cen_bgrp_bgrp_", vintage) := .data[["block_group"]]
    ) %>%
    dplyr::left_join(
      cen_bgrp_data,
      by = sprintf(
        "cen_bgrp_%s_%s",
        c("state", "county", "tract", "bgrp"),
        vintage
      )
    )
}



#' Retrieve Urban and Rural (p2) Summary File 1 (sf1) Census data
#'
#' @param vintage = Year of dataset, e.g., 2000. This data is only collected
#' during decennial censuses.
#' @param key Your Census API key, requested using
#' <https://api.census.gov/data/key_signup.html>.
#'
#' @return A tibble with P2 variables by zip code tabulation area.
#' @export
#'
#' @examples
#' \dontrun{
#' ## data for all zip code tabulation areas
#' census_zipcode_populations()
#' }
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
census_zipcode_populations <- function(vintage = c("2010", "2000"),
                                       key = Sys.getenv("CENSUS_KEY")) {
  vintage <- match.arg(vintage)

  message(sprintf("Downloading zipcode populations from %s Census.", vintage))

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
    ) %>%
    dplyr::mutate(
      !!paste0("cen_zcta_pct_urban_", vintage) :=
        100 * .data[[paste0("cen_zcta_pop_urban_", vintage)]] /
          .data[[paste0("cen_zcta_pop_total_", vintage)]],
      !!paste0("cen_zcta_pct_rural_", vintage) :=
        100 * .data[[paste0("cen_zcta_pop_rural_", vintage)]] /
          .data[[paste0("cen_zcta_pop_total_", vintage)]]
    )
}



#' Lookup Urban and Rural Data for a Given 5-Digit Zip Code
#'
#' @param zip_code_5 five-digit zip code
#' @param vintage Year of dataset, e.g., 2000. This data is only collected
#' during decennial censuses.
#' @param key Your Census API key, requested using
#' <https://api.census.gov/data/key_signup.html>.
#'
#' @return A tibble with population data for a zip code tabulation area.
#' @export
#'
#' @examples
#' \dontrun{
#' ## only one download from Census per session per vintage
#' census_zipcode_population("63130")
#' census_zipcode_population("63102")
#'
#' ## another vintage
#' census_zipcode_population("63130", vintage = "2000")
#' }
census_zipcode_population <- function(zip_code_5,
                                      vintage = c("2010", "2000"),
                                      key = Sys.getenv("CENSUS_KEY")) {
  stopifnot(nchar(zip_code_5) == 5)
  vintage <- match.arg(vintage)
  cen_zcta_file <- sprintf("census_zipcode_populations_%s.RData", vintage)
  cen_zcta_path <- file.path(tempdir(), cen_zcta_file)

  if (!file.exists(cen_zcta_path)) {
    cen_zcta_data <- census_zipcode_populations(vintage, key)
    save(cen_zcta_data, file = cen_zcta_path)
  }

  load(cen_zcta_path)

  dplyr::tibble(zip_code_5 = zip_code_5) %>%
    dplyr::mutate(
      !!paste0("cen_zcta_zcta_", vintage) := zip_code_5
    ) %>%
    dplyr::left_join(
      cen_zcta_data,
      by = paste0("cen_zcta_zcta_", vintage)
    )
}
