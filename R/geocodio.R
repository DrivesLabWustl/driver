#' Geocode a single address
#'
#' @param street,city,state,postal_code address components
#' @param fields vector of additional fields to return with query results. Note
#' that these count as extra lookups and impact your dailu quota/costs. See
#' [the official documentation](https://geocod.io/docs/#fields) for more
#' information on costs/pricing.
#' @param ... additional arguments passed to
#' [rgeocodio::gio_geocode_components()]
#'
#' @return a [tibble::tibble()] containing geocodio data per the request
#' @export
roe_geocode_components <- function(
  street,
  city,
  state,
  postal_code,
  fields = "census2010",
  ...
) {
  rgeocodio::gio_geocode_components(
    street = street,
    city = city,
    state = state,
    postal_code = postal_code,
    fields = fields,
    ...
  )
}
