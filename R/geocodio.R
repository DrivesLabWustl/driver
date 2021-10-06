#' Geocode a single address
#'
#' @inherit rgeocodio::gio_geocode_components
#' @export
roe_geocode_components <- function(street, city, state, fivedigzipcode, ...) {
  rgeocodio::gio_geocode_components(
    street = street,
    city = city,
    state = state,
    postal_code = fivedigzipcode,
    fields = c("census2010")
  )
}
