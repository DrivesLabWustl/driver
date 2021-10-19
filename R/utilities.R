#' Append a Timestamp to the End of a File Name
#'
#' @param x file name to which a time stamp string should be appended.
#'
#' @return file name with time stamp string appended
#' @export
#'
#' @examples
#' roe_timestamp_filename("roe_redcap_sas_export")
roe_timestamp_filename <- function(x) {
  sprintf("%s_%s", x, format(Sys.time(), "%y%m%d%H%M%S"))
}
