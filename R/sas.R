#' Assert SAS Data Set Name is Valid
#'
#' @param data_set_name The data set name to check.
#'
#' @examples
#' \dontrun{
#' roe_valid_sas_data_set_name("good_name_1")
#'
#' roe_valid_sas_data_set_name("0 bad name")
#' }
#'
#' @references
#' SAS Institute Inc. (1999). Rules for words and names: Names in the SAS
#' language. Retrieved September 30, 2021, from
#' <https://v8doc.sas.com/sashtml/lgref/z1031056.htm>.
roe_valid_sas_data_set_name <- function(data_set_name) {
  if (nchar(data_set_name) > 32) {
    stop("SAS data set names must be less than 32 characters.")
  }
  if (!grepl("^[A-Z|_]", toupper(data_set_name))) {
    stop("SAS data set names must start with a letter or underscore.")
  }
  if (!grepl("^[A-Z0-9_]+$", toupper(data_set_name))) {
    stop(
      "SAS data set names may only contain letters, numeric digits ",
      "(0, 1, ..., 9) or underscores."
    )
  }
}
