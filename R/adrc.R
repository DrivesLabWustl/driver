#' Lookup Drug Information from Kegg.jp
#'
#' @param x either the drug name or code
#'
#' @return A tibble listing drug codes and corresponding names.
#' @export
#'
#' @examples
#' kegg_drug_info("acetaminophen")
#'
#' @references
#' <https://www.kegg.jp/kegg/drug/>
kegg_drug_info <- function(x) {
  sprintf("http://rest.kegg.jp/find/drug/%s", x) %>%
    httr::GET() %>%
    httr::content() %>%
    readr::read_delim(delim = "\t", col_names = FALSE, col_types = "cc") %>%
    dplyr::rename(code = .data$X1, name = .data$X2) %>%
    dplyr::mutate(code = sub("^dr:D", "d", .data$code))
}

#' Lookup Drug Name from Code via Kegg.jp
#'
#' @param code drug code
#'
#' @return character descriptor of drug code
#' @export
#'
#' @examples
#' kegg_drug_name("d00217")
#' @references
#' <https://www.kegg.jp/kegg/drug/>
kegg_drug_name <- function(code) {
  kegg_drug_info(code)$name
}

#' Lookup Drug Code(s) from Name via Kegg.jp
#'
#' @param name drug name
#' @param formal formal Kegg name
#'
#' @return character vector of drug code(s)
#' @export
#'
#' @examples
#' ## lookup all associated codes
#' kegg_drug_code("acetaminophen")
#'
#' ## lookup code for formal name
#' kegg_drug_code(
#'   "acetaminophen",
#'   "Acetaminophen (JP18/USP); Paracetamol (INN); Tylenol (TN)"
#' )
#'
#' @references
#' <https://www.kegg.jp/kegg/drug/>
#'
#' @note Searching via name is often one-to-many. Use `kegg_drug_info()` to get
#' names associated with multiple codes. Results will be filtered to match
#' formal name if supplied.
kegg_drug_code <- function(name, formal = "") {
  results <- kegg_drug_info(name)

  if (nchar(formal) > 0) {
    results %>%
      dplyr::filter(name == formal) %>%
      dplyr::pull(code)
  } else {
    results$code
  }
}
