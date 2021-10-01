#' Lookup Drug Codes by Name from Kegg.jp
#'
#' @param drug_name The name of the drug to lookup.
#'
#' @return A tibble listing drug codes and corresponding names.
#' @export
#'
#' @examples
#' roe_get_drug_codes("acetaminophen")
#'
#' @references
#' \url{https://www.kegg.jp/kegg/drug/}
roe_get_drug_codes <- function(drug_name) {
  sprintf("http://rest.kegg.jp/find/drug/%s", drug_name) %>%
    httr::GET() %>%
    httr::content() %>%
    readr::read_delim(delim = "\t", col_names = FALSE, col_types = "cc") %>%
    dplyr::rename(drug_code = X1, drug_name = X2) %>%
    dplyr::mutate(drug_code = stringr::str_replace(drug_code, "^dr:D", "d"))
}
