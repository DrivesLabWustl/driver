#' Export Full REDCap Databases to SAS
#'
#' @description The SAS export capability of REDCap v9.5 is broken such that the
#'  generated SAS script has voluminous syntactical errors. This function
#' attempts to replicate the intended functionality of REDCap by downloading the
#'  data to a csv file and generating a syntactically correct SAS import script.
#'   If sas.exe is found on the path, the script is executed in SAS batch mode
#'   to produce the desired sas7bdat file.
#'
#' @param token The user-specific string that serves as the password for a project.
#' @param filename The name to be used for all output files (.csv, .sas, .log, and .sas7bdat). Must be a valid SAS name.
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap project.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' roe_redcap_sas_export(token, "static")
#'
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 6785)$token
#' roe_redcap_sas_export(token, "mother")
#' }
roe_redcap_sas_export <- function(
  token,
  filename = "roe_redcap_sas_export",
  redcap_uri = "https://redcap.wustl.edu/redcap/api/"
) {
  # check and prepare file names
  roe_assert_valid_sas_data_set_name(filename)
  csv_filename <- sprintf("%s.csv", filename)
  sas_filename <- sprintf("%s.sas", filename)

  # download data to local csv and generate sas import code with {foreign}
  REDCapR::redcap_read(
    redcap_uri = redcap_uri,
    token = token
    ) %>%
    `[[`("data") %>%
    # drop instrument complete flag fields
    dplyr::select(-dplyr::ends_with("_complete")) %>%
    # {foreign} only works with dataframes not tibbles
    as.data.frame() %>%
    # write csv and sas code to disk
    foreign::write.foreign(csv_filename, sas_filename, "SAS")
  sas_foreign <- readLines(sas_filename)

  # download the data dictionary in order to construct sas label commands
  httr::POST(
    redcap_uri,
    body = list(token = token, content = "metadata", format = "csv")
  ) %>%
    httr::content() %>%
    dplyr::mutate(
      # strip double quotes
      field_label = gsub("\"", "", field_label),
      # double up single quotes
      field_label = gsub("'", "''", field_label),
      # strip html tags
      field_label = gsub("<.*?>", "", field_label),
      # replace new lines with space
      field_label = gsub("\n", " ", field_label),
      # trim any leading or trailing whitespace
      field_label = trimws(field_label)
    ) %>%
    # descriptive fields are not labeled
    dplyr::filter(field_type != "descriptive") -> tbl_data_dictionary

  # make vector of sas label commands
  sas_labeling <- c("data rdata;", "\tset rdata;")
  # for each field in the data dictionary create one or more labeling commands
  for(r in 1:nrow(tbl_data_dictionary)) {
    .field_type <- tbl_data_dictionary$field_type[r]
    .choices <- tbl_data_dictionary$select_choices_or_calculations[r]
    .field_name <- tbl_data_dictionary$field_name[r]
    .field_label <- tbl_data_dictionary$field_label[r]

    if(.field_type == "checkbox") {
      # if fields was of type checkbox, need to add multiple label commands as
      # there will be a field (with "___#" appended) for each checkbox option
      .choices <- unlist(strsplit(.choices, "\\|"))
      .choices_numb <- sub(",.*$", "", .choices)
      .choices_text <- trimws(sub("^\\d+,", "", .choices))

      for(i in 1:length(.choices)) {
        # make new command
        .cmd <- sprintf(
          "\tlabel %s___%s='%s (choice=%s)';",
          .field_name,
          .choices_numb[i],
          .field_label,
          .choices_text[i]
        )
        # append new command
        sas_labeling <- c(sas_labeling, .cmd)
      }
    } else {
      # else this field is not of type checkbox and no special processing needed
      # make new command
      .cmd <- sprintf(
        "\tlabel %s='%s';",
        .field_name,
        .field_label
      )
      # append new command
      sas_labeling <- c(sas_labeling, .cmd)
    }
  }
  sas_labeling <- c(sas_labeling, "run;")

  # sas code to export the sas data set to file named <filename>.sas7bdat
  sas_export <- c(
    sprintf("libname out '%s';", gsub("/", "\\\\", getwd())),
    sprintf("data out.%s;", filename),
    "set rdata;",
    "run;"
  )

  # collate the sas code blocks to a file and run in sas batch mode
  sas_foreign_with_labeling <- c(sas_foreign, "", sas_labeling, "", sas_export)
  writeLines(sas_foreign_with_labeling, sas_filename)
  if(Sys.which("sas")[[1]] != "")
    shell(sprintf("sas.exe -SYSIN %s", sas_filename))
}
