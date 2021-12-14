#' REDCap Array
#'
#' @param name name of the array
#' @param values values to be included in the array
#'
#' @return an object of class "redcap_array"
#' @export
#'
#' @examples
#' ## records array for three records from static
#' redcap_array("records", c(16227, 16342, 16419))
#'
#' ## fields array for two fields from static
#' redcap_array("fields", c("chip1_install_date", "chip2_install_date"))
#'
#' ## events array for one event from mother
#' redcap_array("events", "baseline_arm_1")
redcap_array <- function(name, values) {
  a <- as.list(values)
  names(a) <- sprintf("%s[%i]", name, 0:(length(values) - 1))
  class(a) <- "redcap_array"
  return(a)
}



#' REDCap Logical
#'
#' @param name name to give logical
#' @param value R logical to cast as REDCap API logical
#'
#' @return an R logical represented as a named list of lowercase character
#'
#' @examples
#' \dontrun{
#' a <- TRUE
#' redcap_logical("a", a)
#'
#' b <- FALSE
#' redcap_logical("b", b)
#'
#' c <- "not gonna work"
#' redcap_logical("c", c)
#' }
redcap_logical <- function(name, value) {
  checkmate::assert(checkmate::test_logical(value))
  value <- as.list(tolower(as.character(value)))
  names(value) <- name
  return(value)
}



#' Export Full REDCap Database to SAS
#'
#' @description The SAS export capability of REDCap v9.5 is broken such that the
#'  generated SAS script has voluminous syntactical errors. This function
#' attempts to replicate the intended functionality of REDCap by downloading the
#'  data to a csv file and generating a syntactically correct SAS import script.
#'  If sas is found on the path, the script is executed in SAS batch mode to
#' produce the desired sas7bdat file.
#'
#' @param token The user-specific string that serves as the password for a
#' project.
#' @param filename The name to be used for all output files (.csv, .sas, .log,
#' and .sas7bdat). Must be a valid SAS name.
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param linesize SAS linesize system option for formatting any sas output.
#' @param pagesize SAS pagesize system option for formatting any sas output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' roe_redcap_export_records_sas(token, roe_timestamp_filename("static"))
#'
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 6785)$token
#' roe_redcap_export_records_sas(token, roe_timestamp_filename("mother"))
#' }
roe_redcap_export_records_sas <-
  function(token,
           filename = roe_timestamp_filename("roe_redcap_sas_export"),
           redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           linesize = 78,
           pagesize = 60) {
    # check and prepare file names
    roe_valid_sas_data_set_name(filename)
    csv_filename <- sprintf("%s.csv", filename)
    sas_filename <- sprintf("%s.sas", filename)

    # download the data dictionary
    httr::POST(
      redcap_uri,
      body = list(token = token, content = "metadata", format = "csv")
    ) %>%
      httr::content() %>%
      dplyr::mutate(
        # strip double quotes
        field_label = gsub("\"", "", .data$field_label),
        # double up single quotes
        field_label = gsub("'", "''", .data$field_label),
        # strip html tags
        field_label = gsub("<.*?>", "", .data$field_label),
        # replace new lines with space
        field_label = gsub("\n", " ", .data$field_label),
        # trim any leading or trailing whitespace
        field_label = trimws(.data$field_label)
      ) %>%
      # descriptive fields are not labeled
      dplyr::filter(.data$field_type != "descriptive") -> tbl_data_dictionary

    # get names of note fields to later escape newline characters
    tbl_data_dictionary %>%
      dplyr::filter(.data$field_type == "notes") %>%
      dplyr::pull(.data$field_name) -> note_field_names

    # download data to local csv and make initial sas import code with {foreign}
    REDCapR::redcap_read(
      redcap_uri = redcap_uri,
      token = token
    ) %>%
      `[[`("data") %>%
      # drop instrument complete flag fields
      dplyr::select(-dplyr::ends_with("_complete")) %>%
      # escape newlines in note fields
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(note_field_names),
          ~ gsub("\n", "NEWLINE", .)
        )
      ) %>%
      # {foreign} only works with dataframes not tibbles
      as.data.frame() %>%
      # write csv and sas code to disk
      foreign::write.foreign(
        csv_filename,
        sas_filename,
        "SAS"
      ) # tidyverse::haven?
    # read the outputted script into memory for editing
    sas_foreign <- readLines(sas_filename)

    # overwrite {foreign} comments with a filename and libname statements
    sas_foreign[1] <- sprintf(
      "FILENAME in '%s';",
      tools::file_path_as_absolute(csv_filename)
    )
    sas_foreign[2] <- sprintf(
      "LIBNAME out '%s';",
      tools::file_path_as_absolute(".")
    )

    # re-write the infile line to use previously set filename
    infile_line <- which(grepl("^INFILE", sas_foreign))
    sas_foreign[infile_line] <- "INFILE in"

    # correct the {foreign} script by adding informats for the time fields
    # get names of time fields
    tbl_data_dictionary %>%
      dplyr::filter(
        .data$text_validation_type_or_show_slider_number == "time"
      ) %>%
      dplyr::pull(.data$field_name) -> time_field_names

    # determine line positions of the existing {foreign} INFORMAT statement
    informat_start <- which(grepl("INFORMAT", sas_foreign))
    informat_end <- which(
      grepl(";", sas_foreign[informat_start:length(sas_foreign)])
    )[1] + informat_start - 1

    # insert a second INFORMAT statement for the time fields following the first
    sas_foreign <- append(
      sas_foreign,
      c(
        "",
        "INFORMAT",
        paste(" ", time_field_names),
        " time8.",
        ";"
      ),
      informat_end
    )

    # insert a FORMAT statement for each time field at the end of the script
    sas_foreign <- append(
      sas_foreign,
      paste("FORMAT", time_field_names, "time8.;"),
      length(sas_foreign) - 1
    )

    # make vector of sas label commands
    sas_labeling <- c("data rdata;", "\tset rdata;")
    # for each field in the data dictionary create one or more labeling commands
    for (r in seq_len(nrow(tbl_data_dictionary))) {
      .field_type <- tbl_data_dictionary$field_type[r]
      .choices <- tbl_data_dictionary$select_choices_or_calculations[r]
      .field_name <- tbl_data_dictionary$field_name[r]
      .field_label <- tbl_data_dictionary$field_label[r]

      if (.field_type == "checkbox") {
        # if fields was of type checkbox, need to add multiple label commands as
        # there will be a field (with "___#" appended) for each checkbox option
        .choices <- unlist(strsplit(.choices, "\\|"))
        .choices_numb <- trimws(sub(",.*$", "", .choices))
        .choices_text <- trimws(sub("^\\d+,", "", .choices))

        for (i in seq_len(length(.choices))) {
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
        # else this field is not of type checkbox; no special processing needed
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
    # libname out was inserted at beginning of script
    sas_export <- c(
      sprintf("data out.%s;", filename),
      "set rdata;",
      "run;"
    )

    # collate the sas code blocks to a file
    sas_foreign_with_labeling <- c(
      sas_foreign, "",
      sas_labeling, "",
      sas_export
    )
    writeLines(sas_foreign_with_labeling, sas_filename)

    # if sas located on system, run the script to produce the data file
    sas_path <- Sys.which("sas")[[1]]
    if (sas_path != "") {
      message(sprintf(
        "SAS executable found at %s.",
        sas_path
      ))
      message(sprintf(
        "Running %s in SAS to produce the SAS data file.",
        sas_filename
      ))
      shell(
        sprintf(
          "sas -SYSIN %s -linesize %s -pagesize %s",
          sas_filename,
          linesize,
          pagesize
        )
      )
    } else {
      message("SAS executable not found on system path.")
      message(sprintf(
        "Run %s in SAS to produce the SAS data file.",
        sas_filename
      ))
    }
  }



#' Export Entire Project as REDCap XML File (containing metadata & data)
#'
#' @description The entire project (all records, events, arms, instruments,
#' fields, and project attributes) can be downloaded as a single XML file, which
#'  is in CDISC ODM format (ODM version 1.3.1). This XML file can be used to
#'  create a clone of the project (including its data, optionally) on this
#'  REDCap server or on another REDCap server (it can be uploaded on the Create
#'  New Project page). Because it is in CDISC ODM format, it can also be used to
#'   import the project into another ODM-compatible system. NOTE: All the option
#'    paramters listed below ONLY apply to data returned if the
#'    'returnMetadataOnly' parameter is set to FALSE (default). For this API
#'    method, ALL metadata (all fields, forms, events, and arms) will always be
#'    exported. Only the data returned can be filtered using the optional
#'    parameters.
#'
#' Note about export rights: If the 'returnMetadataOnly' parameter is set to
#' FALSE, then please be aware that Data Export user rights will be applied to
#' any data returned from this API request. For example, if you have
#' 'De-Identified' or 'Remove all tagged Identifier fields' data export rights,
#' then some data fields *might* be removed and filtered out of the data set
#' returned from the API. To make sure that no data is unnecessarily filtered
#' out of your API request, you should have 'Full Data Set' export rights in the
#'  project.
#'
#' @note To use this method, you must have API Export privileges in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param filename name of xml file to write results
#' @param overwrite only overwrite existing filename if TRUE
#' @param return_metadata_only TRUE returns only metadata (all fields, forms,
#' events, and arms), whereas FALSE returns all metadata and also data (and
#' optionally filters the data according to any of the optional parameters
#' provided in the request)
#' @param records an array of record names specifying specific records you wish
#' to pull (by default, all records are pulled)
#' @param fields an array of field names specifying specific fields you wish to
#' pull (by default, all fields are pulled)
#' @param events an array of unique event names that you wish to pull records
#' for - only for longitudinal projects
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#' @param export_survey_fields specifies whether or not to export the survey
#' identifier field (e.g., 'redcap_survey_identifier') or survey timestamp
#' fields (e.g., instrument+'_timestamp') when surveys are utilized in the
#' project. If you do not pass in this flag, it will default to 'false'. If set
#' to 'true', it will return the redcap_survey_identifier field and also the
#' survey timestamp field for a particular survey when at least one field from
#' that survey is being exported. NOTE: If the survey identifier field or survey
#'  timestamp fields are imported via API data import, they will simply be
#'  ignored since they are not real fields in the project but rather are
#'  pseudo-fields.
#' @param export_data_access_groups specifies whether or not to export the
#' 'redcap_data_access_group' field when data access groups are utilized in the
#' project. If you do not pass in this flag, it will default to 'false'. NOTE:
#' This flag is only viable if the user whose token is being used to make the
#' API request is *not* in a data access group. If the user is in a group, then
#' this flag will revert to its default value.
#' @param filter_logic String of logic text (e.g., \[age\] > 30) for filtering
#' the data to be returned by this API method, in which the API will only return
#'  the records (or record-events, if a longitudinal project) where the logic
#'  evaluates as TRUE. This parameter is blank/null by default unless a value is
#'   supplied. Please note that if the filter logic contains any incorrect
#'   syntax, the API will respond with an error message.
#' @param export_files TRUE will cause the XML returned to include all files
#' uploaded for File Upload and Signature fields for all records in the project,
#'  whereas FALSE will cause all such fields not to be included. NOTE: Setting
#'  this option to TRUE can make the export very large and may prevent it from
#'  completing if the project contains many files or very large files.
#'
#' @return httr::response() object
#' @export
#'
#' @examples
#' \dontrun{
#' ## full export from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' roe_redcap_export_project_xml(
#'   token,
#'   roe_timestamp_filename("static"),
#'   exportFiles = TRUE
#' )
#'
#' ## full export from mother
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 6785)$token
#' roe_redcap_export_project_xml(
#'   token,
#'   roe_timestamp_filename("mother"),
#'   exportFiles = TRUE
#' )
#'
#' ## one record and two fields from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' roe_redcap_export_project_xml(
#'   token,
#'   roe_timestamp_filename("partial_static"),
#'   records = redcap_array("records", 16227),
#'   fields = redcap_array("fields", c("id", "updatedate"))
#' )
#' }
roe_redcap_export_project_xml <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           filename = roe_timestamp_filename("roe_redcap_project_xml"),
           overwrite = FALSE,
           return_metadata_only = FALSE,
           records,
           fields,
           events,
           return_format = c("xml", "json", "csv"),
           export_survey_fields = FALSE,
           export_data_access_groups = FALSE,
           filter_logic = NULL,
           export_files = FALSE) {
    filename <- paste0(filename, ".xml")

    body <- list(token = token, content = "project_xml")

    body <- append(
      body,
      redcap_logical("returnMetadataOnly", return_metadata_only)
    )

    if (missing(records)) {
      records <- NULL
    } else {
      checkmate::assert(checkmate::check_class(records, "redcap_array"))
    }
    body <- append(body, records)

    if (missing(fields)) {
      fields <- NULL
    } else {
      checkmate::assert(checkmate::check_class(fields, "redcap_array"))
    }
    body <- append(body, fields)

    if (missing(events)) {
      events <- NULL
    } else {
      checkmate::assert(checkmate::check_class(events, "redcap_array"))
    }
    body <- append(body, events)

    body <- append(
      body,
      list("returnFormat" = match.arg(return_format))
    )

    body <- append(body, redcap_logical(
      "exportSurveyFields",
      export_survey_fields
    ))

    body <- append(body, redcap_logical(
      "exportDataAccessGroups",
      export_data_access_groups
    ))

    if (!is.null(filter_logic)) {
      filter_logic <- list("filterLogic" = filter_logic)
    }
    body <- append(body, filter_logic)

    body <- append(body, redcap_logical(
      "exportFiles",
      export_files
    ))

    httr::POST(redcap_uri, httr::write_disk(filename, overwrite), body = body)
  }



#' Delete Records
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param records an array of record names specifying specific records you wish
#' to delete
#' @param arm the arm number of the arm in which the record(s) should be
#' deleted. (This can only be used if the project is longitudinal with more than
#'  one arm.) NOTE: If the arm parameter is not provided, the specified records
#'  will be deleted from all arms in which they exist. Whereas, if arm is
#'  provided, they will only be deleted from the specified arm.
#'
#' @return httr::response() object containing the number of records deleted.
#' @export
#'
#' @examples
#' \dontrun{
#' ## delete two records from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' roe_redcap_delete_records(
#'   token,
#'   records = redcap_array("records", c(16227, 16342))
#' )
#' }
roe_redcap_delete_records <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           records,
           arm) {
    body <- list(
      token = token,
      action = "delete",
      content = "record"
    )

    checkmate::assert(checkmate::check_class(records, "redcap_array"))
    body <- append(body, records)

    if (missing(arm)) {
      arm <- NULL
    } else {
      checkmate::assert(checkmate::check_integer(arm))
    }
    body <- append(body, list(arm = arm))

    httr::POST(redcap_uri, body = body)
  }



#' Import Records
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml \[default\], odm ('odm' refers to CDISC ODM XML
#' format, specifically ODM version 1.3.1)
#' @param type
#' * flat - output as one record per row \[default\]
#' * eav - input as one data point per row
#'     + Non-longitudinal: Will have the fields - record(1), field_name, value
#'     + Longitudinal: Will have the fields - record*, field_name, value,
#'     redcap_event_name(2)
#'
#' 1. 'record' refers to the record ID for the project
#' 2. Event name is the unique name for an event, not the event label
#' @param overwrite_behavior
#' * normal - blank/empty values will be ignored \[default\]
#' * overwrite - blank/empty values are valid and will overwrite data
#' @param force_auto_number If record auto-numbering has been enabled in the
#' project, it may be desirable to import records where each record's record
#' name is automatically determined by REDCap (just as it does in the user
#' interface). If this parameter is set to 'true', the record names provided in
#' the request will not be used (although they are still required in order to
#' associate multiple rows of data to an individual record in the request), but
#' instead those records in the request will receive new record names during the
#'  import process. NOTE: To see how the provided record names get translated
#'  into new auto record names, the returnContent parameter should be set to
#'  'auto_ids', which will return a record list similar to 'ids' value, but it
#'  will have the new record name followed by the provided record name in the
#'  request, in which the two are comma-delimited. For example, if false (or
#'  'false') - The record names provided in the request will be used.
#'  \[default\] true (or 'true') - New record names will be automatically
#'  determined.
#' @param data The formatted data to be imported.
#'
#' TIP: If importing repeating instances for a repeating event or repeating
#' instrument, you may auto-number the instances by providing a value of 'new'
#' for the 'redcap_repeat_instance' field in the dataset you are importing. This
#'  is useful because it allows you to import such data without the need to
#'  determine how many instances already exist for a given repeating
#'  event/instance prior to the import. NOTICE: The 'new' value option for
#'  auto-numbering instances does NOT work for 'eav' type data but only for
#'  'flat' type.
#'
#' NOTE: When importing data in EAV type format, please be aware that checkbox
#' fields must have their field_name listed as variable+'___'+optionCode and its
#'  value as either '0' or '1' (unchecked or checked, respectively). For
#'  example, for a checkbox field with variable name 'icecream', it would be
#'  imported as EAV with the field_name as 'icecream___4' having a value of '1'
#'  in order to set the option coded with '4' (which might be 'Chocolate') as '
#'  checked'.
#' @param date_format MDY, DMY, YMD \[default\] - the format of values being
#' imported for dates or datetime fields (understood with M representing
#' 'month', D as 'day', and Y as 'year') - NOTE: The default format is Y-M-D
#' (with dashes), while MDY and DMY values should always be formatted as M/D/Y
#' or D/M/Y (with slashes), respectively.
#' @param csv_delimiter Set the delimiter used to separate values in the CSV
#' data file (for CSV format only). Options include: comma ',' (default), 'tab',
#'  semi-colon ';', pipe '|', or caret '^'. Simply provide the value in quotes
#'  for this parameter.
#' @param return_content count \[default\] - the number of records imported, ids
#'  - a list of all record IDs that were imported, auto_ids = (used only when
#'  forceAutoNumber=true) a list of pairs of all record IDs that were imported,
#'  includes the new ID created and the ID value that was sent in the API
#'  request (e.g., 323,10).
#' @param return_format csv, json, xml - specifies the format of error messages.
#'  If you do not pass in this flag, it will select the default format for you
#'  passed based on the 'format' flag you passed in or if no format flag was
#'  passed in, it will default to 'xml'.
#'
#' @return httr::response() object containing the number of records deleted.
#' @export
#'
#' @examples
#' \dontrun{
#' ## csv eav example to edit a single data point
#' ### write data to disk and read back in to get correctly parsed csv string
#' csv_data <- data.frame(
#'   record = 2,
#'   field_name = "text_box",
#'   value = "a new value"
#' )
#' csv_file <- tempfile("data", fileext = ".csv")
#' write.csv(csv_data, csv_file, row.names = FALSE)
#' data <- paste(readLines(csv_file), collapse = "\n")
#'
#' ### retreive credentials
#' path <- system.file("misc/example.credentials", package = "REDCapR")
#' p1 <- REDCapR::retrieve_credential_local(path, 153L)
#'
#' ### submit api request and check response
#' httr::content(
#'   roe_redcap_import_records(
#'     token = p1$token,
#'     format = "csv",
#'     type = "eav",
#'     data = data,
#'     return_format = "json"
#'   )
#' )
#' }
roe_redcap_import_records <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           format = c("xml", "csv", "json", "odm"),
           type = c("flat", "eav"),
           overwrite_behavior = c("normal", "overwrite"),
           force_auto_number = FALSE,
           data,
           date_format = c("YMD", "MDY", "DMY"),
           csv_delimiter = c(",", "tab", ";", "|", "^"),
           return_content = c("count", "ids", "auto_ids"),
           return_format = c("xml", "csv", "json")) {
    format <- match.arg(format)
    type <- match.arg(type)
    overwrite_behavior <- match.arg(overwrite_behavior)
    checkmate::assert_logical(force_auto_number)
    date_format <- match.arg(date_format)
    csv_delimiter <- match.arg(csv_delimiter)
    return_content <- match.arg(return_content)
    return_format <- match.arg(return_format)

    body <- list(
      "token" = token,
      "content" = "record",
      "format" = format,
      "type" = type,
      "overwriteBehavior" = overwrite_behavior,
      "forceAutoNumber" = tolower(as.character(force_auto_number)),
      "data" = data,
      "dateFormat" = date_format,
      "csvDelimiter" = csv_delimiter,
      "returnContent" = return_content,
      "returnFormat" = return_format
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }



#' Retrieve credentials from a csv file and read records from a REDCap project
#'
#' @param project_id The ID assigned to the project withing REDCap. This allows
#' the user to store tokens to multiple REDCap projects in one file.
#' @param path_credential The file path to the CSV containing the credentials.
#' @param ... Additional arguments passed to
#' [REDCapR::retrieve_credential_local()] and [REDCapR::redcap_read()].
#'
#' @return An R [base::data.frame()] of the desired records and columns.
#' @export
#'
#' @examples
#' \dontrun{
#' roe_redcap_read(6648)
#' }
roe_redcap_read <- function(project_id, path_credential = "~/.REDCapR", ...) {
  REDCapR::retrieve_credential_local(
    path_credential = path_credential,
    project_id = project_id,
    ...
  ) -> p

  REDCapR::redcap_read(redcap_uri = p$redcap_uri, token = p$token, ...)$data
}
