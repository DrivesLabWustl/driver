#' Master Breadcrumb Template
#'
#' Daily breadcrumb formatting is inconsistent where the fields provided
#' differ across files. Notably some contain a datetime field where the norm is
#' to split date and time into separate fields. This tibble represents a base
#' template to store all known breadcrumb data in a common format.
#'
#' @export
master_crumb_template <- dplyr::tibble(
  vehicle_name          = character(),
  latitude              = double(),
  longitude             = double(),
  vehicle_speed         = double(),
  event_name            = character(),
  address               = character(),
  event_type            = character(),
  date                  = as.Date(character()),
  time                  = hms::new_hms(),
  odometer_reading      = double(),
  gps_fix_quality       = double(),
  peak_speed            = double(),
  average_speed         = double(),
  initial_speed         = double(),
  final_speed           = double(),
  previous_fuel_level   = double(),
  new_fuel_level        = double(),
  nearest_landmark_name = character(),
  trip_distance         = double(),
  driver_name           = character(),
  occupants             = double(),
  speed_limit           = double(),
  difference            = double()
)



#' Daily Breadcrumb Name Patterns
#'
#' Breadcrumb files are inconsistently named. This character vector contains
#' regular expressions for all known name patterns. This is useful for
#' determining how to extract the date from the file name in
#' `collate_all_daily_crumb_metadata()`.
#'
#' @export
daily_crumb_name_patterns <- c(
  # Type 1; e.g., Breadcrumb_01Apr2016.csv
  "^Breadcrumb_\\d{2}[A-Z][a-z]{2}\\d{4}\\.csv$",

  # Type 2; e.g., Breadcrumb_14June2015.csv
  "^Breadcrumb_\\d{2}[A-Z][a-z]{3,}\\d{4}\\.csv$",

  # Type 3; e.g., Breadcrumb_12_June_2015.csv
  "^Breadcrumb_\\d{2}_[A-Z][a-z]+_\\d{4}\\.csv$",

  # Type 4; e.g., Breadcrumb-2015-01-28.csv
  "^Breadcrumb-\\d{4}-\\d{2}-\\d{2}\\.csv$"
)



#' Path to Research Infrastructure Services Storage
#'
#' @param path additional path beyond root mapping
#'
#' @return the desired path
#' @export
#'
#' @examples
#' \dontrun{
#' crumb_directory <- ris_storage_path("Reports")
#' }
ris_storage_path <- function(path = "") {
  ris_storage <- dplyr::case_when(
    # 1. matt's laptop
    Sys.info()["nodename"] == "3236-WL-05010" ~
      "G:/Active",
    # 2. node on the ris compute cluster
    stringr::str_detect(Sys.info()["nodename"], "ris.wustl.edu$") ~
      "/storage1/fs1/babulalg/Active",
    TRUE ~
      NA_character_
  )

  if (is.na(ris_storage)) {
    stop("Failed to auto detect RIS storage mapping.")
  } else {
    return(file.path(ris_storage, path))
  }
}



#' Read Daily Breadcrumb Header
#'
#' @param x breadcrumb file from which to extract header
#'
#' @return character representation of the breadcrumb header
#' @export
#'
#' @examples
#' \dontrun{
#' read_daily_crumb_header("Breadcrumb_07Nov2017.csv")
#' }
#'
#' @note Used to determine breadcrumb read in `read_daily_crumb()`.
read_daily_crumb_header <- function(x) {
  f <- file(x, "r")
  header <- readLines(f, n = 1)
  close(f)
  return(header)
}



#' Read Daily Breadcrumb
#'
#' @param file breadcrumb to read
#'
#' @return tibble of properly formatted breadcrumb data according to
#' `master_crumb_template`
#' @export
#'
#' @examples
#' \dontrun{
#' read_daily_crumb("Breadcrumb_07Nov2017.csv")
#' }
#'
#' @note Breadcrumb files are inconsistently formatted. Known issues include
#' that some are comma delimited and some semicolon, some contain datetimes and
#' some contain dates and times, some use '' and some use '.' as missing, and
#' some included fields are inconsistent from one file to the next - especially
#' the datetimes vs date and time issue. This function explicitly reads the
#' file according to the file header in an effort to detect data issues.
read_daily_crumb <- function(file) {
  header <- read_daily_crumb_header(file)

  contents <- NULL

  # Type 1
  if (header == daily_crumb_headers$header[1]) {
    contents <- readr::read_csv(
      file,
      col_types = readr::cols(
        Vehicle_Name     = readr::col_character(),
        Driver_Name      = readr::col_character(),
        Occupants        = readr::col_number(),
        Latitude         = readr::col_double(),
        Longitude        = readr::col_double(),
        Vehicle_Speed    = readr::col_double(),
        `Speed Limit`    = readr::col_double(),
        Difference       = readr::col_double(),
        Event_Name       = readr::col_character(),
        Address          = readr::col_character(),
        Event_Type       = readr::col_character(),
        Date             = readr::col_date(format = "%d%b%Y"),
        Time             = readr::col_time(format = ""),
        Odometer_Reading = readr::col_double(),
        `Trip Distance`  = readr::col_double(),
        GPS_Fix_Quality  = readr::col_double(),
        Peak_Speed       = readr::col_double(),
        Average_Speed    = readr::col_double(),
        Initial_Speed    = readr::col_double(),
        Final_Speed      = readr::col_double()
      )
    )
  }

  # Type 2
  if (header == daily_crumb_headers$header[2]) {
    contents <- readr::read_csv(
      file,
      col_types = readr::cols(
        Vehicle_Name          = readr::col_character(),
        Latitude              = readr::col_double(),
        Longitude             = readr::col_double(),
        Vehicle_Speed         = readr::col_double(),
        Event_Name            = readr::col_character(),
        Address               = readr::col_character(),
        Event_Type            = readr::col_character(),
        Date                  = readr::col_date(format = "%d%b%Y"),
        Time                  = readr::col_time(format = ""),
        Odometer_Reading      = readr::col_double(),
        GPS_Fix_Quality       = readr::col_double(),
        Peak_Speed            = readr::col_double(),
        Average_Speed         = readr::col_double(),
        Initial_Speed         = readr::col_double(),
        Final_Speed           = readr::col_double(),
        Previous_Fuel_Level   = readr::col_double(),
        New_Fuel_Level        = readr::col_double(),
        Nearest_Landmark_Name = readr::col_character()
      )
    )
  }

  # Type 3
  if (header == daily_crumb_headers$header[3]) {
    contents <- readr::read_csv(
      file,
      col_types = readr::cols(
        Vehicle_Name          = readr::col_character(),
        Latitude              = readr::col_double(),
        Longitude             = readr::col_double(),
        Vehicle_Speed         = readr::col_double(),
        Event_Name            = readr::col_character(),
        Address               = readr::col_character(),
        Event_Type            = readr::col_character(),
        Date                  = readr::col_date("%d%b%Y"),
        Time                  = readr::col_time(format = ""),
        Odometer_Reading      = readr::col_double(),
        `Trip Distance`       = readr::col_double(),
        GPS_Fix_Quality       = readr::col_double(),
        Peak_Speed            = readr::col_double(),
        Average_Speed         = readr::col_double(),
        Initial_Speed         = readr::col_double(),
        Final_Speed           = readr::col_double(),
        Previous_Fuel_Level   = readr::col_double(),
        New_Fuel_Level        = readr::col_double(),
        Nearest_Landmark_Name = readr::col_character()
      )
    )
  }

  # Type 4
  if (header == daily_crumb_headers$header[4]) {
    contents <- readr::read_csv(
      file,
      col_types = readr::cols(
        Vehicle_Name          = readr::col_character(),
        Latitude              = readr::col_double(),
        Longitude             = readr::col_double(),
        Vehicle_Speed         = readr::col_double(),
        Event_Name            = readr::col_character(),
        Address               = readr::col_character(),
        Event_Type            = readr::col_character(),
        # Timestamp specification is inconsistent across daily files
        # - read as character and deal with later
        Timestamp             = readr::col_character(),
        Odometer_Reading      = readr::col_double(),
        GPS_Fix_Quality       = readr::col_double(),
        # begin: in some daily files these fields contain a '.'
        Peak_Speed            = readr::col_double(),
        Average_Speed         = readr::col_double(),
        Initial_Speed         = readr::col_double(),
        Final_Speed           = readr::col_double(),
        Previous_Fuel_Level   = readr::col_double(),
        New_Fuel_Level        = readr::col_double(),
        # end: in some daily files these fields contain a '.'
        Nearest_Landmark_Name = readr::col_character()
      ),
      na = c("", "NA", ".")
    ) %>%
      dplyr::mutate(
        # Timestamp specification is inconsistent
        # case_when evaluates all rhs and keeps desired
        # this produces warning messages that do not apply to the final results
        # we will suppress all warnings for now and check for errors later
        Timestamp = dplyr::case_when(
          # sometimes the timestamp is specified like '12 June 2015 06:33:58'
          # requiring a special informat beyond defaults
          # (%b is for abbreviated month, but it also works for full months)
          stringr::str_detect(Timestamp, "[a-z]") ~
            suppressWarnings(
              lubridate::as_datetime(Timestamp, format = "%d %b %Y %H:%M:%S")),
          # and sometimes the datetime is specified like '2022-03-21 01:01:01.0'
          # this is a default format
          TRUE ~ suppressWarnings(lubridate::as_datetime(Timestamp))
        ),
        date = lubridate::date(Timestamp),
        time = hms::as_hms(Timestamp)
      )

    # check for failed timestamp parsings
    contents %>%
      dplyr::filter(is.na(Timestamp)) %>%
      nrow() -> n_na_timestamps
    if (n_na_timestamps > 0) {
      warning(sprintf("%i, `Timestamp` failed to parse.", n_na_timestamps))
    }

    contents %>%
      dplyr::select(-Timestamp) -> contents
  }

  # Type 5
  if (header == daily_crumb_headers$header[5]) {
    contents <- readr::read_csv(
      file,
      col_types = readr::cols(
        Vehicle_Name     = readr::col_character(),
        Latitude         = readr::col_double(),
        Longitude        = readr::col_double(),
        Vehicle_Speed    = readr::col_double(),
        `Speed Limit`    = readr::col_double(),
        Difference       = readr::col_double(),
        Event_Name       = readr::col_character(),
        Address          = readr::col_character(),
        Event_Type       = readr::col_character(),
        Date             = readr::col_date("%d%b%Y"),
        Time             = readr::col_time(format = ""),
        Odometer_Reading = readr::col_double(),
        `Trip Distance`  = readr::col_double(),
        GPS_Fix_Quality  = readr::col_double(),
        Peak_Speed       = readr::col_double(),
        Average_Speed    = readr::col_double(),
        Initial_Speed    = readr::col_double(),
        Final_Speed      = readr::col_double()
      )
    )
  }

  # Type 6
  if (header == daily_crumb_headers$header[6]) {
    contents <- readr::read_csv(
      file,
      col_types = readr::cols(
        Vehicle_Name          = readr::col_character(),
        Latitude              = readr::col_double(),
        Longitude             = readr::col_double(),
        Vehicle_Speed         = readr::col_double(),
        `Speed Limit`         = readr::col_double(),
        Difference            = readr::col_double(),
        Event_Name            = readr::col_character(),
        Address               = readr::col_character(),
        Event_Type            = readr::col_character(),
        Date                  = readr::col_date("%d%b%Y"),
        Time                  = readr::col_time(format = ""),
        Odometer_Reading      = readr::col_double(),
        `Trip Distance`       = readr::col_double(),
        GPS_Fix_Quality       = readr::col_double(),
        Peak_Speed            = readr::col_double(),
        Average_Speed         = readr::col_double(),
        Initial_Speed         = readr::col_double(),
        Final_Speed           = readr::col_double(),
        Previous_Fuel_Level   = readr::col_double(),
        New_Fuel_Level        = readr::col_double(),
        Nearest_Landmark_Name = readr::col_character()
      )
    )
  }

  # Type 7
  if (header == daily_crumb_headers$header[7]) {
    contents <- readr::read_delim(
      file,
      delim = ";",
      col_types = readr::cols(
        `Vehicle Name`          = readr::col_character(),
        Latitude                = readr::col_double(),
        Longitude               = readr::col_double(),
        `Vehicle Speed`         = readr::col_double(),
        `Event Name`            = readr::col_character(),
        Address                 = readr::col_character(),
        `Event Type`            = readr::col_character(),
        Timestamp               = readr::col_datetime(format = ""),
        `Odometer Reading`      = readr::col_double(),
        `GPS Fix Quality`       = readr::col_double(),
        `Peak Speed`            = readr::col_double(),
        `Average Speed`         = readr::col_double(),
        `Initial Speed`         = readr::col_double(),
        `Final Speed`           = readr::col_double(),
        `Previous Fuel Level`   = readr::col_double(),
        `New Fuel Level`        = readr::col_double(),
        `Nearest Landmark Name` = readr::col_character()
      )
    ) %>%
      dplyr::mutate(
        date = lubridate::date(Timestamp),
        time = hms::as_hms(Timestamp)
      ) %>%
      dplyr::select(-Timestamp)
  }

  if (is.null(contents)) {
    stop(
      paste(
        sprintf(
          "Don't know how to read daily breadcrumb %s.",
          file
        ),
        sprintf(
          "Need to append `daily_crumb_headers` with <%s>",
          header
        ),
        sprintf(
          "and add section in `%s` for daily breadcrumbs like `%s`",
          "read_daily_crumb()",
          file
        ),
        collapse = "\n"
      )
    )
  }

  contents %>%
    janitor::clean_names() %>%
    dplyr::bind_rows(master_crumb_template, .)
}



#' Collate Daily Breadcrumb Metadata
#'
#' @param crumb_directory directory containing the breadcrumb files
#'
#' @return tibble containing `file` as full path to each breadcrumb and `date`
#' as extracted from the breadcrumb file name.
#' @export
#'
#' @examples
#' \dontrun{
#' ris_storage_path("Reports") %>%
#'   collate_daily_crumb_metadata()
#' }
collate_daily_crumb_metadata <- function(crumb_directory) {
  dplyr::tibble(
    file = list.files(
      path = crumb_directory,
      pattern = "^Breadcrumb",
      full.names = TRUE
    )
  ) %>%
    dplyr::mutate(
      # strip path
      file_basename = basename(file),
      # strip extension
      file_basename_sans_ext = tools::file_path_sans_ext(file_basename),
      # strip leading 'Breadcrumb_' or 'Breadcrumb-'
      file_date_string = stringr::str_remove(
        file_basename_sans_ext, "^Breadcrumb[_-]"
      ),
      # case_when evaluates all rhs and keeps desired
      # this produces warning messages that do not apply to the final results
      # we will suppress all warnings for now and check for errors later
      date = dplyr::case_when(
        stringr::str_detect(file_basename, daily_crumb_name_patterns[1]) ~
          suppressWarnings(
            lubridate::as_date(file_date_string, format = "%d%b%Y")),
        stringr::str_detect(file_basename, daily_crumb_name_patterns[2]) ~
          suppressWarnings(
            lubridate::as_date(file_date_string, format = "%d%B%Y")),
        stringr::str_detect(file_basename, daily_crumb_name_patterns[3]) ~
          suppressWarnings(
            lubridate::as_date(file_date_string, format = "%d_%B_%Y")),
        stringr::str_detect(file_basename, daily_crumb_name_patterns[4]) ~
          suppressWarnings(
            lubridate::as_date(file_date_string)),
        TRUE ~ lubridate::NA_Date_
      )
    ) %>%
    dplyr::select(file, date) -> daily_crumb_names_and_dates

  daily_crumb_names_and_dates %>%
    dplyr::filter(is.na(date)) %>%
    dplyr::pull(file) -> undated_daily_crumb_names

  if (length(undated_daily_crumb_names) > 0) {
    stop(
      sprintf(
        "Date extraction failed for %i breadcrumb files starting with %s.",
        length(undated_daily_crumb_names),
        undated_daily_crumb_names[1]
      )
    )
  }

  daily_crumb_names_and_dates %>%
    dplyr::arrange(date)
}



#' Read a Collated Breadcrumb
#'
#' @param x collated breadcrumb file
#'
#' @return tibble representation of collated breadcrumb contents
#' @export
#'
#' @examples
#' \dontrun{
#' read_collated_crumb("breadcrumb_2015-01-01_to_2015-02-01.csv")
#' }
#'
#' @note Read is based on `master_crumb_template` format.
read_collated_crumb <- function(x) {
  readr::read_csv(x, col_types = "cdddcccDtddddddddcdcddd")
}



#' Write a Timespan Breadcrumb
#'
#' @param daily_crumb_metadata tibble of breadcrumb names and dates from
#' `collate_daily_crumb_metadata()`
#' @param d0 start date (e.g., '2022-01-01')
#' @param d1 end date (e.g., '2022-02-01')
#'
#' @return name of the timespan breadcrumb file invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' ris_storage_path("Reports") %>%
#'   collate_daily_crumb_metadata() %>%
#'   write_timespan_crumb("2022-01-01", "2022-02-01")
#' }
write_timespan_crumb <- function(daily_crumb_metadata, d0, d1) {
  csv <- sprintf("breadcrumb_%s_to_%s.csv", d0, d1)

  daily_crumb_metadata %>%
    dplyr::filter(d0 <= date, date < d1) -> filtered_daily_crumb_metadata

  message(
    sprintf(
      "Collating %i daily breadcrumb files from %s to %s into %s.",
      nrow(filtered_daily_crumb_metadata), d0, d1, csv
    )
  )

  filtered_daily_crumb_metadata %>%
    purrr::pmap_dfr(function(file, ...) read_daily_crumb(file)) %>%
    readr::write_csv(csv, na = "")

  invisible(csv)
}



#' Write Monthly Breadcrumbs for Year
#'
#' @param daily_crumb_metadata tibble of breadcrumb names and dates from
#' `collate_daily_crumb_metadata()`
#' @param year the desired year
#'
#' @return names of the timespan (monthly) breadcrumb files
#' @export
#'
#' @examples
#' \dontrun{
#' ris_storage_path("Reports") %>%
#'   collate_daily_crumb_metadata() %>%
#'   write_monthly_crumbs_for_year(2015)
#' }
write_monthly_crumbs_for_year <- function(
  daily_crumb_metadata,
  year
) {
  dplyr::tibble(
    d0 = sprintf("%i-%02d-01", year, 1:12),
    d1 = c(sprintf("%i-%02d-01", year, 2:12), sprintf("%i-01-01", year + 1))
  ) %>%
    purrr::pmap_chr(
      write_timespan_crumb,
      daily_crumb_metadata = daily_crumb_metadata
    )
}



#' Collate Monthly Breadcrumbs
#'
#' @param monthly_crumb_files names of the monthly breadcrumb files to
#' collate (i.e., return value from `write_monthly_crumbs_for_year()`)
#' @param debug output word counts for all monthly and the year breadcrumb files
#' to see if the montly add up to the yearly
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ris_storage_path("Reports") %>%
#'   collate_daily_crumb_metadata() %>%
#'   write_monthly_crumbs_for_year(2015) %>%
#'   collate_monthly_crumbs()
#' }
collate_monthly_crumbs <- function(
  monthly_crumb_files,
  debug = FALSE
) {
  year <- stringr::str_extract(monthly_crumb_files[1], "\\d{4}")
  yearly_crumb_file <- sprintf("breadcrumb_%s.csv", year)

  message(
    sprintf(
      "Row-binding %i monthly into yearly breadcrumb file named %s.",
      length(monthly_crumb_files),
      yearly_crumb_file
    )
  )

  system2(
    "awk",
    paste(
      "'(NR == 1) || (FNR > 1)'",
      paste(monthly_crumb_files, collapse = " "),
      ">",
      yearly_crumb_file
    )
  )

  if (debug) {
    system2("wc", sprintf("-l breadcrumb_%s-*", year))
    system2("wc", sprintf("-l breadcrumb_%s.csv", year))
  }
}



#' Remove Monthly Breadcrumb Files
#'
#' @param monthly_crumb_files names of the monthly breadcrumb files to
#' collate (i.e., return value from `write_monthly_crumbs_for_year()`)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # silly example to write breadcrumbs and the immediately delete them
#' ris_storage_path("Reports") %>%
#'   collate_daily_crumb_metadata() %>%
#'   write_monthly_crumbs_for_year(2015) -> monthly_crumb_files
#'
#' monthly_crumb_files %>%
#'   collate_monthly_crumbs()
#'
#' monthly_crumb_files %>%
#'   remove_monthly_crumbs()
#' }
remove_monthly_crumbs <- function(monthly_crumb_files) {
  system2("rm", paste(c("-f", monthly_crumb_files), collapse = " "))
}



#' Create Empty Breadcrumb Database Table
#'
#' @param con a DBIConnection object, as returned by `dbConnect()`.
#' @param year year of the table
#' @param fidelity fidelty of the breadcrumbs
#' (i.e., either 30 seconds or 1 second)
#'
#' @return name of the newly created database table
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   RMariaDB::MariaDB(),
#'   default.file = normalizePath("~./.my.cnf"),
#'   group = "spark.myftp.org"
#' )
#'
#' create_crumb_table(con, 2015)
#'
#' DBI::dbDisconnect(con)
#' }
create_crumb_table <- function(
  con,
  year,
  fidelity = c("30", "1")
) {
  checkmate::assert_int(year)
  fidelity <- match.arg(fidelity)
  database_table <- sprintf("breadcrumb_%ss_%i", fidelity, year)

  DBI::dbExecute(
    con,
    sprintf(
      "CREATE TABLE %s (
        id int NOT NULL AUTO_INCREMENT,
        vehicle_name varchar(255),
        latitude double,
        longitude double,
        vehicle_speed double,
        event_name varchar(255),
        address varchar(255),
        event_type varchar(255),
        date date,
        time time(6),
        odometer_reading double,
        gps_fix_quality double,
        peak_speed double,
        average_speed double,
        initial_speed double,
        final_speed double,
        previous_fuel_level double,
        new_fuel_level double,
        nearest_landmark_name varchar(255),
        trip_distance double,
        driver_name varchar(255),
        occupants double,
        speed_limit double,
        difference double,
        PRIMARY KEY (id)
      );",
      database_table
    )
  ) -> r

  if (r == 0) {
    invisible(database_table)
  }
}



#' Insert into Breadcrumb Database Table
#'
#' @param con a DBIConnection object, as returned by `dbConnect()`.
#' @param crumb_table name of the database table for insertion
#' @param monthly_crumb_files names of the monthly breadcrumb files
#'
#' @return number of records affected
#' @export
#'
#' @examples
#' \dontrun{
#' ris_storage_path("Reports") %>%
#'   collate_daily_crumb_metadata() %>%
#'   write_monthly_crumbs_for_year(2015) -> monthly_crumb_files
#'
#' con <- DBI::dbConnect(
#'   RMariaDB::MariaDB(),
#'   default.file = normalizePath("~./.my.cnf"),
#'   group = "spark.myftp.org"
#' )
#'
#' create_crumb_table(
#'   con, 2015
#' ) -> crumb_table
#'
#' insert_into_crumb_table(
#'   con, crumb_table, monthly_crumb_files
#' )
#'
#' DBI::dbDisconnect(con)
#' }
insert_into_crumb_table <- function(
  con,
  crumb_table,
  monthly_crumb_files
) {
  purrr::map_dbl(monthly_crumb_files, ~ {
    message("Reading monthly breadcrumb file ", .x, "...")
    if (file.size(.x) > 0) {
      read_collated_crumb(.x) %>%
        DBI::dbAppendTable(con, crumb_table, .)
    } else {
      0
    }
  }) -> n_records

  names(n_records) <- monthly_crumb_files
  return(n_records)
}
