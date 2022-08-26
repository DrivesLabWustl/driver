#' Azuga FTP Parse Date
#'
#' @param x string representation of the datetime from the ftp server
#'
#' @return datetime representation of the datetime from the ftp server
#'
#' @examples
#' # Two versions of a datetime observed from the ftp server
#'
#' ## Version 1
#' azuga_ftp_parse_date("Apr 04 2021")
#'
#' ## Version 2
#' azuga_ftp_parse_date("Apr 04 07:39")
azuga_ftp_parse_date <- function(x) {
  month <- stringr::str_extract(x, "^[A-Z][a-z]{2}")
  month <- vapply(month, function(m) which(m == month.abb), integer(1))
  month <- dplyr::if_else(month < 10, paste0("0", month), as.character(month))
  day <- stringr::str_extract(x, "(?<=[a-z] )\\d{2}")
  year <- stringr::str_extract(x, "\\d{4}$")
  this_year <- as.character(lubridate::year(lubridate::today()))
  year <- dplyr::if_else(is.na(year), this_year, year)
  hour <- stringr::str_extract(x, "\\d{2}(?=:)")
  hour <- dplyr::if_else(is.na(hour), "00", hour)
  minute <- stringr::str_extract(x, "(?<=:)\\d{2}")
  minute <- dplyr::if_else(is.na(minute), "00", minute)
  d <- sprintf("%s-%s-%s %s:%s:00", year, month, day, hour, minute)
  d <- lubridate::as_datetime(d)
  # translate datetime to current timezone
  #d <- lubridate::as_datetime(as.numeric(d), tz = Sys.timezone())
  return(d)
}

#' Azuga FTP List
#'
#' @param path directory to list
#' @param host domain name of Azuga FTP server
#' @param user username
#' @param pass password
#'
#' @return tibble representation of the list results
#' @export
#'
#' @examples
#' \dontrun{
#' azuga_ftp_list("Reports")
#' }
azuga_ftp_list <-
  function(path = "",
           host = "ftp://pentaho8.azuga.com",
           user = Sys.getenv("AZUGA_FTP_USER"),
           pass = Sys.getenv("AZUGA_FTP_PASS")) {
    path <- dplyr::if_else(
      stringr::str_detect(path, "^/"),
      path,
      paste0("/", path)
    )
    path <- dplyr::if_else(
      stringr::str_detect(path, "/$"),
      path,
      paste0(path, "/")
    )
    f <- tempfile()
    RCurl::getURL(
      url = paste0(host, path),
      userpwd = sprintf("%s:%s", user, pass)
    ) %>%
      stringr::str_remove("\r\n$") %>%
      writeLines(f)

    # note: server does not support MLSD or MLST commands
    # example
    #          1         2         3         4         5         6         7
    # 123456789012345678901234567890123456789012345678901234567890123456789012345678
    # -rw-r--r--    1 0        0          329363 Apr 04  2021 Activity_01Apr2021.csv
    # -rw-r--r--    1 0        0          422909 Apr 04 07:39 Activity_01Apr2022.csv
    # -rw-r--r--    1 0        0          244113 Aug 04  2021 Activity_01Aug2021.csv

    utils::read.fwf(
      file = f,
      widths = c(14, 1, 10, 10, 7, 14, 30),
      col.names = c(
        'permissions',
        'hard_links',
        'owner',
        'group',
        'bytes',
        'last_modified',
        'filename'
      )
    ) %>%
      dplyr::mutate(
        dplyr::across(is.character, trimws),
        dplyr::across(c(owner, group), as.character),
        last_modified = azuga_ftp_parse_date(last_modified),
        file_type = dplyr::if_else(
          stringr::str_detect(permissions, "^d"),
          "directory",
          "file"
        ),
        path = stringr::str_remove(path, "(?<=.)/$")
      ) %>%
      dplyr::select(
        path,
        filename,
        bytes,
        file_type,
        last_modified,
        permissions,
        owner,
        group,
        hard_links
      ) %>%
      dplyr::arrange(file_type)
  }
