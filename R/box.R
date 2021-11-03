#' Upload content to the Roe Lab folder on Box via FTP
#'
#' @param file name or path of the local file to be uploaded.
#' @param path the path to which the content is to be uploaded.
#' @param user Box username (i.e., WashU email)
#' @param pass unique password for external applications. Created at
#' <https://wustl.app.box.com/account>
#' @param ... additional arguments passed to [RCurl::ftpUpload()]
#'
#' @inherit RCurl::ftpUpload return
#' @export
#'
#' @examples
#' \dontrun{
#' file <- tempfile("roe_box_ftp_upload_test", fileext = ".txt")
#' writeLines(c(
#'   "roelabr::roe_box_ftp_upload()",
#'   sprintf("uploaded this at %s.", Sys.time())
#' ), file)
#' path <- file.path("test_directory", basename(file))
#' user <- Sys.getenv("WUSTL_BOX_USER")
#' pass <- Sys.getenv("WUSTL_BOX_PASS")
#' roe_box_ftp_upload(file, path, user, pass)
#' }
#'
#' @references <https://wustl.app.box.com/services/box_ftp_server/>
roe_box_ftp_upload <- function(file, path, user, pass, ...) {
  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("Package \"RCurl\" needed for this function. Please install it.",
      call. = FALSE
    )
  }

  RCurl::ftpUpload(
    what = file,
    to = sprintf("ftp://ftp.box.com/Roe Lab/%s", path),
    userpwd = sprintf("%s:%s", user, pass),
    ftp.create.missing.dirs = TRUE,
    ...
  )
}
