#' Append a Timestamp to the End of a File Name
#'
#' @param x file name to which a time stamp string should be appended.
#'
#' @return file name with time stamp string appended
#' @export
#'
#' @examples
#' timestamp_filename("redcap_sas_export")
timestamp_filename <- function(x) {
  sprintf("%s_%s", x, format(Sys.time(), "%y%m%d%H%M%S"))
}



#' SSH Tunnel to Drives Relational Database Management System Sever
#'
#' This function issues a system call to open a new xterm window and construct
#' an ssh tunnel to the server over port 3306.
#'
#' @param user wustl key username
#' @param server fqdn of the server
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tunnel_to_database_server()
#' }
#'
#' @note An ssh key can be installed on the server to avoid password entry.
#'  1. issue `ssh-keygen` locally to create the key
#'  2. run `ssh-copy-id user@hostname` to install the key on the remote server
tunnel_to_database_server <-
  function(user = Sys.getenv("WUSTL_KEY_USER"),
           server = Sys.getenv("DRIVES_DATABASE_SERVER")) {
    if (stringr::str_detect(Sys.info()["nodename"], "ris.wustl.edu$", TRUE)) {
      warning(
        paste0(
          "`tunnel_to_database_server()` ",
          "is intended for use on the RIS compute cluster."
        )
      )
    }

    system2(
      command = "xterm",
      args = sprintf("-e ssh -4 -L 3306:127.0.0.1:3306 %s@%s", user, server),
      wait = FALSE
    )
  }
