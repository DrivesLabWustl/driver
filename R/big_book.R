#' Publish Big Book of Drives Notes
#'
#' @param server FQDN of server
#' @param user WUSTL Key username
#' @param pass WUSTL Key password
#'
#' @export
#'
#' @note After building the Big Book of Drives Notes, run this function while
#' connected to msvpn.wusm.wustl.edu to publish on the server.
publish_big_book_of_drives_notes <- # nolint
  function(server = "wuit-s-12552.accounts.ad.wustl.edu",
           user = Sys.getenv("WUSTL_KEY_USER"),
           pass = Sys.getenv("WUSTL_KEY_PASS")) {
    message("Connecting to remote server.")
    ssh::ssh_connect(
      sprintf("%s@wuit-s-12552.accounts.ad.wustl.edu", user),
      passwd = pass
    ) -> session

    message("Compressing local docs directory.")
    tar("docs.tar", list.files("docs", full.names = TRUE, recursive = TRUE))

    message("Uploading tarball:")
    ssh::scp_upload(session, "docs.tar")

    message("Untarring on remote.")
    ssh::ssh_exec_internal(session, "tar -xvf docs.tar")

    message("Decommissioning old website.")
    ssh::ssh_exec_internal(
      session,
      sprintf("echo %s | sudo -S rm -rf /var/www/html/*", pass)
    )

    message("Publishing new website.")
    ssh::ssh_exec_internal(
      session,
      sprintf("echo %s | sudo -S mv ./docs/* /var/www/html/", pass)
    )

    message("Cleaning up.")
    ssh::ssh_exec_internal(session, "rmdir docs")
    ssh::ssh_exec_internal(session, "rm docs.tar")
    unlink("docs.tar")

    message("Disconnecting from remote.")
    ssh::ssh_disconnect(session)
  }
