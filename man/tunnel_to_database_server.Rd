% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{tunnel_to_database_server}
\alias{tunnel_to_database_server}
\title{SSH Tunnel to Drives Relational Database Management System Sever}
\usage{
tunnel_to_database_server(
  user = Sys.getenv("WUSTL_KEY_USER"),
  server = Sys.getenv("DRIVES_DATABASE_SERVER")
)
}
\arguments{
\item{user}{wustl key username}

\item{server}{fqdn of the server}
}
\description{
This function issues a system call to open a new xterm window and construct
an ssh tunnel to the server over port 3306.
}
\note{
An ssh key can be installed on the server to avoid password entry.
\enumerate{
\item issue \code{ssh-keygen} locally to create the key
\item run \verb{ssh-copy-id user@hostname} to install the key on the remote server
}
}
\examples{
\dontrun{
tunnel_to_database_server()
}

}
