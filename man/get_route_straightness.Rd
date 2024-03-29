% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{get_route_straightness}
\alias{get_route_straightness}
\title{Calculate Route Straightness}
\usage{
get_route_straightness(
  Start_Latitude,
  Start_Longitude,
  End_Latitude,
  End_Longitude,
  Distance_Travelled
)
}
\arguments{
\item{Start_Latitude}{latitude coordinate for starting location}

\item{Start_Longitude}{longitude coordinate for starting location}

\item{End_Latitude}{latitude coordinate for ending location}

\item{End_Longitude}{longitude coordinate for ending location}

\item{Distance_Travelled}{distance travelled. Measured by Azuga chip. Units
in Miles.}
}
\value{
straightness The straightness index. A value between 0 and 1 that
indicates route straightness
}
\description{
Calculate Route Straightness
}
\examples{
\dontrun{
get_route_straightness(38.6630107, -90.327784, 38.6247528, -90.2197958, 18.2)
}

}
\references{
\url{https://cran.rstudio.com/web/packages/trajr/vignettes/trajr-vignette.html}
}
\author{
Julie Wisch
}
