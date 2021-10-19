#' Calculate Radius of Gyration
#' @author Julie Wisch
#' @param DF The dataframe that contains all trips that you want to calculate
#' the radius of gyration for
#' @param Latitude_Name Character string of the column name in DF that has the
#' trip end latitude location
#' @param Longitude_Name Character string of the column name in DF that has the
#' trip end longitude location
#'
#' @returns  Rg a single numeric value that gives the radius of gyration for the
#'  supplied dataframe in kilometers
#'
#' @examples
#' \dontrun{
#' DF <- data.frame(
#'   "MapID" = c(68771, 68771, 69876),
#'   "Trip_End_Latitude" = c(38.88768, 38.606689, 40.23523),
#'   "Trip_End_Longitude" = c(99.6986, 97.76478, 98.56789)
#' )
#' get_radius_gyration(
#'   DF[DF$MapID == 68771,],
#'   "Trip_End_Latitude",
#'   "Trip_End_Longitude"
#' )
#' }
#'
#' @references
#' \url{https://blog.jlevente.com/radius-of-gyration/}
#' \url{https://alzres.biomedcentral.com/articles/10.1186/s13195-021-00852-1}


get_radius_gyration <- function(
  DF,
  Latitude_Name = "Trip_End_Latitude",
  Longitude_Name = "Trip_End_Longitude"
){

  #radius of gyration = sqrt(1/n * sum( distance(point - center of mass)^2)

  DF <- data.frame(DF)
  DF[, Latitude_Name] <- as.numeric(DF[, Latitude_Name] )
  DF[, Longitude_Name] <- as.numeric(DF[, Longitude_Name] )

  N <- nrow(DF)
  for(i in 1:N){
    #If there is an error when calculating the haversine distance, just skip it
    #and print an error message
    DF[i, "haversine_dist"] <- tryCatch(
      {pracma::haversine(c(DF[i, Latitude_Name], DF[i, Longitude_Name]),
                         c(mean(DF[, Latitude_Name], na.rm = TRUE),
                           mean(DF[, Longitude_Name], na.rm = TRUE)))   },
      error = function(cond) {
        message(paste0("Error in ", DF[i, "Group"]))
        return(NA)
      } )

  }
  #haversine distance is in km
  Rg = sqrt((1/N) * sum(DF[, "haversine_dist"], na.rm = TRUE) ^ 2)
  return(Rg)
}




#' Calculate Route Straightness
#' @author Julie Wisch
#' @param Start_Latitude latitude coordinate for starting location
#' @param Start_Longitude longitude coordinate for starting location
#' @param End_Latitude latitude coordinate for ending location
#' @param End_Longitude longitude coordinate for ending location
#' @param Distance_Travelled distance travelled. Measured by Azuga chip. Units
#' in Miles.
#'
#' @returns  straightness The straightness index. A value between 0 and 1 that
#' indicates route straightness
#'
#' @examples
#' \dontrun{
#' get_route_straightness(38.6630107, -90.327784, 38.6247528, -90.2197958, 18.2)
#' }
#'
#' @references
#' \url{https://cran.rstudio.com/web/packages/trajr/vignettes/trajr-vignette.html}


get_route_straightness <- function(
  Start_Latitude, Start_Longitude,
  End_Latitude, End_Longitude,
  Distance_Travelled
){
    hav_distance_km <- pracma::haversine(
      c(Start_Latitude, Start_Longitude),
      c(End_Latitude, End_Longitude)
    ) #gives distance in km
    #distance traveled / length of trajectory. Ranges between 0 and 1 where 1 is a straight line.
    #Distance Travelled comes from Azuga in miles
    straightness <- (hav_distance_km / 1.609) / Distance_Travelled
    return(straightness)
}
