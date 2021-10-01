#' Calculate Radius of Gyration
#' @author Julie Wisch
#' @param DF The dataframe that contains all trips that you want to calculate the radius of gyration for
#' @param Latitude_Name Character string of the column name in DF that has the trip end latitude location
#' @param Longitude_Name Character string of the column name in DF that has the trip end longitude location
#'
#' @returns  Rg a single numeric value that gives the radius of gyration for the supplied dataframe in kilometers
#' @examples
#' \dontrun{
#' get_radius_gyration(df[df$Map_ID == 67710, ], "Trip_End_Latitude", "Trip_End_Longitude)
#' }
#'
#' @references
#' \url{https://blog.jlevente.com/radius-of-gyration/}
#' \url{https://alzres.biomedcentral.com/articles/10.1186/s13195-021-00852-1}


get_radius_gyration <- function(DF, Latitude_Name = "Trip_End_Latitude", Longitude_Name = "Trip_End_Longitude"){

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
