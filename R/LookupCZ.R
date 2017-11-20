#' LookupCZ
#'
#' This function will return the climate zone for the co-ordinates provided.
#'
#' @name LookupCZ
#' @param data The co-ordinates for the location you are trying to predict the climate zone of (format is three column dataframe, first column site IDs, second column longitude values, third column latitude values).
#' @param res Specify the resolution as a string - 'course' for lower resolution (default, 0.5 degrees), 'fine' for higher resolution (100s).
#' @param rc Specify whether to generate the rounded coordinate columns in the input dataframe, default is FALSE.
#' @return The climate zone for the co-ordinates provided.
#' @export
#' @examples
#' data <- data.frame(Site = c("GC","UFS","NEG"),
#'   Longitude = c(-15.42,10.98,34.78),
#'   Latitude = c(27.82,47.42,30.86))
#' data <- data.frame(data,
#'   rndCoord.lon = RoundCoordinates(data$Longitude),
#'   rndCoord.lat = RoundCoordinates(data$Latitude))
#' data <- data.frame(data,ClimateZ=LookupCZ(data))
LookupCZ <- function(data,res = 'course',rc = FALSE){
  if(rc){
    data <- data.frame(data, rndCoord.lon = RoundCoordinates(data$Longitude,res = res,latlong = 'lon'),
      rndCoord.lat = RoundCoordinates(data$Latitude,res = res,latlong = 'lat'))
  }

  if(res == 'fine'){
    # Fine resolution
    ClimateZ <- rep(NA, nrow(data))
    for(i in 1:nrow(data)){
      if (!(is.na(data$rndCoord.lon[i])|is.na(data$rndCoord.lat[i]))){
        loncoords <- genCoords(latlong = 'lon',full = TRUE)
        latcoords <- genCoords(latlong = 'lat',full = TRUE)
	indstf1 <- loncoords == data$rndCoord.lon[i]
	indstf2 <- latcoords == data$rndCoord.lat[i]
	inds <- which(indstf1 & indstf2)

	if(length(inds)>0){
	  ClimateZ[i] <- as.character(getZone(kmz[inds]))
	}else{
          ClimateZ[i] <- "Climate Zone info missing"
        }
      }else{
        ClimateZ[i] <- NA
      }
    }

  }else{
    # Course resolution
    ClimateZ <- rep(NA, nrow(data))
    for(i in 1:nrow(data)){
      if (!(is.na(data$rndCoord.lon[i])|is.na(data$rndCoord.lat[i]))){
	if(length(which((climatezones$Lon == data$rndCoord.lon[i]) & (climatezones$Lat == data$rndCoord.lat[i])))){
	  ClimateZ[i] <- as.character(climatezones[which((climatezones$Lon == data$rndCoord.lon[i]) & (climatezones$Lat == data$rndCoord.lat[i])),3])
	}else{
          ClimateZ[i] <- "Climate Zone info missing"
        }
      }else{
        ClimateZ[i] <- NA
      }
    }
  }

  return(as.factor(ClimateZ))

}
