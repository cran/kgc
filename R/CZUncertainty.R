#' CZUncertainty
#'
#' This function will return the uncertainty associated with the predicted climate zone along with other potential climate zones.
#'
#' @name CZUncertainty
#' @param data The co-ordinates for the location you are trying to predict the climate zone of.
#' @return The uncertainty associated with the located climate zone along with other potential climate zones.
#' @export
#' @examples
#' data<- data.frame(Site = c("GC","UFS","NEG"),
#'   Longitude = c(-15.42,10.98,34.78),
#'   Latitude = c(27.82,47.42,30.86))
#' data <- data.frame(data,
#'   rndCoord.lon = RoundCoordinates(data$Longitude),
#'   rndCoord.lat = RoundCoordinates(data$Latitude))
#' data <- data.frame(data,ClimateZ=LookupCZ(data))
#' data <- data.frame(data, CZUncertainty(data))
CZUncertainty <- function(data){
  uncertainty <- rep(NA,nrow(data))
  possible.cz <- rep(NA,nrow(data))
  for ( i in 1:nrow(data)){
    check <- subset(climatezones,(!(Lat > data$rndCoord.lat[i] + 0.5)) & (Lat >= data$rndCoord.lat[i]-0.5))
    check <- subset(check,(!(Lon > data$rndCoord.lon[i] + 0.5)) & (Lon >= data$rndCoord.lon[i] - 0.5))
    check <- droplevels(check)
    tot <- nrow(check)
    check2 <- subset(check,!(as.character(Cls) == as.character(data$ClimateZ[i])))
    check2 <- droplevels(check2)
    n = nrow(check2)
    uncertainty[i] <- n/tot
    possible.cz[i] <- paste(levels(check2$Cls), sep = "", collapse = " ")
  }

  dat<-data.frame(uncertainty, possible.cz)

  return(dat)
}
