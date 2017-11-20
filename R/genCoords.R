#' genCoords
#'
#' This function will generate a list of coordinates for the 'fine' resolution climate zone map data, stored in 'kgz.rda'.
#'
#' @name genCoords
#' @param latlong Whether a vector of latitude values, or longitude values, is being created.
#' @param full Specify whether to generate a full list of all coords in the full image, or just one row/column (default FALSE, means just one row/column).
#' @param latdim The number of pixels in the image in the latitude dimension (y)
#' @param londim The number of pixels in the image in the longitude dimension (x)
#' @return A vector of latitude or longitude coordinate values (default for a map 12960x 6480y).
#' @export
#' @examples
#' ccoords <- genCoords(latlong='lon',full='true')
genCoords <- function(latlong = 'lat',full = FALSE, latdim = 6480, londim = 12960){
  # Notes about geographic conventions
  #   latitude is north/south with equator being 0 degrees, north pole being 90 degrees, south pole being -90 degrees (180 degree span)
  #   longitude is east/west with greenwhich meridian being 0 degrees, and antipodal meridian being +/-180 degrees (360 degree span)

  if(latlong == 'lat'){
    coordvec <- seq(90,-90,length.out = latdim+1)
  }else if(latlong == 'lon'){
    coordvec <- seq(-180,180,length.out = londim+1)
  }else{
    cat("\nBad value for 'latlong' in genCoords()\n")
  }

  # Shift over by half a span, remove first value
  hd <- coordvec[2] - coordvec[1]
  coordvec <- coordvec - (hd/2)
  coordvec <- coordvec[-1]


  if(full){
    # Expand to entire list of coords for full picture
    if(latlong == 'lat'){
      # Reading by rows like a book, so one row of first value, next row of second value, repeat
      coordvec <- unlist(lapply(1:length(coordvec),function(x){rep(coordvec[x],londim)}))
    }else if(latlong == 'lon'){
      # Reading by rows like a book, so one row of all lon values, repeat times number of rows
      coordvec <- rep(coordvec,latdim)
    }else{
      cat("\nBad value for 'latlong' in genCoords()\n")
    }
  }

  return(coordvec)

}
