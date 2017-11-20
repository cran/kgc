#' RoundCoordinates
#'
#' This function will round the inputed value to a co-ordinate present in the KG Climate Zone look up table.
#'
#' @name RoundCoordinates
#' @param number The value to round
#' @param res Specify the resolution as a string - 'course' for lower resolution (default, 0.5 degrees), 'fine' for higher resolution (100s).
#' @param latlong Whether the number is a latitude value ('lat', default), or longitude value ('lon') (this parameter is only used if res='fine').
#' @return The inputed number, rounded to a value ending in either .25 or .75 (course), or to nearest 'fine' (100s) resolution grid point.
#' @export
#' @examples
#' RoundCoordinates(10.98)
#' RoundCoordinates(10.98,res='fine',latlong='lon')
RoundCoordinates <- function(number,res = 'course',latlong = 'lat'){

  if(res == 'fine'){
    # Fine resolution
    num <- NULL
    for (i in 1:length(number)){
      cl <- genCoords(latlong = latlong)

      if(latlong == 'lat'){
	lcl <- cl[which(cl > number[i])]; lcl <- lcl[length(lcl)]
	hcl <- cl[which(cl < number[i])]; hcl <- hcl[1]
      }else if(latlong == 'lon'){
	lcl <- cl[which(cl < number[i])]; lcl <- lcl[length(lcl)]
	hcl <- cl[which(cl > number[i])]; hcl <- hcl[1]
      }else{
        cat("\nBad value for 'latlong' in RoundCoordinates()\n")
      }

      cl <- c(lcl,hcl)

      # This rounds by nearest neighbor interpolation
      num[i] <- cl[which(abs(cl - rep(number[i],2)) == min(abs(cl - rep(number[i],2))))[1]]
    }

  }else{
    # Course resolution
    num <- NULL
    for (i in 1:length(number)){

      if (!is.na(number[i])){
	if (number[i] >= round(number[i])){
	  num[i] <- (round(number[i]) + round(number[i]) + 0.5)/2
	}
	else num[i] <- (2*round(number[i]) - 0.5)/2
      }
      else num[i] <- NA
    }

  }

  return(num)
}
