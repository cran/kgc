#' TranslateZipCode
#'
#' This function will return the data frame with the longitude and latitude of the zip codes
#'
#' @name TranslateZipCode
#' @param number A dataframe that contains zip codes in a column labeled 'zip'
#' @return The original data frame, with two additional columns for the corresponding latitude and longitude
#' @export
#' @examples
#' example <- data.frame("zip" = c(44106,638, 1106))
#' TranslateZipCode(example)

TranslateZipCode <- function(number){
  load(system.file("exdata", "zipcodes.RData", package = "kgc"))
  if (is.data.frame(number)) {
    df <- number
  }
  else {
    df <- data.frame(number)
    names(df) <- c("zip")
  }
  Latitude <- rep(NA,nrow(df))
  Longitude <- rep(NA,nrow(df))
  for (i in 1:nrow(df)){
    if (!is.na(df$zip[i])){
      if(length(which((zipcodes$zip == df$zip[i])))) {
        df$Latitude[i] <- as.numeric(zipcodes[which((zipcodes$zip == df$zip[i])),2])
        df$Longitude[i] <- as.numeric(zipcodes[which((zipcodes$zip == df$zip[i])),3])
      }
      else {
        df$Latitude[i] <- NA
        df$Longitude[i] <- NA
      }
    }
  }
  return(df)
}
