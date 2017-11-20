#' getZone
#'
#' This function will return the character code of a given climate zone corresponding to a numeric value (factor levels of the fine resolution data).
#'
#' @name getZone
#' @param num The numeric code describing a climate zone.
#' @return A string describing the climate zone as a character code.
#' @export
#' @examples
#' ccz <- getZone(16)
getZone <- function(num){
  charvec <- c('Af', 'Am', 'As', 'Aw',
    'BSh', 'BSk', 'BWh', 'BWk',
    'Cfa', 'Cfb', 'Cfc',
    'Csa', 'Csb', 'Csc',
    'Cwa', 'Cwb', 'Cwc',
    'Dfa', 'Dfb', 'Dfc','Dfd',
    'Dsa', 'Dsb', 'Dsc', 'Dsd',
    'Dwa', 'Dwb', 'Dwc', 'Dwd',
    'EF','ET', 'Ocean')

  return(charvec[num])
}
