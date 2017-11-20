#' RunExample
#'
#' This function will run the shiny app found in this package.
#'
#' @name RunExample
#' @return launches the shiny app created for the package.
#' @import plyr shiny shinythemes
#' @export
#' @examples
#' \dontrun{
#' kgc::RunExample()
#' }
RunExample <- function() {
  appDir <- system.file("shiny-examples", "kgcshiny", package = "kgc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing kgc.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
