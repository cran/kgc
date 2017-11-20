library(plyr)
library(shiny)
library(shinythemes)

server <- function(input, output, session) {
  
  location <- eventReactive(input$submit, {

    if (is.na(input$zip)) {
      loc <- data.frame(zip = as.numeric(NA))
      loc <- data.frame(loc,lat = input$lat ,long = input$long)
    }
    
    else {
      loc <- data.frame(TranslateZipCode(input$zip))
    }
    
    loc <- data.frame(loc, rndCoord.lat = RoundCoordinates(loc$lat), rndCoord.lon = RoundCoordinates(loc$long))
    loc <- data.frame(loc,ClimateZ=LookupCZ(loc))
    loc <- data.frame(loc, CZUncertainty(loc))
    rename(loc, c("rndCoord.lat" = "rounded lat", "rndCoord.lon" = "rounded long", "ClimateZ" = "predicted KG-CZ", "possible.cz"="possible KG-CZ"))
  })
  
  output$view <- renderTable({
    head(location()) 
  })
  
  
  observe({
    # Run whenever reset button is pressed
    input$reset
    
    # Send an update to fields to reset values to NA
    updateNumericInput(session, "zip", value = NA)
    updateNumericInput(session, "lat", value = NA)
    updateNumericInput(session, "long", value = NA)
    
    
  })
}

ui <- fluidPage(
  shinyUI(fluidPage(
    theme = shinytheme("spacelab"),
    titlePanel("Koppen-Geiger Climatic Zones"),
    
    sidebarLayout(
      sidebarPanel(
        helpText("Enter either a zip code OR coordinates for the location."),
        numericInput(inputId = "zip", "Zip", label = "Zip Code", value = NA),
        numericInput(inputId = "lat", "Latitude", label = "Latitude", value = NA),
        numericInput(inputId = "long", "Longitude", label = "Longitude", value = NA),
        actionButton("submit", "Submit"),
        actionButton("reset", "Reset")
      ),
      mainPanel(
      img(src="kgc-map.jpg", width = 600, height = 400),
      tableOutput("view")
    ))
  ))
)

shinyApp(ui = ui, server = server)