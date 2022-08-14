##########################################
# Project Land Use change in London
# Clementine Cottineau 
# UCL - CASA - UDL
# 1 November 2016
##########################################

library(shiny)
library(leaflet)



shinyUI(
  fluidPage(
    headerPanel(
      "Land Use Change in London 2007-2010"
    ),
      column(6,  selectInput(
        "year",
        "Year",
        choices = c(2007, 2010),
        selected = 2007
    )),
    column(6,   selectInput(
      "variable",
      "Colour Map by...",
      choices = c("PREVIOUS_LAND_USE", "CURRENT_LAND_USE", "PROPOSED_USE"),
      selected = "PREVIOUS_LAND_USE"
    )),
    leafletOutput('map'),
    selectInput(
      "table",
      "Table to print",
      choices = c("Absolute number of conversion"="N", 
                  "Percentage in lines" = "LinePct",
                  "Percentage in columns" = "ColPct"),
      selected = "N"
    ),
    h3("In column: previous land  use. In line: proposed use"),
    dataTableOutput('transitions'),
    HTML(
      'For More:
      <a href=https://github.com/ClementineCttn/LandUseChangeLondon>https://github.com/ClementineCttn/LandUseChangeLondon</a>'
    )
    
  )
  )
    
    