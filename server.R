##########################################
# Project Land Use change in London
# Clementine Cottineau 
# UCL - CASA - UDL
# 1 November 2016
##########################################

require(sp)
require(rgdal)
require(leaflet)
require(rgeos)
require(raster)
require(maptools)



#############################
######  Import and pre-process Data
##############################


ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

LandUse2007 = read.csv("data/NLUD_ABCD_2007_LDN.csv", sep=",", dec=".")
LandUse2007$PREVIOUS_USE = LandUse2007$PREVIOUS_LAND_USE

LandUse2010 = read.csv("data/NLUD_ABCD_2010.csv", sep=",", dec=".", stringsAsFactors = F)
LandUse2010 = LandUse2010[LandUse2010$REGION == "London", ]
LandUse2010$PREVIOUS_LAND_USE = LandUse2010$PREVIOUS_USE
LandUse2010$CURRENT_LAND_USE = LandUse2010$CURRENT_USE

mapLandUse = function(nlud, year, WhatToMap){
  nlud@data$varToMap = nlud@data[, WhatToMap]
  vals = unique(nlud@data$varToMap)
  n = length(vals)
  lookup = data.frame(plu = vals, plu_id = 1:n)
  nlud@data = data.frame(nlud@data, lookup[match(nlud@data$varToMap, lookup$plu),])
  factpal <- colorFactor(rainbow(n), nlud@data$plu_id)
  vLegendBox <- as.character(vals)
  leaflet(data = nlud) %>% addProviderTiles("CartoDB.Positron") %>%
    clearShapes() %>% 
    setView(lng=-0.1855676, lat=51.5371635, zoom=10) %>% 
    addCircleMarkers(~long, ~lat, radius = ~AREA, col=~factpal(plu_id) , 
                     popup = ~paste(PAO_DESCRIPTION, " | Previously: ",PREVIOUS_LAND_USE,
                                    " | In ", year, ": ",CURRENT_LAND_USE, " | Proposed: ",PROPOSED_USE, sep=" "))
}

Transition = function(nlud, table, FromUse = "PREVIOUS_USE" , ToUse = "PROPOSED_USE"){
  transitions_m = table(nlud@data[,FromUse], nlud@data[,ToUse])
  transitions_m = transitions_m[rownames(transitions_m)!="", colnames(transitions_m)!=""]
  if (table == "N") { 
    return(transitions_m)
    }
  if (table == "LinePct") { 
    pctFrom = transitions_m
  for (i in 1:dim(transitions_m)[1]){
    pctFrom[i,] = round(transitions_m[i,] * 100/ rowSums(transitions_m)[i],1)
  }
  return(pctFrom)
  }
  if (table == "ColPct") { 
    pctTo = transitions_m
  for (j in 1:dim(transitions_m)[2]){
    pctTo[,j] = round(transitions_m[,j] * 100/ colSums(transitions_m)[j],1)
  }
  return(pctTo)
  }
}
  
##############################
######  Outputs functions
##############################

shinyServer(function(input, output, session) {
  
  
  
  ##############################
  ######  Reactive functions
  ##############################
  
  
  nlud_year <- reactive({
    if (input$year == 2007) nlud = LandUse2007
    if (input$year == 2010) nlud = LandUse2010
    nlud = subset(nlud, EASTING != "" | NORTHING != "" | !is.na(EASTING) | !is.na(NORTHING))
    nlud$nlud_ID <- 1:nrow(nlud)
    coords <- as.data.frame(cbind( Easting = nlud$EASTING,Northing = nlud$NORTHING))
    nlud_SP <-SpatialPointsDataFrame(coords, data = data.frame(nlud), proj4string =CRS(ukgrid))
    nlud_SP_LL <- spTransform(nlud_SP,CRS(latlong))
    colnames (nlud_SP_LL@coords)[colnames(nlud_SP_LL@coords) == "Easting"] <- "Longitude"
    colnames(nlud_SP_LL@coords)[colnames (nlud_SP_LL@coords) == "Northing"] <- "Latitude"
    nlud_SP_LL@data$long = nlud_SP_LL@coords[,1]
    nlud_SP_LL@data$lat = nlud_SP_LL@coords[,2]
    return(nlud_SP_LL)
      })
    
  
  ##############################
  ######  Outputs 
  ##############################
  
  output$map = renderLeaflet({
    nlud_table = nlud_year()
    selected_year = input$year
    selected_variable = input$variable
    mapLandUse(nlud = nlud_table, year = selected_year, WhatToMap = selected_variable)
  })
  
  
  output$transitions = renderDataTable({
    nlud_table = nlud_year()
    selected_table = input$table
    t = Transition(nlud = nlud_table, table = selected_table)
    t = cbind(rownames(t), t)
    return(t)
  }, options = list(paging = FALSE, searching = FALSE))
  
  
  
})
    