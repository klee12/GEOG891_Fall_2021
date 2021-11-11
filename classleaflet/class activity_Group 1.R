library(tidyverse)
library(sf)
library(leaflet) # for interactive maps

countyboundries <- sf::read_sf("../classleaflet/County_Boundaries-_Census.shp") %>% sf::st_make_valid() %>% 
  mutate(Popdensity= Total/(ALAND10 + AWATER10))

glimpse(countyboundries)

bins <- c(0, 0.00006,20 , 4.482e-06,5.976e-06 , 7.47e-06, Inf)
pal <- colorBin("YlOrRd", domain = countyboundries$Popdensity, bins = bins)

m <- leaflet(countyboundries) %>% 
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
  addPolygons(
    fillColor = ~pal(Popdensity),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7, highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE), 
    label = paste0(countyboundries$Popdensity, '\n', countyboundries$NAME10 )) %>% 
  addLegend(pal = pal, values = countyboundries$Popdensity, opacity = 0.7, title = NULL,
            position = "bottomright")


m %>% addTiles()%>% fitBounds(-100.76134, 40.675499, -100.0884, 40.33812) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>% addTerminator()


summary(countyboundries$Popdensity)