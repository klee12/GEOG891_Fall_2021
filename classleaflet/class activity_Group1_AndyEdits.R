library(tidyverse)
library(sf)
library(leaflet) # for interactive maps

countyboundries <- sf::read_sf("../classleaflet/County_Boundaries-_Census.shp") %>% sf::st_make_valid()

#check crs for linear units ie do we need to change from square meters to 
crs = st_crs(countyboundries)  
print(crs)

#calculate new fields
countyboundries$area = st_area(countyboundries) #get area of polygons
countyboundries <- mutate(countyboundries, Popdensity= as.numeric(Total/(area/2.59e+6))) #area/2.59e+6 = square miles,
                                                                              #gets rid of float and as numeric drops
                                                                              #area units from end

#find stats for Popdensity to create bins
summary(countyboundries$Popdensity)

#create bins and palette
bins <- c(0, 3.8762, 8.4108, 17.4659, Inf)
pal <- colorBin("YlOrRd", domain = countyboundries$Popdensity, bins = bins)

m <- leaflet(countyboundries) %>% 
  addProviderTiles(provider = providers$Esri.WorldStreetMap) %>% 
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
    label = paste0(countyboundries$NAME10, " County has the average population desnity of \n", countyboundries$Popdensity, " per sq miles")) %>% 
  addLegend(pal = pal, values = countyboundries$Popdensity, opacity = 0.7, title = "Population density (sq/miles)",
            position = "bottomleft") %>% 
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE) %>% addScaleBar()

m
