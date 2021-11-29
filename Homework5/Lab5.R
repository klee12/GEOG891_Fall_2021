#Lab 5
#Name: Kun-Yuan Lee

#Libraries for all three tasks

library(tidyverse)
library(sf)
library(tmap)
library(dplyr)
library(leaflet)
library(rgeoda)
library(spdep)
library(colorspace)
library(htmltools)
library(htmlwidgets)
library(leaflet.esri)
library(leaflet.extras)



#Task 1


counties <- sf::read_sf("../Homework5/data/CBW/County_Boundaries.shp") %>% sf::st_make_valid()
bmps <- read_csv("../Homework5/data/CBW/BMPreport2016_landbmps.csv")
BmpData<-bmps%>% mutate(., Name.trimmed = stringr::str_sub(GeographyName, 1, 5)) %>% 
  group_by(Name.trimmed) %>% summarise(Total_cost = sum(Cost, na.rm = T))
DataJoin <-left_join(counties,BmpData, by= c("GEOID10" = "Name.trimmed"))
summary(DataJoin$Total_cost)
bins<- c(181, 1567783, 3627300, 12193896, Inf)
pal <- colorBin("YlOrRd", domain = DataJoin$Total_cost, bins = bins)
Ans1 <- leaflet(DataJoin) %>% addProviderTiles(provider = providers$Esri.WorldStreetMap)%>% 
  addPolygons(
    fillColor = ~pal(Total_cost),
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
    label = paste0("Total Cost of BMPS in ", DataJoin$NAME10, " County: \n", DataJoin$Total_cost)) %>% 
  addLegend(pal = pal, values = DataJoin$Total_cost, opacity = 0.7, title = NULL,
            position = "bottomleft") %>% 
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE)

Ans1


#Task 2  
#Ref https://r-spatial.github.io/spdep/reference/localmoran.html


d.all <-  sf::read_sf("../Homework5/data/areaSelect.shp")
CanArea2 <-
  d.all %>% dplyr::filter(
    stringr::str_starts(GEOID10, "31") |
      stringr::str_starts(GEOID10, "20") |
      stringr::str_starts(GEOID10, "19") |
      stringr::str_starts(GEOID10, "29")
  )
Area_Proj <- CanArea2 %>% sf::st_transform(., "ESRI:102010")
Var1nor <-Area_Proj %>% dplyr::select(DP0010004, DP0010005, DP0010001) %>% 
  mutate(TeenGroupPop = ((DP0010004 + DP0010005) / DP0010001 *100))
Area <- spdep::poly2nb(Area_Proj, queen = TRUE)
LineWeight <- nb2listw(Area, style = "W", zero.policy = TRUE)
LiW1 <- localmoran(Var1nor$TeenGroupPop, LineWeight)[, 1]     #Ref
Liw2 <- localmoran(Var1nor$TeenGroupPop, LineWeight)[, 5]    
Fplot <- st_transform(Var1nor, crs = "EPSG:4326")   # Try this many time, you have to set EPSG 4326 in order to plot!
maxnum = 0.1 + max(LiW1)     #set scale value and the percentile
minnum = 0.1 + min(LiW1) 
dfference = maxnum - minnum
bins2 <-  c(minnum,maxnum - (dfference*(75/100)),maxnum - (dfference*(50/100)),maxnum - (dfference*(25/100)), maxnum)
pal2 <- colorBin("RdYlBu", domain = LiW1, bins = bins2)   #Ref
# plot the LISA using leaflet function 

plots <- leaflet(Fplot) %>% 
  addProviderTiles(providers$Stamen.Toner, group = "Black and White (Base map)") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Grey Map") %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "National Geography World Map") %>%
  addPolygons(fillColor = ~ pal2(LiW1), weight = 1, opacity = 1,  color = "white",fillOpacity = 0.9,
    label = paste0("P-value: ", Liw2)
  ) %>% addLegend(
    pal = pal2,
    values = ~LiW1,
    title = "Local Indicators of Spatial Assoication",
    position = "bottomleft"
  ) %>% addLayersControl(
    baseGroups = c("Black and White (Base map)","Map (Gray)","National Geography World Map"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE)

plots





#Task 3 

#load the live web API server

radar = "https://opendata.arcgis.com/datasets/85c59fe951504e9b9919e24d7a684084_3.geojson"
rlayer <- geojsonio::geojson_read("https://opendata.arcgis.com/datasets/85c59fe951504e9b9919e24d7a684084_3.geojson", what = "sp")
sat <- "https://nowcoast.noaa.gov/arcgis/rest/services/nowcoast/sat_meteo_imagery_time/MapServer"
sst <-"https://nowcoast.noaa.gov/arcgis/rest/services/nowcoast/analysis_ocean_sfc_sst_time/MapServer"
covlayer <- "https://nowcoast.noaa.gov/arcgis/rest/services/nowcoast/guidance_natlcenters_meteoceanhydro_outlooks_time/MapServer"
sfc6 <-"https://nowcoast.noaa.gov/arcgis/rest/services/nowcoast/forecast_meteoceanhydro_sfc_ndfd_qpf6hrs_offsets/MapServer"
sfctemp <- "https://nowcoast.noaa.gov/arcgis/rest/services/nowcoast/obs_meteocean_insitu_sfc_time/MapServer"
dailym <- "https://nowcoast.noaa.gov/arcgis/rest/services/nowcoast/forecast_meteoceanhydro_sfc_ndfd_dailymaxairtemp_offsets/MapServer"

# plot and add leaflet dynamic Map layers

leaflet() %>%
  setView(lng = -96.7026, lat = 40.8136, zoom = 5) %>%
  addTiles(group = "baselayer") %>%addEsriDynamicMapLayer(url = sst, group = "SST")%>%
  addWMSTiles("http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
              layers = "nexrad-n0r-900913",
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              group = "Radar" ) %>%
  
  addEsriDynamicMapLayer(url = radar, group = "radar Location") %>%
  addCircles(data = rlayer,color = "red",label = rlayer$siteName, popup =rlayer$siteID ,
             radius = 5000,stroke = FALSE,fillOpacity = 0.9,group = "88-D Radar Station") %>% 
  
  addEsriDynamicMapLayer(url = sfc6, group = "6-hour Quantitative Precipitation Amount") %>%
  addEsriDynamicMapLayer(url = sfctemp, group = "surface observation") %>%
  addEsriDynamicMapLayer(url = covlayer, group = "Convective Layer") %>%
  addEsriDynamicMapLayer(url = sat, group = "Visiable satellite") %>%
  addLayersControl(baseGroups = "World Map",
                   overlayGroups = c("Radar", "SST","Visiable satellite","88-D Radar Station" ,"Convective Layer", "surface observation", "6-hour Quantitative Precipitation Amount"),
                   options = layersControlOptions(collapsed = TRUE)
  )%>% addTerminator()%>%addMiniMap(toggleDisplay = TRUE)%>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>% addScaleBar()

################################################################################

#1. Reflect on the labs from this semester. What did you learn? What did you like? What did you not like? 
#   I learn that R can be one of the useful visualization tools added to my programming skills. 
# Since I took the python class last semester, I learn that R can be applied using a similar function but the visual product can be different (some are better, some are okay). 
# I like the instructor teaching us many useful programming applications toward the spatial & analytical method, especially the step-by-step process were very intuitive and easy to follow. 
# I really like the group project and each of the class members can take a role and solve the problem. 
# The students were all very eager to learn the applications, and the environment was active and fun. I recommend the class can be structured for a longer period. 
# For example, change to the Tuesday and Thursday class, and each class is allowed up to 1 hour and 15 minutes, so the student can have more time to discuss the problem and have time present during the same period. 

#2. Describe the ¡§one thing¡¨ you chose to add to your map in Task 3 above. What did you do, and why is it applicable to your map.
#   I choose a different map from the previous homework 3. However, my goal is to let meteorologists be able to switch different map layers with only important meteorological information added on.
# (*Note: if you see the dark map, it is because the visible satellite appeared dark in the nighttime).
# In this map, I included the measurement tool (bottom left), the tool can measure length as well as the area in m2 if it made the polygon. 
# I reviewed many meteorological maps, and many of the maps do not have the measurement tools, and in many real-case, we need the measurement tool to know the storm size and its effective area. 
# Therefore, I think it is important to include it, so the people reading this map are not only able to see the storm, but also know its size.  




