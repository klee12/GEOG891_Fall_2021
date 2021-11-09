#lab 4
#Name: Kun-Yuan Lee
#Due 11/11/21

library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(GISTools)

#Group 1 ;---------------------------------------------------------------------

countyboundries <-
  sf::read_sf("../homework4/County_Boundaries-_Census.shp")
NE.counties <- read.csv(file = '../homework4/ne_counties.csv')
Populationdensity <-  NE.counties %>% mutate(Popden = Total / ALAND10)
join_ne <-
  merge(countyboundries,
        Populationdensity,
        by.x = "NAME10",
        by.y = "NAME10")
Group1.map <- tm_shape(join_ne) +
  tm_fill(col = "Popden", alpha = 0.9, style = "quantile") +
  tm_borders(col = "brown", lwd = 1.5, lty = "solid") +
  tm_layout(legend.show = FALSE) +
  tm_scale_bar(
    breaks = c(0, 50, 100),
    text.size = 0.5,
    position = c("left", "bottom"),
    bg.color = "grey"
  )
#Group 2 :---------------------------------------------------------------------
CountyData <- "../homework4/lancaster_county.shp"
MuncData <- "../homework4/Municipal_Boundaries.shp"
StatePLoc <- "../homework4/State_Park_Locations.shp"
StreamData <- "../homework4/Streams_303_d_.shp"
# Read Shapefile data
RCountyData <- sf::read_sf(CountyData)
RMuncData <- sf::read_sf(MuncData) %>% sf::st_make_valid()
RStatePLoc <- sf::read_sf(StatePLoc)
RStreamData <- sf::read_sf(StreamData)
#intersect the stream and countydata
MunciLan <- st_intersection(RMuncData, RCountyData)
Seg.RStreamData <- st_intersection(RStreamData, RCountyData)

#Group 3:----------------------------------------------------------------------
ne.state <- read.csv("../homework4/ne_counties.csv")
ne.counties <-
  sf::read_sf("../homework4/County_Boundaries-_Census.shp") %>%
  sf::st_make_valid()
Lancaster <-
  ne.counties[ne.counties$NAMELSAD10 == "Lancaster County",]
streams <- sf::read_sf("../homework4/Streams_303_d_.shp") %>%
  sf::st_make_valid()
parks <- sf::read_sf("../homework4/State_Park_Locations.shp") %>%
  sf::st_make_valid()
municipal <-
  sf::read_sf("../homework4/Municipal_Boundaries.shp") %>%
  sf::st_make_valid()
Dem <- raster("../homework4/lc_dem.tif")

# Developed the inset Map
Lancaster.region = st_bbox(c(
  xmin = -96.91394,
  xmax = -96.46363,
  ymin = 40.52302,
  ymax = 41.04612
),
crs = st_crs(ne.counties)) %>% st_as_sfc()
Inset.map <-
  tm_shape(Lancaster.region) + tm_borders(lwd = 5, col = "red")
# make sure crs are the same
raster::crs(Dem) <- crs(Lancaster)
library(grid)

#plot Map (Pretty)
PrettyMap <-  tm_shape(MunciLan) + tm_borders() +
  tm_shape(Dem, raster.downsample = FALSE) +
  tm_raster(alpha = 0.7,
            palette = colorRampPalette(c("darkolivegreen4", "yellow", "brown"))(12),
            legend.show = F) +
  tm_compass(
    type = "8star",
    position = c("right", "bottom"),
    size = 5
  ) +
  tm_shape(MunciLan) + tm_polygons(alpha = 0.5) + tm_text("NAME", size = 0.7, col = "grey39") +
  tm_shape(RStatePLoc) + tm_dots(col = "red", size = 1.1, shape = 20) +
  tm_shape(Seg.RStreamData) + tm_lines(col = "Impairment", lwd = 1.5) +
  tm_scale_bar(
    breaks = c(0, 5, 10),
    text.size = 0.5,
    position = c("center", "bottom"),
    bg.color = "grey"
  ) +
  tm_layout(
    legend.show = FALSE,
    title = expression (italic("Lancaster cartographic map")),
    title.position = c("center", "top"),
    title.color = "black",
    title.bg.color = "white",
    title.bg.alpha = 0.8,
    fontface = "plain",
    fontfamily = "sans"
  )

#Plot the Map !!!
PrettyMap
print(Group1.map + Inset.map, vp = viewport(0.85, 0.137, width = 0.3, height = 0.3))


#2. -------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(data.table)
library(rgdal)


twnR <-
  readOGR(dsn = "../homework4",
          layer = "RIVERL",
          encoding = "UTF-8")
taiwan_shp <-
  readOGR(dsn = "../homework4",
          layer = "gadm36_TWN_2",
          encoding = "UTF-8")
DemTW <- raster("../homework4/dem_20m.tif")
tm_shape(DemTW) + tm_raster() +
  tm_shape(taiwan_shp) + tm_polygons(col = "MAP_COLORS", alpha = 0.3) +
  tm_shape(twnR) + tm_lines(col = "blue", lwd = 0.5) +
  tm_layout(
    legend.show = FALSE,
    title = expression (italic("Taiwan topographic map")),
    title.position = c("left", "top"),
    title.color = "white",
    title.bg.color = "black",
    title.bg.alpha = 0.8,
    fontface = "plain",
    fontfamily = "sans"
  ) + tm_compass(type = "8star",
                 position = c("left", "bottom"),
                 size = 5) + tm_scale_bar(
                   breaks = c(0, 30, 60),
                   text.size = 0.6,
                   position = c("right", "bottom"),
                   bg.color = "grey"
                 )


