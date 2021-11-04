#Lab 3
#Name: Kun-Yuan Lee

library(spdep)
library(sf)
library(tidyverse)
library(tmap)
library(rgeoda)
d.all <-  sf::read_sf("../homework3/data/areaSelect.shp")

#1 I choose Nebraska, Kansas, South Dakota, Iowa

CanArea2 <-
  d.all %>% dplyr::filter(
    stringr::str_starts(GEOID10, "31") |
      stringr::str_starts(GEOID10, "20") |
      stringr::str_starts(GEOID10, "19") |
      stringr::str_starts(GEOID10, "29")
  )
#tmap::tm_shape(CanArea2) + tm_polygons()
Area_Proj <- CanArea2 %>% sf::st_transform(., "ESRI:102010")
#tmap::tm_shape(Area_Proj) + tm_polygons()

#st_write(CanArea2,"../homework3/data/areaSelect.shp", "driver = ESRI Shapefile")
#tm_shape(CanArea2)+ tm_polygons()
#2 choice the variables
Var1nor <-
  Area_Proj %>% dplyr::select(DP0010004, DP0010005, DP0010001) %>%  mutate(TeenGroupPop = ((DP0010004 + DP0010005) / DP0010001 *
                                                                                             100))
#3 make histogram 
hist(Var1nor$TeenGroupPop)

#4 Make a choropleth map 
# make some bbox magic (to enlarge the box for legend)
bbox_new <- st_bbox(Var1nor) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.1*xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.1*yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box 
  st_as_sfc() # and make it a sf polygon


tm_shape(Var1nor, bbox = bbox_new) + tm_polygons("TeenGroupPop") +
  tm_layout(
    legend.position = c("right", "center"),
    title = ' Teenager % (10-19 years old ) over the total population',
    title.position = c('center', 'top')
  )

#5. Develop a contiguity-based spatial weight matrix (queen)

Area <- spdep::poly2nb(Area_Proj, queen = TRUE)
#5a Row-standardize the W
LineWeight <- nb2listw(Area, style = "W", zero.policy = TRUE)
#5b Plot a histogram of the number of neighbors
neig <- attr(LineWeight$weights, "comp")$d
hist(neig)
#5c Calculate the average number of neighbors
Avgnei <- neig %>% mean()
Avgnei

#5d Make a Moran Plot
moran.plot(Var1nor$TeenGroupPop,
           LineWeight,
           zero.policy = TRUE,
           plot = TRUE)

#6 Repeat #5 using IDW methods

#make it to point-based
IDWData <- Area_Proj %>% st_centroid()

#define radius 50 km 
IDWLW <- dnearneigh(IDWData, 0, 500000)

#6a Row-standardized the W

LineW <- nb2listwdist(IDWLW, IDWData, type = 'idw', style = "W")

#6b Plot histogram (**it may take a little while to load**)

neigIDW <- attr(LineW$weights, "comp")$d
hist(neigIDW)

#6c Calculate the average number of neighbors (**it may take a little while to load**)

AvgIDWnei <- neigIDW %>% mean()
AvgIDWnei

#6d make a moran plot
moran.plot(Var1nor$TeenGroupPop,
           LineW,
           zero.policy = TRUE,
           plot = TRUE)




