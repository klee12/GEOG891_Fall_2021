#lab 1.1
library(tidyverse)
library(ggplot2)
library(sf)
library(sp)
p.counties <- "../homework1/data/CBW/County_Boundaries.shp"
p.stations <-"../homework1/data/CBW/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)
glimpse(d.counties)
glimpse(d.stations)

d.counties %>% dplyr::select(NAME10,ALAND10,AWATER10) %>% head()
#1.1
d.counties %>% select(NAME10,ALAND10,AWATER10) %>% mutate(landAreaPrec = ALAND10/(ALAND10+AWATER10))
#1.2
d.counties %>% as_tibble() %>% dplyr::select(-geometry) %>%
  group_by(STATEFP10) %>% select(NAME10,ALAND10,AWATER10) %>% 
  mutate(LargPLandArea=max(AWATER10/(ALAND10+AWATER10)))

#1.3
d.counties %>% as_tibble() %>% dplyr::select(-geometry) %>%
  group_by(STATEFP10) %>% count(NAME10,STATEFP10)

d.counties %>% as_tibble() %>% dplyr::select(-geometry) %>% count(STATEFP10,sort = TRUE)

#1.4
d.stations %>% as_tibble() %>% dplyr::select(-geometry) %>% length(STATION_NA)
#min(nchar(d.stations$STATION_NA))

#min(d.stations$STATION_NA)
#max(d.stations$STATION_NA)
#sort(nchar(d.stations$STATION_NA))

which(nchar(d.stations$STATION_NA)==min(nchar(d.stations$STATION_NA))) %>%d.stations[.,]%>% dplyr::select(STATION_NA)

#2.1 
d.counties %>% ggplot(., aes(x= ALAND10,y=AWATER10)) +
  geom_point(aes(col=COUNTYFP10))+
  labs(title = "Land area and Water area for each County")+
  xlab("Land Area") + 
  ylab("Water Area")
  
#2.2

d.stations %>%  ggplot(., aes (x = Drainage_A)) + 
  geom_histogram(color="black", fill="lightblue", linetype="dashed") +
  labs(title = "The histogram of drainage area for all monitoring stations")+
  xlab("Monitoring stations") + 
  ylab("Counts")

#2.3

d.stations %>%  ggplot(., aes (x = Drainage_A)) + 
  geom_histogram(aes(fill = MAJOR_WATE )) +
  labs(title = "The histogram of drainage area for all monitoring stations (with color using the state variable)")+
  xlab("Monitoring stations") + 
  ylab("Counts")


#3.1A
MeanVal <- function(x){
  return (mean(x))
}

MedianVal<- function(x){
  return (median(x))
}

MaxVal<- function(x){
  return (max(x))
}

MinVal<- function(x){
  return (min(x))
}


#3.1B

SortVec <- function(x){
  return (sort(x))
}

#3.1C

SortA_list_B_sort <- function(a,b){
  list_a <- as.list(a)
  return(c(list_a, sort(b))) 
}

#3.1D
testNumVal <- function(a){
    ifelse(is.numeric(a),"True", "Error... this input is not numeric")
  }

a <- c(1,0,-1)
b <- c(10,100,9,1000)
SortA_list_B_sort(a,b)
SortVec(b)
tst = c("a","b","c")


#4.1

#d.stations %>% group_by(STATION_NA) %>% summarise(USGS_STATI = n())
#d.stations %>% aggregate(d.stations$USGS_STATI, by = list(d.stations$STATION_NA),FUN = length)
#d.stations %>% count(USGS_STATI)
#d.stations %>% as_tibble() %>% dplyr::select(-geometry) %>% group_by(STATION_NA) %>% summarise(STATION_NA = n())
#d.stations %>% as_tibble() %>% dplyr::select(-geometry) %>% dplyr::select(start)
 
d.counties %>% sf::st_crs()
d.stations %>% sf::st_crs()                                                             
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()

del.counties_all <- d.counties 
del.stations_all <- sf::st_intersection(del.counties_all,d.stations)

del.counties51 <- d.counties %>% dplyr::filter(STATEFP10==51)
del.stations51 <- sf::st_intersection(d.stations, del.counties51)


del.counties42 <- d.counties %>% dplyr::filter(STATEFP10==42)
del.stations42 <- sf::st_intersection(d.stations, del.counties42)

del.counties24 <- d.counties %>% dplyr::filter(STATEFP10 == 24)
del.stations24 <- sf::st_intersection(d.stations, del.counties24)


del.counties36 <- d.counties %>% dplyr::filter(STATEFP10==36)
del.stations36 <- sf::st_intersection(d.stations, del.counties36)
del.stations36 %>% count(STATION_NA, sort = TRUE)

del.counties54 <- d.counties %>% dplyr::filter(STATEFP10 == 54)
del.stations54 <- sf::st_intersection(d.stations, del.counties54)
del.stations10 %>% count(STATION_NA, sort = TRUE)

del.counties10 <- d.counties %>% dplyr::filter(STATEFP10==10)
del.stations10 <- sf::st_intersection(d.stations, del.counties10)
del.stations10 %>% count(STATION_NA, sort = TRUE)

del.counties11 <- d.counties %>% dplyr::filter(STATEFP10==11)
del.stations11 <- sf::st_intersection(d.stations, del.counties11)
del.stations11 %>% as_tibble() %>% dplyr::select(-geometry) %>% count(STATION_NA, sort = TRUE)

#4.2

NYCountyArea <- del.counties36  %>% st_area()
mean(NYCountyArea)
