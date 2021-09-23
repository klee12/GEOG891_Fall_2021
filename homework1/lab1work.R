#lab 1.1
library(tidyverse)
library(ggplot2)
library(sf)
library(sp)
p.counties <- "../homework1/data/CBW/County_Boundaries.shp"
p.stations <-"../homework1/data/CBW/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)


#1.1
d.counties %>% as_tibble() %>% dplyr::select(NAME10,ALAND10,AWATER10) %>% mutate(landAreaPrec = ALAND10/sum(ALAND10+AWATER10)*100)%>%
  dplyr::select(NAME10,landAreaPrec) 




#1.2
d.counties %>% as_tibble() %>% dplyr::select(-geometry) %>%
  group_by(STATEFP10) %>% select(NAME10,ALAND10,AWATER10) %>% 
  mutate(LargPLandArea=max(AWATER10/sum(ALAND10+AWATER10))) %>% 
  slice(which.max(LargPLandArea)) %>% dplyr::select(STATEFP10,NAME10,LargPLandArea) 
  

#1.3

d.counties %>% as_tibble() %>%count(STATEFP10,sort = TRUE)

#1.4

which(nchar(d.stations$STATION_NA)==min(nchar(d.stations$STATION_NA))) %>%d.stations[.,]%>% dplyr::select(STATION_NA)

#2.1 
d.counties %>% ggplot(., aes(x= ALAND10,y=AWATER10)) +
  geom_point(aes(col=STATEFP10))+
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


#3


InputVal <- function(x){
  
   if (is.numeric(x) == TRUE){
   calStat <- list(mean(x), median(x),max(x),min(x))
   SortVec <- sort(x)
   ans <- list(calStat,SortVec)
   return(ans)  # It only can be return a single value
   } else {"Error... this input is not numeric"}
}

#4.1


d.counties %>% sf::st_crs()
d.stations %>% sf::st_crs()                                                             
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()
#---------------------------------------------------
d.countiesdata <- st_make_valid(d.counties)
d.stationsdata <- d.stations 
d.stations_all <- sf::st_intersection(d.stationsdata,d.countiesdata)
d.stations_all %>% as_tibble() %>% count(STATEFP10)
#-------------------------------------------------


#4.2

NYCountyArea <- d.counties %>% dplyr::filter(STATEFP10 == 36 ) %>% st_area()
mean(NYCountyArea)


#4.3

maxdra <- d.stations_all %>% as_tibble() %>% group_by(STATEFP10)%>%summarize(result = mean(Drainage_A))
which((maxdra$result)==max(maxdra$result)) %>% d.stations_all[.,] %>% dplyr::select(STATEFP10, STATION_NA)




# Testing the Q&A 

d.stations_all1 <- sf::st_intersection(d.stationsdata,d.countiesdata)
d.stations_all2 <- sf::st_intersection(d.countiesdata,d.stationsdata)
