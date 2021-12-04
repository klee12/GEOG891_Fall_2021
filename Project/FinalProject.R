#Final Project 
rm(list = ls())

library(tidyverse)
library(ggplot2)
library(dplyr)


#Reading all the COVID Case 
COVID_ND<- read_csv("../Project/Cass_COVID_1.csv")
COVID_NE<- read_csv("../Project/Lancaster_COVID.csv")
COVID_TX<- read_csv("../Project/Nuece_COVID.csv")

#Reading all the weather information data 
WeatherInf_ND<- read_csv("../Project/history_data_ND.csv")
WeatherInf_NE<- read_csv("../Project/history_data_NE.csv")
WeatherInf_TX<- read_csv("../Project/history_data_TX.csv")


#Perform Join function 
Cass_ND <- merge(COVID_ND,WeatherInf_ND, by = "datetime", all.x = FALSE)
Lancaster_NE <- merge(COVID_NE,WeatherInf_NE,by = "Datetime",all.x = FALSE)
Nuece_TX <- merge(COVID_TX,WeatherInf_TX,by = "datetime", all.x = FALSE)


Lancaster %>%  group_by(Conditions) %>% dplyr::summarise(mean = mean(Temperature))



Lancaster %>% as_tibble() %>% mutate(week = cut.Date(Datetime, breaks = "1 week", labels = FALSE)) %>% 
  arrange(Datetime)

t<-Lancaster %>% as_tibble() %>% mutate(Date = as.Date(Datetime,"%m/%d/%Y")) #convert to Datetime

t1<-t%>% as_tibble() %>% mutate(week = cut.Date(t$Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(t)
