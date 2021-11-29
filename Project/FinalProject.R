#Final Project 
library(tidyverse)
library(ggplot2)
library(dplyr)


#Reading all the COVID Case 
COVID_ND<- read_csv("../Project/Cass_COVID.csv")
COVID_NE<- read_csv("../Project/Lancaster_COVIDf.csv")
COVID_TX<- read_csv("../Project/Nuece_COVID.csv")

#Reading all the weather information data 
WeatherInf_ND<- read_csv("../Project/history_data_ND.csv")
WeatherInf_NE<- read_csv("../Project/history_data_NE.csv")
WeatherInf_TX<- read_csv("../Project/history_data_TX.csv")


#Perform Join function 
Cass <- merge(COVID_NE,WeatherInf_NE, by = "date", all.x = FALSE)
Lancaster <- merge(COVID_NE,WeatherInf_NE,by = "date",all.x = FALSE)
Crups <-

