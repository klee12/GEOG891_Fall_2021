#Final Project  Kun-Yuan Lee Fall 2021 
rm(list = ls())
install.packages("ggridges")
library(ggridges)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(viridis)     ## color palette
library(ggplot2)
library(dplyr)
library(ggpubr)
library(plotrix)
#Reading all the COVID Case 
COVID_ND<- read_csv("../Project/Cass_COVID2.csv")
COVID_NE<- read_csv("../Project/Lancaster_COVID2.csv")
COVID_TX<- read_csv("../Project/Nuece_COVID2.csv")

#Reading all the weather information data 
WeatherInf_ND<- read_csv("../Project/history_data_ND1.csv")
WeatherInf_NE<- read_csv("../Project/history_data_NE.csv")
WeatherInf_TX<- read_csv("../Project/history_data_TX.csv")


#Perform Join function 
Cass_ND <- merge(COVID_ND,WeatherInf_ND, by = "datetime", all.x = FALSE)
Lancaster_NE <- merge(COVID_NE,WeatherInf_NE,by = "Datetime",all.x = FALSE)
Nuece_TX <- merge(COVID_TX,WeatherInf_TX,by = "datetime", all.x = FALSE)

#==================================================================================
#pie chart for Weather Condtions vs COVID-19 cases 
Cass_ND %>%  group_by(Conditions) %>% dplyr::summarise(mean = mean(Temperature))
stat<-Cass_ND %>%  group_by(Conditions) %>% dplyr::summarise(case= mean(casecount))

lbls <-stat$Conditions
pie(stat$case, labels = lbls)
pct <- round(stat$case/sum(stat$case)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie3D(stat$case,labels=lbls,col=rainbow(length(lbls)),
      main="COVID-19 case number and weather condition in Fargo, ND ")

#==================================================================================
#pie chart for Weather Condtions vs COVID-19 cases
Lancaster_NE %>%  group_by(Conditions) %>% dplyr::summarise(mean = mean(Temperature))
stat<-Lancaster_NE %>%  group_by(Conditions) %>% dplyr::summarise(case= mean(casecount))

lbls <-stat$Conditions
pie(stat$case, labels = lbls)
pct <- round(stat$case/sum(stat$case)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie3D(stat$case,labels=lbls,col=rainbow(length(lbls)),
      main="COVID-19 case number and weather condition in Lincoln, NE ")

#==================================================================================
#pie chart for Weather Condtions vs COVID-19 cases
Nuece_TX %>%  group_by(Conditions) %>% dplyr::summarise(mean = mean(Temperature))
stat<-Nuece_TX %>%  group_by(Conditions) %>% dplyr::summarise(case= mean(casecount))

lbls <-stat$Conditions
pie(stat$case, labels = lbls)
pct <- round(stat$case/sum(stat$case)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie3D(stat$case,labels=lbls,col=rainbow(length(lbls)),
      main="COVID-19 case number and weather condition in Cropus Christi,TX ")


#===============================================
#Temperature plots
months <- c("October","September","August","July","June","May")


Cass_ND$months <- as.Date(Cass_ND$datetime, "%m/%d/%Y") %>%
  months() %>%as.factor() %>% factor(., levels = months)

mins <- min(Cass_ND$`Minimum Temperature`)
maxs <- max(Cass_ND$`Maximum Temperature`)


ggplot(Cass_ND,aes(x = Temperature,y=months,height=..density.., fill = ..x..))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Temp. [oF]", option = "C") +
  labs(title = 'Temperatures in North Dakota (Fargo)',
       subtitle = 'Mean temperatures (Fahrenheit) by month for 2020\nData: Original CSV from the NOAA/NWS.', 
       x = "Mean Temperature [oF]") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

#Daily Count numbercase vs month (overall) 

ggplot(Cass_ND,aes(x = casecount,y=months,height=..density.., fill = ..x..))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Case #", option = "C") +
  labs(title = 'COVID-19 Case count in North Dakota (Fargo)',
       subtitle = 'COVID Casecount by month for 2020\nData: Original CSV from the NYTimes.', 
       x = "COVID Case # [Not cummlative]") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

#Temperature vs. Daily COVID Rate

pND<-ggplot(data = Cass_ND, mapping = aes(x = casecount, y = Temperature))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title = 'Degree Temperature VS COVID Case  in North Dakota (Fargo)',
       subtitle = 'Mean temperatures (Fahrenheit) Between May 1, 2020  and October 31, 2020\nData: Original CSV from the NOAA/NWS. The COVID-19 data from the NYTimes', 
       y = "[oF]", x = "COVID Case # [Not cummlative]") +
  theme_ridges(font_size = 13, grid = TRUE) 
#=================================================================================

pND+ stat_cor(method = "pearson", p.accuracy = 0.001,label.x.npc = "middle")


#pND+ stat_cor(aes(color = months, label.x = '3'))
#Test1 <- ggscatter(Cass_ND, x = "casecount", y = "Temperature",
 #           color = "months", palette = "jco",
#        add = "reg.line", conf.int = TRUE)
#Test1+stat_cor(aes(color = months),  p.accuracy = 0.0001,label.x.npc = "middle")
#Test1
#ND_data <- Cass_ND %>% as_tibble() %>%  mutate(months = as.Date(datetime,"%m/%d/%Y")) 

#==========================================

#case count and windchill 
plot1<-ggplot(data = Cass_ND, mapping = aes(x = casecount, y = `Wind Chill`))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title = 'Windchill Temperature VS COVID Case  in North Dakota (Fargo)',
       subtitle = 'Windchill temperatures (Fahrenheit) Between May 1, 2020  and October 31, 2020\nData: Original CSV from the NOAA/NWS', 
       y = "[oF]", x = "COVID Case # [Not cummlative ]") +
  theme_ridges(font_size = 13, grid = TRUE) 


plot1+ stat_cor(method = "pearson", p.accuracy = 0.001,label.x.npc = "middle")


#===============================================================================================NE

Lancaster_NE$months <- as.Date(Lancaster_NE$Datetime, "%m/%d/%Y") %>%
  months() %>%as.factor() %>% factor(., levels = months)
mins <- min(Lancaster_NE$`Minimum Temperature`)
maxs <- max(Lancaster_NE$`Maximum Temperature`)


ggplot(Lancaster_NE,aes(x = Temperature,y=months,height=..density.., fill = ..x..))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Temp. [oF]", option = "C") +
  labs(title = 'Temperatures in Nebraska (Lincoln)',
       subtitle = 'Mean temperatures (Fahrenheit) by month for 2020\nData: Original CSV from the NOAA/NWS.', 
       x = "Mean Temperature [oF]") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

#Daily Count numbercase vs month (overall) 

ggplot(Lancaster_NE,aes(x = casecount,y=months,height=..density.., fill = ..x..))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Case #", option = "C") +
  labs(title = 'COVID-19 case count in Nebraska (Lincoln)',
       subtitle = 'COVID Casecount by month for 2020\nData: Original CSV from the NYTimes.', 
       x = "COVID Case # [Not cummlative]") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

#Temperature vs. Daily COVID Rate

pND<-ggplot(data = Lancaster_NE, mapping = aes(x = casecount, y = Temperature))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title = 'Degree Temperature VS COVID case in Nebraska (Lincoln)',
       subtitle = 'Mean temperatures (Fahrenheit) Between May 1, 2020  and October 31, 2020\nData: Original CSV from the NOAA/NWS. The COVID-19 data from the NYTimes', 
       y = "[oF]",x = "COVID Case # [Not cummlative]") +
  theme_ridges(font_size = 13, grid = TRUE) 
#=================================================================================

pND+ stat_cor(method = "pearson", p.accuracy = 0.001,label.x.npc = "middle")


ggscatter(Nuece_TX, x = "casecount", y = "Temperature",
          add = "reg.line",
          conf.int = TRUE,
          color = "months", palette = "jco", 
          rug = TRUE                                
)+  stat_cor(aes(color = months, label = ..r.label..), label.x.npc = "middle")           

#==========================================

#case count and windchill 
plot1<-ggplot(data = Lancaster_NE, mapping = aes(x = casecount, y = `Wind Chill`))+
  geom_point()+
geom_smooth(method = 'lm')+
  labs(title = 'Windchill Temperature VS COVID Case  in Nebraska (Lincoln)',
       subtitle = 'Windchill temperatures (Fahrenheit) Between May 1, 2020  and October 31, 2020\nData: Original CSV from the NOAA/NWS', 
       y = "[oF]", x = "COVID Case # [Not cummlative ]") +
  theme_ridges(font_size = 13, grid = TRUE) 


plot1+ stat_cor(method = "pearson", p.accuracy = 0.001,label.x.npc = "middle")
plot1+ stat_cor(method = "pearson", p.accuracy = 0.001)

#another plot for heat Index (NE)
plot1<-ggplot(data = Lancaster_NE, mapping = aes(x = casecount, y = `Heat Index`))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title = 'Heat Index VS COVID Case in Nebraska (Lincoln)',
       subtitle = 'Heat Index (>=50F) (Fahrenheit) Between May 1, 2020  and October 31, 2020\nData: Original CSV from the NOAA/NWS', 
       y = "[oF]", x = "COVID Case # [Not cummlative ]") +
  theme_ridges(font_size = 13, grid = TRUE) 


plot1+ stat_cor(method = "pearson", p.accuracy = 0.001,label.x.npc = "middle")
plot1+ stat_cor(method = "pearson", p.accuracy = 0.001)



#===========================================================================================TX

Nuece_TX$months <- as.Date(Nuece_TX$datetime, "%m/%d/%Y") %>%
  months() %>%as.factor() %>% factor(., levels = months)
mins <- min(Nuece_TX$`Minimum Temperature`)
maxs <- max(Nuece_TX$`Maximum Temperature`)


ggplot(Nuece_TX,aes(x = Temperature,y=months,height=..density.., fill = ..x..))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Temp. [oF]", option = "C") +
  labs(title = 'Temperatures in Texas (Corpus Christi)',
       subtitle = 'Mean temperatures (Fahrenheit) by month for 2020\nData: Original CSV from the NOAA/NWS.', 
       x = "Mean Temperature [oF]") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

#Daily Count numbercase vs month (overall) 

ggplot(Nuece_TX,aes(x = casecount,y=months,height=..density.., fill = ..x..))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Case #", option = "C") +
  labs(title = 'COVID-19 case count in Texas (Corpus Christi)',
       subtitle = 'COVID Casecount by month for 2020\nData: Original CSV from the NYTimes.', 
       x = "COVID Case # [Not cummlative]") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

#Temperature vs. Daily COVID Rate

pND<-ggplot(data = Nuece_TX, mapping = aes(x = casecount, y = Temperature))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title = 'Degree Temperature VS COVID Case in Texas (Corpus Christi)',
       subtitle = 'Mean temperatures (Fahrenheit) Between May 1, 2020 and October 31, 2020\nData: Original CSV from the NOAA/NWS. The COVID-19 data from the NYTimes', 
       y = "[oF]",x = "COVID Case # [Not cummlative]") +
  theme_ridges(font_size = 13, grid = TRUE) 
#=================================================================================

pND+ stat_cor(method = "pearson", p.accuracy = 0.001,label.x.npc = "middle")

#==========================================

#case count and windchill 
plot1<-ggplot(data = Nuece_TX, mapping = aes(x = casecount, y = `Heat Index`))+
  geom_point()
plot1+ stat_cor(method = "pearson", p.accuracy = 0.001)


pTX<-ggplot(data = Nuece_TX, mapping = aes(x = casecount, y = `Heat Index` ))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(title = 'Heat Index VS COVID Case  in Texas  (Corpus Christi)',
       subtitle = 'Heat Index (>=50F) Between May 1, 2021  and October 31, 2021\nData: Original CSV from the NOAA/NWS.', 
       y = "[oF]",x = "COVID Case # [Not cummlative]")+
  theme_ridges(font_size = 13, grid = TRUE) 


pTX+stat_cor(method = "pearson", p.accuracy = 0.001,label.x.npc = "middle")

#=======================================================extra

Lancaster_NE %>% as_tibble() %>% mutate(week = cut.Date(Datetime, breaks = "1 week", labels = FALSE)) %>% 
  arrange(Datetime)
t<-Lancaster_NE %>% as_tibble() %>% mutate(Date = as.Date(Datetime,"%m/%d/%Y")) #convert to Datetime
t1<-t%>% as_tibble() %>% mutate(week = cut.Date(t$Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(t)





#================================================================================Timeline=======
#Timeseris with leaflet interaction 
library(tidyverse)
library(shiny)
library(leaflet)
library(RColorBrewer)
rm(list = ls())

data <-read_csv("../Project/mainTable.csv")
data1 <-as.data.frame(data,stringsAsFactors = FALSE)
reds <- colorNumeric("Reds", domain = NULL)

# specify shiny user interface and time slider
ui2 <- bootstrapPage(tags$style(type = "text/css", "html, body, .leaflet {width:100%; height:100%}"),
                    leafletOutput("map", width = "100%", height = "100%"),
                    # position and properties of the time slider
                    absolutePanel(bottom = 10, right = 300, draggable = TRUE,
                                  # slider title, step increments, and ticks
                                  sliderInput("integer", "Days since first reported case as of January 21, 2020:",ticks = FALSE, min = min(data1$day), max = max(data1$day), value = range(data1$day), step = 1,
                                              animate = animationOptions(interval = 1000, loop = TRUE))))


# shiny server input/output
server5 <- function(input, output, session) {
  filteredData <- reactive({
    data1 %>%
      filter(day >= input$integer[1] & day <= input$integer[2])
  })
  output$map <- renderLeaflet({
    reds <- colorNumeric("Reds", domain = NULL)
    leaflet(data1) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      # set boundaries for map
      fitBounds(lng1 = min(data1$long), 
                lat1 = min(data1$lat), 
                lng2 = max(data1$long), 
                lat2 = max(data1$lat)) %>%
      # add legend for the map
      addLegend("bottomleft", pal = reds, values = ~casecount,
                title = "Confirmed COVID Cases <br> (data: NYTimes)",
                opacity = 0.5,
                bins = 5)
  })
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~long,
                       lat = ~lat,
                       radius = ~log(casecount) * 3.5,
                       weight = 1,
                       opacity = 10,
                       color = ~ifelse(casecount > 0, reds(casecount), NA),
                       popup = ~paste0(county, ",  ", casecount, " cases"))
  }) 
}



shinyApp(ui2, server5,options = list(height = 550))





