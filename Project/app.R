#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(tidyverse)
library(shiny)
library(leaflet)
library(RColorBrewer)


data <-read_csv("../Project/mainTable1.csv")
data1 <-as.data.frame(data,stringsAsFactors = FALSE)
reds <- colorNumeric("Reds", domain = NULL)
blues <- colorNumeric("blue", domain = NULL)
palc <- colorNumeric(
    palette = "RdYlBu",
    domain = data1$Temperature
)

# specify shiny user interface and time slider
ui2 <- bootstrapPage(tags$style(type = "text/css", "html, body, .leaflet {width:100%; height:100%}"),
                     leafletOutput("map", width = "100%", height = "100%"),
                     # position and properties of the time slider
                     absolutePanel(bottom = 10, right = 300, draggable = TRUE,
                                   # slider title, step increments, and ticks
                                   sliderInput("integer", "Days since May 01, 2021:",ticks = FALSE, min = min(data1$day), max = max(data1$day), value = range(data1$day), step = 1,
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
            addProviderTiles(providers$OpenStreetMap) %>%
            # set boundaries for map
            fitBounds(lng1 = min(data1$long), 
                      lat1 = min(data1$lat), 
                      lng2 = max(data1$long), 
                      lat2 = max(data1$lat)) %>%
            # add legend for the map
            addLegend("bottomleft", pal = reds, values = ~casecount,
                      title = "Confirmed COVID Cases <br> (data: NYTimes)",
                      opacity = 1,
                      bins = 5) %>%
            addLegend("bottomleft", pal = palc, values = ~Temperature,
                      labFormat = labelFormat(transform = function(Temperature) sort(Temperature, decreasing = TRUE)),
                      title = "Mean Temperature [F] <br> (data: NWS/NOAA)",
                      opacity = 1,
                      bins = 5)
            
    })
    observe({
        leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            addCircleMarkers(lng = ~long,
                             lat = ~lat,
                             radius = ~log(casecount) * 10,
                             weight = 1,
                             opacity = 1,
                             color = ~ifelse(casecount > 0, reds(casecount), NA),
                             popup = ~paste0(county, ",  ", casecount, " cases"))%>%
            
            addCircleMarkers(lng = ~long,
                             lat = ~lat,
                             radius = ~log(Temperature) * 5,
                             weight = 1,
                             opacity = 1,
                             fillOpacity = 1,
                             color = ~ifelse(Temperature > 0, palc(Temperature), NA),
                             popup = ~paste0(county, ",  ", deaths, " cases"),
                             labelOptions =(noHide= TRUE))
    
        
        
        
        }) 
}



shinyApp(ui2, server5,options = list(height = 550))
