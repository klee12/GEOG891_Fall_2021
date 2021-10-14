
library(tidyverse)
library(sf)
library(tmap)
library(dplyr)

# dtat loading section==========================================================================================
counties <- sf::read_sf("../homework2/data/CBW/County_Boundaries.shp") %>% sf::st_make_valid()
dams <- sf::read_sf("../homework2/data/CBW/Dam_or_Other_Blockage_Removed_2012_2017.shp") %>% sf::st_make_valid()
streams <- sf::read_sf("../homework2/data/CBW/Streams_Opened_by_Dam_Removal_2012_2017.shp") %>% sf::st_make_valid()
# aspatial dataset
bmps <- read_csv("../homework2/data/CBW/BMPreport2016_landbmps.csv")
#================================================================================================================

#Task 1.1 Calculate summary statistics for the Cost of BMPs for each State (including DC)

Data1<- bmps %>%  group_by(StateAbbreviation) %>% summarise(Cost = na.omit(Cost))
by(Data1,Data1$StateAbbreviation,summary)     

#using the group_by function by state, then using the na.omit to omit the NA value in the column,
#following the by function to categorized each of the state summary statistic. 
#Ref. https://www.statmethods.net/stats/withby.html


# 1.2 Make a scatterplot of Cost vs. TotalAmountCredited, ONLY FOR Units of type "Acres". You may need
#to apply a data transformation to one or more axes if the data are heavily skewed.

bmps %>% dplyr::filter(., Cost > 1 & Cost < 100000) %>%
  ggplot(., aes(x = Cost, y = TotalAmountCredited)) +
  geom_point(aes(fill =Unit))+ ggtitle("Cost vs. TotalAmountCredited")

#I used the dplyr to filter out the heavily skew data, then plotted according to the question stated with
#unit set for "Acres" 
#Ref. Lab instruction.

#1.3 Make a boxplot with "tateAbbreviation" on the x-axis and "TotalAmountCredited" on the y-axis.
#HOWEVER, the only data I want plotted are for cover crop BMPs. Note, there are many types of cover
#crops in this dataset, and I want you to include them ALL. There are handy functions within the stringr
#package that can help you here.

Bplot13<-bmps %>% mutate(.,CCBs = stringr:: str_detect(BMP,"Cover Crop",negate = T))
Bplot13 %>% dplyr::filter(CCBs == F, AmountCredited > 1 & AmountCredited < 100)%>%
  ggplot(., aes(x = StateAbbreviation, y = TotalAmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation))+ ggtitle("StateAbbreviation vs. TotalAmountCredited")

#Using the stringr_detect function to detect Cover Crop BMPS (CCBs), first, use the mutate function to create 
#another column that detect the CCBs, negate = T means if the pattern do not much return True. Therefore, we
#need only the False value in this case. Then, I filtered out the false value, and plot it, to prevent over skewed
#data, I used the same reference as the exercise AmountCredited > 1 & AmountCredited < 100. 

#Ref: https://www.rdocumentation.org/packages/stringr/versions/1.4.0/topics/str_detect

#1.4 make a scatterplot of the dam dataset, this time with ：YEAR； on the x-axis and ：STATE； on y-axis
#(think of it like a timeline). Assume no dams were built in year 0, so you・ll need to remove those data points

dams %>% dplyr::filter(YEAR > 1900)%>%ggplot(., aes(x=YEAR, y=STATE, label = DAM_NAME, color = STATE)) + 
  geom_point(size=3) + labs(x= "Year",y="State",title = "xxx")+
  geom_text(hjust = 0.5, vjust = 1.5, nudge_x = 0.5, angle = 45, size = 5)+
  theme_classic()
 
# I used the filter to filter out the 0 and non-numeric value. Then plot it with
# with ggplot features 

#1.5 make one last (aspatial) visualization. But this time, it・s your choice 
# what data and plots to use. The only requirement is that you link two of the 
# datasets together in some manner. Be creative. Make it look #nice 
# (e.g., use proper labels, interesting colors/shading/size).


Data1 <- dams %>% as.tibble() %>% dplyr::select(STATE,YEAR,DamRemoval,DAM_NAME)
Data2 <- bmps  %>% as.tibble() %>% group_by(StateAbbreviation) %>% summarise(tcost = sum(TotalAmountCredited, na.rm = T))
D_combine <- left_join(Data1, Data2, by = c("STATE" = "StateAbbreviation"))
D_combine %>% dplyr::filter(YEAR > 1900 )%>%mutate(Dam_life = DamRemoval-YEAR)%>%
  mutate(AvgCost = tcost/Dam_life)%>% ggplot(.,aes(x=Dam_life, y=AvgCost, label = DAM_NAME, color =STATE))+
  geom_point(size=3) + 
  geom_text(hjust = 0.7, vjust = 2.5, nudge_x = 0.5, angle = 0, size = 3.5) +
  labs(x= "Dam Duration",y="Average Cost",title = "Dam Duration and the average cost per year ")+
  theme_classic() 


# After I reexamined the data, I discovered the only dams and BMPS have a common 
# meanful attribute (STATE). I also discovered that the output value will be 
# the same for the join variable if the variable was calculated before join the dataset. In this 
# question, I plotted the dam duration and the average cost per year. The 
# dam duration was calculated by subtracting the year with the removal year (with NA value remove),
# Then join with the total cost per state (with NA value remove). Then I divided 
# the cost by the number of the year. This graph make sense because the longer
# the dam duration, the average cost per year going down. 


#Task 2
  
#2.1 Find the 5 longest streams in the ．streams opened by dam removal・ dataset

streams %>% as.tibble()%>%group_by(GNIS_Name) %>%  
   summarise(Stream_length_Km = sum(LengthKM))%>%slice_max(Stream_length_Km, n = 6) %>% na.omit()

# First I passed the data as tibble, the group by using the stream name. I
# created a column call Stream_length using sum of the LengthKM column. Then
# summarize the column, using slice function to get the maximum of 5. However,
# while I was doing that, I discovered there was a NA value at first row, therefore, I 
# used the na.omit function to omit the na value and switch the slice_max
# function to 6 to get the 5 longest streams. 

# Ref. lab 1 examples 

#2.2  Find the three counties with the greatest TOTAL length of streams (opened by dam removal) in them

streams1 <- st_make_valid(streams)
counties1 <- st_make_valid(counties)
strCounty <-  sf::st_intersection(streams1, counties1) 
strCounty %>% as.tibble()%>% group_by(NAME10)%>%
  summarise(Total_Length_Stream = sum(LengthKM))%>%
  slice_max(Total_Length_Stream, n =3)

# Using the st_make_valid function to validate the spatial dataset. Then
# I used the st_intersection between stream and county dataset. The I 
# calculate the total length stream, and used the slice_max function 
# to list three counties with the greatest TOTAL length of streams.

# Ref. lab 1 examples


#2.3 Make a map of the counties, shading each county by the total cost of BMPs funded/implemented in that
# county. This will required you to join multiple datasets together

  BmpData<-bmps%>% mutate(., Name.trimmed = stringr::str_sub(GeographyName, 1, 5)) %>% 
    group_by(Name.trimmed) %>% summarise(Total_cost = sum(Cost, na.rm = T))
  DataJoin <-left_join(counties,BmpData, by= c("GEOID10" = "Name.trimmed"))
  tm_shape(DataJoin)+ tm_polygons(col ="Total_cost") +
   tm_layout(title = "Total Cost of BMPs per County")
  
# I used the mutate function to trim the variables in order to perform join with
# the counties dataset. After I examine the detail of the each dataset,
# I discovered that the GEOID10 will be the "key" to join the table (match Name.trimmed)
# the using the tm function to make a map.   

#Ref. Lab 2 examples  
  
  
#2.4  For each removed dam, find the closest stream segment
  
  SpJoinFeature <- st_join(dams,streams,st_nearest_feature)
  SpJoinFeature%>%select(DAM_NAME.x,GNIS_Name)%>%na.omit()

# I used the spatial Join function with near feature included to find 
# the closest stream segment, and select the dam and correspond stream segment.
# Then, I omit some of the stream segment with NA values.

# Ref. ??St_Join and st_nearst_feature  
  
#2.5 Calculate how many removed dams are (or were) in each state
  
  dams %>% group_by(STATE) %>% tally
  
# I think I used a little bit cheat technique here. After I reexamine the data, I discovered 
# we only need to count how many states within the dataset because each column has the correspond state and 
# dam removal year, and no missing data we need to deal with. Therefore, we can just 
# simply count how many state within the dam dataset, then we know how many dam 
# were removed in each state. 