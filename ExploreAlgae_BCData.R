#Load Libraries 
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(readxl)
library(leaflet)
library(plotly)
library(lubridate)
library(lasso2)

#read in data frame containing BC algal collections data from the UBC Herbarium 

read_xlsx("data/2019Sep26_algae_bc.xlsx")

#Set variable for data frame 
dfBC <- read_xlsx("data/2019Sep26_algae_bc.xlsx")
dfBC <- add_column(dfBC, Genus_spc = paste(dfBC$Genus," ", dfBC$Species), .after = 'Species')
dfBC <- mutate(dfBC, ScieName = paste(Genus, Species), 
             new_yr = as.Date(dfBC$`Date Collected`, format = "%Y %b %d"),
             yr = format(new_yr, "%Y")) 

dfBC_omit <- dfBC %>% 
  drop_na(yr, Geo_LatDecimal, Geo_LongDecimal)
dfBC_omit$yr[dfBC_omit$yr == '2984'] <- '1984'
dfBC_omit$yr[dfBC_omit$yr == "0013"] <- "2013"
dfBC_omit$yr[dfBC_omit$yr == '0014'] <- '2014'
dfBC_omit$yr[dfBC_omit$yr == '2968'] <- '1968'
dfBC_omit$yr[dfBC_omit$yr == '9689'] <- '1689'
dfBC_omit$new_yr[dfBC_omit$new_yr == '2984-05-16'] <- '1984-05-16'
dfBC_omit$`Date Collected`[dfBC_omit$`Date Collected` == 	'2984 May 16'] <- 	'1984 May 16'

write_csv(dfBC_omit, "dfBC_omit.csv")

tm_shape(s.sf) + tm_polygons("Income",  border.col = "white") + 
  tm_legend(outside = TRUE)
#___________________________________________________________________________________________________________
#Find species within the data with varying sample sizes 

#Create a new data frame with the number of each individual algal species in the origional 
#data frame
#Columns: Genus, Species, Count 
algaetotalBC <- group_by(dfBC_omit, Genus_spc) %>%  
  summarize(count = n()) %>% 
  ungroup()  

#Create three new data frames filtering algal species with 100+, 200+ and 300+ samples in the 
#UBC Herbarium Collections 
over99BC <- filter(algaetotalBC, count >= 100)
over199BC <- filter(algaetotalBC, count >=200)
over299BC <- filter(algaetotalBC, count >=300)
over399BC <- filter(algaetotalBC, count >=400)
over499BC <- filter(algaetotalBC, count >=500)


df_filteredBC <- filter(dfBC_omit, Genus_spc %in% over499BC$Genus_spc)


#_________________________________________________________________________________________
#Filter out species of interest 

#function filter_ScieName is a function that filters our origional data frame and selects 
#only the collections data for the species of interest; Inputs are Genus & Species; Output 
#is a data frame with the collections data of just our species of interest called df_ScieName
filter_ScieName <- function(gs){
  # filter out the items you are interested in 
  df_ScieName <- dfBC_omit %>% 
    filter(Genus_spc == gs)}

#These seven species of interest were chosed based on the number of samples we have 
Am <- 'Alaria   marginata' #546 samples
Cc <- 'Costaria   costata' #612 samples
Fg <- 'Fucus   gardneri' #672 samples
Ms <- 'Mazzaella   splendens' #658 samples
Of <- 'Odonthalia   floccosa' #649 samples
Ss <- 'Saccharina   subsimplex' #485 samples
Uf <- 'Ulva   fenestrata' #506 samples

#Call function filter_ScieName and input variable associated with algal species of interest. 
df_selectspcBC <- filter_ScieName(Fg)
#___________________________________________________________________________________________
#Filter out samples that are located within the specified spatial boundaries. 
latmax <- '49.21'
latmin <- '48.99'
longmax <- '-123.82'
longmin <- '-123.57'


df_SelectArea <- dfBC_omit[dfBC_omit[,'Geo_LatDecimal'] <= latmax,]
df_SelectArea <- df_SelectArea[df_SelectArea[,'Geo_LatDecimal'] >= latmin,]
df_SelectArea <- df_SelectArea[df_SelectArea[,'Geo_LongDecimal'] <= longmax,]    
df_SelectArea <- df_SelectArea[df_SelectArea[,'Geo_LongDecimal'] >= longmin,]
#___________________________________________________________________________________________
#Map your species of interest, labelling with the collectors name. Use df_selectspcBC to map all samples of your species of interest 
# or df_SelectArea to only map samples that are within the specified spatial boundaries. 

#Layers of leaflet 
leaflet() %>% 
  addProviderTiles("OpenStreetMap.DE") %>% 
  addCircleMarkers(lat = df_selectspcBC$Geo_LatDecimal, lng = df_selectspcBC$Geo_LongDecimal,
                   popup = paste("Collector:",df_selectspcBC$'Geo_LongDecimal', df_selectspcBC$'Geo_LatDecimal'),
                   clusterOptions = markerClusterOptions())

loc <- group_by(df_selectspcBC, Geo_LatDecimal, Geo_LongDecimal) %>%  
  summarize(total = n())

#__________________________________________________________________________________________
#Boxplot of species numbers for species which we have at least 500 samples 

# ggplot(data = over499) + 
#   geom_bar(mapping = aes(x = Genus_spc, y = count), stat = "identity") + 
#   facet_wrap(~yr)
plotspec <- ggplot(data = df_filteredBC) + 
  geom_bar(aes(x = yr)) + 
  theme(axis.text.x=element_text(angle=90)) + 
  facet_wrap(~ Genus_spc, ncol = 1)
ggplotly(plotspec)

#Create a single Box plot for your species of interest 
plotspecAl <- ggplot(data = df_selectspcBC) + 
  geom_bar(aes(x = yr))+ 
  xlab("Collection Year") + 
  ylab("Number of samples collected")+ 
  ggtitle("Collection Frequency of Ulva fenestrata over time" )+
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90)) 
ggplotly(plotspecAl)


#_______________________________________________________________________________________________________
#Plot species of interest within your specified spatial boundaries 

#Area Analysis 
Areatotalspc <- group_by(df_SelectArea, Genus_spc) %>%  
  summarize(count = n()) %>% 
  ungroup()                          
                  
Areatotalloc <- group_by(df_SelectArea, Location) %>%  
  summarize(count = n()) %>% 
  ungroup()  

Areatotalcol <- group_by(df_SelectArea, `Primary Collector`) %>%  
  summarize(count = n()) %>% 
  ungroup() 
 


over20BC <- filter(Areatotalspc, count >= 20)
over40BC <- filter(Areatotalspc, count >=40)
over60BC <- filter(Areatotalspc, count >=60)
over80BC <- filter(Areatotalspc, count >=80)


df_locfilteredBC <- filter(df_SelectArea, Genus_spc %in% over40BC$Genus_spc) 
df_locspc <- filter(df_SelectArea, Genus_spc == "Mazzaella   splendens")

plotspec <- ggplot(data = df_locfilteredBC) + 
  geom_bar(aes(x = yr)) + 
  theme(axis.text.x=element_text(angle=90)) + 
  facet_wrap(~ Genus_spc, ncol = 1)
ggplotly(plotspec)  
  
plotspec <- ggplot(data = df_locspc) + 
  geom_bar(aes(x = yr))  + 
  xlab("Year") + 
  ylab("Number of Samples") + 
  ggtitle("Temporal Trend for Mazzaella splendens near Gabriola Island") + 
  theme_bw()
ggplotly(plotspec)

#__________________________________________________________________________________________
#Plot the running total of samples collected within BC 
yearly_counts <- dfBC_omit %>% 
  count(yr)
yearly_counts$cumsum <- cumsum(yearly_counts$n)

plot(cumsum ~ yr, yearly_counts, pch = 19, xlab = "Year", ylab = "Collection Samples", xlim = c(1861,2017))

ggplot(data =yearly_counts) + 
  geom_point(mapping = aes(x=yr, y=cumsum), color = "black")

