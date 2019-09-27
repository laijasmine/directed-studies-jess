#Load necessary libraries 
library(tidyverse)
library(readxl)
library(leaflet)

#Import data file 
read_xlsx("data/2019Sep26_algae_bc.xlsx")

#Assign the result of the read file to variables 
df <- read_xlsx("data/2019Sep26_algae_bc.xlsx")

#shows your first/last few rows of your spreadsheet
head(df)
tail(df)

#library in tidyverse 'dplyr' -> group_by(variable, column 1, column 2)
totals <- group_by(df, Genus, Species) %>%  
  summarize(n())

# filter out the items you are interested in 
df_fg<- df %>% 
  filter(Genus == "Fucus", Species == "gardneri")

#Layers of leaflet 
leaflet() %>% 
  addProviderTiles("OpenStreetMap.DE") %>% 
  addCircleMarkers(lat = df_fg$Geo_LatDecimal, lng = df_fg$Geo_LongDecimal,
                   popup = paste("Collector:",df_fg$'Primary Collector'),
                   clusterOptions = markerClusterOptions())

loc <- group_by(df_fg, Geo_LatDecimal, Geo_LongDecimal) %>%  
  summarize(total = n())
