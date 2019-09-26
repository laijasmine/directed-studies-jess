library(tidyverse)
library(readxl)
library(leaflet)

ak <- read_xlsx("data/2019Sep26_algae_alaska.xlsx")

top_sp <- ak %>% 
  mutate(gs = paste(Genus,Species)) %>% 
  group_by(gs) %>% 
  summarise(n())

df_ss <- ak %>% 
  filter(Genus == "Saccharina", Species ==  "subsimplex")

df_ss %>% 
  leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lat = df_ss$Geo_LatDecimal, lng = df_ss$Geo_LongDecimal,
                   popup = paste("Collector:", df_ss$`Primary Collector`,
                                 "Date collected:", df_ss$`Date Collected`,
                                 ", Location:", ds_ss$Location, 
                                 ", Collect no.:",ds_ss$`Collector Number`),
                   clusterOptions = markerClusterOptions())

bc <- read_xlsx("data/2019Sep26_algae_bc.xlsx")

