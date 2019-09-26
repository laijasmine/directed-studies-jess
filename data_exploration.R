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


leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lat = df_ss$Geo_LatDecimal, lng = df_ss$Geo_LongDecimal,
                   popup = paste("Collector:", df_ss$`Primary Collector`,
                                 "Date collected:", df_ss$`Date Collected`,
                                 ", Location:", df_ss$Location, 
                                 ", Collect no.:",df_ss$`Collector Number`),
                   clusterOptions = markerClusterOptions())

bc <- read_xlsx("data/2019Sep26_algae_bc.xlsx")

top_sp_bc <- bc %>% 
  mutate(gs = paste(Genus,Species)) %>% 
  group_by(gs) %>% 
  summarise(n())

df_ms <- bc %>% 
  filter(Genus == "Mazzaella", Species == "splendens")


leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lat = df_ms$Geo_LatDecimal, lng = df_ms$Geo_LongDecimal,
                   popup = paste("Collector:", df_ms$`Primary Collector`,
                                 "Date collected:", df_ms$`Date Collected`,
                                 ", Location:", df_ms$Location, 
                                 ", Collect no.:",df_ms$`Collector Number`),
                   clusterOptions = markerClusterOptions())
