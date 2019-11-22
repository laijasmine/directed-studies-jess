library(spData)
library(tmap)
library(readxl)
library(tidyverse)
library(sf)
library(raster)
library(leaflet)

leaflet(fg) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = fg_size$Geo_LongDecimal,
                    lat = fg_size$Geo_LatDecimal,
                   clusterOptions = markerClusterOptions())

ak <- read_xlsx("data/2019Sep26_algae_alaska.xlsx")

fg <- ak %>% 
  filter(Genus == "Fucus", Species == "gardneri",
         !is.na(Geo_LatDecimal)) %>% 
  mutate(loc = paste(Geo_LatDecimal, Geo_LongDecimal))

at <- st_transform(alaska, 4326)

wgs84 = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
cat_raster_wgs84 = projectRaster(cat_raster, crs = wgs84, method = "ngb")

fg_ll = data.frame(lon = fg$Geo_LongDecimal, lat = fg$Geo_LatDecimal) %>% 
  st_as_sf(coords = c("lon", "lat"))
fg_ll_geo = st_set_crs(fg_ll, 4326)

tm_shape(at) +
  tm_bubbles(fg_ll_geo) + 
  tm_layout(title = "Alaska", frame = FALSE, bg.color = NA)
  
  
ggplot() +
  geom_sf(data = at) +
  geom_hex(aes(x = fg_size$Geo_LongDecimal,
                 y = fg_size$Geo_LatDecimal,
                 colour = "red",
                 fill= fg_size$`n()`,
               size =1), bins = 100) +
  coord_sf(datum = 4326)

ggplot(fg) +
  geom_point(aes(x = fg$Geo_LongDecimal,
                 y = fg$Geo_LatDecimal))



ggplot(fg) +
  geom_histogram(aes(x = loc), stat = "count")

fg_size <- fg %>% 
  group_by(Geo_LongDecimal, Geo_LatDecimal) %>% 
  summarise(colSums())

library(plotly)
ggplotly()


#shape file
algae_r <- st_read("bcmca_eco_algae_rockyintertidalsheltered_marxan_data/BCMCA_ECO_Algae_RockyIntertidalSheltered_MARXAN.shp") %>% 
  mutate(type = "sheltered")
algae_e <- st_read("bcmca_eco_algae_rockyintertidalexposed_marxan_data/BCMCA_ECO_Algae_RockyIntertidalExposed_MARXAN.shp") %>% 
  mutate(type = "exposed")
algae_me <-  st_read("bcmca_eco_algae_mudflatsestuarine_marxan_data/BCMCA_ECO_Algae_MudflatsEstuarine_MARXAN.shp") %>% 
  mutate(type = "mudflat_estuarine")

algae_all <- rbind(algae_r ,algae_e ,algae_me)

algae_all_w <- st_transform(algae_all,4326)

# Nad 83 Albers
projection <- 3153

df_bc <- read_xlsx("data/2019Sep26_algae_bc.xlsx")
fg_bc <- df_bc %>% filter(Genus == "Fucus", Species == "gardneri", !is.na(Geo_LatDecimal)) %>% 
  mutate(g = list(Geo_LatDecimal,Geo_LongDecimal))
st_as_sf(fg_bc)

library(bcmaps)
bc <- bc_bound()
plot(st_geometry(ecoprovinces()))
plot(st_geometry(bc))
bc_w <- st_transform(bc, 4326)

ggplot() +
  geom_sf(data = bc) +
  geom_sf(data = st_geometry(ecoprovinces()))

ggplot() +
  geom_sf(data = bc_w) +
  geom_sf(data = algae_all_w, aes(col = algae_all_w$type))+
coord_sf(datum = projection)

+
  geom_point(aes(x = fg_bc$Geo_LongDecimal,
               y = fg_bc$Geo_LatDecimal,
               colour = "red")) +
  coord_sf(datum = 4326)

ggplotly()



