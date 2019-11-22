library(leaflet)
library(tidyverse)
library(readxl)
#Import data file 
read_xlsx("data/2019Sep26_algae_bc.xlsx")

#Assign the result of the read file to variables 
df <- read_xlsx("data/2019Sep26_algae_bc.xlsx")

#library in tidyverse 'dplyr' -> group_by(variable, column 1, column 2)
totals <- group_by(df, Genus, Species) %>%  
  summarize(n())

# filter out the items you are interested in 
df_fg<- df %>% 
  filter(Genus == "Fucus", Species == "gardneri")

map <- leaflet() %>% 
  addProviderTiles("CartoDB") %>%
  addWMSTiles("https://openmaps.gov.bc.ca/geo/pub/WHSE_ENVIRONMENTAL_MONITORING.SHZN_HAB_OBS_POLYS_SV/ows?service=WMS",
              layers = "pub:WHSE_ENVIRONMENTAL_MONITORING.SHZN_HAB_OBS_POLYS_SV",
              options = WMSTileOptions(format = "image/png", 
                                       transparent = TRUE,
                                       zoomToBoundsOnCLick = TRUE)) %>% 
  addCircleMarkers(lat = df_fg$Geo_LatDecimal, lng = df_fg$Geo_LongDecimal,
                   popup = paste("Collector:",df_fg$'Primary Collector',
                                 "Lat ",df_fg$Geo_LatDecimal,
                                 "Lng ", df_fg$Geo_LongDecimal),
                   clusterOptions = markerClusterOptions())


library(raster)
xmn = 123;xmx = 124;ymn=49;ymx=49.5

r = raster(matrix(1:12,3,4), xmx=xmx, xmn=xmn,ymx=ymx,ymn=ymn)

pts = data.frame(x=runif(df_fg$Geo_LongDecimal,xmn,xmx), y=runif(df_fg$Geo_LatDecimal,ymn,ymx))

pointcount = function(r, pts){
  # make a raster of zeroes like the input
  r2 = r
  r2[] = 0
  # get the cell index for each point and make a table:
  counts = table(cellFromXY(r,pts))
  # fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

r2 = pointcount(r, pts)
plot(r2)
points(pts)
