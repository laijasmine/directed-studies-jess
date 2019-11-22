require(sp)
require(rgdal)
d=40  # Distance threshold         

# Create example data and transform into projected coordinate system
x <- c(-1.482156, -1.482318, -1.482129, -1.482880, -1.485735, -1.485770, -1.485913, -1.484275, -1.485866)
y <- c(54.90083, 54.90078, 54.90077, 54.90011, 54.89936, 54.89935, 54.89935, 54.89879, 54.89902)

xy <- SpatialPointsDataFrame(matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
                             proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

xy <- spTransform(xy, CRS("+init=epsg:27700 +datum=WGS84"))

chc <- hclust(dist(data.frame(rownames=rownames(xy@data), x=coordinates(xy)[,1],
                              y=coordinates(xy)[,2])), method="complete")

# Distance with a 40m threshold  
chc.d40 <- cutree(chc, h=d) 

# Join results to meuse sp points
xy@data <- data.frame(xy@data, Clust=chc.d40)

# Plot results
plot(xy, col=factor(xy@data$Clust), pch=19)
box(col="black")
title(main="Clustering")
legend("topleft", legend=paste("Cluster", 1:4,sep=""),
       col=palette()[1:4], pch=rep(19,4), bg="white")


#-----
d=40  # Distance threshold

#Assign the result of the read file to variables 
df <- read_xlsx("data/2019Sep26_algae_bc.xlsx")

#library in tidyverse 'dplyr' -> group_by(variable, column 1, column 2)
totals <- group_by(df, Genus, Species) %>%  
  summarize(n())

# filter out the items you are interested in 
df_fg<- df %>% 
  filter(Genus == "Fucus", Species == "gardneri",!is.na(Geo_LongDecimal))

xy <- SpatialPointsDataFrame(matrix(c(df_fg$Geo_LongDecimal,df_fg$Geo_LatDecimal), ncol=2), data.frame(ID=seq(1:length(df_fg$Geo_LatDecimal))),
                             proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

chc <- hclust(dist(data.frame(rownames=rownames(xy@data), x=coordinates(xy)[,1],
                              y=coordinates(xy)[,2])), method="complete")

# Distance with a 40m threshold  
chc.d40 <- cutree(chc, h=d) 

# Join results to meuse sp points
xy@data <- data.frame(xy@data, Clust=chc.d40)

# Plot results
plot(xy, col=factor(xy@data$Clust), pch=19)
box(col="black")
title(main="Clustering")
legend("topleft", legend=paste("Cluster", 1:length(df_fg$Geo_LatDecimal),sep=""),
       col=palette()[1:length(df_fg$Geo_LatDecimal)], pch=rep(19,4), bg="white")


df_clust <- cbind(df_fg, "cluster" = xy@data[["Clust"]])

df_sub <- df_clust %>% 
  filter(cluster %in% c(1,2,3))

library(viridis)

ggplot(df_clust, aes(x = Geo_LongDecimal, y = Geo_LatDecimal, alpha = 0.5)) +
  geom_point(aes(colour = cluster)) +
  scale_colour_viridis_c()


ggplot(df_sub, aes(x = Geo_LongDecimal, y = Geo_LatDecimal, alpha = 0.5)) +
  geom_point(aes(colour = cluster)) +
  scale_colour_viridis_c()

# see the most frequent location
pt_fd <- df_clust %>% 
  group_by(Geo_LongDecimal,Geo_LatDecimal) %>% 
  summarise(n())

# see the spread of years at that location
df_clust %>% 
  filter(Geo_LongDecimal == -124.000, Geo_LatDecimal ==	48.40000) %>% 
  group_by(`Date Collected`) %>% 
  summarise(n()) %>% 
  arrange()
