  rm(list=ls())
install.packages("rgbif")

library(redlistr) 
library(tidyverse)
library(rgdal)
library(ggmap)
library(ggsn)
library(scales)
library(cowplot)
library(rgbif)
library(maps)
library(sp)
library(maptools)


#Downloading GBIF data
# Setting the years for occurrences to be included
# only occurences in the last 33 years (three generations)
years <- seq(1989, 2021, 1)
australis_raw <- occ_data(taxonKey =  2481091, # Key from GBIF 
                      year = years,
                      hasCoordinate = T, # Only want points with coordinates
                      limit = 1500)

# A limit is needed for the function to work
# There are no years where there are more than 1500 records
# Get number of occurrence points
# Create empty vector
count <- vector()
for(i in seq_along(years)){
  temp <- eval(parse(text = sprintf('australis_raw[%d]$"%s"$meta$count', i, years[i])))
  # Gets the 'count' for each year 
  count <- c(count, temp) # Create vector of count values of each year
}
n.occ <- sum(count)# Total count for all years
n.occ
plot (count ~ years, pch=19) #counts/year


# Pick out the wanted columns and merging into single dataframe
wanted <- c("decimalLatitude", "decimalLongitude", "year")
australis_occurrence <- NULL
for(i in seq_along(years)){
  temp <- australis_raw[[i]]$data[wanted]
  australis_occurrence <- rbind(australis_occurrence, temp)
}

#filter by unreal areas
australis_occurrence <- australis_occurrence %>% filter(australis_occurrence$decimalLongitude < -55)
australis_occurrence <- australis_occurrence %>% filter(australis_occurrence$decimalLatitude < -45)

head(australis_occurrence)

#save csv
csv <-  write.csv(australis_occurrence, file =  "GBIF_australis_occurrence2021.csv")


#-------------------------------------------------------#
##### From now on, I use the curated database ####
#-------------------------------------------------------#
data<-read.csv("GBIF_australis_occurrence2021.csv",header=T,sep=",")
head(data)


# Use world land outline from ggplot2 as background ARREGLAR PARA QUE QUEDE BIEN PIOLA!!!!

map_world <- borders("world", colour="gray50", fill="gray50")
world_plot <- ggplot() + map_world
world_plot +
  geom_point(data = data, aes(x = decimalLongitude, y = decimalLatitude,
                                          color = "red")) +
  coord_fixed() + # Ensure that x and y are equal
    # Use custome theme to create map layout
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        plot.background = element_blank()) +
  # Use some manual colours for each species
  scale_colour_manual(name = "Species Name (n)",
                      values = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                 "#F0E442", "#0072B2", "#D55E00")) +
  # Make legend italicised
  theme(legend.text = element_text(face = "italic"))

#---------------------------------------------------------#

#### using redlistr ####
# CRITERION B
# category   EOO     AOO
# VU          20,000  2,000
# EN          5,000   500
# CR          100     10

# Set the CRS for the data
#It must be in a projected coordinate system (PCS) measured in metres
#South America Equidistant Conic
eqdcsa <- CRS('+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs')
#WGS84
wgs84 <- CRS("+init=epsg:4326")
#UTM zone 20S
utm <- CRS("+proj=utm +zone=20 +south +datum=WGS84 +units=m +no_defs") 

# Create shapefile from the data frame that can be used by redlistr
xy <- SpatialPointsDataFrame(matrix(c(data$decimalLongitude, data$decimalLatitude), ncol=2),
                             data=data,
                             proj4string = wgs84)
xy <- spTransform(xy, eqdcsa)

#Calculation EOO ####

EOO_polygons <- makeEOO(xy)
area <- getAreaEOO(EOO_polygons)
area # 536,601 km2 falta masking con sudamerica

# plot EOO
sudamerica <- readShapePoly("data/sudamericafundida.shp", proj4string=wgs84)
sudamerica <- spTransform(sudamerica, eqdcsa)

plot(EOO_polygons)
plot(xy, pch=19, add=T)
plot(sudamerica, add=T)

# Calculating AOO ####
#using 2km x 2km (4 km2) grid squares coastal criteria

AOO_australis <- getAOO(xy, 2000, min.percent.rule=F)
area_AOO <- AOO_australis*4 #932 km2

#grid uncertainty
AOO_uncertainty <-  gridUncertainty(xy, 2000, 5, min.percent.rule = F)
range(AOO_uncertainty$min.AOO.df$min.AOO)*4 #912-940 km2

#plotting

AOO_grid <- makeAOOGrid(xy, 2000)

plot(EOO_polygons)
plot(sudamerica, add=T)
plot(AOO_grid, col="red", border="red", add=T)

