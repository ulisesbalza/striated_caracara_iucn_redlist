#-------------------------------------------------------------------------#
#  Estimation of Extention of occupancy (EOO) and Area of ocupancy (AOO)
#    for the Striated caracara (Phalcoboenus australis Gmelin 1788)
#-------------------------------------------------------------------------#
# 

# Ulises Balza
# Centro Austral de Investigaciones Científicas (CADIC-CONICET)
# Ushuaia, Argentina


# Packages
library(rgbif)
library(redlistr) 
library(tidyverse)
library(rgdal)
library(ggmap)
library(sp)
library(maptools)

#Downloading GBIF data
# Setting the years for occurrences to be included
# only occurences in the last 33 years (i.e., aprox. three generations)
years <- seq(1989, 2021, 1)
australis_raw <- occ_data(taxonKey =  2481091, # Key from GBIF 
                      year = years,
                      hasCoordinate = T, # Only want points with coordinates
                      limit = 1500)

#getting citation for the dataset

gbif_citation(x= australis_raw)

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

# filter by unreal geographic areas (i.e., errors or captive individuals)
australis_occurrence <- australis_occurrence %>% filter(australis_occurrence$decimalLongitude < -55)

#save csv
csv <-  write.csv(australis_occurrence, file =  "GBIF_australis_occurrence2021.csv")


#-------------------------------------------------------#
# Now from reading the database ####
#-------------------------------------------------------#

data<-read.csv("data/GBIF_australis_occurrence2021.csv",header=T,sep=",")

#---------------------------------------------------------#

#### using redlistr to estimate EOO and AOO ####

# Set the CRS for the data
# projected coordinate system (PCS) measured in metres
# In this case, South America Equidistant Conic
eqdcsa <- CRS('+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs')

# but it would work similarly with UTM zone 20S
utm_20s <- CRS("+proj=utm +zone=20 +south +datum=WGS84 +units=m +no_defs") 

# As the original data is in lat/lon, I have to create the shapefile in WGS84 first
wgs84 <- CRS("+init=epsg:4326")

# Create shapefile from the data frame that can be used by redlistr
xy <- SpatialPointsDataFrame(matrix(c(data$decimalLongitude, data$decimalLatitude), ncol=2),
                             data=data,
                             proj4string = wgs84)
# Now reproject
xy <- spTransform(xy, eqdcsa)

#Calculation EOO ####

EOO_polygons <- makeEOO(xy)
area <- getAreaEOO(EOO_polygons)
area # 536,601 km2 

# plot EOO
sudamerica <- readShapePoly("data/sudamericafundida.shp", proj4string=wgs84)
sudamerica <- spTransform(sudamerica, eqdcsa)

plot(EOO_polygons)
plot(xy, pch=19, add=T)
plot(sudamerica, add=T)

# Calculating AOO ####
#using 2km x 2km (4 km2) grid squares

AOO_australis <- getAOO(xy, 2000, min.percent.rule=F)
area_AOO <- AOO_australis*4 
area_AOO #932 km2

#grid uncertainty
AOO_uncertainty <-  gridUncertainty(xy, 2000, 5, min.percent.rule = F)
range(AOO_uncertainty$min.AOO.df$min.AOO)*4 #912-940 km2

#plot AOO

AOO_grid <- makeAOOGrid(xy, 2000)

plot(EOO_polygons)
plot(sudamerica, add=T)
plot(AOO_grid, col="red", border="red", add=T)

