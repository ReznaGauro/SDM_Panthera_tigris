# ############### Spatial modeling of Panthera tigris in Nepal ################
###############################################################################

# Install and load the necesary packages
install.packages("dismo")

library(dismo)
library(terra)
library(rnaturalearth)
library(sf)

# Download the occurrence data for Panthera tigris from GBIF

tiger <- gbif("Panthera ", "tigris*", sp=TRUE)

# change the projection to WGS84
crs(tiger) <-"epsg:4326" 
tiger <- vect(tiger)

# Read the Nepal shapefile
nepal <- vect("D:/MSc_EAGLE/Summer_Semester_2024/SpatioTemporal/SDM_Panthera_tigris/SDM_Panthera_tigris/Nepal_shpfile/Nepal_new.gpkg")

# clip and plot the occurence data to the country
tiger <- crop(tiger, nepal)
plot(nepal)
plot(tiger, add = TRUE, col = "red", pch = 19)
