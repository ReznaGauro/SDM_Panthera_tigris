# ############### Spatial modeling of Panthera tigris in Nepal ################
###############################################################################

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
nepal <- vect("C:/Users/Jado/Documents/EAGLE/Semester2/Spatial_modeling/Exam_task/SDM_Panthera_tigris/SDM_Panthera_tigris/Nepal_shpfile/hermes_NPL_new_wgs_0.shp")

# clip and plot the occurence data to the country
tiger <- crop(tiger, nepal)
plot(nepal$geometry)
plot(tiger, add = TRUE, col = "red", pch = 19)