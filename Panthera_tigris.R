# ############### Spatial modeling of Panthera tigris in Nepal ################
###############################################################################

# <<<<<<< 
# Install and load the necesary packages
=======
# Install and load the necessary packages
install.packages("dismo")
install.packages("biomod2")
# >>>>>>> 

library(dismo)
library(biomod2)
library(raster)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Download the occurrence data for Panthera tigris from GBIF
tiger <- gbif("Panthera ", "tigris*", sp=TRUE)

# change the projection to WGS84
crs(tiger) <-"epsg:4326" 
tiger <- vect(tiger)

# Read the Nepal shapefile
nepal <- vect("C:/Users/Jado/Documents/EAGLE/Semester2/Spatial_modeling/Exam_task/SDM_Panthera_tigris/SDM_Panthera_tigris/Nepal_shpfile/Nepal_new.gpkg")

# clip and plot the occurence data to the country
tiger <- crop(tiger, nepal)
plot(nepal)
plot(tiger, add = TRUE, col = "orange", pch = 19)

# Read bioclimatic data
