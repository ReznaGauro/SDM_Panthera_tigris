# ############### Spatial modeling of Panthera tigris in Nepal ################
###############################################################################

library(dismo)
library(biomod2)
library(raster)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(geodata)

# Download the occurrence data for Panthera tigris from GBIF
tiger <- gbif("panthera", "tigris*", sp=TRUE)

# change the projection to WGS84
crs(tiger) <-"epsg:4326" 
tiger <- vect(tiger)

# Read the Nepal shapefile
nepal <- vect("C:/Users/Jado/Documents/EAGLE/Semester2/Spatial_modeling/Exam_task/SDM_Panthera_tigris/SDM_Panthera_tigris/Nepal_shpfile/Nepal_new.gpkg")

# clip the occurence data to nepal
tiger <- crop(tiger, nepal)


# ########### Let's now use Random Forest to model #############
# ############ the distribution of Panthera tigris #############
###############################################################
library(randomForest)

# Set the working directories
dirpath <- "C:/Users/Jado/Documents/EAGLE/Semester2/Spatial_modeling/Exam_task/SDM_Panthera_tigris/SDM_Panthera_tigris/Data_ENV/wc2.1_2.5m"

# Download Bioclim data at 2.5 minutes resolution
# envdata <- rast(worldclim_global(var="bio", res=2.5, path= dirpath))

# List the environmental data from the folder
envdata <- list.files(dirpath, pattern = ".tif$", full.names = TRUE)

# Read each raster file into a list
env_list <- sapply(envdata, rast, simplify = FALSE)

# Assign all env data a crs:4326
env_list <- lapply(env_list, function(x) {crs(x) <- "EPSG:4326"; x})

# Crop each raster to the extent of Nepal using mask
env_clipped <- lapply(env_list, function(x) mask(x, nepal))

# Add occurrence data and Nepal shapefile to the plot and add the first env layer
plot(nepal)
plot(env_clipped[[1]], add=T)
plot(tiger, add = TRUE, col = "orange", pch = 19)

# Check the names of all env layers
names(env_clipped)

# Subset the env layers to bio1, bio5, bio6, bio7, bio8, bio12, bio16, bio17 
# from the env_clipped
env <- env_clipped[c(1,4,8, 9, 15,16,17, 18)]
names(env)
env <- rast(env)

# Rename each layer to a more meaningful name
names(env) <- c("mean annual air temperature", "annual precipitation amount", "mean monthly precipitation amount of the wettest quarter",
                "mean monthly precipitation amount of the driest quarter", "Precip.Wettest",
                "mean daily maximum air temperature of the warmest month", "mean daily minimum air temperature of the coldest month", "annual range of air temperature")

names(env) <- c("MeanTemp.", "AnnualPrecip.", "MeanPrecip.Wettest", "MeanPrecip.Driest", "Precip.Wettest", "MaxTemp.Warmest", "MinTemp.Coldest", "Temp.Range")
# Set the minimum and maximum longitude and latitude
xmin <- 80
xmax <- 89 
ymin <- 26 
ymax <- 31 

# Adjust plot to specified extent
plot(env, xlim = c(xmin, xmax), ylim = c(ymin, ymax))

# Generate some background points for the study area
set.seed(123)
back <- spatSample(env, size=10000, as.points=TRUE, method="random", na.rm=TRUE)

# convert to sf object 
back<-st_as_sf(back)

# check number of background points
nrow(back)
# get environmental covariates at presence locations
eP<-extract(env,tiger)

# create data frames from values
Pres.cov<-data.frame(eP,Pres=1)

# Remove the first column which is just an ID field.
Pres.cov<-Pres.cov[,-1]

# check the data
summary(Pres.cov)

# Remove NA values
Pres.cov<-Pres.cov[!is.na(Pres.cov$MeanTemp.),]

head(Pres.cov)

# create data frame for background values 
Back.cov<-data.frame(st_drop_geometry(back),Pres=0)

# create data frame for background values 
Back.cov<-data.frame(st_drop_geometry(back),Pres=0)

# combine
all.cov<-rbind(Pres.cov,Back.cov)

# let's first have a look with Bioclim model
# Biolclim() function performs a distribution model based on 
# the envelope modelling approach and takes presence data only
# It assesses the suitability of a location by comparing 
# the values of environmental variables at that location to the distribution of 
# values at known presence locations
bc_tiger <- bioclim(Pres.cov[,c('MeanTemp.', 'Temp.Range', 'AnnualPrecip.')])

# Predict the model
bioclim.map <- predict(env, bc_tiger)

# plot
plot(bioclim.map, xlim = c(xmin, xmax), ylim = c(ymin, ymax), main = "bioclim: MeanTemp.,Temp.Range,AnnualPrecip.")

# Predict from all bioclimatic variables
bc_tiger2<-bioclim(Pres.cov[,1:8])

bc_tiger2

# Predict the model with all the bioclimatic variables
bioclim.map2 <- predict(env, bc_tiger2)

plot(bioclim.map2, xlim = c(xmin, xmax), ylim = c(ymin, ymax), main = "bioclimAll")


# The randomForest model takes two arguments - parameters that can be tuned 
# mtry and ntree. mtry refers to the number of predictor variables to try at 
# each iteration of the model and and ntree specifies the number of trees grown.
# let's use 500 as the default value which is mostly used. Model performance is 
# sensitive to mtry than ntree and this can be tuned with the tuneRF function.


# tune mtry
tuneRF(x=all.cov[,1:8],y=as.factor(all.cov$Pres)) #checking for model fitting

# If the Out of Box (OOB) error statistic is lowest with an mtry of 1 
# so, go with that for fitting the model

rf.tiger<-randomForest(as.factor(Pres)~.,mtry=1,ntree=500,data=all.cov)

# use the varImpPlot() function to get a measure of variable importance
varImpPlot(rf.tiger)

# Now for better evaluation, let's divide the data to the testing and training subsets,
# where in 5 folds, testing:training is 1:4, iteratively.

# set number of folds to use
folds=5

# presence and absence data according to folds 
kfold_pres <- kfold(Pres.cov, folds)
kfold_back <- kfold(Back.cov, folds)

# create an empty list to hold the results
eRF<-list()
par(mfrow=c(2,3))

for (i in 1:folds) {
  train <- Pres.cov[kfold_pres!= i,]
  test <- Pres.cov[kfold_pres == i,]
  backTrain<-Back.cov[kfold_back!=i,]
  backTest<-Back.cov[kfold_back==i,]
  dataTrain<-rbind(train,backTrain)
  dataTest<-rbind(test,backTest)
  RF_eval <- randomForest(as.factor(Pres)~., data=dataTrain)#this is the RF model
  rf.pred <- predict(RF_eval, type="prob")[,2]#make prediction
  eRF[[i]]<-evaluate(p = rf.pred[which(dataTrain$Pres == "1")], #set p to be 1s from the dataTrain object 
                     a = rf.pred[which(dataTrain$Pres == "0")])# set a to be 0s from the dataTrain object
  
  #check the AUC by plotting ROC values
  
  plot(eRF[[i]],'ROC')
  
}

# Check the results

eRF

# Get model evaluation statistics:

aucRF <- sapply( eRF, function(x){slot(x, 'auc')} )

# Get maxTPR+TNR for the Random Forest model

Opt_RF<-sapply( eRF, function(x){ x@t[which.max(x@TPR + x@TNR)] } )

Opt_RF
Mean_OptRF <- mean(Opt_RF)

# Let's predict and map the distribution estimate
prRF <- predict(env, rf.tiger,type="prob",index=2)
par(mfrow=c(1,2))
plot(prRF, xlim = c(xmin, xmax), ylim = c(ymin, ymax), main='Random Forest Prediction for Panthera tigris in Nepal')

plot(prRF > Mean_OptRF, xlim = c(xmin, xmax), ylim = c(ymin, ymax), main='presence/absence')

# ############################################################################################
# ####################### End ###############################################################
