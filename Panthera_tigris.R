# ############### Spatial modeling of Panthera tigris in Nepal ################
###############################################################################

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
bioclim <- 

# ########### Let's now use Random Forest to model #############
# ############ the distribution of Panthera tigris #############
###############################################################
library(randomForest)

# Generate some background points for the study area
set.seed(123)
back <- spatSample(env, size=1000, as.points=TRUE, method="random", na.rm=TRUE)

#convert to sf object 
back<-st_as_sf(back)

#check number of background points
nrow(back)
# get environmental covariates at presence locations
eP<-extract(env,panthera)

#create data frames from values
Pres.cov<-data.frame(eP,Pres=1)

#Remove the first column which is just an ID field.
Pres.cov<-Pres.cov[,-1]

#check the data
summary(Pres.cov)

# Remove NA values
Pres.cov<-Pres.cov[!is.na(Pres.cov$MeanTemp.),]

head(Pres.cov)

#create data frame for background values 
Back.cov<-data.frame(st_drop_geometry(back),Pres=0)

#create data frame for background values 
Back.cov<-data.frame(st_drop_geometry(back),Pres=0)

#combine
all.cov<-rbind(Pres.cov,Back.cov)

# Create a model
# Biolclim() function performs a distribution model based on 
# the envelope modelling approach and takes presence data only
# The Bioclim() algorithm assess the suitability of a location by comparing 
# the values of environmental variables at that location to the distribution of 
# values at known presence locations
bc_panthera <- bioclim(Pres.cov[,c('MeanTemp.', 'Temp.Range', 'AnnualPrecip.')])

# Predict the model
bioclim.map <- predict(env, bc_panthera)

#plot
plot(bioclim.map, axes = F, box = F, main = "bioclim: MeanTemp.,Temp.Range,AnnualPrecip.")

# Predict from other bioclimatic variables
bc_panthera2<-bioclim(Pres.cov[,1:8])

bc_panthera2

bioclim.map2 <- predict(env, bc_panthera2)

plot(bioclim.map2, axes = F, box = F, main = "bioclimAll")


# The randomForest model takes two arguments - parameters that can be tuned 
# mtry and ntree. mtry refers to the number of predictor variables to try at 
# each iteration of the model and and ntree specifies the number of trees grown.
# 500 is the default value often used. Model performance is thought to be more 
# sensitive to mtry than ntree and this can be tuned with the tuneRF function.


#tune mtry
tuneRF(x=all.cov[,1:8],y=as.factor(all.cov$Pres)) #checking for model fitting

# If the Out of Box (OOB) error statistic is lowest with an mtry of 1 
# so, go with that for fitting the model

rf.Panthera<-randomForest(as.factor(Pres)~.,mtry=1,ntree=500,data=all.cov)

# use the varImpPlot() function to get a measure of variable importance
varImpPlot(rf.Panthera)

#create an empty list to hold the results
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

#inspect

eRF

#Get model evaluation statistics:

aucRF <- sapply( eRF, function(x){slot(x, 'auc')} )

#Get maxTPR+TNR for the Random Forest model

Opt_RF<-sapply( eRF, function(x){ x@t[which.max(x@TPR + x@TNR)] } )

Opt_RF

# Let's predict and map the distribution estimate
prRF <- predict(env, rf.Panthera,type="prob",index=2)
par(mfrow=c(1,2))
plot(prRF, main='Random Forest Prediction for Panthera tigris in Nepal')

plot(prRF > Mean_OptRF, main='presence/absence')

