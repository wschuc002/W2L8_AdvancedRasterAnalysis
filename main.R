# Student: William Schuch & Rik van Berkum
# Team: Geodetic Engineers of Utrecht
# Institute: Wageningen University and Research
# Course: Geo-scripting (GRS-33806)
# Date: 2016-01-12
# Week 2, Lesson 8: Advanced Raster Analysis


rm(list = ls())  # Clear the workspace!
ls() ## no objects left in the workspace

# Installing/updating packages (for the R Markdown)
#install.packages(c("knitr", "yaml", "htmltools", "caTools", "bitops", "rmarkdown"))

# Installing/updating packages (for the R Markdown publish)
#install.packages(c("RCurl", "PKI", "packrat", "rstudioapi"))

# The rsconnect package might be installed after being asked.
# It will create a new folder: rsconnect

# Installing/updating packages the random forest model
install.packages("randomForest")

# load librarys
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(knitr)
library(yaml)
library(htmltools)
library(caTools)
library(bitops)
library(rmarkdown)
library(RCurl)
library(PKI)
library(packrat)
library(rstudioapi)
#library(rsconnect)
library(randomForest)

# Open the web page of my created Document Report #Windows only)
#shell.exec("http://rpubs.com/wschuc002/143014")

# referring to functions in R folder
#source("./R/Preprocessing.R")


## load data
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")

# check out the attributes
GewataB2
# some basic statistics using cellStats()
cellStats(GewataB2, stat=max)
cellStats(GewataB2, stat=mean)
# This is equivalent to:
maxValue(GewataB2)
# what is the maximum value of all three bands?
max(c(maxValue(GewataB2), maxValue(GewataB3), maxValue(GewataB4)))
# summary() is useful function for a quick overview
summary(GewataB2)

# put the 3 bands into a rasterBrick object to summarize together
gewata <- brick(GewataB2, GewataB3, GewataB4)
# 3 histograms in one window (automatic, if a rasterBrick is supplied)
hist(gewata)
?graphics::hist
par(mfrow = c(1, 1)) # reset plotting window
hist(gewata, xlim = c(0, 5000), ylim = c(0, 750000), breaks = seq(0, 5000, by = 100))

pairs(gewata)

ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
plot(ndvi)



# load the data and check it out
# @ can we show a bit more about this data set - where does it come from
# @ can we provide an option to download it themselve for any location?
load("data/vcfGewata.rda")
vcfGewata
plot(vcfGewata)
hist(vcfGewata)

vcfGewata[vcfGewata > 100] <- NA
plot(vcfGewata)
summary(vcfGewata)
hist(vcfGewata)

gewata <- calc(gewata, fun=function(x) x / 10000)
# make a new raster brick of covariates by adding NDVI and VCF layers
covs <- addLayer(gewata, ndvi, vcfGewata)
plot(covs)

names(covs) <- c("band2", "band3", "band4", "NDVI", "VCF")
plot(covs)

# load the training polygons
load("data/trainingPoly.rda")
par(mfrow = c(1, 1)) # reset plotting window
# superimpose training polygons onto ndvi plot
plot(ndvi)
plot(trainingPoly, add = TRUE)

# inspect the data slot of the trainingPoly object
trainingPoly@data

# the 'Class' column is actually an ordered factor type
trainingPoly@data$Class

str(trainingPoly@data$Class)

# we can convert to integer by using the as.numeric() function, 
# which takes the factor levels
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data

# assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, ndvi, field='Code', progress= "text")

# plotting
# define a colour scale for the classes (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue")
# plot without a legend
plot(classes, col=cols, legend=FALSE)
# add a customized legend
legend("topright", legend=c("cropland", "forest", "wetland"), fill=cols, bg="white")

covmasked <- mask(covs, classes)
plot(covmasked)

# combine this new brick with the classes layer to make our input training dataset
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
plot(trainingbrick)


# extract all values into a matrix
valuetable <- getValues(trainingbrick)

# get rid ofthe NA rows
valuetable <- na.omit(valuetable)

# convert to data frame
valuetable <- as.data.frame(valuetable)
head(valuetable, n = 10)
tail(valuetable, n = 10)

# convert the class column into a factor
valuetable$class <- factor(valuetable$class, levels = c(1:3))

val_crop <- subset(valuetable, class == 1)
val_forest <- subset(valuetable, class == 2)
val_wetland <- subset(valuetable, class == 3)

# 1. NDVI
par(mfrow = c(3, 1))
hist(val_crop$NDVI, main = "cropland", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 4000), col = "orange")
hist(val_forest$NDVI, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 4000), col = "dark green")
hist(val_wetland$NDVI, main = "wetland", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 4000), col = "light blue")

par(mfrow = c(1, 1))

# 2. VCF
par(mfrow = c(3, 1))
hist(val_crop$VCF, main = "cropland", xlab = "% tree cover", xlim = c(0, 100), ylim = c(0, 7500), col = "orange")
hist(val_forest$VCF, main = "forest", xlab = "% tree cover", xlim = c(0, 100), ylim = c(0, 7500), col = "dark green")
hist(val_wetland$VCF, main = "wetland", xlab = "% tree cover", xlim = c(0, 100), ylim = c(0, 7500), col = "light blue")

par(mfrow = c(1, 1))

# 3. Bands 3 and 4 (scatterplots)
plot(band4 ~ band3, data = val_crop, pch = ".", col = "orange", xlim = c(0, 1), ylim = c(0, 0.5))
points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")

# 3+. Bands 2 and 3 (scatterplots)
plot(band2 ~ band3, data = val_crop, pch = ".", col = "orange", xlim = c(0, 1), ylim = c(0, 0.5))
points(band2 ~ band3, data = val_forest, pch = ".", col = "dark green")
points(band2 ~ band3, data = val_wetland, pch = ".", col = "light blue")
legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")

# 3+. Bands 2 and 4 (scatterplots)
plot(band2 ~ band4, data = val_crop, pch = ".", col = "orange", xlim = c(0, 1), ylim = c(0, 0.5))
points(band2 ~ band4, data = val_forest, pch = ".", col = "dark green")
points(band2 ~ band4, data = val_wetland, pch = ".", col = "light blue")
legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")

# 3+. Bands 2 and VCF (scatterplots)
plot(band2 ~ NDVI, data = val_crop, pch = ".", col = "orange", xlim = c(0, 1), ylim = c(0, 0.5))
points(band2 ~ NDVI, data = val_forest, pch = ".", col = "dark green")
points(band2 ~ NDVI, data = val_wetland, pch = ".", col = "light blue")
legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")

# construct a random forest model
# covariates (x) are found in columns 1 to 5 of valuetable
# training classes (y) are found in the 'class' column of valuetable
# caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
modelRF <- randomForest(x=valuetable[ ,c(1:5)], y=valuetable$class, importance = TRUE)
plot(modelRF)

# check system time (1:5)
system.time(randomForest(x=valuetable[ ,c(1:5)], y=valuetable$class, importance = TRUE))
#user     system    elapsed 
#24.89    0.73      25.69 

# inspect the structure and element names of the resulting model
modelRF
class(modelRF)
str(modelRF)
names(modelRF)
# inspect the confusion matrix of the OOB error assessment
modelRF$confusion
# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("cropland", "forest", "wetland", "class.error")
rownames(modelRF$confusion) <- c("cropland", "forest", "wetland")
modelRF$confusion

#system.time(varImpPlot(modelRF))
varImpPlot(modelRF)

# check system time (1:4)
system.time(randomForest(x=valuetable[ ,c(1:4)], y=valuetable$class, importance = TRUE))
# user      system    elapsed 
# 21.10     0.64      21.76 

modelRF2 <- randomForest(x=valuetable[ ,c(1:4)], y=valuetable$class, importance = TRUE)

# to make the confusion matrix more readable
colnames(modelRF2$confusion) <- c("cropland", "forest", "wetland", "class.error")
rownames(modelRF2$confusion) <- c("cropland", "forest", "wetland")
modelRF2$confusion
varImpPlot(modelRF2)


# double check layer and column names to make sure they match
names(covs)

names(valuetable)

# predict land cover using the RF model
predLC <- predict(covs, model=modelRF, na.rm=TRUE)
predLC2 <- predict(covs, model=modelRF2, na.rm=TRUE)

# plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("orange", "dark green", "light blue")

plot(predLC, col=cols, legend=FALSE)
legend("bottomright", 
       legend=c("cropland", "forest", "wetland"), 
       fill=cols, bg="white")

plot(predLC2, col=cols, legend=FALSE)
legend("bottomright", 
       legend=c("cropland", "forest", "wetland"), 
       fill=cols, bg="white")

# write 'predLC' and 'predLC2' to the disk
dir.create("./temps", showWarnings = FALSE)
writeRaster(predLC, filename = "./temps/predLC.tif")
writeRaster(predLC2, filename = "./temps/predLC2.tif")

valuetable <- getValues(covs)
head(valuetable)

km <- kmeans(na.omit(valuetable), centers = 3, iter.max = 100, nstart = 10)
# km contains the clusters (classes) assigned to the cells
head(km$cluster)
unique(km$cluster) # displays unique values

# create a blank raster with default values of 0
rNA <- setValues(raster(covs), 0)
# loop through layers of covs
# assign a 1 to rNA wherever an NA is enountered in covs
for(i in 1:nlayers(covs)){
  rNA[is.na(covs[[i]])] <- 1
}
# convert rNA to an integer vector
rNA <- getValues(rNA)

# convert valuetable to a data.frame
valuetable <- as.data.frame(valuetable)
# if rNA is a 0, assign the cluster value at that position
valuetable$class[rNA==0] <- km$cluster
# if rNA is a 1, assign an NA at that position
valuetable$class[rNA==1] <- NA

# create a blank raster
classes <- raster(covs)
# assign values from the 'class' column of valuetable
classes <- setValues(classes, valuetable$class)
plot(classes, legend=FALSE, col=c("dark green", "orange", "light blue"))

system.time(km <- kmeans(na.omit(valuetable), centers = 3, iter.max = 100, nstart = 10))
# user      system    elapsed 
# 161.41    0.68      163.34 

# Make an NA-value raster based on the LC raster attributes
formask <- setValues(raster(predLC), NA)
# assign 1 to formask to all cells corresponding to the forest class
formask[predLC==2] <- 1
plot(formask, col="dark green", legend = FALSE)

# Group raster cells into clumps based on the Queen's Case
if(!file.exists(fn <- "data/clumformask.grd")) {
  forestclumps <- clump(formask, directions=8, filename=fn)
} else {
  forestclumps <- raster(fn)
}
plot(forestclumps)

# assign freqency table to a matrix
clumpFreq <- freq(forestclumps)
head(clumpFreq)
tail(clumpFreq)

pixelArea <- 30*30
clumCount <- 0.5*10000 / pixelArea

# Coerce freq table to data.frame
clumpFreq <- as.data.frame(clumpFreq)
# which rows of the data.frame are only represented by one cell?
str(which(clumpFreq$count < clumCount))
# which values do these correspond to?
str(clumpFreq$value[which(clumpFreq$count < clumCount)])
# put these into a vector of clump ID's to be removed
excludeID <- clumpFreq$value[which(clumpFreq$count < clumCount)]
# make a new forest mask to be sieved
formaskSieve <- formask
# assign NA to all clumps whose IDs are found in excludeID
formaskSieve[forestclumps %in% excludeID] <- NA
# zoom in to a small extent to check the results
# Note: you can define your own zoom by using e <- drawExtent()
e <- extent(c(811744.8, 812764.3, 849997.8, 850920.3))
opar <- par(mfrow=c(1, 2)) # allow 2 plots side-by-side
plot(formask, ext=e, col="dark green", legend=FALSE)
plot(formaskSieve, ext=e, col="dark green", legend=FALSE)

par(opar) # reset plotting window


load("data/lulcGewata.rda")
# check out the distribution of the values
freq(lulcGewata)
hist(lulcGewata)

load("data/LUTGewata.rda")
LUTGewata

lulc <- as.factor(lulcGewata)
# assign a raster attribute table (RAT)
levels(lulc) <- LUTGewata
lulc

classes <- layerize(lulc)
# layer names follow the order of classes in the LUT
names(classes) <- LUTGewata$Class
plot(classes, legend=FALSE)

forest <- raster(classes, 5)
# is equivalent to:
forest <- classes[[5]]
# or (since the layers are named):
forest <- classes$forest
# replace 0's (non-forest) with NA's
forest[forest==0] <- NA
plot(forest, col="dark green", legend=FALSE)
