##################################################################
# Remove WAPs with signals of 100 or below -90 on both datasets
# If column doesnt exist in validation, remove from training (opposite doesnt matter)
# 

#Load Libs
if(!require(pacman)) install.packages("pacman")
p_load(dplyr,caret, tictoc, plotly, lubridate, parsnip, doParallel, ggplot2)

# Uploading Training and Validation dataset -------------------------------
trainingData <- readRDS("data/trainingData.rds")
testData <- readRDS("data/validationData.rds")

## Preprocessing stage -------------------
## TRAINING DATASET
# Create subset variables
subTraining <- select(trainingData, starts_with("WAP"))

# For training, remove low/no signals by converting to 100 first
subTraining[subTraining > -30 ] <- 100
subTraining[subTraining < -80 ] <- 100
subTraining[subTraining == 100 ] <- -104
subTraining <- subTraining + 104


# List of WAPs with low/no signal
noSignalList <- apply(subTraining, 2,var) == 0
subTraining <- subTraining[,!noSignalList]

# Remove rows (WAP) where all the values = 100 (WAP was not detected)
idx <- which(apply(subTraining[,1:length(subTraining)], 
                   1, function(x) all(x == 0)) == T)
subTraining<-subTraining[-idx,]


#add buildingid, we need to remove the extra rows first
ExtractTrain <- select(trainingData, BUILDINGID, FLOOR, LONGITUDE, LATITUDE)
ExtractTrain <- ExtractTrain[-idx,]
ExtractTrain$BUILDINGID <- as.factor(ExtractTrain$BUILDINGID)
# ExtractTrain$FLOOR <- as.factor(ExtractTrain$FLOOR)
# levels(ExtractTrain$FLOOR) <- c("F0", "F1", "F2", "F3", "F4")
levels(ExtractTrain$BUILDINGID) <- c("B0", "B1", "B2")

#bind the 2 dataframes
subTraining<-cbind(subTraining, ExtractTrain)



## TESTING DATASET
# Create subset variables
subTesting <- select(testData, starts_with("WAP"))

# For Testing dataset, remove low/no signals by converting to 100 first
subTesting[subTesting > -30 ] <- 100
subTesting[subTesting < -80 ] <- 100
subTesting[subTesting == 100 ] <- -104
subTesting <- subTesting + 104

# List of WAPs with low/no signal
noSignalListTesting <- apply(subTesting, 2,var) == 0
subTesting <- subTesting[,!noSignalListTesting]

# Remove rows (WAP) where all the values = 100 (WAP was not detected)
idy <- which(apply(subTesting[,1:length(subTesting)], 
                   1, function(x) all(x == 0)) == T)
subTesting<-subTesting[-idy,]

#add buildingid, we need to remove the extra rows first
ExtractTest <- select(testData, BUILDINGID, LONGITUDE, LATITUDE, FLOOR)
ExtractTest <- ExtractTest[-idy,]
ExtractTest$BUILDINGID <- as.factor(ExtractTest$BUILDINGID)
#ExtractTest$FLOOR <- as.factor(ExtractTest$FLOOR)
#levels(ExtractTest$FLOOR) <- c("F0", "F1", "F2", "F3", "F4")
levels(ExtractTest$BUILDINGID) <- c("B0", "B1", "B2")

subTesting<-cbind(subTesting, ExtractTest)
# Making same columns for Testing and Validation set ----------------------
# Validation set will have more columns but it is not a problem
# If Training set has more column than the Traning one, the additional columns have to be deleted
subTraining <- subTraining[, which(colnames(subTraining) %in% colnames(subTesting))]

# Delete extra vars that we dont require anymore
rm(idx, idy, noSignalList, noSignalListTesting, ExtractTrain, ExtractTest)



# Change WAP values so that no signal is 0 and highest signal is 104
# Training Data
# subTraining[subTraining == 100] <- -105
# subTraining[,1:249] <- subTraining[,1:249] + 105
# 
# # Test data
# subTesting[subTesting == 100] <- -105
# subTesting[,1:282] <- subTesting[,1:282] + 105

## Converting longitude and latitude to a smaller range (faster calculations)
## we didnt divide to preserve them in meters
#formatC(max(subTesting$LONGITUDE), format = "f", digits = 30)
latTrainRange <- max(subTraining$LATITUDE) - min(subTraining$LATITUDE)
subTraining$LATITUDE <- normalize(subTraining$LATITUDE, method = "range",
                                  range = c(0, latTrainRange), margin = 2)

longTrainRange <- max(subTraining$LONGITUDE) - min(subTraining$LONGITUDE)
subTraining$LONGITUDE <- normalize(subTraining$LONGITUDE, method = "range",
                                   range = c(0, longTrainRange), margin = 2)

latTestRange <- max(subTesting$LATITUDE) - min(subTesting$LATITUDE)
subTesting$LATITUDE <- normalize(subTesting$LATITUDE, method = "range",
                                 range = c(0, latTestRange), margin = 2)

longTestRange <- max(subTesting$LONGITUDE) - min(subTesting$LONGITUDE)
subTesting$LONGITUDE <- normalize(subTesting$LONGITUDE, method = "range",
                                  range = c(0, longTestRange), margin = 2)

rm(latTestRange, longTestRange, latTrainRange, longTrainRange)



# # Center the matrix at the origin
# subTraining$cLATITUDE <- subTraining$LATITUDE - mean(subTraining$LATITUDE)
# subTraining$cLONGITUDE <- subTraining$LONGITUDE - mean(subTraining$LONGITUDE)
# 
# X <- data.frame() 
# X <- cbind(Long = subTraining$cLONGITUDE,Lati = subTraining$cLATITUDE)
# 
# 
# X <- as.data.frame(t(X))
# X <- matrix(X)
# # Scaling matrix
# sx <- 1
# sy <- 1
# 
# Scale <- matrix(c(sx, 0, 0, sy),
#                 nrow=2,
#                 ncol=2,
#                 byrow = TRUE) 
# 
# # Rotation matrix
# theta = 0.77*pi
# c <- cos(theta)
# s <- sin(theta)
# Rot <- matrix(c(c, -s, s, c),
#               nrow=2,
#               ncol=2,
#               byrow = TRUE) 
# 
# # Transformation matrix 
# Trans = Scale*Rot
# Trans = matrix(Trans)
# 
# # Apply transformation matrix to X
# testt <- X %*% Trans
# testt <- X * Trans
# 
# t(X)%*%as.matrix(X)
# Trans
# # Calculate covariance matrix
# cov_mat(Y.T)


# 
# tmpTraining <- select(subTraining, starts_with("WAP"))
# 
# # dBm to Quality:
# #convert out of bound to max/min value
# tmpTraining[tmpTraining == 100] <- -104
# tmpTraining[tmpTraining >= -30] <- -30
# tmpTraining[tmpTraining <= -80] <- -104
# tmpTraining <- tmpTraining + 104
# 
# 
# #convert dBm to Quality then convert back to dataframe
# #tmpTraining <- apply(tmpTraining, MARGIN = c(1,2), function(x) {2 * (x - -80)})
# tmpTraining <- as.data.frame(tmpTraining)
# 
# subTraining <- cbind(tmpTraining, select(subTraining, BUILDINGID, FLOOR, LONGITUDE, LATITUDE))
# 
# 
# 
# 
# 
# tmpTesting <- select(subTesting, starts_with("WAP"))
# 
# # dBm to Quality:
# #convert out of bound to max/min value
# tmpTesting[tmpTesting == 100] <- -104
# tmpTesting[tmpTesting >= -30] <- -30
# tmpTesting[tmpTesting <= -80] <- -104
# 
# #convert dBm to Quality then convert back to dataframe
# #tmpTesting <- apply(tmpTesting, MARGIN = c(1,2), function(x) {2 * (x - -80)})
# tmpTesting <- as.data.frame(tmpTesting)
# 
# subTesting <- cbind(tmpTesting, select(subTesting, BUILDINGID, FLOOR, LONGITUDE, LATITUDE))
# 
# 
# 
# test <- subTesting %>%
#   select(starts_with("WAP"))
# testVars <- select(subTesting, BUILDINGID, FLOOR, LONGITUDE, LATITUDE)
# 
# idy <- which(apply(test[,1:length(test)], 
#                    1, function(x) all(x < 1 | x > 99)) == T)
# test<-test[-idy,]
# testVars <- testVars[-idy,]
# subTesting <- cbind(test, testVars)
