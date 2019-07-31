###Predicting Building
source("Preprocess.R")
## Start Parallel Processing ------------------
#detectCores() # Detect cores
#cl <- makeCluster(7) # Create Cluster
#registerDoParallel(cl) # Register Cluster
#getDoParWorkers() # Confirm number of core used
#stopCluster(cl) # Stop cluster
#remove(cl) # Remove cluster

###FUNCTIONS -----------------------------
# Returns wrong predictions
wrongPredRows <- function(dataset) {
  errorList <- c()
  x<-1
  for (i in 1:nrow(dataset)){
    if(dataset[i,]$pred != dataset[i,]$BUILDINGID){
      errorList[x] <- i
      x<-x+1
    }
  }
  return(errorList)
}

removeRowsCols <- function(dataset) {
  var1 <- dataset %>% select(-FLOOR, -BUILDINGID)
  # List of WAPs with low/no signal
  noSignalList <- apply(var1, 2,var) == 0
  var1 <- var1[,!noSignalList]
  
  # Remove rows (WAP) where all the values = 100 (WAP was not detected)
  idx <- which(apply(var1[,1:(length(var1))], 1, function(x) all(x > 75 | x < 25)) == T)
  if(!is_empty(idx)){
    var1<-var1[-idx,]
  }
  
  #add buildingid, we need to remove the extra rows first
  ExtractTrain <- select(dataset, FLOOR)
  if(!is_empty(idx)){
    ExtractTrain <- ExtractTrain[-idx,]
  } else {
    ExtractTrain <- as.integer(ExtractTrain$FLOOR)
  }
  
  var1$FLOOR <- ExtractTrain
  return(var1)
}

## ALL BUILDINGS
#All Buildings to check our floor detection accuracy
BsubTraining <- subTraining
BsubTraining$FLOOR <- as.factor(BsubTraining$FLOOR)
levels(BsubTraining$FLOOR) <- c("F0", "F1", "F2", "F3", "F4")

#All Buildings subset for testing
BsubTesting <- subTesting

BsubTesting$FLOOR <- as.factor(BsubTesting$FLOOR)
levels(BsubTesting$FLOOR) <- c("F0", "F1", "F2", "F3", "F4")


## BUILDING 0
#Building 0 to check our floor detection accuracy
B0subTraining <- subTraining %>%
  filter(BUILDINGID == "B0") #%>%

B0subTraining <- removeRowsCols(B0subTraining)

B0subTraining$FLOOR <- as.factor(B0subTraining$FLOOR)
levels(B0subTraining$FLOOR) <- c("F0", "F1", "F2", "F3")

#Building 0 subset for testing
B0subTesting <- subTesting %>%
  filter(BUILDINGID == "B0")

B0subTesting$FLOOR <- as.factor(B0subTesting$FLOOR)
levels(B0subTesting$FLOOR) <- c("F0", "F1", "F2", "F3")

## BUILDING 1
#Building 1 to check our floor detection accuracy
B1subTraining <- subTraining %>%
  filter(BUILDINGID == "B1") #%>%

B1subTraining <- removeRowsCols(B1subTraining)

B1subTraining$FLOOR <- as.factor(B1subTraining$FLOOR)
levels(B1subTraining$FLOOR) <- c("F0", "F1", "F2", "F3")

#Building 1 subset for testing
B1subTesting <- subTesting %>%
  filter(BUILDINGID == "B1")

B1subTesting$FLOOR <- as.factor(B1subTesting$FLOOR)
levels(B1subTesting$FLOOR) <- c("F0", "F1", "F2", "F3")



## BUILDING 2
#Building 2 to check our floor detection accuracy
B2subTraining <- subTraining %>%
  filter(BUILDINGID == "B2")

B2subTraining <- removeRowsCols(B2subTraining)

B2subTraining$FLOOR <- as.factor(B2subTraining$FLOOR)
levels(B2subTraining$FLOOR) <- c("F0", "F1", "F2", "F3", "F4")

#Building 2 subset for testing
B2subTesting <- subTesting %>%
  filter(BUILDINGID == "B2")

B2subTesting$FLOOR <- as.factor(B2subTesting$FLOOR)
levels(B2subTesting$FLOOR) <- c("F0", "F1", "F2", "F3", "F4")

## Model Creation stage ------------------
#Splitting data into training set and testing set for cross validation
inTraining <- createDataPartition(BsubTraining$FLOOR, p = .80, list = FALSE)
training <- BsubTraining[inTraining,]
testing <- BsubTraining[-inTraining,]

###
## C5.0 Model
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", 
                           number = 3, 
                           repeats = 1)

#Set seed to know the random order
set.seed(33)

c5grid <- expand.grid(.winnow = c(FALSE, TRUE), 
                      .trials=c(30), 
                      .model="trees" )
tic()
#train Linear Regression model
C5Fit <- train(FLOOR~., data = training, 
               method = "C5.0", 
               trControl=fitControl, 
               tuneLength = 5)
#tuneGrid = c5grid)
toc()

#c5 model
C5Fit


#Check validation set for accuracy on predicting the building
testing$pred <- predict(C5Fit, newdata = testing)
postResample(testing$pred, testing$FLOOR)

BsubTesting$pred <- predict(C5Fit, newdata = BsubTesting)
postResample(BsubTesting$pred, BsubTesting$FLOOR)
confusionMatrix(BsubTesting$pred, BsubTesting$FLOOR)


## Random Forest Model --------------
rfControl <- trainControl(method = "repeatedcv", 
                          number = 3, 
                          repeats = 1, 
                          search="random")

rfGrid <- expand.grid(mtry=c(214,60))

#Set seed to know the random order
set.seed(33)

tic()
#Creating RF model
rfModel <- train(FLOOR~., data = training, 
                 method = "rf",
                 trControl=rfControl,
                 tuneLength = 3,
                 #tuneGrid=rfGrid, 
                 importance=T)
toc()
#view model
rfModel
rfModel<-readRDS("rfModel-Allfloors.rds")
#saveRDS(rfModel, file = "rfModel-Allfloors.rds")

testing$pred <- predict(rfModel, newdata = testing)
postResample(testing$pred, testing$FLOOR)

BsubTesting$pred <- predict(rfModel, newdata = BsubTesting)
postResample(BsubTesting$pred, BsubTesting$FLOOR)
confusionMatrix(BsubTesting$pred, BsubTesting$FLOOR)


## kNN Model
set.seed(1234)
knnControl = trainControl(method = "repeatedcv",
                          number = 3,
                          repeats = 1,
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary)
tic()
knnModel <- train(FLOOR~. , data = training, method = "knn",
                  #preProcess = c("center","scale"),
                  trControl = knnControl,
                  #metric = "ROC",
                  tuneLength = 2)
toc()
# Summary of model
knnModel

#Check validation set for accuracy on predicting the building
testing$pred <- predict(knnModel, newdata = testing)
postResample(testing$pred, testing$FLOOR)

BsubTesting$pred <- predict(knnModel, newdata = BsubTesting)
postResample(BsubTesting$pred, BsubTesting$FLOOR)
confusionMatrix(BsubTesting$pred, BsubTesting$FLOOR)



## SVM Model
set.seed(63)

SVMControl <- trainControl(method = "repeatedcv", 
                           number = 3, 
                           repeats = 1)
#preProc = c("center", "scale", "range"))
SVMgrid <- expand.grid(C = c(1:20))

#Set seed to know the random order
tic()
SVMModel <- train(FLOOR ~., data = training, method = "svmLinear",
                  trControl=SVMControl,
                  #tuneGrid = SVMgrid
                  tuneLength = 25
)
toc()

#Run model and show output
SVMModel

#Check validation set for accuracy on predicting the building
testing$pred <- predict(SVMModel, newdata = testing)
postResample(testing$pred, testing$FLOOR)

BsubTesting$pred <- predict(SVMModel, newdata = BsubTesting)
postResample(BsubTesting$pred, BsubTesting$FLOOR)
#confusionMatrix(B1subTesting$pred, B1subTesting$FLOOR)


### GBM Model
fitControlGBM <- trainControl(method = "repeatedcv", 
                              number = 3, 
                              repeats = 1)

#Grid to define our own parameters for this classifier
gbmGrid <-  expand.grid(interaction.depth = c(3,5), 
                        n.trees = (5)*50, 
                        shrinkage = c(0.1),
                        n.minobsinnode = 20)

#Set seed to know the random order
set.seed(998)

#Train GBM Regression model
tic()
gbmFit <- train(FLOOR~., data = training, 
                method = "gbm", 
                trControl = fitControlGBM, 
                verbose = FALSE, 
                tuneLength = 5)
#tuneGrid = gbmGrid)
toc()
#training results
gbmFit

#Check validation set for accuracy on predicting the building
testing$pred <- predict(gbmFit, newdata = testing)
postResample(testing$pred, testing$FLOOR)

BsubTesting$pred <- predict(gbmFit, newdata = BsubTesting)
postResample(BsubTesting$pred, BsubTesting$FLOOR)


## Error check --------------
errorList <- wrongPredRows(subTesting)

#get rows with mistakes
temp <- subTesting %>%
  slice(errorList)

#leave only WAPs
temp2 <- select(temp, starts_with("WAP"))

# List of WAPs with low/no signal
noSignalList <- apply(temp2, 2,var) == 0
temp2 <- temp2[,!noSignalList]
temp2<- cbind(temp2, temp[283:287])

# Building Preview 3D
BsubTesting %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~pred, 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

errorcheck <- subTesting %>%
  filter(BUILDINGID == "B0" & FLOOR == 3 & LATITUDE > 4864951 & LATITUDE < 4864999)

