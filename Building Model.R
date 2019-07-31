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

BsubTraining <- select(subTraining, starts_with("WAP"), BUILDINGID)
BsubTraining <- subTraining %>% 
  select(starts_with("WAP"), BUILDINGID) %>%
  sample_n(2000)


## Model Creation stage ------------------
#Splitting data into training set and testing set for cross validation
inTraining <- createDataPartition(BsubTraining$BUILDINGID, p = .75, list = FALSE)
training <- BsubTraining[inTraining,]
testing <- BsubTraining[-inTraining,]

###
## C5.0 Model
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", 
                           number = 12, 
                           repeats = 3)

#Set seed to know the random order
set.seed(998)

c5grid <- expand.grid(.winnow = c(FALSE, TRUE), 
                      .trials=c(30), 
                      .model="rules" )
tic()
#train Linear Regression model
C5Fit <- train(BUILDINGID~., data = training, 
               method = "C5.0", 
               trControl=fitControl, 
               tuneLength = 3,
               tuneGrid = c5grid)
toc()
# View C5Fit Model
C5Fit

#Check validation set for accuracy on predicting the building
subTesting$pred <- predict(C5Fit, newdata = subTesting)
postResample(subTesting$pred, subTesting$BUILDINGID)
confusionMatrix(subTesting$pred, subTesting$BUILDINGID)


## kNN Model
set.seed(1234)
knnControl = trainControl(method = "repeatedcv",
                          number = 3,
                          repeats = 1)
                          #classProbs = TRUE,
                          #summaryFunction = multiClassSummary)

tic()
knnModel <- train(BUILDINGID~. , data = training, method = "knn",
                  #preProcess = c("center","scale"),
                  trControl = knnControl,
                  #metric = "ROC",
                  tuneLength = 3)
toc()
# Summary of model
knnModel


testing$pred <- predict(knnModel, newdata = testing)
postResample(testing$pred, testing$BUILDINGID)

subTesting$pred <- predict(knnModel, newdata = subTesting)
postResample(subTesting$pred, subTesting$BUILDINGID)
confusionMatrix(subTesting$pred, subTesting$BUILDINGID)


## SVM Model
set.seed(50)

SVMControl <- trainControl(method = "repeatedcv", 
                           number = 3, 
                           repeats = 1)
                           #preProc = c("center", "scale", "range"))
SVMgrid <- expand.grid(C = c(0.2500127321))

#Set seed to know the random order
tic()
SVMModel <- train(BUILDINGID ~., data = training, method = "svmLinear",
                  trControl=SVMControl,
                  #tuneGrid = SVMgrid
                  tuneLength = 1
)
toc()

#Run model and show output
SVMModel


testing$pred <- predict(SVMModel, newdata = testing)
postResample(testing$pred, testing$BUILDINGID)

subTesting$pred <- predict(SVMModel, newdata = subTesting)
postResample(subTesting$pred, subTesting$BUILDINGID)
confusionMatrix(subTesting$pred, subTesting$BUILDINGID)

##Error check procedure -------------
errorList <- wrongPredRows(errorcheck)

#get rows with mistakes
temp <- errorcheck %>%
  slice(errorList)

#leave only WAPs
temp2 <- select(temp, starts_with("WAP"))

# List of WAPs with low/no signal
noSignalList <- apply(temp2, 2,var) == 0
temp2 <- temp2[,!noSignalList]
temp2<- cbind(temp2, temp[283:287])

write.csv(temp2, "ww.csv")

# Building Preview 3D
subTesting %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~pred, 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")),
         legend = list(x = 0.1, y = 0.9))

errorcheck <- subTesting %>%
  filter(BUILDINGID == "B2" & FLOOR == 4)



tic()
rangerModel <- 
  rand_forest(mode = "regression", mtry = 12) %>%
  set_engine("ranger", seed = 63555) %>%
  fit_xy(x = select(BsubTraining, starts_with("WAP")), y = BsubTraining$BUILDINGID)
toc()

rangerModel

testing$pred <- predict(rangerModel, testing)
postResample(testing$BUILDINGID, unlist(testing$pred))

subTesting$pred <- unlist(predict(rangerModel, subTesting))
postResample(subTesting$BUILDINGID, subTesting$pred)
confusionMatrix(subTesting$pred, subTesting$BUILDINGID)
