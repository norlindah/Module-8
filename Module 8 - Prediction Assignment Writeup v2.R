##Loading library

library(caret) 
library(rpart.plot)
library(ggplot2)
library(randomForest)

##Loading Data
url.train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url.test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(url.train), na.strings = c("NA", "", "#DIV0!"))
testing <- read.csv(url(url.test), na.strings = c("NA", "", "#DIV0!"))

#Cleaning the data
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

# Delete variables that are not related 
trainingData <- training[, -c(1:7)]
testingData <- testing[, -c(1:7)]

PartitionData <- createDataPartition(trainingData$classe,  p = 0.7, list = F)
trainingData <- trainingData[PartitionData, ]
testingData <- trainingData[-PartitionData, ]

##Prediction Model 1 - Decision Tree 
DTreeModel <- rpart(classe ~ ., data = trainingData, method = "class")
DTreePrediction <- predict(DTreeModel, testingData, type = "class")

# Plot Decision Tree
rpart.plot(DTreeModel, main = "Decision Tree", under = T, faclen = 0)

# Using confusion matrix to test results
confusionMatrix(DTreePrediction, testingData$classe)

##Prediction model 2 - random forest
RFModel <- randomForest(classe ~. , data = trainingData, method = "class")
RFPrediction <- predict(RFModel, testingData, type = "class")

confusionMatrix(RFPrediction, testingData$classe)

##Prediction model 2 - random forest
#From the result, it show Random Forest accuracy is higher than Decision tree which is 0.9915 > 0.6644. Therefore, we will use random forest to answer the assignment.

predictionFinal <- predict(RFModel, testingData, type = "class")

#------
#testingData2 <- testing[, colSums(is.na(testingData)) == 0]
#testingData2 <- testingData2[, -(1:7)]

#The predicted classes for the 20 tests 
testingData2 <- testing
nzvt <- nearZeroVar(testingData2)
testingData2 <- testingData2[, -nzvt]

predictionTest <- predict(RFModel, testingData2)
