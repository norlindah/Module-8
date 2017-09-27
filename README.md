Prediction Assignment Writeup
================
Norlindah
26 September 2017

Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

Data
----

``` r
#Loading Library
library(caret) 
library(rpart.plot)
library(ggplot2)
library(randomForest)

#Loading Data
 
url.train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url.test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"


#Check the Training and Testing data, identifying the missing data, NA and #DIV/0! as NA everywhere.
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
```

### Prediction Model 1 - Decision Tree

``` r
DTreeModel <- rpart(classe ~ ., data = trainingData, method = "class")
DTreePrediction <- predict(DTreeModel, testingData, type = "class")
```

Plot Decision Tree

``` r
rpart.plot(DTreeModel, main = "Decision Tree", under = T, faclen = 0)
```

![](Module_8_-_Prediction_Assignment_files/figure-markdown_github/unnamed-chunk-3-1.png)

Using confusion matrix to test results

``` r
confusionMatrix(DTreePrediction, testingData$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1094  122   48   80   19
    ##          B   41  454   87   56   60
    ##          C   30   94  523   62   53
    ##          D   22   44   52  403   44
    ##          E   15   43   18   78  565
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.74            
    ##                  95% CI : (0.7263, 0.7533)
    ##     No Information Rate : 0.2927          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.6684          
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9101   0.5997   0.7184  0.59352   0.7625
    ## Specificity            0.9074   0.9272   0.9293  0.95274   0.9542
    ## Pos Pred Value         0.8026   0.6504   0.6864  0.71327   0.7858
    ## Neg Pred Value         0.9606   0.9111   0.9387  0.92208   0.9481
    ## Prevalence             0.2927   0.1843   0.1773  0.16533   0.1804
    ## Detection Rate         0.2664   0.1105   0.1273  0.09813   0.1376
    ## Detection Prevalence   0.3319   0.1700   0.1855  0.13757   0.1751
    ## Balanced Accuracy      0.9088   0.7634   0.8238  0.77313   0.8584

### Prediction model 2 - random forest

``` r
RFModel <- randomForest(classe ~. , data = trainingData, method = "class")
RFPrediction <- predict(RFModel, testingData, type = "class")

confusionMatrix(RFPrediction, testingData$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1202    0    0    0    0
    ##          B    0  757    0    0    0
    ##          C    0    0  728    0    0
    ##          D    0    0    0  679    0
    ##          E    0    0    0    0  741
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9991, 1)
    ##     No Information Rate : 0.2927     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2927   0.1843   0.1773   0.1653   0.1804
    ## Detection Rate         0.2927   0.1843   0.1773   0.1653   0.1804
    ## Detection Prevalence   0.2927   0.1843   0.1773   0.1653   0.1804
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

It show Random Forest accuracy is higher than Decision tree which is 0.9991 &gt; 0.7462.

### The predicted classes for the 20 tests

``` r
testingData2 <- testing
nzvt <- nearZeroVar(testingData2)
testingData2 <- testingData2[, -nzvt]
predictionTest <- predict(RFModel, testingData2)

predictionTest
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

### Conclusion

The Random Forest is a much better predictive model than the Decision Tree, which has a larger accuracy (99.91%).
