---
title: "Machine Learning Assignment Writeup"
author: "Agape"
date: "30. Oktober 2016"
output: html_document
---

##Synopsis
###Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

This Machine Learning Write UP predicts the achievement of participants exercise,
which are represented by categorical variables,classe A, B, C, D, E.
Two model choices for training the predictor are applied. The Random Forest confirms high Accuracy which is than used for answering 20 test cases.


###DATA:  Processing / Loading / Importing / Cleaning / Splitting
####Prepare Workspace
install.packages('rattle');
install.packages('rpart');
install.packages('rpart.plot');
install.packages('RColorBrewer');
library(rattle);
library(rpart.plot);
library(RColorBrewer);
library(caret) ;
library(repmis);
library(randomForest);
###Process the  Data
```{r, echo=TRUE}
testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))
training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
             
```
Summary of the dataset:
Dataset training has 1'9622 observations and 160 variables, where the testing dataset has 20 observations with same number of variables as training-dataset.The prediction of outcome will be applied on the training-dataset.
###CLEANING
Columns to be deleted within the training set which contain missing values.

```{r, echo=TRUE}
testing <-    testing[, colSums(is.na(testing)) == 0]
training <-   training[, colSums(is.na(training)) == 0]

```

```{r, echo=TRUE}
trainData <- training[, -c(1:7)]
testData <- testing[, -c(1:7)]
            
```
###Spliting
Split the cleaned training set trainData (70%/20%) into training set ( computation of out-of-sample errors)


```{r}
library(caret)

```



```{r, echo=TRUE}

set.seed(8000) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
train <- trainData[inTrain, ]
valid <- trainData[-inTrain, ]

```
###Prediction Algorithms (classification trees and random forests) for prediciton of the outcome.
###Classification trees
```{r, echo=TRUE}
control <- trainControl(method = "cv", number = 5)
fit_rpart <- train(classe ~ ., data = train, method = "rpart", trControl = control)
print(fit_rpart, digits = 4)
```
```{r, echo=FALSE}


if (!require("rpart.plot")) {
  install.packages("rpart.plot", repos="http://cran.rstudio.com/") 
  library("rpart.plot")
}


if (!require("rattle")) {
  install.packages("rattle", repos="http://cran.rstudio.com/") 
  library("rattle")
}


if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", repos="http://cran.rstudio.com/") 
  library("RColorBrewer")
}



```

```{r, echo=TRUE}
fancyRpartPlot(fit_rpart$finalModel)
```
###Outcome prediction applied on validation set

```{r, echo=TRUE}
prediction_rpart <- predict(fit_rpart, valid)
#Prediction Results
(conf_rpart <- confusionMatrix(valid$classe, prediction_rpart))
           
```

```{r, echo=TRUE}
(accuracy_rpart <- conf_rpart$overall[1])
```
The accuracy rate is 0.49. Use of classification tree is suboptimal  

### Apply Random forests for optimization 
```{r, echo=TRUE}
#library(randomForest)
fit_rf <- train(classe ~ ., data = train, method = "rf", trControl = control)
print(fit_rf, digits = 4)
```
### prediction of outcomes using validation set
```{r, echo=TRUE}
predict_rf <- predict(fit_rf, valid)
# prediction result
(conf_rf <- confusionMatrix(valid$classe, predict_rf))
```
```{r, echo=TRUE}
(accuracy_rf <- conf_rf$overall[1])
```
Very high Accuracy achieved with Random Forest, 99.42%
###Use random forests to predict the outcome variable classe for the testing set which are in fact the answers for the quiz.

```{r, echo=TRUE}
(predict(fit_rf, testData))
```





































