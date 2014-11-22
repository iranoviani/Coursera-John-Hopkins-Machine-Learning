setwd("~/Documents/Course/Coursera John Hopkins University Machine Learning/Project")
library(AppliedPredictiveModeling)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)

# Question: How well do people perform barbell lifts by predicting the manner in which they did the exercise

# Data: Weight Lifting Exercise Dataset
trainingData = read.csv("pml-training.csv")

# Data preprocessing
# Removing irrelevant columns: 
# X: the transaction ordinal number                   
# user_name: the participant name           
# raw_timestamp_part_1: the timestamp
# raw_timestamp_part_2: the timestamp
# cvtd_timestamp: the timestamp   
trainingData <- trainingData[,6:160]
names(trainingData)

# Removing the columns with all NA values more than 20% of total rows
totalRow <- nrow(trainingData)
colNames <- numeric(0)

for(x in 1:ncol(trainingData))
{
    totalNoValueRow <- sum(is.na(trainingData[,x]))
        
    if(totalNoValueRow > (totalRow * 0.2))
    {
        colNames <- c(colNames, colnames(trainingData[x]))
    }
    
    x <- x + 1 
}

trainingData <- trainingData[,!colnames(trainingData) %in% colNames]

# Removing columns with empty values
totalRow <- nrow(trainingData)
colNames2 <- numeric(0)

for(x in 1:ncol(trainingData))
{
    totalNoValueRow <- nrow(trainingData[trainingData[x] == "", ])
    
    if(totalNoValueRow > (totalRow * 0.2))
    {
        colNames2 <- c(colNames2, colnames(trainingData[x]))
    }
    
    x <- x + 1 
}

trainingData <- trainingData[,!colnames(trainingData) %in% colNames2]


# Convert the Factor column to numeric 
trainingData$new_window <- as.numeric(trainingData$new_window)


# Create data partition between training and testing
inTrain <- createDataPartition(y = trainingData$classe, p = 0.6, list = FALSE)
training <- trainingData[inTrain,]
testing <- trainingData[-inTrain,]


# Train the data with Random Forest model
set.seed(32323)
modelFit10 <- randomForest(training$classe ~ ., data = training, ntree = 10)
modelFit10

modelFit15 <- randomForest(training$classe ~ ., data = training, ntree = 15)
modelFit15

modelFit25 <- randomForest(training$classe ~ ., data = training, ntree = 25)
modelFit25

confusionMatrix(modelFit25$predicted, training$classe)

rfImp <- varImp(modelFit25)


# Predict the test data using the fit model
prediction <- predict(modelFit25, newdata = testing)
confusionMatrix(prediction, testing$classe)
table(prediction, testing$classe)












