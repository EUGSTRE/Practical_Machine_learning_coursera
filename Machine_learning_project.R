# libraries
library(lattice)
library(ggplot2)
library(caret)
library(kernlab)
library(rattle)
library(corrplot)
set.seed(0000)

# We first load the data sets from the sanbox
traincsv <- read.csv("pml-training.csv")
testcsv <- read.csv("pml-testing.csv")

# We then polish the data and remove irrelevant columns 
traincsv <- traincsv[,colMeans(is.na(traincsv)) < .9] 
traincsv <- traincsv[,-c(1:7)] 
nvz <- nearZeroVar(traincsv)
traincsv <- traincsv[,-nvz]
dim(traincsv)

# We split the data in validation and training set t0% to 30%
inTrain <- createDataPartition(y=traincsv$classe, p=0.7, list=F)
train <- traincsv[inTrain,]
valid <- traincsv[-inTrain,]

# We run the model and set fixed training parameters 
control <- trainControl(method="cv", number=3, verboseIter=F) 

## I choose the random forest for this exercise 
mod_rf <- train(classe~., data=train, method="rf", trControl = control, tuneLength = 5)
pred_rf <- predict(mod_rf, valid)
cmrf <- confusionMatrix(pred_rf, factor(valid$classe))


# model evaluation
View(cmrf)

# using random forrest for the test set
print("Random forrest for test set")
pred <- predict(mod_rf, testcsv)
print(pred)


