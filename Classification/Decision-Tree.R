# clear everything and load required libraries/codes
rm(list=ls())

#loading and plotting data
load(file="C:\\Users\\47406\\Downloads\\kdata.RData")

#data plotting - visualization
plot(x[,1],x[,2], col=scales::alpha(x[,3],0.3), pch=20)

# data preparation
set.seed(42) # seed is necessary to make it reproducible
#split the data in proportion 70/30 for training and test purposes.
training_percentage = 70
sample_size <- floor((training_percentage/100) * nrow(x))
train_ind <- sample(seq_len(nrow(x)), size = sample_size)
train_ind
train_set <- x[train_ind, ]
test <- x[-train_ind, ]
train <- train_set[,1:2] # the data we used was initially prepared for the classification example, third column is removed
train_classes <- train_set[,3]

trainDF <- as.data.frame(train_set)
testDF <- as.data.frame(test)

dev.off() #turning off previous plot

#Initialization

#using library tree
library(tree)
# y ~ . forecast y, using all the predictors
treeTrain <- tree(as.factor(trainDF$V3) ~., trainDF) #formula to plot and where data comes from
summary(treeTrain) #some statistics
plot(treeTrain) #plotting of tee structure
text(treeTrain, pretty = 1) #adding text

preds <- predict(treeTrain, testDF, type="class") #to make it return class values
confusionMatrix <- table(testDF$V3, preds) #create confusion matrix
confusionMatrix
accTest <- sum(diag(confusionMatrix)) / sum(confusionMatrix) #get accuracy (ratio of correctly classified instances)
print(paste('Accuracy on Test data', accTest)) 

#using library party -  similar as before
library(party)

treeTrain2 <- ctree(as.factor(trainDF$V3) ~., trainDF)
plot(treeTrain2)

predictModel2 <- predict(treeTrain2, testDF)  
confusionMatrix2 <- table(testDF$V3, predictModel2)
confusionMatrix2
accTest <- sum(diag(confusionMatrix2)) / sum(confusionMatrix2) 
print(paste('Accuracy on Test data', accTest)) 


