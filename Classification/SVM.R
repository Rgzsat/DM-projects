load(file="C:\\Users\\47406\\Downloads\\Data Mining course\\practice_02\\kdata.RData")

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
train <- train_set[,1:2] # the data we used was initially prepared for the classification example please remove third column
train_classes <- train_set[,3]

trainDF <- as.data.frame(train_set)
testDF <- as.data.frame(test)
#SUPPORT VECTOR MACHINES or SVM
library(e1071)

#INITIALIZATION
svmT1 = svm(as.factor(trainDF$V3) ~ ., data = trainDF, kernel = "linear", scale = FALSE) #SVM using linear kernel - supposing data is linearly separable
print(svmT1)
plot(svmT1, trainDF)

svmT2 = svm(as.factor(trainDF$V3) ~ ., data = trainDF, kernel = "radial", scale = FALSE) #SVM using radial / circular kernel - when data is not linearly separable
print(svmT2)
plot(svmT2, trainDF)

svmT3 = svm(as.factor(trainDF$V3) ~ ., data = trainDF, kernel = "polynomial", scale = FALSE) #SVM using polynomial kernel - when data is not linearly separable
print(svmT3)
plot(svmT3, trainDF)

svmpred <- predict(svmT1, testDF, type="class") #to make it return class values
confusionMatrix3 <- table(testDF$V3, svmpred) #confusion matrix
confusionMatrix3
accTest <- sum(diag(confusionMatrix3)) / sum(confusionMatrix3) #get accuracy
print(paste('Accuracy on Test data', accTest)) 


svmpred <- predict(svmT1, testDF, type="class") #to make it return class values
confusionMatrix3 <- table(testDF$V3, svmpred) #confusion matrix
confusionMatrix3
accTest <- sum(diag(confusionMatrix3)) / sum(confusionMatrix3) #get accuracy
print(paste('Accuracy on Test data', accTest)) 

# Compare all the kernels on SVM for our data.
svmpred1 <- predict(svmT1, testDF, type="class") #prediction - class values for each model
svmpred2 <- predict(svmT2, testDF, type="class") 
svmpred3 <- predict(svmT3, testDF, type="class") 

cf1 <- table(testDF$V3, svmpred1) #confusion matrix for each model
cf2 <- table(testDF$V3, svmpred2)
cf3 <- table(testDF$V3, svmpred3)

accTest1 <- sum(diag(cf1)) / sum(cf1) #getting accuracy for each model
accTest2 <- sum(diag(cf2)) / sum(cf2) 
accTest3 <- sum(diag(cf3)) / sum(cf3) 

print(paste('Linear', accTest1)) #plotting the accuracy results
print(paste('Radial', accTest2))
print(paste('Polynomial', accTest3))
