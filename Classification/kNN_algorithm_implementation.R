# Implementation of k-NN classification algorithm

# clear everything and load required libraries/codes
rm(list=ls())
library(car)
library(scales)
library(plyr)

#loading and plotting data
load(file="C:\\Users\\47406\\Downloads\\Data Mining course\\practice_02\\kdata.RData")

#data plotting - visualization
plot(x[,1],x[,2], col=scales::alpha(x[,3],0.3), pch=20)

# data preparation
set.seed(42) # seed is necessary to make it reproducible
# split the data in proportion 70/30 for training and test purposes
training_percentage = 70
sample_size <- floor((training_percentage/100) * nrow(x))
train_ind <- sample(seq_len(nrow(x)), size = sample_size)
train_ind
train_set <- x[train_ind, ]
test <- x[-train_ind,]
train <- train_set[,1:2] # the data we used was initially prepared for the classification example please remove third column
train_classes <- train_set[,3]

dev.off() #turning off previous plot


#distance functions

#***** NOTE!! call your own functions, they are needed to run the whole script properly

euclideanDist <- function(a, b){  #call to your implementation
  dimensions=length(a)
  sqd<-matrix(, dimensions,1)
  for(i in seq(along=a)){
      sqd[i]<-(a[i]-b[i])^2
    }
    dist<-sqrt(colSums(sqd))
  
  # please add here Manhattan, Canberra, Minkowsky and whatever distances are appropriate
  return(dist)
}
manhattanDist <- function(a, b){  #call to your implementation
  #implement it
}
chebyshevDist <- function(a, b){ #call to your implementation
  #implement it
  return(max(d))
}

#k-Nearest neighbors implementation
k=7
nrowsTest <- dim(test)[1] #number of data points of Test data set
ncolsTest <- dim(test)[2] #number of features/cols on Test data set
nrowsTrain <- dim(train)[1] #number of data points on Train data set
distMatrix <- matrix(0, nrow=nrowsTest, ncol=nrowsTrain) #creation of distance Matrix, with shape rows = #test by cols = #train 
cat("The number of test data points is", nrowsTest, "\n")
cat("Running k-Nearest Neighbors with k=",k, "\n") 
for(testPoint in c(1:nrowsTest)) {
  for(trainPoint in c(1:nrowsTrain)) {
    distMatrix[testPoint,trainPoint] <- euclideanDist(test[testPoint,1:2], train[trainPoint,1:2])
  }
}

#Finding k-nearest neighbors labels
results <- matrix(NA, nrow=nrowsTest, ncol=ncolsTest+1) #results matrix where test data + predicted label will be saved
for(vector in c(1:nrowsTest)) { #loop through all the test points
  sortDist <- sort(distMatrix[vector,], index.return=TRUE) #sort from lowest (closest) distance to highest (farthest)
  sortDistK <- sortDist$ix[1:k] #select just the closest k nearest neighbors
  labels <- train_classes[sortDistK] #get labels for the k nerest neighbors
  #label decision
  indiv_labels = unique(labels) #get unique labels list
  max_value = 0 #majority of vote storage
  for(i in indiv_labels){  #loop through all the unique labels of the nearest neighbors
    value = sum(labels == i) #how many data points have each label
    if(value >= max_value){ #updating max value if value is bigger than previous max
      max_label = i #getting max label value
      max_value = value #get count of how many points
    }
  }
  forecasted_label = max_label #assigning/forecasting label based on majority of vote
  new_vector <- matrix(c(test[vector,1:3],forecasted_label),nrow=1) #creating the row vector to be inserted on results
  results[vector,1:4] <- new_vector #adding the vector to results matrix
}

plot(train[,1],train[,2], col=scales::alpha(train_classes,0.2), pch=17)  #plotting training set
points(test[,1],test[,2], col=scales::alpha(results[,3],0.5), pch=1) #plotting test set real label / empty dot - border
points(test[,1],test[,2], col=scales::alpha(results[,4],0.5), pch=16) #plotting test set predicted label / dot filling - inner color

#accuracy calculation implementation
good = 0  #good prediction counter
bad = 0  #bad prediction counter
for(label in c(1:nrowsTest)){   #loop through all the test points
  if(results[label,3] == results[label,4]){  #if predicted label == real label, counter good prediction + 1, otherwise counter bad prediction + 1
    good = good + 1
  }
  else {
    bad = bad + 1
    points(test[label,1],test[label,2], pch=4, col="blue") #marking mistakes with a blue cross
  }
}

acc = (good/nrowsTest) #accuracy calculation 
miss = (bad/nrowsTest) #misclassication error ( 1 - acc)
cat("Accuracy ratio", acc, "\n")
cat("Misclassification/Error ratio", miss)

#Adding Legend
legend(12, 17, legend=c("Train", "Test Real", "Test Pred", "Error"), pch=c(17,1,16,4), col=c("black", "black", "black", "blue"), cex=0.6, box.lty=0)

