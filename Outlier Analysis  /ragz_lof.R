library("readxl")
install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

#data_p = read_excel('dataset')
#data= as.matrix(data_p[,2:4])

#Use to test or generate other data
ro=180
co=3
x1= rnorm(ro)
x2= rnorm(ro)
x3= rnorm(ro)
data= cbind(x1,x2,x3)
data=(matrix(runif(ro*co), nrow = ro, ncol = co))

#Creating the lists that will be used in our functions
kdn= numeric() #"k" neighbor index 
lrd= replicate(nrow(data), 0)#local reachability density
lof= replicate(nrow(data), 0)#local outlier factor


#First, we create a function to calculate the distance function
#Input: vector of features 
#Output: distance, using euclidean distance methodology
f_dist= function(p){
  distance= sqrt(rowSums((p-data)^2))
  return(distance)
}

#Function to calculate the k nearest neighbor distance
#Input: vector of features, neighbors
#Output: a list containing both, indexes and distances
f_kdistace= function(p,k){
  idxf= numeric()
  distance= f_dist(p)
  idx= sort(f_dist(p), index.return=TRUE)$ix
  for (i in(k:length(idx))) {
    if (distance[idx[i]]<= distance[idx[i-1]]) {
      i=i+1
    } else {
      idxf= idx[1:i]
      break
    }
  }
  return(list(idxf, distance[idxf]))
}

#Function to calculate the local reachability density
#Input: index
#Output: Local reachability density of the respective index
f_loc_rden= function(index){
  rd=0
  distance= f_dist(p)
  for (i in seq(length(kdn[index]))) {
    rd= rd+max(distance[index][i], distance[kdn[index][i]][-1])
  }
  ilrd= length(kdn[index]) * 1.0 / rd
  return(ilrd)
}


#Function to calculate the local outlier factor
#Input: Number of neighbours, threshold
#Output: A list, containing the outliers and non-outliers(normal values)
f_loc_outfa= function(k, threshold){
  lof= replicate(nrow(data), 0)
  f_lof= replicate(nrow(data), 0)
  if (k>=length(data))  {
    print('Error in selecting k')
  } 
  for (i in data) 
  {
    for (j in i) {
      kdn= f_kdistace(j, k)[1]
      dis= f_kdistace(j,k)[2]
    }
  }
  for (i in (1:nrow(data))) {
    lrd[i]=f_loc_rden(i)
  }
  for (i in (1:nrow(data))) {
    kdn= unlist(kdn)
    lof[i]= sum(lrd[kdn[i]], na.rm = TRUE)
    lof1= sum(lof)
  }
  for (i in (1:nrow(data))) {
    f_lof[i]=lof1/(length(kdn[i])*lrd[i]) #Applying the formula
  }
  normal= f_lof < threshold
  outliers= f_lof >= threshold
  return(list(normal, outliers))
}

#Initialization of the algorithm, configuration of the initial parameters
p= data[,2]
k= 15
threshold= 12.5
f_loc_outfa(k,threshold)
normal= unlist(f_loc_outfa(k,threshold)[1])
outliers=unlist( f_loc_outfa(k,threshold)[2])

#Identifying the indexes of the outliers an non-outliers(normal values)
id_normal= which(normal == "TRUE")
id_outlier= which(outliers == "TRUE") 
data_normal= data[id_normal, ]
data_outliers= data[id_outlier,]

####PLOT
library("car")
library("rgl")


## Plotting
scatterplot3d(data_normal, pch = 16, color="blue", main="Non-outliers, LOF>=1.25")


scatterplot3d(data_outliers, pch = 16, color="red", main="Outliers, LOF>=1.25")

