install.packages("scatterplot3d") # Install 
library("scatterplot3d") # load
library("readxl")
library("car")
library("rgl")

#We create a pop function in R
pop <- function(list, i = length(list)) {
  stopifnot(inherits(list, "list"))
  res <- list[[i]]
  assign(deparse(substitute(list)), list[-i], envir = .GlobalEnv)
  res
}

#EXCEL FILE
#data_p = read_excel("dataset")
#data= as.matrix(data_p[,2:4])

#TEST AND CREATE YOUR OWN DATASET
x1= floor(runif(100, min=1, max=121))
x2= floor(runif(100, min=1, max=7800))
x3= floor(runif(100, min=180, max=2300))
data=cbind(x1,x2,x3)
#ro=180
#co=3
#data=as.data.frame(matrix(runif(ro*co), nrow = ro, ncol = co))
#data=(matrix(runif(ro*co), nrow = ro, ncol = co))
unvisited1= as.list(c(seq(1,(nrow(data)))))

#Distance function, using euclidean methodology
get_distance= function(a, b){
  distance= sqrt(rowSums((a-b)^2))
  return(distance)
}


#Function to determine the indexes based on the neighborhood "p" and "eps"
get_eps_nei= function(p, eps){
  distance= get_distance(data[p,],data)
  distance[distance<=0]=eps+1
  return(index[distance<=eps])
}

########### Step that implements the previous functions

unvisited= as.list(c(seq(1,(nrow(data)))))
mark= replicate(nrow(data), 0)
num_cluster= 0 #Initialize the number of clusters
i=0
eps=25 #Specify the eps
min_points=10 #Specify the minimum points
index= c(1:nrow(data))

while (i!=length(unvisited)) {
  p=pop(unvisited)
  neighbor= get_eps_nei(p,eps)
  if (length(neighbor)>= min_points) {
    num_cluster= num_cluster+1
    mark[p]=num_cluster
    for (q in neighbor) {
      if (q %in% unvisited) {
        unvisited$q=NULL
        g_neighbor=get_eps_nei(q, eps)
        if (length(g_neighbor)>= min_points) {
          neighbor=c(neighbor, g_neighbor)
        }
        if (mark[q]!=0) {
          mark[q]=num_cluster
        } 
      } 
    }
  }
  else {
    mark[p]= -1
  }
  i=i+1
  print(i)
}

mark
num_cluster

id_mark= which(mark==-1) #take the indexes of the outliers
data_mark= data[id_mark,] #select the features by the indexes in the dataset

#################PLOT THE OUTLIERS IN 3D

s3d=scatterplot3d(data_mark,
                  main="Outliers, eps=35, min points=10",
                  xlab = "L",
                  ylab = "F",
                  zlab = "M", pch = 16, color="red")




scatter3d(x=data_mark[,1], y=data_mark[,2], z= data_mark[,3], 
          point.col = "red",
          surface =FALSE, grid =FALSE,
          axis.scales = FALSE, ellipsoid = FALSE)

#PLOT THE CLUSTER IN 3D
cluster=0 # initialize the counter in 0, it gives the clusters in the dataset
points= matrix(nrow = length(which(mark!=-1)), ncol = ncol(data), 0) #non-outliers
min_cluster_size=2
count=nrow(data_mark)

for (i in (1:num_cluster)) {
  id_points=which(mark==i)
  print(id_points)
  points[i,]= data[id_points,]
  if (nrow(points)<= min_cluster_size) {
    count=count+length(nrow(points))
  } else {
    cluster=cluster+1
  }
}

points
cluster

s3d1=scatterplot3d(points, main="Clusters, eps=35, min points=10",
                   xlab = "L",
                   ylab = "F",
                   zlab = "M", pch = 16, color="blue")


scatter3d(x=points[,1], y=points[,2], z= points[,3], point.col = "red",
          surface =FALSE, grid =FALSE,
          axis.scales = FALSE, ellipsoid = FALSE)
