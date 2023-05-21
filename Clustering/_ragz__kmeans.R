##########
ro=180
co=2
X=as.data.frame(matrix(runif(ro*co), nrow = ro, ncol = co))

#Test the algorithm by generating 2 random vectors and merging them to create a new matrix 
#x1= runif(160, min=0, max=150)
#x2=runif(160, min=120, max=9700)
#X=as.data.frame(cbind(x1,x2))

#Test with excel data
#df = read.csv("C:\\Users\\rogilb\\Downloads\\clustering.csv")
#x1= df[,c('LoanAmount')]
#x2=df[,c('ApplicantIncome')]
#X=as.data.frame(cbind(x2,x1))
k= 3
r= sample(1:nrow(X), size = k)
centroids= X[r, ]

x11(width = 20, height = 4)
layout(matrix(c(1,2), 1, 2, byrow = TRUE))  
plot(X[,1], X[,2],main= "K means", xlab = "Independent variable", ylab ="Depenndent variable", pch=19, col= "black")
points(centroids[,1], centroids[,2], pch=19, col= "red")


diff=1
cond= 0

while (diff==1) {
  XD=X
  i=1
  for (i in 1:nrow(centroids)) {
    ED= numeric()
    f2=numeric()
    f22=numeric()
    f1=numeric()
    f11=numeric()
    d2=numeric()
    d1=numeric()
    for (j in 1:nrow(X)) {
      f2[j]=X[r[i],2]
      f22[j]=X[j,2]
      f1[j]=X[r[i],1]
      f11[j]=X[j,1]
      d2[j]= ((f22[j]-f2[j]))^2
      d1[j]= ((f11[j]-f1[j]))^2
      ED[j]= sqrt(d2[j]+d1[j])
    }
    XD[i]=ED
    i=i+1
    print(i)
  }
  XD
  colnames(XD) <- c("1", "2", "3")
  X
  dmin= apply(XD, 1, which.min)
  dmin
  XF=cbind(X,XD,dmin)
  XF
  names(XF)[ncol(XF)]= "cluster"
  n_centroids= aggregate.data.frame(XF[, 1:2], list(XF$cluster), mean) 
  
  
  if (cond==0) {
    diff=1
    cond=cond+1
  }
  else {
    diff=sum(n_centroids[,3]-centroids[,2])+ sum(n_centroids[,2]-centroids[,1])
    print(diff)
  }
  
  centroids=aggregate(XF[, 1:2], list(XF$cluster), mean)
  
}

for (i in seq(along=1:k)){
  fcluster= XF[XF[, 6] == i, 1:2]
  a=switch(i,"blue","green","cyan") 
  plot(fcluster[, 1], fcluster[, 2], main="K means",pch=19, col=a,xlim=c(min(XF[,1]),max(XF[,1]))
       ,ylim=c(min(XF[,2]),max(XF[,2])), xlab="Independent variable ", ylab="Dependent variable ")
  par(new=TRUE)
  
}

points(centroids[,2], centroids[,3], pch=19, col= "red")
