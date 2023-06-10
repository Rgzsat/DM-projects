install.packages("plot3D")
library(plot3D)
rm(list=ls())
library(mvtnorm)
library(car)
library(scales)
install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
install.packages("rgl")
library("rgl")
rgl.open() # Open a new RGL device

mySum <- function(x) {
  #this function returns sum of the elements of x, whereas infinitely large elements are ignored
  sum(x[is.finite(x)])
}

#
load(file ="C:\\Users\\47406\\Downloads\\practice_04\\\\practice_04\\2gaussiandata.RData")
ro=1800
co=3
#x=matrix(runif(ro*co), nrow = ro, ncol = co)

di=x
x3=runif(n = nrow(di), min = min(x[,1]), max = max(x[,1]))
x=cbind(x,x3)
x= x[,-3]
xt=x
pi1<-0.5
pi2<-0.5

#initial values for means 
mu1<-matrix(c(sample(-10:0, 1),sample(-10:0, 1), sample(-10:0,1)),nrow=3) #random uniformly selected starting values
mu2<-matrix(c(sample(0:10, 1),sample(0:10, 1), sample(0:10,1)),nrow=3) #random uniformly selected starting values
sym1 = sample(-3:3, 1)
sym2 = sample(-3:3, 1)
cla=di[,3]

sigmat1=  cov(matrix( rnorm(nrow(xt)*ncol(xt)), nrow(xt), ncol(xt)))
sigmat2=cov(matrix( rnorm(nrow(xt)*ncol(xt)), nrow(xt), ncol(xt)))

#PLOT INITIAL VALUES

rgl.points(x=x[,1], y=x[,2], z=x[,3],groups = as.factor(cla)) # Scatter plot
rgl.points(x=mu1[1], y=mu1[2], z=mu1[3], color ="red") # Scatter plot
rgl.points(x=mu2[1], y=mu2[2], z=mu2[3], color ="yellow") # Scatter plot

ellips1= ellipse3d(sigmat1, scale = c(1,1,1), centre = c(mu1[1],mu1[2],mu1[3]), 
                   level = 0.95)
shade3d(ellips1, col = "#D95F02", alpha = 0.1, lit = FALSE)
ellips2= ellipse3d(sigmat2, scale = c(1,1,1), centre = c(mu2[1],mu2[2],mu1[3]), 
                   level = 0.95)
shade3d(ellips2, col = "cyan", alpha = 0.1, lit = FALSE)


#initial conditions for stopping the algorithm
loglik<- rep(NA, nrow(xt)*ncol(xt) ) #log likelihoods storage
loglik[1]<-0 #initial log likelihood value
loglik[2]<-mySum(pi1*(log(pi1)+log(matrix(dmvnorm(xt,mu1,sigmat1),nrow=3,ncol=1000))))
k<-2
loglik


while (abs(loglik[k]-loglik[k-1]) >= 0.00001) {
  
  tau1<-pi1*matrix(dmvnorm(xt,mu1,sigmat1))
  tau2<-pi2*matrix(dmvnorm(xt,mu2,sigmat2))
  normalizer<-tau1 + tau2
  
  
  tau1<-tau1/normalizer
  tau2<-tau2/normalizer
  
  # Step 3 -> M step: Maximization - Re-estimate the Component Parameters
  n<-dim(x)[1] #number of datapoints
  
  pi1<-mySum(tau1)/n #recomputing responsabilities
  pi2<-mySum(tau2)/n
  
  mu1[1]<-(t(tau1)%*%x[,1])/mySum(tau1) #recalculating mean values
  mu1[2]<-(t(tau1)%*%x[,2])/mySum(tau1)  #t(tau) to perform matrix multiplication, row by vector == scalar
  mu1[3]<-(t(tau1)%*%x[,3])/mySum(tau1)
  
  mu2[1]<-(t(tau2)%*%x[,1])/mySum(tau2)
  mu2[2]<-(t(tau2)%*%x[,2])/mySum(tau2)
  mu2[3]<-(t(tau2)%*%x[,3])/mySum(tau2)
  
  sigmat1[1,1]<-t(tau1)%*%((xt[,1]-mu1[1])*(xt[,1]-mu1[1]))/(mySum(tau1)) #recalculating covariance matrix
  sigmat1[1,2]<-t(tau1)%*%((xt[,1]-mu1[1])*(xt[,2]-mu1[2]))/(mySum(tau1))
  sigmat1[1,3]<-t(tau1)%*%((xt[,1]-mu1[1])*(xt[,3]-mu1[3]))/(mySum(tau1))
  sigmat1[2,1]<-sigmat1[1,2]
  sigmat1[2,2]<-t(tau1)%*%((xt[,2]-mu1[2])*(xt[,2]-mu1[2]))/(mySum(tau1))
  sigmat1[2,3]<-t(tau1)%*%((xt[,2]-mu1[2])*(xt[,3]-mu1[3]))/(mySum(tau1))
  sigmat1[3,1]<-sigmat1[1,3]
  sigmat1[3,2]<-sigmat1[2,3]
  sigmat1[3,3]<-t(tau1)%*%((xt[,3]-mu1[3])*(xt[,3]-mu1[3]))/(mySum(tau1))
  
  sigmat2[1,1]<-t(tau2)%*%((xt[,1]-mu1[1])*(xt[,1]-mu1[1]))/(mySum(tau2)) #recalculating covariance matrix
  sigmat2[1,2]<-t(tau2)%*%((xt[,1]-mu1[1])*(xt[,2]-mu1[2]))/(mySum(tau2))
  sigmat2[1,3]<-t(tau2)%*%((xt[,1]-mu1[1])*(xt[,3]-mu1[3]))/(mySum(tau2))
  sigmat2[2,1]<-sigmat2[1,2]
  sigmat2[2,2]<-t(tau2)%*%((xt[,2]-mu1[2])*(xt[,2]-mu1[2]))/(mySum(tau2))
  sigmat2[2,3]<-t(tau2)%*%((xt[,2]-mu1[2])*(xt[,3]-mu1[3]))/(mySum(tau2))
  sigmat2[3,1]<-sigmat2[1,3]
  sigmat2[3,2]<-sigmat2[2,3]
  sigmat2[3,3]<-t(tau2)%*%((xt[,3]-mu1[3])*(xt[,3]-mu1[3]))/(mySum(tau2))
  
  
  loglik[k+1]<-mySum(pi1*(log(pi1)+log(matrix(dmvnorm(xt,mu1,sigmat1), nrow=3,ncol=1000))))
  k<-k+1
}

loglik
sigmat1
sigmat2

#FINAL PLOT 
rgl.points(x=x[,1], y=x[,2], z=x[,3],groups = as.factor(cla)) # Scatter plot
rgl.points(x=mu1[1], y=mu1[2], z=mu1[3], color ="red") # Scatter plot
rgl.points(x=mu2[1], y=mu2[2], z=mu2[3], color ="yellow") # Scatter plot

ellips1= ellipse3d(sigmat1, scale = c(1,1,1), centre = c(mu1[1],mu1[2],mu1[3]), 
                   level = 0.95)
shade3d(ellips1, col = "#D95F02", alpha = 0.1, lit = FALSE)
ellips2= ellipse3d(sigmat2, scale = c(1,1,1), centre = c(mu2[1],mu2[2],mu1[3]), 
                   level = 0.95)
shade3d(ellips2, col = "cyan", alpha = 0.1, lit = FALSE)

