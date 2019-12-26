####Using Iris data for a simple example
data(iris)
colnames(iris)<-c("S.L","S.W","P.L","P.W","Species")
iris$Species
##Setting seed for reproducability.
#set.seed(1234)

###The input of the CovComb is a list of partial 
#covariance matrices for the species 'virginica'.
CovList<-vector(mode="list", length=3)
CovList[[1]]<-cov(iris[sample(101:150,20),c(1,2)])
CovList[[2]]<-cov(iris[sample(101:150,25),c(1,3)])
CovList[[3]]<-cov(iris[sample(101:150,30),c(2,4)])
###Note that the covariances between the variables 
##1 and 2, 2 and 3, and 3 and 4 are not observed in 
##the above. We will use these covariance matrices 
##to obtain a 4 by 4 covariance matrix that estimates 
##these unobserved covariances.

library(CovCombR)
outCovComb<-CovComb(CovList, nu=39)



repfunc<-function(repi){
  CovList<-vector(mode="list", length=3)
  CovList[[1]]<-cov(iris[sample(101:150,50, replace=T),c(1,2)])
  CovList[[2]]<-cov(iris[sample(101:150,50, replace=T),c(1,3)])
  CovList[[3]]<-cov(iris[sample(101:150,50, replace=T),c(2,4)])
  ###Note that the covariances between the variables 
  ##1 and 2, 2 and 3, and 3 and 4 are not observed in 
  ##the above. We will use these covariance matrices 
  ##to obtain a 4 by 4 covariance matrix that estimates 
  ##these unobserved covariances.
  
  library(CovCombR)
  outCovComb<-CovComb(CovList, nu=39)
  return(outCovComb)
}

repfunc2<-function(repi){
  CovList<-vector(mode="list", length=3)
  CovList[[1]]<-cov(iris[sample(101:150,50, replace=T),c(1,2)])
  CovList[[2]]<-cov(iris[sample(101:150,50, replace=T),c(1,3)])
  CovList[[3]]<-cov(iris[sample(101:150,50, replace=T),c(2,4)])
  ###Note that the covariances between the variables 
  ##1 and 2, 2 and 3, and 3 and 4 are not observed in 
  ##the above. We will use these covariance matrices 
  ##to obtain a 4 by 4 covariance matrix that estimates 
  ##these unobserved covariances.
  
  library(CovCombR)
  outCovComb<-CovComb(CovList, nu=39)
  SEMAT<-StandardErrors(Hmat=outCovComb,Klist=CovList,nu=39,w=1)
  return(SEMAT)
}



repfunc100<-simplify2array(lapply(1:1000,repfunc))
repfunc2100<-simplify2array(lapply(1:1000,repfunc2))

apply(repfunc100, c(1,2),sd)
sqrt(matrix(apply(repfunc2100, c(1,2),mean),4,4))[c(1,4,3,2),c(1,4,3,2)]
par(mfrow=c(1,2))

image(apply(repfunc100, c(1,2),sd))
image(sqrt(matrix(apply(repfunc2100, c(1,2),mean),4,4))[c(1,4,3,2),c(1,4,3,2)])

round(sqrt(diag(repfunc2100[,,1])),4)
apply(repfunc100, c(1,2),sd)
par(mfrow=c(1,2))
image(apply(repfunc100, c(1,2),sd))
image(sqrt(matrix(apply(repfunc2100, c(1,2),mean),4,4))[c(3,4,1,2),c(3,4,1,2)])

hist(c(apply(repfunc100, c(1,2),sd)-apply(repfunc2100, c(1,2),mean)))
mean(na.omit(c(apply(repfunc100, c(1,2),sd)-apply(repfunc2100, c(1,2),mean)))^2)
mean(na.omit(abs(c(apply(repfunc100, c(1,2),sd)-apply(repfunc2100, c(1,2),mean)))))
