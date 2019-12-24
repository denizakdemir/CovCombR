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
  
  SEMAT<-StandardErrors(Hmat=outCovComb,Klist=CovList,nu=39)
  seest1<-matrix(0,4,4)
  seest1[,]<- 4*round(diag(SEMAT),3)
  rownames(seest1)<-names(diag(SEMAT))[1:4]
  colnames(seest1)<-names(diag(SEMAT))[1:4]
  
  return(seest1)
}



repfunc100<-simplify2array(lapply(1:1000,repfunc))
repfunc2100<-simplify2array(lapply(1:1000,repfunc2))
par(mfrow=c(1,2))
image(apply(repfunc100, c(1,2),sd))
image(apply(repfunc2100, c(1,2),mean)[c(3,4,1,2),c(3,4,1,2)])

