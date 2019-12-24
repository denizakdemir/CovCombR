####Using Iris data for a simple example
data(iris)
colnames(iris)<-c("S.L","S.W","P.L","P.W","Species")
iris$Species
##Setting seed for reproducability.
set.seed(1234)

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
outCovComb<-CovComb(CovList, nu=40)



repfunc<-function(repi){
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
  outCovComb<-CovComb(CovList, nu=40)
  return(outCovComb)
}

repfunc100<-simplify2array(lapply(1:100,repfunc))
apply(repfunc100, c(1,2),sd)


SEMAT<-StandardErrors(Hmat=outCovComb,Klist=CovList,nu=39)
2*round(diag(2*SEMAT),3)
