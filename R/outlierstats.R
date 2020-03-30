outlierstats<-function(Hmat, Klist, nu = 10000, stat="VechDist") {
  if (stat=="loglik"){
    
  loglik1 <- function(x) {
    
    matchout <- match(rownames(x), rownames(Hmat))
    # outfunc<-LaplacesDemon::dwishart(x,nu,Hmat[matchout,matchout],log=TRUE)
    outfunc <- CholWishart::dWishart(x, nu, Hmat[matchout, matchout], 
                                     log = TRUE)
    
    return(outfunc)
  }
  listout <- lapply(Klist, loglik1)
  return(listout)
  }
  if (stat=="VechDist"){
  VechDist <- function(x) {
    
    matchout <- match(rownames(x), rownames(Hmat))
    # outfunc<-LaplacesDemon::dwishart(x,nu,Hmat[matchout,matchout],log=TRUE)
    outfunc <- sum((x-Hmat[matchout, matchout])^2)
    
    return(outfunc)
  }
  listout <- lapply(Klist, VechDist)
  return(listout)
  }
  
}
