outlierstats<-function(Hmat, Klist, nu = 10000, stat=NULL) {
  
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
