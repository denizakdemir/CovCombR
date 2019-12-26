Hmatfunc <- function(Amat, Gmat, w = 0.5, nu = 1000) {
  A11 <- Amat[rownames(Amat) %in% rownames(Gmat), colnames(Amat) %in% 
                colnames(Gmat)]
  G <- Gmat[match(rownames(A11), rownames(Gmat)), match(rownames(A11), 
                                                        rownames(Gmat))]
  invA11 <- solve(A11)
  Gw <- (w) * G + (1 - w) * nu * A11
  A22 <- Amat[!(rownames(Amat) %in% rownames(A11)), !(colnames(Amat) %in% 
                                                        colnames(A11))]
  A12 <- Amat[match(rownames(G), rownames(Amat)), !(colnames(Amat) %in% 
                                                      colnames(A11))]
  Hmat11 <- Gw
  Hmat12 <- t(A12) %*% invA11 %*% Gw
  Hmat22 <- (nu) * A22 + t(A12) %*% invA11 %*% (Gw - (nu) * A11) %*% 
    invA11 %*% A12
  Hmat <- rbind(cbind(Hmat11, t(Hmat12)), cbind(Hmat12, Hmat22))
  Hmat <- Hmat[match(rownames(Amat), rownames(Hmat)), match(colnames(Amat), 
                                                            colnames(Hmat))]
  return(Hmat)
}


StandardErrors <- function(Hmat, Klist, nu = 100, w=1) {
    
    Hmatinv<-solve(Hmat)
    nrowe<-nrow(Hmat)^2
    namesinH<-rownames(Hmat)
    
    geterrorsonesample<-function(K){
    INFmat<-matrix(0,nrowe,nrowe)
    namesforerrormat<-apply(expand.grid(x=namesinH,y=namesinH),1,function(inp){paste(inp[1], inp[2],sep="_.._")})
    rownames(INFmat)<-colnames(INFmat)<-namesforerrormat
    namesinK<-rownames(K[[1]])
    m=length(Klist)
    
    for (i in 1:(length(namesforerrormat))){
        name11<-strsplit(namesforerrormat[i], "_.._")[[1]][1]  
        name12<-strsplit(namesforerrormat[i], "_.._")[[1]][2]  
        mat1<-matrix(0,nrow=nrow(Hmat), ncol=ncol(Hmat))
        rownames(mat1)<-colnames(mat1)<-namesinH
        mat1[name11, name12]<-1
        
                for (j in 1:length(namesforerrormat)){
            
              
          name21<-strsplit(namesforerrormat[j], "_.._")[[1]][1]  
          name22<-strsplit(namesforerrormat[j], "_.._")[[1]][2]  
          mat2<-matrix(0,nrow=nrow(Hmat), ncol=ncol(Hmat))
          rownames(mat2)<-colnames(mat2)<-namesinH
          mat2[name21, name22]<-1
          Hmata <- Hmatfunc(Hmat, K[[1]], w = K[[2]], 
                                           nu = nu)
          colnames(Hmata) <- rownames(Hmata) <- rownames(Hmatinv)
          Hmatainv<-solve(Hmata)
          INFmat[namesforerrormat[i],namesforerrormat[j]]<- (nu/(2))*sum(diag({Hmatainv*(nu)}%*%mat1%*%{Hmatainv*(nu)}%*%mat2))
        }
    }
    return(INFmat)
    }
    
    if (length(w)<2){
      wvec<-rep(w,length(Klist))
    } else{
      wvec<-w
    }
    
    for (k in 1:length(Klist)){
      Klist[[k]]<-list(Klist[[k]],wvec[k])
    }
    
    
    listout<-lapply(Klist, geterrorsonesample)
    outfunc <- Reduce("+", listout)
    namesforerrormat<-apply(expand.grid(x=namesinH,y=namesinH),1,function(inp){paste(inp[1], inp[2],sep="_")})
    rownames(outfunc)<-colnames(outfunc)<-namesforerrormat
    outfunc<-outfunc[order(rownames(outfunc)),order(colnames(outfunc))]
    outfunc<-nu^2*solve(outfunc)
    return(outfunc)
}

