StandardErrors <- function(Hmat, Klist, nu = 100) {
    
#the information matrix is given by 
#    \[ \{I(\Psi)\}_{jk,lh}=\{-E(\frac{\partial^2 l(\Psi)}{\partial \psi_{jk}\partial \psi_{lh}}|\Psi =\widehat{\Psi})\}_{jk,lh} 
#     =\frac{v}{2}\sum_{i=1}^m\left[tr(\widehat{\Psi}^{-1}_{a_i}\frac{\partial\Psi_{a_i}}{\partial\psi_{jk}}\widehat{\Psi}^{-1}_{a_i}\frac{\partial\Psi_{a_i}}{\partial\psi_{lh}})
#                                    \right]\]
    
    Hmatinv<-solve(Hmat)
    nrowe<-nrow(Hmat)^2
    namesinH<-rownames(Hmat)
    
    geterrorsonesample<-function(K){
    INFmat<-matrix(0,nrowe,nrowe)
    namesforerrormat<-apply(expand.grid(x=namesinH,y=namesinH),1,function(inp){paste(inp[1], inp[2],sep="_.._")})
    rownames(INFmat)<-colnames(INFmat)<-namesforerrormat
    namesinK<-rownames(K)
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
          
          INFmat[namesforerrormat[i],namesforerrormat[j]]<- (nu/(2))*sum(diag({Hmatinv*(nu*m)}%*%mat1%*%{Hmatinv*(nu*m)}%*%mat2))
        }
    }
    return(INFmat)
    }
    
    listout<-lapply(Klist, geterrorsonesample)
    outfunc <- Reduce("+", listout)
    namesforerrormat<-apply(expand.grid(x=namesinH,y=namesinH),1,function(inp){paste(inp[1], inp[2],sep="_")})
    rownames(outfunc)<-colnames(outfunc)<-namesforerrormat
    outfunc<-outfunc[order(rownames(outfunc)),order(colnames(outfunc))]
    outfunc<-nu*sqrt(solve(outfunc))
    return(outfunc[!duplicated(rownames(outfunc)),!duplicated(rownames(outfunc))])
}

