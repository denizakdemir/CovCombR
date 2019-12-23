StandardErrors <- function(Hmat, Klist, nu = 10000) {
    
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
    namesforerrormatK<-apply(expand.grid(x=namesinK,y=namesinK),1,function(inp){paste(inp[1], inp[2],sep="_.._")})
    
    
    for (i in 1:(length(namesforerrormatK)-1)){
        for (j in i:length(namesforerrormatK)){
            
            
          name11<-strsplit(namesforerrormatK[i], "_.._")[[1]][1]  
          name12<-strsplit(namesforerrormatK[i], "_.._")[[1]][2]  
          name21<-strsplit(namesforerrormatK[j], "_.._")[[1]][1]  
          name22<-strsplit(namesforerrormatK[j], "_.._")[[1]][2]  
        INFmat[namesforerrormatK[i],namesforerrormatK[j]]<- (nu/2)*Hmatinv[name11,name12]*Hmatinv[name21,name22]
        }
    }
    return(INFmat)
    }
    
    listout<-lapply(Klist, geterrorsonesample)
    outfunc <- Reduce("+", listout)
    namesforerrormat<-apply(expand.grid(x=namesinH,y=namesinH),1,function(inp){paste(inp[1], inp[2],sep="_")})
    rownames(outfunc)<-colnames(outfunc)<-namesforerrormat
    return(solve(outfunc))
    
}
