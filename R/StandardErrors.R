StandardErrors <- function(Hmat, Klist, nu = 10000) {
    
#the information matrix is given by 
#    \[ \{I(\Psi)\}_{jk,lh}=\{-E(\frac{\partial^2 l(\Psi)}{\partial \psi_{jk}\partial \psi_{lh}}|\Psi =\widehat{\Psi})\}_{jk,lh} 
#     =\frac{v}{2}\sum_{i=1}^m\left[tr(\widehat{\Psi}^{-1}_{a_i}\frac{\partial\Psi_{a_i}}{\partial\psi_{jk}}\widehat{\Psi}^{-1}_{a_i}\frac{\partial\Psi_{a_i}}{\partial\psi_{lh}})
#                                    \right]\]
    
    outfunc <- Reduce("+", listout)
    return(outfunc)
    
}
