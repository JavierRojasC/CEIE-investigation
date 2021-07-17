import numpy as np


def rank1DwnSVD(Uo, So, Vo, k):

    Nrows,Ncols = np.shape(Uo)
    N = max(Vo.shape)
    
    #numpy devuelve So como una lista y hace falta transformarlo
    #a una matriz para que el algebra matricial sea consistente(opcional)
    #So=np.diag(So)
    
    #numpy devuelve Vo ya transpuesta mientras que matlab no
    #toca hacer trasnpuerta para obtener los mismos resultados
    #tomar en cuenta que si el input Vo de la funcion ya esta transpuesta
    #no hay que hacer nada (opcional)   
    #Vo=Vo.T
    
    
    r = Vo[k-1:k,:]
    r=r.T  # equivalente a (:) en matlab
    z = -Vo@r
           
    z[k-1] = z[k-1]+1
        
    rho = np.sqrt( 1 - r.T@r )
    
    if( rho > 1e-8 ):
          q = z/rho;
    else:
          q = np.zeros((z.shape))
      
       
        
    St = np.concatenate((So-So@r@r.T,-rho*So@r),axis=1)
    St=np.concatenate((St,np.zeros((1,Ncols+1))),axis=0)    
        
    Gu,S1,Gv = np.linalg.svd(St)
    S1=np.diag(S1)     
    Gv=Gv.T
        
    if 1:    
      S  = S1[0:Ncols, 0:Ncols]
      U  = Uo@Gu[0:Ncols,0:Ncols]
      Vtmp  = Vo@Gv[0:Ncols,0:Ncols] + q*Gv[Ncols:Ncols+1,0:Ncols]
      thresh = np.sqrt(np.sum(Vtmp[k-1,:]*Vtmp[k-1,:]) )                
      if k<N:
            if k > 1:
              V  = np.concatenate((Vtmp[0:k-1,:], Vtmp[k:N,:]),axis=0)
            else:
              V  = Vtmp[1:N,:]
            
      else:
            V  = Vtmp[0:N-1,:]
   

   


    
    return U,S,V,thresh
