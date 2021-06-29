import numpy as np



def rank1RepSVD(Uo, So, Vo, k, curFrame, newFrame):
    
    #So=np.diag(So)#para dejar en el mismo formato de matlab(opcional)
    #Vo=Vo.T#para dejar en el mismo formato de matlab(opcional)
    
    Nrows,Ncols = np.shape(Uo)
    N = max(Vo.shape)

    rQ = Vo[k-1:k,:]
    rQ = rQ.T
    z = -Vo@rQ
    z[k-1] = z[k-1]+1

    rhoQ = np.sqrt( 1 - rQ.T@rQ )

    if( rhoQ > 1e-8 ):
      q = z/rhoQ
    else:
      q = np.zeros((z.shape))
   
    rhoQ = np.asarray(rhoQ).reshape(1,1)

    
    
    rP = Uo.T@newFrame
    z = newFrame - Uo@rP
    rP = rP - So@rQ
    
    rhoP = np.sqrt( np.sum( z*z ) )
    
    if( rhoP > 1e-8 ):
      p = z/rhoP
    else:
      p = np.zeros(z.shape)
   
    

    St = np.append(So+rP@rQ.T,rhoQ*rP,axis=1)
    inter=np.append( rhoP*rQ.T,rhoP*rhoQ,axis=1 )
    St=np.append(St,inter,axis=0)
    
    
    Gu,S1,Gv = np.linalg.svd(St)    
    
    S1=np.diag(S1)#para dejar en el mismo formato de matlab
    Gv=Gv.T#para dejar en el mismo formato de matlab
    
    if 1:
        
      S  = S1[0:Ncols, 0:Ncols]
      U  = Uo@Gu[0:Ncols,0:Ncols] + p@Gu[Ncols:Ncols+1,0:Ncols]
      V  = Vo@Gv[0:Ncols,0:Ncols] + q@Gv[Ncols:Ncols+1,0:Ncols]
    
    
    return U,S,V 
    