import numpy as np


def  rank1IncSVD(Uo, So, Vo, curFrame, flag):
  
    Nrows,Ncols =Uo.shape
    
    r = Uo.T@curFrame
    z = curFrame - Uo@r
    rho = np.sqrt(np.sum( z*z ))


    
    if( rho > 1e-8 ):
      p = z/rho
    else:
      p = np.zeros((np.shape(z)))
      
    rho=np.asarray(rho).reshape(1,1)

    #por si acaso no tiene el formato de matlab(opcional)
    #So=np.diag(So)
    #por si acaso no tiene el formato de matlab(opcional)
    #Vo=Vo.T
    
    St = np.concatenate((So,r),axis=1) 
    zeros=np.concatenate((np.zeros((1,Ncols)),rho),axis=1)
    St= np.append(St,zeros,axis=0)

    Gu,S1,Gv = np.linalg.svd(St)
    
    #diag para transformar en matriz como en matlab(necesario)
    S1=np.diag(S1)
    
    #transpuesta para obener lo mismo de matlab(necesario)
    Gv=Gv.T
    
    dS1 = np.diag(S1)
    rel = 100*dS1[-1]/np.sum(dS1[0:-1])
    
    if rel > 100:
      flag = 0
    
    
    if flag == 0 :
      S  = S1
      U  = np.concatenate((Uo,p),axis=1)@Gu
      
      V =np.concatenate((Vo,np.zeros((max(Vo.shape),1))),axis=1) 
      inter=np.concatenate((np.zeros((1,Ncols)),np.array([[1]])),axis=1)
      V=np.concatenate((V,inter),axis=0)@Gv
   
    
    else:
          S  = S1[1-1:Ncols, 1-1:Ncols]
        
          U  = Uo@Gu[1-1:Ncols, 1-1:Ncols] + p@Gu[Ncols:Ncols+1,1-1:Ncols]


          Gv1=Gv[1-1:Ncols,1-1:Ncols]
          VoGv1=Vo@Gv1
          Gv2=Gv[Ncols:Ncols+1,1-1:Ncols]
          V  = np.concatenate((VoGv1,Gv2))
      
       
    
    return U,S,V
