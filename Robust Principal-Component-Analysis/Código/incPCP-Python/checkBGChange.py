import numpy as np
def checkBGChange(kini,k,Nrows,Ncols,L,Lold,curFrame,localDist,V,backgroundThresh ,backgroundStable,bgChangeFlag,U,Sigma):
    
    vRows  = V.shape[0]
    if k > kini:
        localDist.append(np.sum(abs(L - Lold)) / (Nrows*Ncols))
       
    if k>(kini+1):
        Lfrac = localDist[-1] / localDist[-2]
    else:
        Lfrac = 1
    
    if (Lfrac > backgroundThresh) &  (vRows >= backgroundStable| bgChangeFlag ) : 
        U,Sigma=np.linalg.qr(curFrame)
        V=np.array([[1]])
        vRows = 1
        bgChangeFlag = 1
        
    if vRows >= backgroundStable:
        bgChangeFlag = 0
    
    return localDist, U,Sigma, V, bgChangeFlag, vRows