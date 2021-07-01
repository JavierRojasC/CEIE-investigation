import cv2
import numpy as np
import readFrame as rf
import checkBGChange as bgc
import time
from numba import jit

def shrink(v,lamb):
    
  u = np.sign(v)*np.maximum(0, abs(v) - lamb);
  
  return u


@jit(nopython=True)
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
          print(Gv2.shape)
          V  = np.concatenate((VoGv1,Gv2))
      
       
    
    return U,S,V

@jit(nopython=True)
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
        
      S  = S1[1-1:Ncols, 1-1:Ncols]
      U  = Uo@Gu[1-1:Ncols,1-1:Ncols] + p@Gu[Ncols:Ncols+1,1-1:Ncols]
      V  = Vo@Gv[1-1:Ncols,1-1:Ncols] + q@Gv[Ncols:Ncols+1,1-1:Ncols]
    
    
    return U,S,V 
    
@jit(nopython=True)  
def rank1DwnSVD(Uo, So, Vo, k):

    Nrows,Ncols = np.shape(Uo)
    N = max(Vo.shape)
    
    #numpy devuelve So como una lista y hace falta transformarlo
    #a una matriz para que el algebra matricial sea consistente(opcional)
    #So=np.diag(So)
    
    #numpy devuelve Vo ya transpuesta mientras que matlab no
    #toca hacer trasnpuerta para obtener los mismos resultados
    #tomar en cuenta que si el input Vo de la funcion ya esta transversa 
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
      S  = S1[1-1:Ncols, 1-1:Ncols]
      U  = Uo@Gu[1-1:Ncols,1-1:Ncols]
      Vtmp  = Vo@Gv[1-1:Ncols,1-1:Ncols] + q*Gv[Ncols:Ncols+1,1-1:Ncols]
      thresh = np.sqrt(np.sum(Vtmp[k-1,:]*Vtmp[k-1,:]) )                
      if k<N:
            if k > 1:
              V  = np.concatenate((Vtmp[1-1:k-1,:], Vtmp[k+1-1:N,:]),axis=0)
            else:
              V  = Vtmp[2-1:N,:]
            
      else:
            V  = Vtmp[1-1:N-1,:]
   

   


    
    return U,S,V,thresh
    

##parámetros
#url="http://193.242.215.2:8001/mjpg/video.mjpg"
url="http://50.28.225.134:83/mjpg/video.mjpg"
#url="http://64.77.205.67/mjpg/video.mjpg"
gray=0
innerLoops=2
lamb=0.025*1.25
bgChangeFlag = 0
backgroundThresh = 15;      #threshold de la diferencia entre el fondo actual y el anterior
backgroundStable = 15;      #numero de fotogramas para ser considerado estable
localDist=[]
kini= 1
k=1
winFrames=200
bgChangeFlag=0
vmaxShow = -1e10
vminShow =  1e10

#obtener dimensiones del fotograma
D=rf.readFrame(url,gray)
dims=D.shape

Nrows=dims[0]
Ncols=dims[1]



if gray:
    N3d=1
else:
    N3d=dims[2]
   
Ndata=Nrows*Ncols*N3d  

D=D.reshape(Ndata,1)

## primera aproximación usando decomposición qr
U,Sigma=np.linalg.qr(D)
V=np.array([[1]],dtype="float64")
L=D

#abro stream de datos
cap = cv2.VideoCapture(url)


prev_frame_time = 0

new_frame_time = 0

font = cv2.FONT_HERSHEY_SIMPLEX


while(True):
    ret, curFrame= cap.read()
    
    

    if gray:
        curFrame= cv2.cvtColor(curFrame, cv2.COLOR_BGR2GRAY)   
    curFrame=curFrame/255.0
    #curFrame=curFrame.flatten("F").reshape(Ndata,1)
    curFrame=curFrame.reshape(Ndata,1)
    U,Sigma,V = rank1IncSVD(U, Sigma, V, curFrame, int(1))
    
    if(k > kini):
        Lold = L
    else:
        Lold=""
  
    
    #loop interior
    
    for i in range(innerLoops):
        L = U@Sigma@V[None,-1,:].T
        #L = U@Sigma@V[None,:,:].T

        S = shrink( curFrame - L, lamb)
        r=""
        if(i==0):
            pFrame =curFrame
        else:
            pFrame = r
        r=curFrame-S  #computa residual  
        
        #remplazo de rango-1
        U,Sigma,V =rank1RepSVD(U, Sigma, V, 1, pFrame, r)
    
    
    #backgorund change check
    localDist, U, Sigma, V, bgChangeFlag, vRows=bgc.checkBGChange(kini,k,Nrows,Ncols,L,Lold,curFrame,localDist,V,backgroundThresh ,backgroundStable,bgChangeFlag,U,Sigma)
    print(bgChangeFlag)
    #rank down svd. primera columna
    if vRows >= winFrames:    
        U, Sigma, V ,thresh= rank1DwnSVD(U, Sigma, V, 1)    
        
    #Mostrar resultados        
    if N3d==1:
         
        vmaxShow = max(np.append(S, vmaxShow))
        vminShow = min(np.append(S, vminShow))
        S=((S-vminShow)/ ( vmaxShow  -   vminShow )).reshape(Nrows,Ncols)
        
        new_frame_time = time.time()
        fps = 1/(new_frame_time-prev_frame_time)
        prev_frame_time = new_frame_time
        fps = int(fps)
        fps = str(fps)
        
        cv2.putText(S, fps, (7, 70), font, 3, (100, 255, 0), 3, cv2.LINE_AA)
        cv2.imshow("Foreground",S)
        
        curFrame=curFrame.reshape(Nrows,Ncols)
        cv2.putText(curFrame,fps, (7, 70), font, 3, (100, 255, 0), 3, cv2.LINE_AA)

        #cv2.imshow("",curFrame)
    
    else:
       
        
        vmaxShow = max(np.append(S, vmaxShow))
        vminShow = min(np.append(S, vminShow))
        S=((S-vminShow)/ ( vmaxShow  -   vminShow )).reshape(Nrows,Ncols,N3d)
        
        new_frame_time = time.time()
        fps = 1/(new_frame_time-prev_frame_time)
        prev_frame_time = new_frame_time
        fps = int(fps)
        fps = str(fps)
        
        cv2.putText(S, fps, (7, 70), font, 3, (100, 255, 0), 3, cv2.LINE_AA)


        cv2.imshow("Foreground",S)
        curFrame=curFrame.reshape(Nrows,Ncols,N3d)
        
        cv2.putText(curFrame,fps, (7, 70), font, 3, (100, 255, 0), 3, cv2.LINE_AA)

        #cv2.imshow("",curFrame)
        



    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
    k=k+1  

cap.release()
cv2.destroyAllWindows()