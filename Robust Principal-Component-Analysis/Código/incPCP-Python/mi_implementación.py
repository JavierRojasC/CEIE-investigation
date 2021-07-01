#### implemetación incPCP
import cv2
import numpy as np
import readFrame as rf
import rank1IncSVD as isvd
import rank1RepSVD as rsvd
import rank1DwnSVD as dsvd
import checkBGChange as bgc
import time

###
# hog = cv2.HOGDescriptor()
# hog.setSVMDetector(cv2.HOGDescriptor_getDefaultPeopleDetector())

def shrink(v,lamb):
    
  u = np.sign(v)*np.maximum(0, abs(v) - lamb);
  
  return u


##parámetros
#url="http://193.242.215.2:8001/mjpg/video.mjpg"
#url="http://50.28.225.134:83/mjpg/video.mjpg"
url="http://64.77.205.67/mjpg/video.mjpg"
gray=0
innerLoops=1
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
    U,Sigma,V = isvd.rank1IncSVD(U, Sigma, V, curFrame, 1)
    
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
        U,Sigma,V =rsvd.rank1RepSVD(U, Sigma, V, 1, pFrame, r)
    
    
    #backgorund change check
    localDist, U, Sigma, V, bgChangeFlag, vRows=bgc.checkBGChange(kini,k,Nrows,Ncols,L,Lold,curFrame,localDist,V,backgroundThresh ,backgroundStable,bgChangeFlag,U,Sigma)
    print(bgChangeFlag)
    #rank down svd. primera columna
    if vRows >= winFrames:    
        U, Sigma, V ,thresh= dsvd.rank1DwnSVD(U, Sigma, V, 1)    
        
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
        
        #curFrame=curFrame.reshape(Nrows,Ncols)
        # cv2.putText(curFrame,fps, (7, 70), font, 3, (100, 255, 0), 3, cv2.LINE_AA)

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
        # curFrame=curFrame.reshape(Nrows,Ncols,N3d)
        
        # cv2.putText(curFrame,fps, (7, 70), font, 3, (100, 255, 0), 3, cv2.LINE_AA)

        # cv2.imshow("",curFrame)
        



    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
    k=k+1  

cap.release()
cv2.destroyAllWindows()






