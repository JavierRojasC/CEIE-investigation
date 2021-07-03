import cv2
import numpy as np

def readFrame(url,gray):
    
    cap = cv2.VideoCapture(url)
    
    ret, frame = cap.read()
    
    if gray:
       frame = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)      
    frame=  cv2.resize(frame,None,fx=0.75,fy=0.75)

    cap.release() 
    return frame
  


