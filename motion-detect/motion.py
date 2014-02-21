#!/usr/bin/env python


import numpy as np
import cv2
from collections import deque
import sys
import csv
from datetime import datetime, timedelta
import re
import os
from tesseract.pytesser import *

class Target:

    def __init__(self):
	self.numBlankFrames = 2
	self.numFramesBridge = 8
	self.numFramesLookAheadAfter = 4
	self.buffer = deque(maxlen=self.numFramesBridge+1)
	self.tailBuffer = deque(maxlen=self.numFramesLookAheadAfter)
        #self.capture = cv.CaptureFromCAM(0)
	self.videoFilename = sys.argv[1]
	self.outputFilename = self.videoFilename + "_cut.avi"
	self.sequenceCSV = self.videoFilename + "_seq.csv"
	self.capture = cv2.VideoCapture(self.videoFilename)
	self.total_frames = int(self.capture.get(cv2.cv.CV_CAP_PROP_FRAME_COUNT))
	print self.total_frames
	self.fps = int(self.capture.get(cv2.cv.CV_CAP_PROP_FPS))
        #cv2.namedWindow("Target", 1)
	self.frame_size = (int(self.capture.get(cv2.cv.CV_CAP_PROP_FRAME_WIDTH)), int(self.capture.get(cv2.cv.CV_CAP_PROP_FRAME_HEIGHT)))
	print self.frame_size
	#fourcc = cv2.VideoWriter_fourcc(*'MJPG')
	fourcc = cv2.cv.CV_FOURCC('M','J','P','G')
	self.vidWriter = cv2.VideoWriter(self.outputFilename, fourcc, self.fps, self.frame_size, True);
	self.font = cv2.FONT_HERSHEY_SIMPLEX

	print self.videoFilename
	self.matchPattern = "^[a-zA-Z]+([0-9]+\-[0-9]+)\-[a-zA-Z0-9]+\.avi$"
	print os.path.basename(self.videoFilename)
	m = re.match(self.matchPattern, os.path.basename(self.videoFilename))
	print m.group(1)
	self.strpTimePattern = "%Y%m%d-%H%M%S"
	self.videoStartTime = datetime.strptime(m.group(1), self.strpTimePattern)
	print self.videoStartTime


    def dump(self,obj):
		for attr in dir(obj):
			if hasattr( obj, attr ):
	   			print( "obj.%s = %s" % (attr, getattr(obj, attr)))

    def highlightedImage(self,background,motion,number):
		redChannel = background[:,:,2]
		#highlight motion
		background[:,:,2] = np.bitwise_and(np.bitwise_not(motion), redChannel) +  np.bitwise_and(motion, redChannel//3 + 168)
		cv2.putText(background,'motion!',(self.frame_size[1]-50,self.frame_size[0]//2), self.font, 1, (0,0,255), 2)
		cv2.putText(background,str(number),(self.frame_size[1]//2-100,self.frame_size[0]//2-100), self.font, 2, (0,255,0), 2)
		return background

    def addNumberToImg(self,background,number):
		cv2.putText(background,str(number),(self.frame_size[1]//2-100,self.frame_size[0]//2-100), self.font, 2, (0,255,0), 2)
		return background

    def getGreyImage(self,color_image):
		grey_image = cv2.cvtColor(color_image, cv2.COLOR_BGR2GRAY)
		cv2.rectangle(grey_image, (0,0), (self.frame_size[0],25), (0,0,0), cv2.cv.CV_FILLED)
		grey_image = grey_image.astype(np.float32)
		grey_image = cv2.GaussianBlur(grey_image, (5,5), 0)
		return grey_image

    def getTopRow(self,color_image):
		grey_image = cv2.cvtColor(color_image, cv2.COLOR_BGR2GRAY)
		#grey_image = grey_image.astype(np.float32)
		grey_image = cv2.resize(grey_image, (1864,128))
		#grey_image = cv2.equalizeHist(grey_image)
		#grey_image = cv2.GaussianBlur(grey_image, (13,13), 0)
		grey_image = cv2.medianBlur(grey_image, 13)
		#grey_image = cv2.convertScaleAbs(grey_image)
		ret, grey_image = cv2.threshold(grey_image, 120, 255, cv2.THRESH_BINARY_INV)
		grey_image = cv2.GaussianBlur(grey_image, (11,11), 0)
		ret, grey_image = cv2.threshold(grey_image, 120, 255, cv2.THRESH_BINARY_INV)
		#grey_image = cv2.dilate(grey_image, cv2.getStructuringElement(cv2.MORPH_RECT, (3,3)), iterations=3)
		contours, hierarchy = cv2.findContours(grey_image.copy(),cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
		contourArea = []
		for contour in contours:
			area = cv2.contourArea(contour)
			if area < 950:
				x,y,w,h = cv2.boundingRect(contour)
				cv2.rectangle(grey_image, (x,y),(x+w,y+h), 0, cv2.cv.CV_FILLED)
		
		#cv2.drawContours(grey_image, contours, -1, 120, 3)
		#cv2.imwrite("cont.png", color_image)
		grey_image = (255-grey_image) #invert
		return grey_image

    def posttreatmentOCR(self, ocr_str):
		res_str = re.sub(r'O', '0', ocr_str)
		res_str = re.sub(r'[^0-9]', '', res_str)
		#res_str2 = res_str[1:4]+"-"+res_str[5:6]+"-"+res_str[7:8]
		return res_str

    def getTimeFromTopRow(self, color_image, referenceTime, maxDifferenceInSecs=30):
	ocr_image = color_image[0:16,0:233,:].copy()
	ocr_topRow = self.getTopRow(ocr_image)
	retrieved_text = image_to_stringCV(ocr_topRow)
	#print "retrieved:", retrieved_text
	ocr_text = self.posttreatmentOCR(retrieved_text)
	#print "OCR:", ocr_text
	ret_date = datetime.strptime("19011111111111", "%Y%m%d%H%M%S")
	try:
		ret_date = datetime.strptime(ocr_text, "%Y%m%d%H%M%S")
		absDiff = abs((referenceTime - ret_date).total_seconds())

		if absDiff > maxDifferenceInSecs:
			print absDiff
			print referenceTime
			print ret_date
			cv2.imwrite("cont.bmp", ocr_topRow)
			print retrieved_text
			print ocr_text
			raise ValueError('OCR error')
	except ValueError:
		ret_date = datetime.strptime("19011111111111", "%Y%m%d%H%M%S")
		print "could not read date from video. replacing with dummy. needs to be fixed manually."
	return ret_date


    def run(self):
	struct_elem = cv2.getStructuringElement(cv2.MORPH_RECT, (3,3))

	#get first frame for initialization
	ret, moving_average = self.capture.read()

	topRowStartTime = self.getTimeFromTopRow(moving_average, self.videoStartTime)
	print topRowStartTime
	#compute difference to double check dates
	#print (topRowStartTime - self.videoStartTime)


	moving_average = self.getGreyImage(moving_average)

	#moving_average = np.zeros((self.frame_size[1],self.frame_size[0]), np.float32)

	blank_frame = np.ones((self.frame_size[1],self.frame_size[0],3), np.uint8)
	blank_frame[:,:,2] = 255

	start_frame = np.ones((self.frame_size[1],self.frame_size[0],3), np.uint8)
	start_frame[:,:,1] = 255
	cv2.putText(start_frame, self.videoFilename, (5,30), self.font, 0.5, (0,0,0), 1)


	#reset frame position
	self.capture.set(cv2.cv.CV_CAP_PROP_POS_FRAMES, 0)

	for i in range(self.numBlankFrames*4):
		self.vidWriter.write(start_frame)

	listOfSequences = []

	frame_counter = 0
	motionCount = 0
	multPro = self.total_frames // 10
        while frame_counter < self.total_frames:
	    if frame_counter % multPro == 0:
	    	print 10*(frame_counter // multPro), "%"

            ret, color_image = self.capture.read()
	    grey_image = self.getGreyImage(color_image)

	    cv2.accumulateWeighted(grey_image, moving_average, 0.020)

	    grey_image = cv2.absdiff(grey_image, moving_average)
	    grey_image = cv2.convertScaleAbs(grey_image)

	    ret, grey_image = cv2.threshold(grey_image, 60, 255, cv2.THRESH_BINARY)
	    
	    grey_image = cv2.dilate(grey_image, struct_elem, iterations=18)
            grey_image = cv2.erode(grey_image, struct_elem, iterations=10)

	    pixel_sum = cv2.countNonZero(grey_image)
            if pixel_sum > 40:
		#there is motion: motion handling routine
	    	#print(pixel_sum)

		if len(self.buffer) <= (self.buffer.maxlen-1):
			#we bridge time between motion and add frames / add entire buffer
			while True:
				try:
					self.vidWriter.write(self.addNumberToImg(self.buffer.popleft(),motionCount-1))
				except IndexError:
					break
		else:
			#last motion is more than num of bridge frames in the past
			#add tail frames from last motion event
			while True:
				try:
					self.vidWriter.write(self.addNumberToImg(self.tailBuffer.popleft(),motionCount-1))
				except IndexError:
					break
			#insert blank frames / scene cut
			for i in range(self.numBlankFrames):
				self.vidWriter.write(blank_frame)
			motionCount = motionCount + 1
			videoTimeStampMSec = self.capture.get(cv2.cv.CV_CAP_PROP_POS_MSEC)
			realVideoTime = self.videoStartTime + timedelta(seconds=(frame_counter/self.fps)) #timedelta(milliseconds=videoTimeStampMSec)
			timeFromPicture = self.getTimeFromTopRow(color_image, realVideoTime)
			listOfSequences.append({'FrameCounter': frame_counter, 'VideoTimeInMSec': videoTimeStampMSec, 'TimeStamp': realVideoTime.strftime("%Y-%m-%d %H:%M:%S"), 'TimeFromPicture': timeFromPicture.strftime("%Y-%m-%d %H:%M:%S"), 'MotionSceneCount': motionCount-1, 'Event': ''})
			#and insert some of most recent frames
			self.buffer.rotate(-(self.buffer.maxlen-self.numFramesLookAheadAfter))
			for i in range(self.numFramesLookAheadAfter):
				self.vidWriter.write(self.addNumberToImg(self.buffer.popleft(),motionCount-1))
		# add actual motion frame
		self.vidWriter.write(self.highlightedImage(color_image,grey_image,motionCount-1))
		self.buffer.clear()
		self.tailBuffer.clear()
	    else:
		if len(self.tailBuffer) != self.tailBuffer.maxlen and motionCount > 0:
			self.tailBuffer.append(color_image)
		self.buffer.append(color_image)

            #cv2.imshow("Target", color_image)

            # Listen for ESC key
            #c = cv2.waitKey(7) % 0x100
            #if c == 27:
            #    break
	    frame_counter = frame_counter + 1

	while True:
		try:
			self.vidWriter.write(self.addNumberToImg(self.tailBuffer.popleft(),motionCount-1))
		except IndexError:
			break
	cv2.destroyAllWindows()
	self.capture.release()
	#self.vidWriter.release() # not implemented??

	#write timestamps and motion events to csv
	with open(self.sequenceCSV, 'wb') as outfile:
		write = csv.DictWriter(outfile, ['FrameCounter', 'VideoTimeInMSec', 'TimeStamp', 'TimeFromPicture', 'MotionSceneCount', 'Event'], delimiter='\t', quoting=csv.QUOTE_NONNUMERIC)
		write.writeheader()
		if len(listOfSequences) > 0:
			write.writerows(listOfSequences)


if __name__=="__main__":
    t = Target()
    t.run()
