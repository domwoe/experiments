"""Utility functions for processing images for delivery to Tesseract"""

import os
import numpy as np
import cv2

def image_to_scratch(im, scratch_image_name):
	"""Saves image in memory to scratch file.  .bmp format will be read correctly by Tesseract"""
	im.save(scratch_image_name, dpi=(200,200))

def image_to_scratchCV(numpyArr, scratch_image_name):
	"""Saves image in memory to scratch file.  .bmp format will be read correctly by Tesseract"""
	#print "Write temp image to", scratch_image_name
	cv2.imwrite(scratch_image_name, numpyArr)
	

def	retrieve_text(scratch_text_name_root):
	inf = file(scratch_text_name_root + '.txt')
	text = inf.read()
	inf.close()
	return text

def perform_cleanup(scratch_image_name, scratch_text_name_root):
	print "Clean up temporary files from disk"	
	for name in (scratch_image_name, scratch_text_name_root + '.txt', "tesseract.log"):
		try:
			os.remove(name)
		except OSError:
			pass
