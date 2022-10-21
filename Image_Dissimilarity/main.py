# -*- coding: utf-8 -*-
"""
Created on Wed Jun  8 11:06:16 2022

@author: udyan.sachdev
"""

import cv2
from skimage.metrics import structural_similarity as compare_ssim
import numpy as np
from skimage.io import imread
import os

def coal_mining(img_historic,img_recent):
    # convert the images to grayscale
    Image_Recent = cv2.cvtColor(imread(img_recent), cv2.COLOR_BGR2GRAY)
    Image_Historic = cv2.cvtColor(imread(img_historic), cv2.COLOR_BGR2GRAY)
    Image_Recent = cv2.resize(Image_Recent, (1176, 196))
    
    # convert image to the 3 channel BGR color image.
    before = cv2.cvtColor(imread(img_recent), cv2.IMREAD_COLOR)
    after = cv2.cvtColor(imread(img_historic), cv2.IMREAD_COLOR)
    before = cv2.resize(before, (1176, 196))
    
    # compute the Structural Similarity Index (SSIM) between the two
    # images, ensuring that the difference image is returned
    (score, Image_dissimilarity) = compare_ssim(Image_Historic, Image_Recent, full=True)
    Image_dissimilarity = (Image_dissimilarity * 255).astype("uint8")
    
    Image_dissimilarity[Image_dissimilarity > 10 ] = -1

    
    # Image_dissimilarity_box = cv2.merge([Image_dissimilarity, Image_dissimilarity, Image_dissimilarity])
    # cv2.imshow("Image_dissimilarity_box", Image_dissimilarity_box)
    # cv2.waitKey(0)
    
    # threshold the difference image, followed by finding contours to
    # obtain the regions of the two input images that differ
    thresh = cv2.threshold(Image_dissimilarity, 0, 255, cv2.THRESH_BINARY_INV | cv2.THRESH_OTSU)[1]
    contours = cv2.findContours(thresh.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    contours = contours[0] if len(contours) == 2 else contours[1]
    mask = np.zeros(before.shape, dtype='uint8')
    filled_after = after.copy()
    
    # cv2.imshow("thresh", thresh)
    # cv2.waitKey(0)
    
    for c in contours:
        area = cv2.contourArea(c)
        if area > 15:
            # print(area)
            x,y,w,h = cv2.boundingRect(c)
            # compute the bounding box of the contour and then draw the
        	    # bounding box on both input images to represent where the two
        	    # images differ
            cv2.rectangle(before, (x, y), (x + w, y + h), (0,0,255), 2,)
            cv2.rectangle(after, (x, y), (x + w, y + h), (0,0,255), 2)
            cv2.drawContours(mask, [c], 0, (0,255,0), -1)
            cv2.drawContours(filled_after, [c], 0, (0,255,0), -1)
    
    # cv2.waitKey(0)
    
    # cv2.imshow("mask", mask)
    # cv2.waitKey(0)
    
    # cv2.imshow("filled_after", filled_after)
    # cv2.waitKey(0)
    
    cv2.imshow("Image_Recent", Image_Recent)
    cv2.imshow("Image_Historic", Image_Historic)
    cv2.imshow("Image_dissimilarity", Image_dissimilarity)
    # cv2.waitKey(0)
    # cv2.imshow("before", before)
    cv2.imshow("Recent Image with detected changes", after)
    # area2 = cv2.countNonZero(thresh)
    # print(area2)
    cv2.waitKey(0)
    
    return Image_Recent , Image_Historic , Image_dissimilarity

wd = os.getcwd()
path_img_recent = wd + '\\' + 'USA_Mine_Recent.png'
path_img_historic = wd + '\\' +'USA_Mine_Historic_1.png'

result = coal_mining(path_img_historic, path_img_recent)
Image_Recent = result[0]
Image_Historic = result[1]
Image_dissimilarity = result[2]

##Although this method works very well, there are some important limitations. 
#The two input images must have the same size/dimensions and also suffers from a 
#few problems including scaling, translations, rotations, and distortions. 
#SSIM also does not perform very well on blurry or noisy images. 