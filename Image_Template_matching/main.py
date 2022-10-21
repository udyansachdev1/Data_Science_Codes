# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 00:51:21 2022

@author: udyan.sachdev
"""

import os
import cv2
import numpy as np
from collections import OrderedDict
from scipy.spatial import distance as dist

 
def main():
    input_dir = "Input_Data/"
    output_dir = "Output_Data/"

    base_image_name = ""
    template_image_name = ""

    base_image_path = os.path.join(input_dir, base_image_name)
    template_image_path = os.path.join(input_dir, template_image_name)

    image = cv2.imread(base_image_path)
    template = cv2.imread(template_image_path)
    
    # convert both the image and template to grayscale
    imageGray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    templateGray = cv2.cvtColor(template, cv2.COLOR_BGR2GRAY)
                
    # perform template matching
    result = cv2.matchTemplate(imageGray, templateGray,
        cv2.TM_CCOEFF_NORMED)
    (minVal, maxVal, minLoc, maxLoc) = cv2.minMaxLoc(result)

    # determine the starting and ending (x, y)-coordinates of the
    # bounding box
    (startX, startY) = maxLoc
    endX = startX + template.shape[1]
    endY = startY + template.shape[0]

    # draw the bounding box on the image
    cv2.rectangle(image, (startX, startY), (endX, endY), (255, 0, 0), 3)

    output_image_name = 'matched_' + base_image_name
    output_path = os.path.join(output_dir, output_image_name)
    cv2.imwrite(output_path, image)


if __name__ == "__main__":
    main()