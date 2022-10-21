# -*- coding: utf-8 -*-
"""
Created on Tue Jul 19 16:21:00 2022

@author: udyan.sachdev
"""

import pandas as pd
import os

def data_input():

    file_path = r"C:\Users\udyan.sachdev\OneDrive - Accenture\Recommendation System\Code\Input"
    all_files = os.listdir(file_path)

    output = []
    for f in all_files:
        # read the excel file
        df = pd.read_excel(file_path + '\\'+ f)
        output.append(df)
    
    output = pd.concat(output)

    return output
