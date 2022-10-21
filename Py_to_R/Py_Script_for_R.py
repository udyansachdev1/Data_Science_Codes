# -*- coding: utf-8 -*-
"""
Created on Sat May 21 19:48:35 2022

@author: udyan.sachdev
"""

import pandas 

def group_by(dataframe):
    
    data = pandas.read_excel(dataframe)
    data = data.groupby(["subtheme"], as_index=False)["sentence_proper"].count()
    
    return data 