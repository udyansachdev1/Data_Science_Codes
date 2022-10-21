# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 00:31:58 2022

@author: udyan.sachdev
"""

import pandas as pd
import os
import glob
import datetime
import time
import numpy as np
from pathlib import Path
from datetime import date, timedelta
import warnings
import scipy.cluster.hierarchy as shc
import matplotlib.pyplot as plt
from sklearn.preprocessing import normalize
from sklearn.cluster import AgglomerativeClustering
%matplotlib qt
warnings.filterwarnings('ignore')

#Working Directory & Path selection
cwd=os.getcwd() # Creating path based on platform #
input_dir = cwd+'\Input_Data'
output_dir = cwd+'\Output_Data'
start_date_modi = str(date.today()).replace("-", "_")

#Reading File
input_files = glob.glob(input_dir +'\*.csv')
df = pd.read_csv(input_files[0])

Hierarchical_Clustering(df)

def Hierarchical_Clustering(dataframe): 
    df = dataframe.copy()
    
    data_scaled = normalize(df)
    data_scaled = pd.DataFrame(data_scaled, columns=df.columns)
    plt.figure(figsize=(10, 7))  
    plt.title("Dendrograms")  
    dend = shc.dendrogram(shc.linkage(data_scaled, method='ward'))
    plt.savefig(output_dir+'\\'+start_date_modi+"_DendroGram"+'.png')
    plt.clf()
    
    
    plt.figure(figsize=(10, 7))  
    plt.title("Dendrograms")  
    dend = shc.dendrogram(shc.linkage(data_scaled, method='ward'))
    plt.axhline(y=6, color='r', linestyle='--')
    plt.savefig(output_dir+'\\'+start_date_modi+"_DendroGram_cutoff"+'.png')
    plt.clf()
    
    cluster = AgglomerativeClustering(n_clusters=2, affinity='euclidean', linkage='ward')  
    cluster.fit_predict(data_scaled)
    plt.figure(figsize=(10, 7))  
    plt.title("Scatter_Plot")  
    plt.scatter(data_scaled['Milk'], data_scaled['Grocery'], c=cluster.labels_) 
    plt.savefig(output_dir+'\\'+start_date_modi+"_ScatterPlot"+'.png')
    plt.clf()
    
    return