# -*- coding: utf-8 -*-
"""
Created on Tue Jul 19 16:25:23 2022

@author: udyan.sachdev
"""
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import pandas as pd



def cosine_matrix(data_req):
    
    count = CountVectorizer(stop_words='english')
    count_matrix = count.fit_transform(data_req['concat_column'])
    count_matrix.shape
    

    cosine_sim2 = cosine_similarity(count_matrix, count_matrix)
    
    # data_req = data_req.reset_index()
    indices = pd.Series(data_req.index, index=data_req['doc_fullName'])
    
    return cosine_sim2 , indices