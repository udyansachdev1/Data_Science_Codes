# -*- coding: utf-8 -*-
"""
Created on Mon Sep 26 15:31:10 2022

@author: udyan.sachdev
"""

import datetime
from time import  strftime
import os
import pandas as pd

def output_upload(Final_Output):
      
    path = r'C:\Users\udyan.sachdev\OneDrive - Accenture\Recommendation System\Code\Output'
    
    all_files = os.listdir(path)
    
    ##running for the first time only case
    if len(all_files) == 0:
        ###adding time columns
        Final_Output['Creation Datetime'] = strftime("%Y-%m-%d %H:%M:%S")
        Final_Output['Last Updated Datetime'] = strftime("%Y-%m-%d %H:%M:%S") 
    
        Final_Output.to_excel(path + '\\' + 'Marketplace_US_Output_' + str(len(Final_Output)) + 'size' + '_' + str(datetime.datetime.now().strftime("%d-%m-%Y"))+'.xlsx',index = False)               

        
    ###rest of the times
    else:
        output = []
        for f in all_files:
            # read the excel file
            df = pd.read_excel(path + '\\'+ f)
            output.append(df)
        
        Old_Output = pd.concat(output)
        
        ####dropping duplicates after concatenating all outputs 
        # total = Old_Output.sort_values('Last Updated Datetime').drop_duplicates(['Input_doc_id', 'Recommended_doc_id'],keep='last') 
        total =  Old_Output.sort_values(['Input_doc_id','Last Updated Datetime','Score'],ascending=False).groupby('Input_doc_id').head(10).reset_index(drop=True)

        ### comparing last output with the new generated one
        total1 = total.drop(['Score','Count_Overlapped_Tags','Overlapped_Percentage','Creation Datetime','Last Updated Datetime'],axis = 1)
        Final_Output1 = Final_Output.drop(['Score','Count_Overlapped_Tags','Overlapped_Percentage'],axis=1)        
        Output_diff = pd.concat([total1,Final_Output1]).drop_duplicates(keep=False).reset_index(drop=True)
        
        ##list of updated names
        ##list of all updated names
        list_unique_datasetname = Output_diff['Input_doc_fullName'].unique()
        list_unique_all_ids = Output_diff['Input_doc_id'].unique()
        
        ###list of old names
        list_unique_old_ids = total1['Input_doc_id'].unique()
        
        ##list of existing update 
        updated_ids = list(set(list_unique_all_ids).intersection(list_unique_old_ids))
        
        Final_Output = Final_Output[Final_Output['Input_doc_fullName'].isin(list_unique_datasetname.tolist())].reset_index(drop=True)
        
        ###adding time columns
        Final_Output['Creation Datetime'] = strftime("%Y-%m-%d %H:%M:%S")
        Final_Output['Last Updated Datetime'] = strftime("%Y-%m-%d %H:%M:%S")
    
        Final_Output.to_excel(path + '\\' + 'Marketplace_US_Output_' + str(len(Final_Output)) + 'size' + '_' + str(datetime.datetime.now().strftime("%d-%m-%Y"))+'.xlsx',index = False)               
       
     
    return Final_Output , updated_ids

