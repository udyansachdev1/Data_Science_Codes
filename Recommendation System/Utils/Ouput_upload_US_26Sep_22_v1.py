# -*- coding: utf-8 -*-
"""
Created on Mon Sep 26 15:31:10 2022

@author: udyan.sachdev
"""

import pandas as pd


def output_upload(Final_Output_df, merge_output_df, current_time):

    output_file_name = "Marketplace_RS_Output_Dataset_" + current_time.strftime("%m-%d-%Y") + ".xlsx"

    # running for the first time only case
    if len(merge_output_df) > 0:
        total = merge_output_df.sort_values(['Input_doc_id', 'Last Updated Datetime', 'Score'], ascending=False).groupby('Input_doc_id').head(10).reset_index(drop=True)

        # comparing last output with the new generated one
        total1 = total.drop(['Score','Count_Overlapped_Tags','Overlapped_Percentage','Creation Datetime','Last Updated Datetime'],axis = 1)
        Final_Output1 = Final_Output_df.drop(['Score','Count_Overlapped_Tags','Overlapped_Percentage'],axis=1)
        Output_diff = pd.concat([total1,Final_Output1]).drop_duplicates(keep=False).reset_index(drop=True)
   
        ##list of all updated names
        list_unique_datasetname = Output_diff['Input_doc_fullName'].unique() 
        list_unique_all_ids = Output_diff['Input_doc_id'].unique() 
        
        ###list of old names
        list_unique_old_ids = total1['Input_doc_id'].unique()
        
        ##list of existing update 
        updated_ids = list(set(list_unique_all_ids).intersection(list_unique_old_ids))
        
        Final_Output_df = Final_Output_df[Final_Output_df['Input_doc_fullName'].isin(list_unique_datasetname.tolist())].reset_index(drop=True)

    Final_Output_df['Creation Datetime'] = current_time.strftime("%Y-%m-%d %H:%M:%S")
    Final_Output_df['Last Updated Datetime'] = current_time.strftime("%Y-%m-%d %H:%M:%S")
    Final_Output_df.to_excel(output_file_name, index=None)
     
    return Final_Output_df, output_file_name , updated_ids

