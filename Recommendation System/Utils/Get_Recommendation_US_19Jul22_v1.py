# -*- coding: utf-8 -*-
"""
Created on Tue Jul 19 16:28:15 2022

@author: udyan.sachdev
"""
import pandas as pd

def get_recommendations(pre_processed_data, cosine_sim ,idx):

    # Get the index of the dataset that matches the title
    total_out = []

    for i in range(len(pre_processed_data)):

            x = pre_processed_data['doc_fullName'][i]
            index = idx[x]
            
            # Get the pairwsie similarity scores of all datasets with that dataset
            sim_scores = list(enumerate(cosine_sim[index]))
        
            # Sort the dataset based on the similarity scores
            sim_scores = sorted(sim_scores, key=lambda x: x[1], reverse=True)
            tup_dict = dict(sim_scores) 
            tup_dict.pop(index)
            sim_scores = list(tuple(tup_dict.items()))[0:10]
            # Get the dataset indices
            dataset_indices = [i[0] for i in sim_scores]
            # Get the scores of the 10 most similar dataset
            # dataset_indices = dataset_indices
        
        
            result = pd.DataFrame(columns=['Output_Dataset','Score'])
            result['Output_Dataset'] = pre_processed_data['doc_fullName'].iloc[dataset_indices].tolist()
            result['Score'] =[x[1] for x in sim_scores]
            result['Input_doc_id']= pre_processed_data.loc[pre_processed_data['doc_fullName'] == idx.index[index], 'id'].values[0]    
            result['doc_fullName'] = idx.index[index]
            result['Input_Dataset_Tags'] = pre_processed_data.loc[pre_processed_data['doc_fullName'] == idx.index[index], 'concat_column'].values[0]
            result['Input_doc_Industry']= pre_processed_data.loc[pre_processed_data['doc_fullName'] == idx.index[index], 'Industry'].values[0]
            result['Overlapped_Tags']= " "
            result['Output_Dataset_Tags'] = " "
            result['Output_Dataset_Industry'] = " "
            result['Recommended_doc_id'] = ''
        
            for j in range(len(result)):
                result['Overlapped_Tags'][j]  = str(set(pre_processed_data.loc[pre_processed_data['doc_fullName'] == result['doc_fullName'][j], 'concat_column'].values[0].split()) & set(pre_processed_data.loc[pre_processed_data['doc_fullName'] == result['Output_Dataset'][j], 'concat_column'].values[0].split()))
                result['Output_Dataset_Tags'][j] = pre_processed_data.loc[pre_processed_data['doc_fullName'] == result['Output_Dataset'][j], 'concat_column'].values[0]   
                result['Output_Dataset_Industry'][j] = pre_processed_data.loc[pre_processed_data['doc_fullName'] == result['Output_Dataset'][j], 'Industry'].values[0]   
                result['Recommended_doc_id'][j] = pre_processed_data.loc[pre_processed_data['doc_fullName'] == result['Output_Dataset'][j], 'id'].values[0]   
        
            # Return the top 10 most similar dataset
            result.rename(columns = {'doc_fullName':'Input_Dataset'}, inplace = True)
            result = result[['Input_doc_id','Input_Dataset','Input_Dataset_Tags','Input_doc_Industry','Recommended_doc_id','Output_Dataset','Output_Dataset_Tags','Output_Dataset_Industry','Score','Overlapped_Tags']]
            total_out.append(result)
    Final_Output = pd.concat(total_out)
        
    return Final_Output