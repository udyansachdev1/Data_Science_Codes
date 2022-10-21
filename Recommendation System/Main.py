# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import warnings
warnings.filterwarnings("ignore")

def process_run():
    
    ## importing data
    from Utils.Data_Sourcing_US_19Jul22_v1 import data_input
    print('Data has been imported')
    
    ## data pre processing
    from Utils.Data_Pre_Processing_US_19Jul22_v1 import data_preprocessing
    pre_processed_data = data_preprocessing(data_input())   
    print('Data has been pre-processed')

    ## Construct cosine matrix
    from Utils.Cosine_Matrix_US_19Jul22_v1 import cosine_matrix
    cosinematrix = cosine_matrix(pre_processed_data)
    print('Cosine Matrix constructed')

    ## get recommendation
    from Utils.Get_Recommendation_US_19Jul22_v1 import get_recommendations
    result = get_recommendations(pre_processed_data, cosine_sim = cosinematrix[0] ,idx = cosinematrix[1])
    print('Recommendation dataframe created')

    ## Create output for recommendation system
    from Utils.Recommendation_Sys_US_26_Aug22_v1 import all_recommendation
    result_total = all_recommendation(result)
    print('Final output created')
    
    ##export updated output to excel
    from Utils.Ouput_upload_US_26Sep_22_v1 import output_upload
    updated_file = output_upload(result_total)
    
    ## Creating Json file
    from Utils.Output_Json_Dump_25Jul22_v1 import json_creation
    json_output = json_creation(updated_file)
    print('Json conversion done')
    
    return json_output

if __name__ == '__main__':
    json = process_run()


    