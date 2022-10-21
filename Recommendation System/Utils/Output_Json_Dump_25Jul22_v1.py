# -*- coding: utf-8 -*-
"""
Created on Mon Jul 25 23:35:15 2022

@author: udyan.sachdev
"""

from time import gmtime, strftime
import json
 
def json_creation(Final_Output):
    output_dict = []
    for i in range(0,len(Final_Output),10):    
        productname = Final_Output['Input_doc_fullName'][i]
        productid = Final_Output['Input_doc_id'][i]
        inputindustry = Final_Output['Input_doc_Industry'][i]
        rows = Final_Output.loc[(Final_Output['Input_doc_fullName'] == productname)]
        
        rows = rows.drop(['Input_doc_id','Input_doc_fullName','Input_doc_Tags','Input_doc_Industry'], axis=1)
    
        final_nested_recommendation = rows.to_dict('records')
      
        input_dict = {
                    'Input_doc_fullName': productname,
                    'Input_doc_id': productid,
                    'Input_doc_Industry':inputindustry,
                    'Recommendations': final_nested_recommendation,
                    'Creation Datetime':strftime("%Y-%m-%dT%H:%M:%SZ", gmtime()),
                    'Last Updated Datetime': strftime("%Y-%m-%dT%H:%M:%SZ", gmtime())   
                }
        
        output_dict.append(input_dict)
        
    #######nested dict to json
    
    data_to_upload = json.dumps(output_dict)
    
    return data_to_upload





