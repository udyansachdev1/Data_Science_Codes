# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

def all_recommendation(result):
    Final_Output = result.reset_index(drop=True)
    Final_Output['Count_Overlapped_Tags'] = ''
    Final_Output['Overlapped_Percentage'] = ''
    for i in range(len(Final_Output)):
        Final_Output['Count_Overlapped_Tags'][i] = len(Final_Output['Overlapped_Tags'][i].split())

        if Final_Output['Overlapped_Tags'][i] == 'set()':
            Final_Output['Count_Overlapped_Tags'][i] = 0
   
    ##Overlapping of tags perecentage
        x = Final_Output['Input_Dataset_Tags'][i] + ' ' + Final_Output['Output_Dataset_Tags'][i]
        length = len(' '.join(dict.fromkeys(x.split())).split())
        Final_Output['Overlapped_Percentage'][i] = (Final_Output['Count_Overlapped_Tags'][i]/length)*100
    
    
    Final_Output.rename(columns = {'Input_Dataset':'Input_doc_fullName','Input_Dataset_Tags':'Input_doc_Tags',
                                   'Output_Dataset':'Recommended_doc_fullName','Output_Dataset_Tags':'Recommended_doc_Tags',
                                  'Output_Dataset_Industry':'Recommended_doc_Industry'}, inplace = True)
    return Final_Output

    
    