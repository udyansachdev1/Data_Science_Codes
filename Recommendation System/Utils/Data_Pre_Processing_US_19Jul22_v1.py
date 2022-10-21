# -*- coding: utf-8 -*-
"""
Created on Tue Jul 19 16:25:04 2022

@author: udyan.sachdev
"""

from nltk.corpus import stopwords
stop = stopwords.words('english')

def data_preprocessing(output):
    
    # output= output[output["_source_datasetContractStatus"] == 'Contracted Dataset'].reset_index(drop=True)

     #rename column due to new data format
    output.rename(columns={'_source_fullName': 'doc_fullName', "_id": "id",
                           "_source_definitionDisplay": "doc_definitionDisplay"}, inplace=True)
     
    data_req = output           
    # ##filtering for banking industry
    # data_req = output[output['Industry'] == 'Banking'].reset_index(drop=True)
    
    # ##filtering for non banking
    # data_not_req = output[output['Industry'] != 'Banking'].reset_index(drop=True)
    
    # ## deleting rows from data_not_req where doc_fullName is same in data_req
    # for i in range(len(data_req)):
    #     if data_req['doc_fullName'][i] in data_not_req['doc_fullName'].values:
    #         data_not_req.drop(data_not_req.index[data_not_req['doc_fullName'] == data_req['doc_fullName'][i]], inplace=True)
    
    # ##deleting duplicate rows
    # data_not_req = data_not_req.drop_duplicates(subset = ['doc_fullName'])

    # ## appending data frame   
    # data_req = pd.concat([data_req, data_not_req], axis=0).reset_index(drop=True)
    
    tag_col = [col for col in output if col.startswith('_source_tagNames')]
    # tag_col = [col for col in output if col.startswith('doc_functionNames') or col.startswith('doc_dataSubCategoryNames') or col.startswith('doc_dataCategoryNames_')]

    # # tag_col = [col for col in output if col.startswith('doc_functionNames') or
    # #        col.startswith('doc_dataSubCategoryNames') or 
    # #        col.startswith('doc_dataCategoryNames_') or
    # #        col.startswith('doc_relatedUsecases_')]
    # #        # or
    # #        #  col.startswith('doc_industryGroupNames_')]

    
    # ##selecting first word of underscore
    # for i in tag_col:
    #     # data_req[i] = data_req[i].str.split('_').str.get(0)
    #     data_req[i] = data_req[i].str.replace("US","")
    #     data_req[i] = data_req[i].str.replace("U.S.","")
    #     # data_req[i] = data_req[i].str.replace("M","")

   
    ##joining all the columns
    data_req['concat_column'] = data_req[data_req.columns.intersection(tag_col)].apply(
    lambda x: ' '.join(x.dropna().astype(str)),
    axis=1)
    
    ###removing stop words
    data_req['concat_column'] = data_req['concat_column'].apply(lambda x: ' '.join([word for word in x.split() if word not in (stop)]))
    
    
    ##subset the dataframe
    subset_col = ['id' , 'doc_fullName' , 'doc_definitionDisplay' , 'Industry' ,'concat_column'] 
    data_req = data_req[subset_col]

    
    data_req['concat_column'] = data_req['concat_column'].astype(str)
    
    # ##applying rake for empty concat_column
    # rake = Rake(min_chars = 1 , max_words = 2)
    # for i in range(len(data_req)):
    #     if data_req['concat_column'][i] == '':
    #         if pd.isna(data_req['doc_definitionDisplay'][i]) == False:
    #             keywords = rake.apply(data_req['doc_definitionDisplay'][i])
    #             tag = ' '.join([x[0] for x in keywords])
    #             data_req['concat_column'][i] =' '.join(dict.fromkeys(tag.split()))               
    #         else:
    #             keywords = rake.apply(data_req['doc_definitionDisplay'][i])
    #             tag = ' '.join([x[0] for x in keywords])
    #             data_req['concat_column'][i] =' '.join(dict.fromkeys(tag.split()))    
    
    

    #text cleaning
    data_req['concat_column'] = data_req['concat_column'].str.lower()
    data_req['concat_column'] = data_req['concat_column'].str.replace("united_states","")
    data_req['concat_column'] = data_req['concat_column'].str.replace("US","")
    data_req['concat_column'] = data_req['concat_column'].str.replace("U.S.","")
    data_req['concat_column'] = data_req['concat_column'].str.replace("audm","")
    data_req['concat_column'] = data_req['concat_column'].str.replace("aspects","")
    data_req['concat_column'] = data_req['concat_column'].str.replace("amp","")  
    data_req['concat_column'] = data_req['concat_column'].str.replace(";"," ")
    data_req['concat_column'] = data_req['concat_column'].str.replace("&"," ")
    data_req['concat_column'] = data_req['concat_column'].str.replace("_"," ")
    data_req['concat_column'] = data_req['concat_column'].str.replace("-"," ")
    data_req['concat_column'] = data_req['concat_column'].str.replace("</p>"," ")
    data_req['concat_column'] = data_req['concat_column'].str.replace("<p>"," ")
    data_req['concat_column'] = data_req['concat_column'].str.replace("<strong>"," ")
    data_req['concat_column'] = data_req['concat_column'].str.replace("<br/>"," ")
    data_req['concat_column'] = data_req['concat_column'].str.replace("</strong>"," ")
    data_req['concat_column'] = data_req['concat_column'].str.lstrip()
    
    data_req['Industry1'] = ''
    
    data_req['Industry1'] = data_req[['id' , 'doc_fullName'  ,'Industry','concat_column']].groupby(['id'])['Industry'].transform(lambda x: ','.join(x)).reset_index(drop=True)
    
    data_req = data_req.drop_duplicates(subset='id').reset_index(drop=True)
    data_req.drop("Industry", axis=1, inplace=True)
    data_req.rename(columns={'Industry1': 'Industry'}, inplace=True)
     
    
    return data_req
