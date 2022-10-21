# -*- coding: utf-8 -*-
"""
Created on Tue Jun  7 12:30:55 2022

@author: udyan.sachdev
"""

import os
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)
import pandas as pd
import spacy
# spacy.cli.download("en_core_web_lg")
# nlp = spacy.load(name='en_core_web_lg')
nlp = spacy.load(name='en_core_web_sm')
nlp.max_length = 3000000
import re
import fitz
from datetime import date


def inputs():
    # Pdf Document
    print("\n","Enter Location of PDFs Document with PDF's name': e.g.",r'C:\Users\udyan.sachdev\pdf_name.pdf')
    pdf_path = input()
    
    print("\n","Enter comma separated following information: Starting Page, Ending Page: e.g. '1, 7'")
    pdf_range = input()

    # initializing dictionary with PDF's inputs
    val = []
    for i in pdf_range.split(","):
        val.append(re.sub(r"^\s+|\s+$", "", i))
    li = {}
    key = pdf_path.split("\\")[-1]
    li[key] = val

    # Load PDF
    doc = fitz.open(pdf_path)
    
    # Mapping File
    print("\n","Enter Location with Input Excel/CSV Name (Theme -> Subtheme -> Token Mapping): e.g.",r'C:\Users\udyan.sachdev\filename.xlsx')
    filename = input()
    if filename.split(".")[-1] == "xlsx":
        mapping_df = pd.read_excel(filename)
    else:
        mapping_df = pd.read_csv(filename)
        
    return li, doc, mapping_df


def sent_extract_with_frequency():
    # loading input data frame
    li, doc, mapping_df = inputs()

    # Iterating through the annual report (Get matched sentence & Frequency Distribution)
    for key,value in li.items():
        # Extract text
        text = ""
        for i in range(int(value[0])-1,int(value[1])):
            data = doc[i].get_text("dict")["blocks"]
            for block in data: # dict
                try:
                    for line in block["lines"]:
                        for line1 in line["spans"]:
                            if "heavy" in line1["font"].lower() or "medium" in line1["font"].lower() or "bold" in line1["font"].lower() or "400" in line1["font"].lower():
                                text1 = "bboldd" + line1["text"] + "bboldd"
                            else:
                                text1 = line1["text"]
                            text = text + " " + text1
                except:
                    pass
        text = re.sub(' +',' ', text)
        text = re.sub(r"^\s+|\s+$", "", text)        
        print("Text Extraction Done.")
    
        # Direct Match Sentences
        similar_df = mapping_df[mapping_df['Sub Themes'] != 'Others']
        similar_tokens_dict = dict(zip(similar_df['Sub Themes'], similar_df['similar_tokens']))
        
        doc_sent = nlp(text)
        word_count, sentence, searchtoken, subthemes = [], [], [], []
        for subtheme, search_keywords in similar_tokens_dict.items():
                for search_token in search_keywords.split(','):
                    sentences = ''
                    wordcount=0
                    for sent in doc_sent.sents:
                        sent = sent.text
                        r2 = re.findall(search_token, sent, flags=re.IGNORECASE)
                        if len(r2)>0:
                            wordcount = wordcount + len(r2)
                            sentences = sentences + sent + " |||||| " 
                    if wordcount > 0:
                        word_count.append(wordcount)
                        sentence.append(sentences)  
                        searchtoken.append(search_token)
                        subthemes.append(subtheme)
        df_final = pd.DataFrame()
        df_final['word_frequency'] = word_count
        df_final['searchtoken'] = searchtoken
        df_final['subtheme'] = subthemes
        df_final['sentence'] = sentence
        df_final1 = pd.merge(df_final,mapping_df[['Sub Themes','Themes']],left_on="subtheme",right_on = "Sub Themes", how='left')
        print("Direct Matching Done.")

        #to change output file name dynamically
        key = key[:-4]
        key = key.replace(" ","").replace("_","")
        df_final1["sentence"] = df_final1["sentence"].apply(lambda x: x.replace("\n", " ").replace("=",""))
        df_final1["sentence"] = df_final1["sentence"].apply(lambda x: re.sub(' +',' ', x))
        df_final1["sentence"] = df_final1["sentence"].apply(lambda x: re.sub(r"^\s+|\s+$", "", x))
        
        # Export Aggregated values of token frequency at theme & subtheme level
        df_theme = df_final1.groupby('Themes')["word_frequency"].agg(sum).reset_index()
        df_theme.rename(columns={'word_frequency':'Themes_count'}, inplace=True)
        df_theme["Themes_percentage"] = df_theme["Themes_count"]/df_theme["Themes_count"].sum()
        df_theme = df_theme.sort_values('Themes_count',ascending=False)
        df_subtheme = df_final1.groupby(["Themes","subtheme"])["word_frequency"].agg(sum).reset_index()
        df_subtheme.rename(columns={'word_frequency':'Subthemes_count'}, inplace=True)
        df_final2 = df_theme.merge(df_subtheme,how="left",on="Themes")
        df_final2["Subthemes_percentage"] = df_final2["Subthemes_count"]/df_final2["Themes_count"]
        df_final2.insert(1, 'subtheme', df_final2.pop('subtheme'))        
        df_final2 = df_final2.sort_values(['Themes', 'Subthemes_count'], ascending=[True,False])
                
        # Export outputs
        file1 = "All_Sentence_TextReport_SK_31Aug22_v1.xlsx"
        file2 = "Frequency_TextReport_SK_31Aug22_v1.xlsx"

        print("\n","Enter Location for Output files': e.g.",r'C:\Users\udyan.sachdev')
        output_path = input()
        del df_final1["subtheme"]
        df_final1.to_excel(os.path.join(output_path,file1),index=False)
        df_final2.to_excel(os.path.join(output_path,file2),index=False)

        print("Frequency Count Done.")


sent_extract_with_frequency()
