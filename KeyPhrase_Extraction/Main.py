# -*- coding: utf-8 -*-
"""
Created on Thu Jun  9 16:09:40 2022

@author: udyan.sachdev
"""

import re
import string
import RAKE
import nltk
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer


def inputs():
    print("\n","Enter Location with Input Excel/CSV Name: e.g.",r'C:\Users\udyan.sachdev\filename.xlsx')
    filename = input()
    if filename.split(".")[-1] == "xlsx":
        df = pd.read_excel(filename)
    else:
        df = pd.read_csv(filename)

    return df


def data_cleaning(phrase,searchtoken):
    """ Removing digits, punctuations & extra space from the phrases """
    lst_punctuation = string.punctuation.replace("&","").replace("-","")
    if any(char.isdigit() for char in searchtoken) == False:
        phrase = ''.join([i for i in phrase if not i.isdigit()])
    phrase = phrase.translate(str.maketrans(lst_punctuation, ' '*len(lst_punctuation)))
    phrase = re.sub(r'[!.,#? |$|]*%@ (D)]^]', r'', phrase)
    phrase = phrase.replace("—", "").replace("”", "").replace("“", "").replace("‘", "")
    phrase = re.sub(' +',' ',phrase)
    phrase = re.sub(r"^\s+|\s+$", "", phrase)
    if phrase.split()[0] == "and":
        phrase = phrase[4:]
    if phrase.split()[-1] == "and":
        phrase = phrase[:-4]
    return phrase


def Sort_Tuple(tup):
    """ Sorting tuple output """
    tup.sort(key = lambda x:x[1],reverse=True)
    
    return tup


def phrase_Extraction_Level1(df):
    """ Extracting Key-Phrases from the sentences"""
    print("Starting Key-Phrase Extraction!!!")

    # Stop-Words List
    stopword_list = nltk.corpus.stopwords.words ("english")
    stopword_list = list(set(stopword_list) - {"and"})

    # Rake Algorithm calling as a object
    rake_object = RAKE.Rake(stopword_list)
    lst_sentence1, lst_phrase1, lst_MT1, lst_ST1, lst_token1 = [],[],[],[],[]
    
    for i in range(df.shape[0]):
        searchtoken = str(df["searchtoken"][i]).replace("\n", " ")
        searchtoken = re.sub(r"^\s+|\s+$", "", searchtoken.lower())
        text = df["sentence_proper"][i]
            
        # Removing setences if number to text ratio greater than 10%
        if len(text) > 0 and len(re.sub("[^0-9]", "", text))/len(text) <= .1:
            text = text.replace(" the "," ")
            # if "&" in searchtoken then replacing it with "and"                
            if "&" in searchtoken:
                searchtoken = searchtoken.replace("&","and")
                text = text.replace("&","and")
            phrase_dict = Sort_Tuple(rake_object.run(text,maxWords=11))
            
            # if no phrase extracted from sentence (tag as "Not-Applicable")
            if len(phrase_dict) == 0:
                lst_phrase1.append("0... No phrase Found!!!")
                lst_sentence1.append(text)
                lst_MT1.append(df["Themes"][i])
                lst_ST1.append(df["Sub Themes"][i])
                lst_token1.append(df["searchtoken"][i])
            else:
                cnt = 0
                for item in phrase_dict:
                    phrase = str(item[0])
                    total_words = phrase.replace("-"," ").split(" ")                        
                    # Keeping phrases only if searchtoken in phrase & total words in phrase greater than 1
                    if searchtoken in phrase and len(total_words) > 1:
                        # phrase data cleaning
                        phrase = data_cleaning(phrase,searchtoken)
                        if "&" in df["searchtoken"][i]:
                            phrase = phrase.replace("and","&")
                        # Appending output in the list
                        lst_phrase1.append(phrase)
                        lst_sentence1.append(text)
                        lst_MT1.append(df["Themes"][i])
                        lst_ST1.append(df["Sub Themes"][i])
                        lst_token1.append(df["searchtoken"][i])
                        cnt = cnt + 1
                        break
                    else:
                        continue
                if cnt == 0:
                    lst_phrase1.append("0... Criteria Failed!!!")
                    lst_sentence1.append(text)
                    lst_MT1.append(df["Themes"][i])
                    lst_ST1.append(df["Sub Themes"][i])
                    lst_token1.append(df["searchtoken"][i])
        else:
            lst_phrase1.append("0... Possible TABLE!!!")
            lst_sentence1.append(text)
            lst_MT1.append(df["Themes"][i])
            lst_ST1.append(df["Sub Themes"][i])
            lst_token1.append(df["searchtoken"][i])
            
    df_phrase = pd.DataFrame({
                            "Main Theme":lst_MT1,
                            "Sub Theme":lst_ST1,
                            "searchtoken":lst_token1,
                            "Sentences":lst_sentence1,
                            "Top Phrase":lst_phrase1
                            })
    return df_phrase


def phrase_Extraction_Level2(df):
    # Stop-Words List
    stopword_list = nltk.corpus.stopwords.words ("english")
    
    # Get phrase for sentences where RAKE failed
    lst_phrase1, lst_index = [], []
    for i in range(df.shape[0]):
        lst_text = []
        searchtoken = str(df["searchtoken"][i]).replace("\n", " ")
        searchtoken = re.sub(r"^\s+|\s+$", "", searchtoken.lower())
        searchtoken = searchtoken.split(",")[0]
        text = df["Sentences"][i]
        # if "&" in searchtoken then replacing it with "and"
        if "&" in searchtoken:
            searchtoken = searchtoken.replace("&","and")
            text = text.replace("&","and")
        lst_text.append(text)
        current_phrase = str(df["Top Phrase"][i])
        if current_phrase != "0... Criteria Failed!!!" and current_phrase.count(" ") >= 3:
            lst_phrase1.append(current_phrase)
            lst_index.append(i)
        else:
            for j in range(5,12):
                cnt = 0
                vec = CountVectorizer(analyzer='word', ngram_range=(j,j))
                vec.fit_transform(lst_text)
                for phrase in vec.get_feature_names():
                    lst_words = phrase.split()
                    if searchtoken in phrase and lst_words[0] not in stopword_list and lst_words[-1] not in stopword_list:
                        if "&" in df["searchtoken"][i]:
                            phrase = phrase.replace("and","&")
                        phrase = data_cleaning(phrase,searchtoken)
                        lst_phrase1.append(phrase)
                        lst_index.append(i)
                        cnt = cnt + 1
                        break
                if cnt > 0:
                    break

    # print(len(lst_phrase1),df.shape)
    df["Top Phrase"] = lst_phrase1
    print("Key-Phrase Extracted!!!")
    print("Data's Shape :::",df.shape)
    
    return df


def main():
    # Loading inputs
    df = inputs()
        
    # Key-Phrase Extraction (Level 1)
    df_phrase = phrase_Extraction_Level1(df=df)
    
    # Key-Phrase Extraction (Level 2)
    df_phrase = phrase_Extraction_Level2(df=df_phrase)

    print("\n","Enter Location with Output Excel Name: e.g.",r'C:\Users\udyan.sachdev\filename.xlsx')
    filename = input()
    df_phrase.to_excel(filename,index=False)

main()
