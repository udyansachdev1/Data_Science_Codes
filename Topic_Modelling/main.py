# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 00:39:43 2022

@author: udyan.sachdev
"""

import pandas as pd
import os
import glob
import nltk
from nltk.corpus import stopwords
from nltk import ngrams
from datetime import date, timedelta
from gensim import corpora, models, similarities
from gensim.models.phrases import Phrases, Phraser
from textblob import Word
# Load the library with the CountVectorizer method
import seaborn as sns
sns.set_style('whitegrid')


#Working Directory & Path selection
cwd=os.getcwd() # Creating path based on platform #
input_dir = cwd+'\Input_Data'
output_dir = cwd+'\Output_Data'  
start_date_modi = str(date.today()).replace("-", "_")

#Reading File
input_files = glob.glob(input_dir +'\*.csv')
df = pd.read_csv(input_files[0])

def Topic_Modeling(dataframe,var1): 
    df = dataframe.copy()
    #Modifying variable name
    var2 = var1.replace(" ", "_") +"_Cleaned"
    
    #Cleaning the text column
    #replace newline characer with space
    df[var2]=df[var1].str.replace("\n"," ")
    #fill null rows with blank
    df[var2]=df[var2].fillna("")
    #convert data to lowercase
    df[var2]=df[var2].apply(lambda x: x.lower())
    #remove numerics
    df[var2]=df[var2].str.replace("[0-9]","")
    #remove punctuation
    df[var2]=df[var2].str.replace("[^'\w\s]","")
    #Remove stopwords 
    stop = stopwords.words("english")
    stop.extend(['want','getting','made','per','said','never','know','will','date','call','detail','posted','customer','requesting',
                 'wanted','requested','request','received','complaining','unable','related','able','back','issue','stated',
                 'however','shared'])
    df[var2]=df[var2].apply(lambda x: " ".join(x for x in x.split() if x not in stop))
    #lemmatization
    df[var2]=df[var2].apply(lambda x: " ".join([Word(myword).lemmatize() for myword in x.split()]))
    
    #Tokenizing cleaned sentence
    df['tokenized_sents'] = df.apply(lambda row: nltk.word_tokenize(row[var2]), axis=1)
    
    documents=df[var1].tolist()
    cleaned_documents=df[var2].tolist()
    tokens=df['tokenized_sents'].tolist()
    
    bigram_tokens=[]
    for i in cleaned_documents:
        token = nltk.word_tokenize(i)
        bigram = list(ngrams(token, 2))
        res = [' '.join(tups) for tups in bigram] 
        bigram_tokens.append(res)
    # Create Dictionary.
    id2word = corpora.Dictionary(bigram_tokens)
    # Creates the Bag of Word corpus.
    mm = [id2word.doc2bow(text) for text in bigram_tokens]
    
    # Trains the LDA models.
    lda = models.ldamodel.LdaModel(corpus=mm, id2word=id2word, num_topics=4,
                                   random_state=100,update_every=1, chunksize=10, passes=2,minimum_probability=0,
                                   alpha='symmetric',iterations=100,per_word_topics=True)
    
    def format_topics_sentences(ldamodel=None, corpus=mm, texts=bigram_tokens):
        # Init output
        sent_topics_df = pd.DataFrame()
    
        # Get main topic in each document
        for i, row_list in enumerate(ldamodel[corpus]):
            row = row_list[0] if ldamodel.per_word_topics else row_list            
            # print(row)
            row = sorted(row, key=lambda x: (x[1]), reverse=True)
            # Get the Dominant topic, Perc Contribution and Keywords for each document
            for j, (topic_num, prop_topic) in enumerate(row):
                if j == 0:  # => dominant topic
                    wp = ldamodel.show_topic(topic_num)
                    topic_keywords = ", ".join([word for word, prop in wp])
                    sent_topics_df = sent_topics_df.append(pd.Series([int(topic_num), round(prop_topic,4), topic_keywords]), ignore_index=True)
                else:
                    break
        sent_topics_df.columns = ['Dominant_Topic', 'Perc_Contribution', 'Topic_Keywords']
    
        # Add original text to the end of the output
        contents = pd.Series(texts)
        sent_topics_df = pd.concat([sent_topics_df, contents], axis=1)
        return(sent_topics_df)

    df_topic_sents_keywords = format_topics_sentences(ldamodel=lda, corpus=mm, texts=bigram_tokens)
    
    # Format
    df_dominant_topic = df_topic_sents_keywords.reset_index()
    df_dominant_topic.columns = ['Document_No','Dominant_Topic','Topic_Perc_Contrib','Keywords', 'Text']
    
    df1=df.join(df_dominant_topic)
    
    df1.to_excel(output_dir+"/"+ "Topic_Modeling"+"_US_"+start_date_modi +".xlsx",index=False)
    
    return

Topic_Modeling(df,'Gist of the case')