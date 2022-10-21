# -*- coding: utf-8 -*-
"""
Created on Sun Dec 12 22:33:43 2021

@author: udyan.sachdev
"""


import re
from nltk.corpus import stopwords
# # nltk.download('stopwords')
from nltk.tokenize import word_tokenize
import pytesseract
from pdf2image import convert_from_path
import glob

## OCR 

pytesseract.pytesseract.tesseract_cmd = r"C:\Users\udyan.sachdev\Desktop\Tesseract OCR\tesseract.exe"

pdfs = glob.glob(r"C:\Users\udyan.sachdev\OneDrive - Accenture\Valentine C- Suite\Input\Annual Report 10K_G2K_1 to 15.pdf")

text = []

for pdf_path in pdfs:
    pages = convert_from_path(pdf_path, 500,first_page=1007,last_page=1034)

for pageNum,imgBlob in enumerate(pages):
    text_page = pytesseract.image_to_string(imgBlob,lang='eng')
    text.append(text_page)
    # with open(f'{pdf_path[:-4]}_page{pageNum}.txt', 'w') as the_file:
    #     the_file.write(text)
     
comments = ''
for i in text:
    comments = comments + i
    
#%%
import nltk

def text_cleaning(text_str):
    tmp = re.sub(r"[-()\"#/@;&:<>{}%^*`'+_/=~|®.!‘?,]", "", text_str)
    tmp = re.sub(r'\d+', '', tmp)
    tmp = tmp.lower()
    
    text_tokens = word_tokenize(tmp)
    words = nltk.corpus.stopwords.words('english')
    new_words = ['date', 'created', 'copyright', '©', 'sp','global','market','intelligence','division','inc']
    words.extend(new_words)
    tokens_without_sw = [word for word in text_tokens if not word in words]
    result = ' '.join(tokens_without_sw)
    
    return result

clean_text = text_cleaning(comments)

#%%
import nltk

bigrams = nltk.collocations.BigramAssocMeasures()
trigrams = nltk.collocations.TrigramAssocMeasures()

tokens = nltk.wordpunct_tokenize(clean_text)
bigramFinder = nltk.collocations.BigramCollocationFinder.from_words(tokens)
trigramFinder = nltk.collocations.TrigramCollocationFinder.from_words(tokens)

#%%
import pandas as pd

#bigrams
bigram_freq = bigramFinder.ngram_fd.items()
bigramFreqTable = pd.DataFrame(list(bigram_freq), columns=['bigram','freq']).sort_values(by='freq', ascending=False)
#trigrams
trigram_freq = trigramFinder.ngram_fd.items()
trigramFreqTable = pd.DataFrame(list(trigram_freq), columns=['trigram','freq']).sort_values(by='freq', ascending=False)



#%%
#get english stopwords
from nltk.corpus import stopwords

en_stopwords = set(stopwords.words('english'))
#function to filter for Verb/Noun bigrams
def rightTypes(ngram):
    if '-pron-' in ngram or 't' in ngram:
        return False
    for word in ngram:
        if word in en_stopwords or word.isspace():
            return False
    acceptable_types = ('VB', 'VBD', 'VBG', 'VBN','VBP','VBZ','NN', 'NNS', 'NNP', 'NNPS')
    second_type = ('VB', 'VBD', 'VBG', 'VBN','VBP','VBZ','NN', 'NNS', 'NNP', 'NNPS')
    tags = nltk.pos_tag(ngram)
    if tags[0][1] in acceptable_types and tags[1][1] in second_type:
        return True
    else:
        return False
#filter bigrams
filtered_bi = bigramFreqTable[bigramFreqTable.bigram.map(lambda x: rightTypes(x))]
#function to filter for trigrams
def rightTypesTri(ngram):
    if '-pron-' in ngram or 't' in ngram:
        return False
    for word in ngram:
        if word in en_stopwords or word.isspace():
            return False
    first_type = ('VB', 'VBD', 'VBG', 'VBN','VBP','VBZ', 'NN', 'NNS', 'NNP', 'NNPS')
    second_type = ('VB', 'VBD', 'VBG', 'VBN','VBP','VBZ', 'NN', 'NNS', 'NNP', 'NNPS')
    third_type = ('VB', 'VBD', 'VBG', 'VBN','VBP','VBZ', 'NN', 'NNS', 'NNP', 'NNPS')
    tags = nltk.pos_tag(ngram)
    if tags[0][1] in first_type and tags[1][1] in second_type and tags[2][1] in third_type:
        return True
    else:
        return False
#filter trigrams
filtered_tri = trigramFreqTable[trigramFreqTable.trigram.map(lambda x: rightTypesTri(x))]

filtered_bi.to_excel(r"C:/Users/udyan.sachdev/.xlsx")
filtered_tri.to_excel(r"C:/Users/udyan.sachdev/.xlsx")


#%%

#Chi-Square

bigramChiTable = pd.DataFrame(list(bigramFinder.score_ngrams(bigrams.chi_sq)), columns=['bigram','chi-sq']).sort_values(by='chi-sq', ascending=False)
trigramChiTable = pd.DataFrame(list(trigramFinder.score_ngrams(trigrams.chi_sq)), columns=['trigram','chi-sq']).sort_values(by='chi-sq', ascending=False)



#%%

####Bubble Chart

import plotly.express as px

fig = px.scatter(filtered_bi, x = 'bigram', y = 'freq', hover_name='bigram', text='bigram', size='freq'
, template='plotly_white', title='Bigram similarity and frequency', labels={'words': 'Avg. Length<BR>(words)'}
, color_continuous_scale=px.colors.sequential.Sunsetdark)
fig.update_traces(marker=dict(line=dict(width=1, color='Gray')))
fig.update_xaxes(visible=False)
fig.update_yaxes(visible=False)
fig.show()

