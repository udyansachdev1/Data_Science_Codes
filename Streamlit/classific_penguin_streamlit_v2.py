# -*- coding: utf-8 -*-
"""
Created on Sun Mar 13 23:47:55 2022

@author: udyan.sachdev
"""

from numpy import mean
from numpy import std
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.ensemble import RandomForestClassifier
import pandas as pd
import streamlit as st
import numpy as np

st.write("""
# Penguin Prediction App
This app predicts the **Palmer Penguin** species!""")


# os.chdir(r'C:\Users\udyan.sachdev\OneDrive - Accenture\Python practice code\Streamlit')
# penguins = pd.read_csv('penguins_data.csv')

uploaded_file = st.sidebar.file_uploader("Upload your input CSV file", type=["csv"])

if uploaded_file is not None:
    penguins = pd.read_csv(uploaded_file)

    st.sidebar.header('Hyperparameters input')
    #########hyperparameters input################################################
    def hyperparameters_input():
           max_depth = st.sidebar.slider('max_depth', 0.0,30.0,15.0)
           n_estimators = st.sidebar.slider('n_estimators', 1.0,200.0,100.0)
           min_samples_split = st.sidebar.slider('min_sample_split', 1.0,10.0,5.0)
           max_leaf_nodes = st.sidebar.slider('max_leaf_nodes', 1.0,30.0,10.0)
           max_samples = st.sidebar.slider('max_samples', 1.0,249.0,100.0)
           max_features = st.sidebar.slider('max_features', 1.0,10.0,6.0)
           
           n_splits = st.sidebar.slider('n_splits', 1.0,20.0,10.0)
           n_repeats = st.sidebar.slider('n_repeats', 1.0,5.0,3.0)
           random_state = st.sidebar.slider('random_state', 1.0,5.0,1.0)
           data = {
                   'n_splits': n_splits,
                   'n_repeats': n_repeats,
                   'random_state': random_state,
                   'max_depth':max_depth,
                   'n_estimators':n_estimators,
                   'min_samples_split':min_samples_split,
                   'max_leaf_nodes':max_leaf_nodes,
                   'max_samples':max_samples,
                   'max_features':max_features,
                   }
           parameters = pd.DataFrame(data, index=[0])
           return parameters
    
     
    parameters_df = hyperparameters_input()
    
    ###############################################################
    st.sidebar.header('User Input Features')
    def user_input_features():
           island = st.sidebar.selectbox('Island',('Biscoe','Dream','Torgersen'))
           sex = st.sidebar.selectbox('Sex',('male','female'))
           bill_length_mm = st.sidebar.slider('Bill length (mm)', 32.1,59.6,43.9)
           bill_depth_mm = st.sidebar.slider('Bill depth (mm)', 13.1,21.5,17.2)
           flipper_length_mm = st.sidebar.slider('Flipper length (mm)', 172.0,231.0,201.0)
           body_mass_g = st.sidebar.slider('Body mass (g)', 2700.0,6300.0,4207.0)
           data = {'island': island,
                   'bill_length_mm': bill_length_mm,
                   'bill_depth_mm': bill_depth_mm,
                   'flipper_length_mm': flipper_length_mm,
                   'body_mass_g': body_mass_g,
                   'sex': sex}
           features = pd.DataFrame(data, index=[0])
           
           return features
       
    input_df = user_input_features()
    
    ###########################Target and predictor selection#############################
    df = penguins.copy()
    
    def target_select():
        target = st.sidebar.selectbox('Target variable',df.columns.tolist())
        return target
    
    def predict_select():
        predictor = st.sidebar.multiselect('Predictor variable',df.columns.tolist())
        return predictor
        
    target = target_select().split()
    
    predict = predict_select()
    
    df = df[target + predict]
    
    #######Apply if condition for no object var
    encode = df[predict]
    encode = encode.select_dtypes(include=['object'])
    
    for col in encode:
        dummy = pd.get_dummies(df[col], prefix=col)
        df = pd.concat([df,dummy], axis=1)
        del df[col]
    
    l1 = df[''.join(target)].unique().tolist()
    l2 = list(range(0, len(l1)))
    target_mapper = dict(zip(l1,l2))
    def target_encode(val):
        return target_mapper[val]
    
    df[target] = df[''.join(target)].apply(target_encode)
    
    # Separating X and y
    X = df.drop(target, axis=1)
    y = df[target]
    model = RandomForestClassifier(max_depth=int(parameters_df.iloc[0][3]) ,n_estimators = int(parameters_df.iloc[0][4]),min_samples_split = int(parameters_df.iloc[0][5]),max_leaf_nodes=int(parameters_df.iloc[0][6]),max_samples = int(parameters_df.iloc[0][7]), max_features =int(parameters_df.iloc[0][8]))
    # evaluate the model
    cv = RepeatedStratifiedKFold(n_splits=int(parameters_df.iloc[0][0]), n_repeats=int(parameters_df.iloc[0][1]), random_state=int(parameters_df.iloc[0][2]))
    n_scores = cross_val_score(model, X, y, scoring='accuracy', cv=cv, n_jobs=-1, error_score='raise')
    # report performance
    
    st.subheader('Model Performance')
    st.write('Accuracy: %.3f (%.3f)' % (mean(n_scores), std(n_scores)))
    # print('Accuracy: %.3f (%.3f)' % (mean(n_scores), std(n_scores)))
    
    # fit the model on the whole dataset
    model.fit(X, y)
    
    
    ###################################################################
    # penguins_raw = pd.read_csv('penguins_data.csv')
    penguins = penguins.drop(columns=['species'])
    df = pd.concat([input_df,penguins],axis=0)
    
    encode = ['sex','island']
    for col in encode:
        dummy = pd.get_dummies(df[col], prefix=col)
        df = pd.concat([df,dummy], axis=1)
        del df[col]
    input_df = df[:1] # Selects only the first row (the user input data)
    
    # make a single prediction
    row = [input_df.values[0].tolist()]
    yhat = model.predict(row)
    # print('Predicted Class: %d' % yhat[0])
    prediction_proba = model.predict_proba(row)
    
    ##Making predictions
    
    st.subheader('Prediction')
    penguins_species = np.array(['Adelie','Chinstrap','Gentoo'])
    st.write(penguins_species[yhat])
    
    st.subheader('Prediction Probability')
    st.write(prediction_proba)

else:
    st.header('Input a file to continue')

    
