# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 01:12:18 2022

@author: udyan.sachdev
"""

def single_exponential(data, Variable, forcast_period):
    test_period = forcast_period
    month_ahead = len(data)+forcast_period-1
    Train_MAPE={}
    Test_MAPE={}
    
    import pandas as pd
    import numpy as np
    from statsmodels.tsa.api import SimpleExpSmoothing
    # To calculate the MAPE
    def MAPE(Y_actual,Y_Predicted):
        mape=np.mean(np.abs((np.array(Y_actual)-np.array(Y_Predicted))/np.array(Y_actual)))*100
        return mape
    
    df = data[[Variable]]
    forecast_df1=pd.DataFrame()
    for n,i in enumerate(df.columns):
        print(n,i)
        data_df=df.iloc[:,n]
        #Splitting data into train and test
        train=data_df.head(len(data)-test_period)
        test=data_df.tail(test_period)
        #End date of prediction is the last date of the test dataset
        END_DATE=test.index[-1]
        #Create an empty dataframe with the specified columns
        column_names = ["Smoothing_level_param", "Train_MAPE", "Test_MAPE"]
        SES_model_results= pd.DataFrame(columns = column_names)
        columns = list(SES_model_results)
        data_values = []
    
        smoothing_level_param=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
        
            # Loop for the hyperparameter
        for p in range(len(smoothing_level_param)):
            print(p)
            print(smoothing_level_param[p])
            model2=SimpleExpSmoothing(train,initialization_method="heuristic")
            model_fit2=model2.fit(smoothing_level=smoothing_level_param[p])
            yhat2=model_fit2.predict(start=train.index[0],end=END_DATE)
    
            forecast_df1.at[:, df.columns[n]]=yhat2 # Predicted value is in the forecast_df1
                  
            #Train Mape
            y_true_train=[float(i) for i in train.values]
            y_preds_train=[float(i) for i in yhat2.values[:len(train)]]
    
            #Test Mape
            y_true_test=[float(i) for i in test.values]
            y_preds_test=[float(i) for i in yhat2.values[len(train):]]
    
    
                  
            # Storing the MAPE
            ##Train
            #Train_MAPE['Single exp.smoothing_Train']=MAPE(y_true_train,y_preds_train)
            ##Test
            #Test_MAPE['Single exp.smoothing_Test']=MAPE(y_true_test,y_preds_test)
            row_values=[smoothing_level_param[p],MAPE(y_true_train,y_preds_train),MAPE(y_true_test,y_preds_test)]
            zipped = zip(columns, row_values)
            a_dictionary = dict(zipped)
            #print(a_dictionary)
            data_values.append(a_dictionary)
    
    SES_model_results = SES_model_results.append(data_values, True)
    del data_values
    
    #Smoothing_level_param corresponding to the min Test_MAPE
    Smoothing_level_param_min_Test_MAPE=SES_model_results.loc[SES_model_results['Test_MAPE'] == SES_model_results['Test_MAPE'].min(), 'Smoothing_level_param'].values[0]
    
    #Full data used in the model to generate oot forecast
    forecast_full=pd.DataFrame()
    model_full=SimpleExpSmoothing(data_df,initialization_method="heuristic")
    model_fit_full=model_full.fit(smoothing_level=Smoothing_level_param_min_Test_MAPE)
    yhat2_oot=model_fit_full.predict(start=train.index[0],end=month_ahead)
    
    forecast_full.at[:, df.columns[n]]=yhat2_oot
    
    ff_SES=pd.DataFrame(yhat2_oot[len(data_df):])
    #ff_SES=pd.DataFrame(yhat2_oot)
    
    # Rename the column of the final forecast
    
    ff_SES.rename(columns = {0:'Forecasted_'+Variable+''},inplace = True)
    
    # Recording the min Train MAPE and Test MAPE
    Train_MAPE['MAPE'] = SES_model_results.loc[SES_model_results['Test_MAPE'] == SES_model_results['Test_MAPE'].min(), 'Train_MAPE'].values[0]
    Test_MAPE['MAPE'] = SES_model_results['Test_MAPE'].min()
    
    MAPE_df=pd.DataFrame(columns = ['Train_MAPE','Test_MAPE'])
    MAPE_df['Train_MAPE'] = Train_MAPE
    MAPE_df['Test_MAPE'] = Test_MAPE
    
    MAPE_df=pd.DataFrame([Train_MAPE,Test_MAPE])
    MAPE_df['Split'] = ['Train_MAPE','Test_MAPE']
    MAPE_df = MAPE_df[['Split','MAPE']]
    
    
    return ff_SES, MAPE_df


def double_exponential(data, Variable, forcast_period):
    #Double Exponential Smoothing model
    #DES hyper parameter tuning
    
    test_period = forcast_period
    month_ahead = len(data)+forcast_period-1
    Train_MAPE={}
    Test_MAPE={}
    
    import pandas as pd
    import numpy as np
    from statsmodels.tsa.api import Holt
    # To calculate the MAPE
    def MAPE(Y_actual,Y_Predicted):
        mape=np.mean(np.abs((np.array(Y_actual)-np.array(Y_Predicted))/np.array(Y_actual)))*100
        return mape
    
    df = data[[Variable]]
    #df=df.replace(0,0.1)
    forecast_df1=pd.DataFrame()
    
    for n,i in enumerate(df.columns):
        print(n,i)
        data_df=df.iloc[:,n]
        
        #Splitting data into train and test
        train=data_df.head(len(data)-test_period)
        test=data_df.tail(test_period)
        #End date of prediction is the last date of the test dataset
        END_DATE=test.index[-1]
        #print(data_df.head())
        
        #Create an empty dataframe with the specified columns
        column_names = ["Smoothing_level_param","Smoothing_slope_param","Train_MAPE", "Test_MAPE"]
        DES_model_results= pd.DataFrame(columns = column_names)
        columns = list(DES_model_results)
        data_values = []
        
        smoothing_level_param=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
        smoothing_slope_param=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
        
        # Loop for the hyperparameter
        for p in range(len(smoothing_level_param)):
            print(p)
            print(smoothing_level_param[p])
            for j in range(len(smoothing_slope_param)):
                print(j)
                print(smoothing_slope_param[j])
            
                model2=Holt(train)
                model_fit2=model2.fit(smoothing_level=smoothing_level_param[p],smoothing_slope=smoothing_slope_param[j])
                yhat2=model_fit2.predict(start=train.index[0],end=END_DATE)
        
                forecast_df1.at[:, df.columns[n]]=yhat2# Predicted value is in the forecast_df1
         
                #Train Mape
                y_true_train=[float(k) for k in train.values]
                y_preds_train=[float(k) for k in yhat2.values[:len(train)]]
    
                #Test Mape
                y_true_test=[float(k) for k in test.values]
                y_preds_test=[float(k) for k in yhat2.values[len(train):]]
        
        
    
                # Storing the MAPE
                ##Train
                #train_model_name='H-T_Train'+'level_'+str(smoothing_level_param[i])+'slope_'+str(smoothing_slope_param[j])
                #test_model_name= 'H-T_Test'+'level_'+str(smoothing_level_param[i])+'slope_'+str(smoothing_slope_param[j])                                         
                #Train_MAPE_DES[train_model_name]=MAPE(y_true_train,y_preds_train)
                ##Test
                #Test_MAPE_DES[test_model_name]=MAPE(y_true_test,y_preds_test)
                
                row_values=[smoothing_level_param[p],smoothing_slope_param[j],MAPE(y_true_train,y_preds_train),MAPE(y_true_test,y_preds_test)]
                zipped = zip(columns, row_values)
                a_dictionary = dict(zipped)
                #print(a_dictionary)
                data_values.append(a_dictionary)
    
                #print('Train MAPE '+str(MAPE(y_true_train,y_preds_train)))
                #print('Test MAPE '+str(MAPE(y_true_test,y_preds_test)))
    
    # Appending the results to the dataframe
    DES_model_results = DES_model_results.append(data_values, True)
    del data_values# deleting the object so that it does not append more than once if accidently run the code
    
    #Smoothing_level_param Smoothing_slope_param corresponding to the min Test_MAPE
    Smoothing_level_param_min_Test_MAPE_DES=DES_model_results.loc[DES_model_results['Test_MAPE'] == DES_model_results['Test_MAPE'].min(), 'Smoothing_level_param'].values[0]
    
    Smoothing_slope_param_min_Test_MAPE_DES=DES_model_results.loc[DES_model_results['Test_MAPE'] == DES_model_results['Test_MAPE'].min(), 'Smoothing_slope_param'].values[0]
    
    
    #Full data used in the model to generate oot forecast for the DES model
    forecast_full_DES=pd.DataFrame()
    model_full_DES=Holt(data_df)
    model_fit_full_DES=model_full_DES.fit(smoothing_level=Smoothing_level_param_min_Test_MAPE_DES,smoothing_slope=Smoothing_slope_param_min_Test_MAPE_DES)
    yhat2_oot_DES=model_fit_full_DES.predict(start=train.index[0],end=month_ahead)
        
    forecast_full_DES.at[:, df.columns[n]]=yhat2_oot_DES# Predicted value is in the forecast_full_DES
    
    ff_DES=pd.DataFrame(yhat2_oot_DES[len(data_df):])
    
    # Renaming the column 
    ff_DES.rename(columns = {0:'Forecasted_'+Variable+''},inplace = True)
    
    # Recording the min Train MAPE and Test MAPE 
    Train_MAPE['MAPE']=DES_model_results.loc[DES_model_results['Test_MAPE'] == DES_model_results['Test_MAPE'].min(), 'Train_MAPE'].values[0]
    Test_MAPE['MAPE']=DES_model_results['Test_MAPE'].min()
    
    
    MAPE_df=pd.DataFrame([Train_MAPE,Test_MAPE])
    MAPE_df['Split'] = ['Train_MAPE','Test_MAPE']
    MAPE_df = MAPE_df[['Split','MAPE']]
    
    
    return ff_DES, MAPE_df
    
def tripple_exponential(data, Variable, forcast_period,Seasonal_Periods ):
    
    from statsmodels.tsa.holtwinters import ExponentialSmoothing
    test_period = forcast_period
    month_ahead = len(data)+forcast_period-1
    Train_MAPE={}
    Test_MAPE={}
    
    import pandas as pd
    import numpy as np

    # To calculate the MAPE
    def MAPE(Y_actual,Y_Predicted):
        mape=np.mean(np.abs((np.array(Y_actual)-np.array(Y_Predicted))/np.array(Y_actual)))*100
        return mape
    
    df = data[[Variable]]
    
    forecast_df1=pd.DataFrame()
    
    for n,i in enumerate(df.columns):
        #print(n,i)
        data_df=df.iloc[:,n]
        
        #Splitting data into train and test
        train=data_df.head(len(data)-test_period)
        test=data_df.tail(test_period)
        #End date of prediction is the last date of the test dataset
        END_DATE=test.index[-1]
        #print(data_df.head())
        
        #Create an empty dataframe with the specified columns
        column_names = ["Trend_Type","Seasonal_Type","Train_MAPE", "Test_MAPE"]
        TES_model_results= pd.DataFrame(columns = column_names)
        columns = list(TES_model_results)
        data_values = []
        
        Trend_Type=['additive','multiplicative']
        Seasonal_Type=['additive','multiplicative']
        
        
        for m in range(len(Trend_Type)):
            print(m)
            print(Trend_Type[m])
            for o in range(len(Seasonal_Type)):
                print(o)
                print(Seasonal_Type[o])
                
                model2=ExponentialSmoothing(train,trend=Trend_Type[m],seasonal=Seasonal_Type[o],seasonal_periods=Seasonal_Periods,damped_trend=False)
                model_fit2=model2.fit()
                yhat2=model_fit2.predict(start=train.index[0],end=END_DATE)
        
                forecast_df1.at[:, df.columns[n]]=yhat2# Predicted value is in the forecast_df1
                #Train Mape
                y_true_train=[float(i) for i in train.values]
                y_preds_train=[float(i) for i in yhat2.values[:len(train)]]
        
                #Test Mape
                y_true_test=[float(i) for i in test.values]
                y_preds_test=[float(i) for i in yhat2.values[len(train):]]
                
                row_values=[Trend_Type[m],Seasonal_Type[o],MAPE(y_true_train,y_preds_train),MAPE(y_true_test,y_preds_test)]
                zipped = zip(columns, row_values)
                a_dictionary = dict(zipped)
                #print(a_dictionary)
                data_values.append(a_dictionary)
        
                #print('Train MAPE '+str(MAPE(y_true_train,y_preds_train)))
                #print('Test MAPE '+str(MAPE(y_true_test,y_preds_test)))
    
    # Appending the results to the dataframe
    TES_model_results = TES_model_results.append(data_values, True)
    del data_values# deleting the object so that it does not append more than once if accidently run the code
    
    #Smoothing_level_param Smoothing_slope_param corresponding to the min Test_MAPE
    Trend_Type_min_Test_MAPE_TES=TES_model_results.loc[TES_model_results['Test_MAPE'] == TES_model_results['Test_MAPE'].min(), 'Trend_Type'].values[0]
    
    Seasonal_Type_min_Test_MAPE_TES=TES_model_results.loc[TES_model_results['Test_MAPE'] == TES_model_results['Test_MAPE'].min(), 'Seasonal_Type'].values[0]
    
    
    #Full data used in the model to generate oot forecast for the TES model
    forecast_full_TES=pd.DataFrame()
    model_full_TES=ExponentialSmoothing(train,trend=Trend_Type_min_Test_MAPE_TES,seasonal=Seasonal_Type_min_Test_MAPE_TES,seasonal_periods=Seasonal_Periods,damped_trend=False)
    model_fit_full_TES=model_full_TES.fit()
    yhat2_oot_TES=model_fit_full_TES.predict(start=train.index[0],end=month_ahead)
    
    forecast_full_TES.at[:, df.columns[n]]=yhat2_oot_TES# Predicted value is in the forecast_full_TES
    
    ff_TES=pd.DataFrame(yhat2_oot_TES[len(data_df):])
    #ff_TES=pd.DataFrame(yhat2_oot)
    #Renaming the column
    ff_TES.rename(columns = {0:'Forecasted_'+Variable+''},inplace = True)
    
    # Recording the min Train MAPE and Test MAPE 
    Train_MAPE['MAPE']=TES_model_results.loc[TES_model_results['Test_MAPE'] == TES_model_results['Test_MAPE'].min(), 'Train_MAPE'].values[0]
    Test_MAPE['MAPE']=TES_model_results['Test_MAPE'].min()
    
    MAPE_df=pd.DataFrame([Train_MAPE,Test_MAPE])
    MAPE_df['Split'] = ['Train_MAPE','Test_MAPE']
    MAPE_df = MAPE_df[['Split','MAPE']]
    
    
    return ff_TES, MAPE_df
    
def ARIMA_Forecasting(data, Variable, forcast_period):
    # ARIMA parameters range
    p_range = range (0,6)
    d_range = range (0,3)
    q_range = range (0,6)
    
    test_period = forcast_period
    month_ahead = len(data)+forcast_period-1
    Train_MAPE={}
    Test_MAPE={}
    
    import pandas as pd
    import numpy as np
    #import statsmodels as sm
    from statsmodels.tsa.arima.model import ARIMA

    # To calculate the MAPE
    def MAPE(Y_actual,Y_Predicted):
        mape=np.mean(np.abs((np.array(Y_actual)-np.array(Y_Predicted))/np.array(Y_actual)))*100
        return mape
    

    #ARIMA
    
    df = data[[Variable]]
    #df=df.replace(0,0.1)
    #df_pred=pd.DataFrame()
    
    #### splitting into train and test
    train_size = len(df)-test_period
    train, test = df[0:train_size], df[train_size:len(df)]
    #print('Observations: %d' % (len(df)))
    #print('Training Observations: %d' % (len(train)))
    #print('Testing Observations: %d' % (len(test)))
    
    column_names = ["P","D","Q","Train_MAPE","Test_MAPE"]
    arima_model_results= pd.DataFrame(columns = column_names)
    columns = list(arima_model_results)
    data_values = [] 
    
    for p in p_range:
         for d in d_range:
            for q in q_range:
                
                # fit model
                model = ARIMA(train, order=(p, d, q))
                model_fit = model.fit()
                #Fitted values
                fitted=model_fit.fittedvalues
    
                # make prediction for the test period
                if d>0:
                    yhat = model_fit.predict(len(train), len(train)+(test_period-1), typ='levels')
                    #print(' typ')
                else:
                    yhat = model_fit.predict(len(train), len(train)+(test_period-1))
                    #print('no typ')
    
                #Train Mape
                y_true_train=[float(i) for i in train.values]
                #print(y_true_train)
                y_preds_train=[float(i) for i in fitted.values]
                #print(y_preds_train)
    
                #Test Mape
                y_true_test=[float(i) for i in test.values]
                #print(y_true_test)
                y_preds_test=[float(i) for i in yhat.values]
                #print(y_preds_test)
    
                ##Train
                train_mape=MAPE(y_true_train,y_preds_train)
    
                ##Test
                test_mape=MAPE(y_true_test,y_preds_test)
                
                
                
                row_values=[p,d,q,train_mape,test_mape]
                zipped = zip(columns, row_values)
                a_dictionary = dict(zipped)
                #print(a_dictionary)
                data_values.append(a_dictionary)
    
    # Appending the results to the dataframe
    arima_model_results = arima_model_results.append(data_values, True)
    
    # P,D,Q corresponding to min Test MAPE
    
    #Smoothing_level_param Smoothing_slope_param corresponding to the min Test_MAPE
    P_min=arima_model_results.loc[arima_model_results['Test_MAPE'] == arima_model_results['Test_MAPE'].min(), 'P'].values[0]
    
    D_min=arima_model_results.loc[arima_model_results['Test_MAPE'] == arima_model_results['Test_MAPE'].min(), 'D'].values[0]
    
    Q_min=arima_model_results.loc[arima_model_results['Test_MAPE'] == arima_model_results['Test_MAPE'].min(), 'Q'].values[0]
    
    
    # fit model on full data
    model_arima_full = ARIMA(df, order=(P_min, D_min, Q_min))
    model_fit_arima_full = model_arima_full.fit()
    
    # make prediction for the oot period
    if D_min>0:
        yhat_oot = model_fit_arima_full.predict(len(df), len(df)+(test_period-1), typ='levels')
    else:
        yhat_oot = model_fit_arima_full.predict(len(df), len(df)+(test_period-1))
        
        # make prediction for the full period
# =============================================================================
#     if D_min>0:
#         yhat_oot = model_fit_arima_full.predict(0, len(df)+(test_period-1), typ='levels')
#     else:
#         yhat_oot = model_fit_arima_full.predict(0, len(df)+(test_period-1)) 
# =============================================================================

    ff_ARIMA=pd.DataFrame(yhat_oot)
    ff_ARIMA.rename(columns={'predicted_mean':'Forecasted_'+Variable+''},inplace = True)
    
    Train_MAPE['MAPE']=arima_model_results.loc[arima_model_results['Test_MAPE'] == arima_model_results['Test_MAPE'].min(),'Train_MAPE'].values[0]
    Test_MAPE['MAPE']=arima_model_results['Test_MAPE'].min()
    
    MAPE_df=pd.DataFrame([Train_MAPE,Test_MAPE])
    MAPE_df['Split'] = ['Train_MAPE','Test_MAPE']
    MAPE_df = MAPE_df[['Split','MAPE']]
    
    
    return ff_ARIMA, MAPE_df