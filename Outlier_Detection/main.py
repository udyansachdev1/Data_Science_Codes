# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 00:29:02 2022

@author: udyan.sachdev
"""

def get_outliers(df,method):
    
    # Given a dataframe and a column name along with Outlier method finds a list of probable outliers 
    # Input: Dataframe, variable of interest as string & Outlier detetction method
    # Output: List of Probable outliers

    if method=='tukey': 
        probable_ouliers = {}
        for variable in df.columns:
            q1 = df[variable].quantile(0.25)
            q3 = df[variable].quantile(0.75)
            iqr = q3-q1
            inner_fence = 1.5*iqr
            outer_fence = 3*iqr
            
            #inner fence lower and upper end
            inner_fence_le = q1-inner_fence
            inner_fence_ue = q3+inner_fence
            
            #outer fence lower and upper end
            outer_fence_le = q1-outer_fence
            outer_fence_ue = q3+outer_fence
            
            outliers_prob = []
            outliers_poss = []
            for index, x in enumerate(df[variable]):
                if x <= outer_fence_le or x >= outer_fence_ue:
                    outliers_prob.append(index)
            for index, x in enumerate(df[variable]):
                if x <= inner_fence_le or x >= inner_fence_ue:
                    outliers_poss.append(index)
            print("Outliers in " + str(variable) + ": " +str( len(outliers_poss)))
            probable_out=df[variable].iloc[outliers_poss]
            probable_ouliers.update({variable : probable_out}) 
        
        return probable_ouliers
    
    elif method=='IQR':
        probable_ouliers = {}
        for variable in df.columns:
            Q1 = df[variable].quantile(0.25)
            Q3 = df[variable].quantile(0.75)
            IQR = Q3 - Q1
            probable_out=df[variable][((df[variable] < (Q1 - 1.5 * IQR)) |(df[variable]> (Q3 + 1.5 * IQR)))]
            print("Outliers in " + str(variable) + ": " +str(len(probable_out)))
            probable_ouliers.update({variable : probable_out}) 
        return probable_ouliers

        
def main():        
    # Testing 
    import pandas as pd
    import numpy as np
    import warnings
    warnings.filterwarnings('ignore')
    data=pd.read_excel('Input_Data/') ## input file location
    numeric_data = data.select_dtypes(include=[np.number])
    detection_method = input("Please select outlier detection method from ['tukey','IQR'] \n")
    outlier_list = pd.DataFrame(get_outliers(numeric_data,detection_method))
    outlier_list = pd.concat([outlier_list[x].dropna().reset_index(drop=True) for x in outlier_list], axis=1)
    outlier_list.to_excel('Output_Data/AlgoLib_Oulier_Detectn.xlsx', index=False)

if __name__ == '__main__':
    main()