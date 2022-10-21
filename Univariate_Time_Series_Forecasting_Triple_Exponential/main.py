# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 01:14:15 2022

@author: udyan.sachdev
"""

import pandas as pd
import numpy as np
import os
from pathlib import Path
import Univariate_Forecasting

working_dir = Path(os.getcwd())  # Creating path based on platform#
input_dir = working_dir.joinpath('Input_Data')

df = pd.read_csv(input_dir.joinpath('.csv'))

output_df , MAPE_df = Univariate_Forecasting.tripple_exponential(df, 'X1', 7, 12)