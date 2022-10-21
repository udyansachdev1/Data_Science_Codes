# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 00:47:36 2022

@author: udyan.sachdev
"""

import os
import pandas as pd
import multiprocessing as mp
import logging
import shutil
import time
from datetime import datetime

log_f_name = "script_" + str(datetime.now().strftime("%m-%d-%Y_%H-%M")) + ".log"
logging.basicConfig(level=logging.INFO, filename=log_f_name, filemode='a',
                    format='%(asctime)s :  %(message)s')

dir_path = os.path.dirname(os.path.realpath(__file__))
if os.path.exists(os.path.join(dir_path, "Input.csv")) is False:
    print("Please place Input.csv file on {} path".format(dir_path))
    logging.info("Please place Input.csv file on {} path".format(dir_path))
    exit()
    
input_file = pd.read_csv("Input.csv", na_filter=False)

g_input_path = input_file.iloc[0, 1]
if g_input_path == "":
    g_input_path = dir_path
g_column = input_file.iloc[2, 1]
g_filename = input_file.iloc[1, 1]

del input_file


def check_output():
    output_filepath = os.path.join(g_input_path, "Output")
    if os.path.exists(output_filepath):
        try:
            shutil.rmtree(output_filepath)
            logging.info("%s removed successfully" % output_filepath)
        except OSError as error:
            print(error)
            logging.error("File path can not be removed")
    os.mkdir(output_filepath)


def preprocess(x):
    df_bs_grouped = x.groupby(g_column)
    for group_name, df_group in df_bs_grouped:
        output_file_path = os.path.join(g_input_path, "Output", group_name + ".csv")
        while True:
            try:
                if os.path.exists(output_file_path):
                    df_group.to_csv(output_file_path, mode="a", header=False, index=False)
                else:
                    df_group.to_csv(output_file_path, index=False)
                break
            except IOError as e:
                logging.error(e)
                logging.info("Try to access the file again in 5 seconds")
                time.sleep(5)


if __name__ == '__main__':
    logging.info("Starting of the script.")

    if os.path.exists(os.path.join(g_input_path, g_filename)) is False:
        print("Please place you input file on {} path".format(g_input_path))
        logging.info("Please place you input file on {} path".format(g_input_path))
        exit()

    check_output()
    reader = pd.read_csv(os.path.join(g_input_path, g_filename), chunksize=1000,
                         low_memory=False)  # chunksize depends with you colsize

    pool = mp.Pool(processes=(mp.cpu_count() - 1))
    pool.map(preprocess, reader)
    pool.close()
    pool.join()

    logging.info("Script ran successfully.")
