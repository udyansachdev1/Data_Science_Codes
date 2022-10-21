# install.packages("reticulate")
library(reticulate)
# py_install("pandas")
# py_install("openpyxl")
os <- import("os")
pandas <- import("pandas")
openpyxl <- import("openpyxl")

setwd('C:/Users/udyan.sachdev/OneDrive - Accenture/Python practice code/Py_to_R')
getwd()

path_to_python <- "C:/Users/udyan.sachdev/AppData/Local/r-miniconda/envs/r-reticulate"
use_python(path_to_python)

source_python("Py_Script_for_R.py")
data <- group_by("Data.xlsx")
