# For Windows system set working directory (For Linux server use current wd)
if (Sys.info()['sysname']=="Windows") setwd("C:/Users/konstantin/git/raiff/")

# Load ML Utils
source("code/0_utils.R")

# Load data
source("code/1_load.R")

# Filter data
source("code/2_filter.R")

# Agregate data
source("code/3_generate_pos_attributes.R")

# Compute distance based attributes
source("code/4_process_customer.R")

# Compute distance-based attributes
source("code/5_generate_distance_attributes.R")

# Feature Selection and Datasets
source("code/feature_selection.R")

# Fit models
source("code/model.R")
