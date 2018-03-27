# For Windows system set working directory (For Linux server use current wd)
if (Sys.info()['sysname']=="Windows") setwd("C:/Users/konstantin/git/raiff/")

# Load ML Utils
source("code/utils.R")

# Load data
source("code/load.R")

# Filter data
source("code/dq.R")

# Aggregate data
source("code/generate_pos_attributes.R")

# Compute distance based attributes
source("code/process_customer_function.R")

# Compute distance-based attributes
source("code/generate_distance_attributes.R")

# Feature Selection and Datasets
source("code/feature_selection.R")

# Optimize hyperparams
source("code/model_optimize.R")

# Fit models
source("code/model_score.R")

# Average Predictions
# source("code/average.R")