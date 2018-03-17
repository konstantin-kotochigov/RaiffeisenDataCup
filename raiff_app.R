# For Windows system set working directory (For Linux server use current wd)
if (Sys.info()['sysname']=="Windows") setwd("C:/Users/konstantin/git/raiff/")

# Load data
source("code/raiffeisen_load.R")

# Filter data
source("code/raiff_dq.R")

# Agregate data
source("code/raiffeisen_scoring.R")

# Compute distance based attributes
source("code/raiff_process_customer.R")

# Compute distance-based attributes
source("code/raiff_generate.R")

# Load ML Utils
source("code/raiff_utils.R")

# Predict missing customers
source("code/raiff_add_test.R")

# Fit models
source("code/raiff_model.R")
