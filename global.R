# source("data/generate_dummy_data.R")  # This will load the dataset from dummy_data.R

df <- haven::read_xpt("data/adpc.xpt")
df_1 <- haven::read_xpt("data/adpc_small.xpt")


source("functions.R")
