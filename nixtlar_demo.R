
# nixtlar demo ---- 

install.packages("nixtlar")
packageVersion("nixtlar")

library(nixtlar)
library(data.table)
library(tidyverse)

# 1. Load data ----
df <- fread("data/demo_data.csv")
head(df)

## Data requirements ----
# ds: A timestamp in format YYYY-MM-DD or YYYY-MM-DD hh:mm:ss, either as characters or date objects 
# y: The numerical target variable 
# unique_id: A unique identifier 
df$ds <- as.POSIXct(df$ds)

## Separate train and tests sets 
train_df <- df |> 
  filter(ds <= "2017-08-01")

length(unique(train_df$unique_id)) # total number of series 

test_df <- df |> 
  filter(ds > "2017-08-01") 

## Visualize data ----


# 2. Set up your API key ----
# Get yours here: dashboard.nixtla.io

nixtla_client_setup(
  api_key = "Your API key here"
)

options(NIXTLA_API_KEY="Your API key here")

library(usethis)
usethis::edit_r_environ() # Set your API key as NIXTLA_API_KEY='Your API key here'

nixtla_validate_api_key()

# 3. Start forecasting! ----


## Plot results ----


## Use the long horizon model ---- 



# 4. Compute accuracy ----
## Here we will use MAE and MASE

# 5. Additional features 
fc <- nixtla_client_forecast(train_df, h = 14, level = c(80,95))

nixtla_client_plot(train_df, fc, max_insample_length = 200)




# 6. Anomaly detection ---- 
anomalies <- nixtla_client_detect_anomalies(df, level = c(95))
head(anomalies) 

anomalies |> 
  filter(anomaly == TRUE)

nixtla_client_plot(df, anomalies, plot_anomalies = TRUE, max_insample_length = 100)

# 7. Cross validation ---- 
cv <- nixtla_client_cross_validation(df, h = 7, n_windows = 3, step_size = 7)
head(cv)

nixtla_client_plot(df, cv, max_insample_length = 200)
