
# Compute accuracy of forecasts ---- 

library(tidyverse)
library(data.table)

# 1. Load data ---- 
df <- fread("data/demo_data.csv")

train_df <- df |> 
  filter(ds <= "2017-08-01")

test_df <- df |> 
  filter(ds > "2017-08-01") 

# 2. Load forecasts ----  
timegpt <- fread("output/timegpt.csv")
timegpt_longh <- fread("output/timegpt_longh.csv")
snaive <- fread("output/snaive_fable.csv")
arima <- fread("output/arima_fable.csv")
ets <- fread("output/ets_fable.csv")
prophet <- fread("output/prophet_fable.csv")

# 3. MAE ---- 
compute_mae <- function(test, fc, model){
  res <- merge(test, fc, by = c("unique_id", "ds"))
  res <- res |> 
    mutate(resid = abs(y-.data[[model]])) |> 
    summarise(mae = mean(resid, na.rm = TRUE)) 
  
  return(paste0("MAE for ", model, ": ", round(mean(res$mae, na.rm = TRUE),2)))
}

compute_mae(test_df, snaive, "SeasonalNaive")
compute_mae(test_df, arima, "AutoARIMA")
compute_mae(test_df, ets, "AutoETS")
compute_mae(test_df, prophet, "Prophet")
compute_mae(test_df, timegpt, "TimeGPT")
compute_mae(test_df, timegpt_longh, "TimeGPT")

# 4. MASE ---- 
compute_mase <- function(train, test, fc, model){
  mae_train <- train |> 
    mutate(snaive = lag(y, n = 1)) |> 
    na.omit() |> 
    mutate(resid = abs(y-snaive)) |> 
    group_by(unique_id) |> 
    summarise(mae_train = mean(resid, na.rm = TRUE))
  
  mae_fc <- merge(test, fc, by = c("unique_id", "ds")) |>
    mutate(resid = abs(y-.data[[model]])) |> 
    group_by(unique_id) |>
    summarise(mae_fc = mean(resid, na.rm = TRUE))
  
  mase <- merge(mae_train, mae_fc, by = "unique_id") |> 
    mutate(mase = mae_fc/mae_train)

  return(paste0("MASE for ", model, ": ", round(mean(mase$mase, na.rm = TRUE),3)))  
}

compute_mase(train_df, test_df, snaive, "SeasonalNaive")
compute_mase(train_df, test_df, arima, "AutoARIMA")
compute_mase(train_df, test_df, ets, "AutoETS")
compute_mase(train_df, test_df, prophet, "Prophet")
compute_mase(train_df, test_df, timegpt, "TimeGPT")
compute_mase(train_df, test_df, timegpt_longh, "TimeGPT")
