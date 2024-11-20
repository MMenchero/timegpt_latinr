
# Compute accuracy of forecasts ---- 

library(data.table)
library(fabletools)

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
    mutate(resid = y-.data[[model]])
  
  mae <- MAE(res$resid)
  return(round(mae,2))
}

compute_mae(test_df, snaive, "SeasonalNaive")
compute_mae(test_df, arima, "AutoARIMA")
compute_mae(test_df, ets, "AutoETS")
compute_mae(test_df, prophet, "Prophet")
compute_mae(test_df, timegpt, "TimeGPT")
compute_mae(test_df, timegpt_longh, "TimeGPT")

# 4. MASE ---- 
compute_mase <- function(train, test, fc, model, season_length){
  res <- merge(test, fc, by = c("unique_id", "ds"))
  res <- res |> 
    mutate(resid = y-.data[[model]])
  
  mase <- MASE(res$resid, train$y, .period = season_length) # for daily data, season_length = 7
  return(round(mase,2))
}

