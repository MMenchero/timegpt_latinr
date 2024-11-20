
# fable forecasts ----

library(tictoc)
library(data.table)
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(fable.prophet)

# 1. Load data ---- 
df <- fread("data/demo_data.csv")
df$ds <- ymd(df$ds)

tbl <- as_tsibble(df, key = "unique_id", index = "ds")

## Separate train and tests sets 
train_tbl <- tbl |> 
  filter(ds <= "2017-08-01")

# test_df <- df |> # not required here 
#  filter(ds > "2017-08-01") 

# We'll run each model separately to compute its total execution time 

# 2. Seasonal naive (benchmark) ----
tic()
snaive_fit <- train_tbl |> 
  model(
    `Seasonal Naive` = SNAIVE(y)
  )
snaive_fc <- snaive_fit |> forecast(h = 14)
toc()

## Keep mean forecast, format output to match output from nixtlar 
names(snaive_fc) <- c("unique_id", "model", "ds", "y", "SeasonalNaive")
snaive_fc <- snaive_fc |> 
  select(c(unique_id, ds, SeasonalNaive))

## Save results 
write.table(snaive_fc, "output/snaive_fable.csv" , sep = ",", row.names = FALSE, quote = FALSE)

# 3. AutoARIMA ---- 
tic()
arima_fit <- train_tbl |> 
  model(
    `ARIMA` = ARIMA(y)
  )
arima_fc <- arima_fit |> forecast(h = 14)
toc()

names(arima_fc) <- c("unique_id", "model", "ds", "y", "AutoARIMA")
arima_fc <- arima_fc |> 
  select(c(unique_id, ds, AutoARIMA))

write.table(arima_fc, "output/arima_fable.csv" , sep = ",", row.names = FALSE, quote = FALSE)

# 4. AutoETS ---- 
tic()
ets_fit <- train_tbl |> 
  model(
    `ETS` = ETS(y)
  )
ets_fc <- ets_fit |> forecast(h = 14)
toc()

names(ets_fc) <- c("unique_id", "model", "ds", "y", "AutoETS")
ets_fc <- ets_fc |> 
  select(c(unique_id, ds, AutoETS))

write.table(ets_fc, "output/ets_fable.csv" , sep = ",", row.names = FALSE, quote = FALSE)

# 5. Prophet ----
tictoc::tic()
prophet_fit <- train_tbl |> 
  model(
    prophet = prophet(y)
  )

prophet_fc <- prophet_fit |>
  forecast(h = 14)
tictoc::toc()

names(prophet_fc) <- c("unique_id", "model", "ds", "y", "Prophet")
prophet_fc <- prophet_fc |> 
  select(c(unique_id, ds, Prophet))

write.table(prophet_fc, "output/prophet_fable.csv" , sep = ",", row.names = FALSE, quote = FALSE)
