# This project has been designed and published by DataCamp
# Loading Packages
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(readr, dplyr, forecast, lubridate, magrittr, ggplot2, gt)

# Reading Potato Prices Data
potato_prices <- read_csv("Potatoes+(Irish).csv")

# First Few Rows of Potato Data
head(potato_prices)

# Renaming Columns
potato_prices <- potato_prices |> 
  select(adm1_name, mkt_name, cm_name, mp_month, mp_year, mp_price) |> 
  rename(c(region = adm1_name, market = mkt_name, commodity_kg = cm_name, 
           month_col = mp_month, year_col = mp_year, 
           price_rwf = mp_price))

# Confirming Changes
glimpse(potato_prices)

# Creating Date Column
potato_prices <- potato_prices |> 
  rowwise() |> 
  mutate(date = make_date(year_col, month_col)) |> 
  select(-c(year_col, month_col))

# Checking for the Updates
potato_prices |> 
  head()

glimpse(potato_prices)

# Reading Price Data Function
read_price_data <- function(commodity) {
  file_path <- paste(commodity, ".csv", sep = "")
  tb_data <- read_csv(
    file_path,
    col_types = cols_only(
      adm1_name = col_character(),
      mkt_name = col_character(),
      cm_name = col_character(),
      mp_month = col_integer(),
      mp_year = col_integer(),
      mp_price = col_double()
    )
  )
  
  tb_data <- tb_data |> 
    rename(
      region = adm1_name, 
      market = mkt_name,
      commodity_kg = cm_name,
      month = mp_month,
      year = mp_year,
      price_rwf = mp_price
    )
  
  tb_data <- tb_data |> 
    mutate(date = ymd(paste(year, month, "01"))) |> 
    select(-month, -year)
  
  return(tb_data)
}

# Checking the Function on a New Peas Data
pea_prices <- read_price_data("Peas+(fresh)")
glimpse(pea_prices)

# Line Plot of Potato Price in Different Markets
potato_prices |> 
  ggplot(aes(x = date, y = price_rwf, group = market)) + 
  geom_line(alpha = 0.2) +
  labs(x = "Date", y = "Price", title = "Potato price over time")

# Function for Line Plots
plot_price_vs_time <- function(prices, commodity) {
  prices |>  
    ggplot(aes(date, price_rwf, group = market)) +
    geom_line(alpha = 0.2) +
    labs(
      title = paste(commodity ,"price over time", sep = " "),
      x = "Date",
      y = "Price"
    )
}

# Checking the Line Plot Function
plot_price_vs_time(pea_prices, "Pea")

# Aggregating Prices across Markets
potato_prices_summarized <- potato_prices |> 
  group_by(date) |> 
  summarize(price_med = median(price_rwf))

# Checking for Updates
potato_prices_summarized |> 
  head()

# Creating ts Object
potato_time_series <- potato_prices_summarized %$% 
  ts(
    price_med, 
    start = c(year(min(date)), month(min(date))), 
    end   = c(year(max(date)), month(max(date))), 
    frequency = 12
  )

# Function for Creating ts Objects
create_price_time_series <- function(prices) {
  prices_summarized <- prices |> 
    group_by(date) |> 
    summarize(median_price_rwf = median(price_rwf))
  
  time_series <- prices_summarized %$% 
    ts(
      median_price_rwf, 
      start = c(year(min(date)), month(min(date))), 
      end   = c(year(max(date)), month(max(date))), 
      frequency = 12
    )
  
  return(time_series)
}

# Checking the Function on Peas Data
pea_ts <- create_price_time_series(pea_prices)

# Forecasting Potato Prices
potato_price_forecast <- forecast(potato_time_series)
potato_price_forecast

# Plotting Historical Prices with Forecasted Prices
autoplot(potato_price_forecast, main = "Potato price forecast") + 
  scale_x_continuous(breaks = seq(2008, 2018))

# Forecasting and Plotting Function
plot_price_forecast <- function(time_series, commodity) {
  price_forecast <- forecast(time_series)
  autoplot(price_forecast, main = paste(commodity, "price forecast"))
}

# Checking the Function for Peas Data
plot_price_forecast(pea_ts, "Pea")

# Checking the Whole Process for a New Data
beans <- read_price_data("Beans (dry)")
plot_price_vs_time(beans, "Beans")
beans_ts <- create_price_time_series(beans)
plot_price_forecast(beans_ts, "Beans")








