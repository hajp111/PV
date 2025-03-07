library(tidyverse)
library(lubridate)
library(forecast)
library(tsibble)
library(fable)
library(feasts)
#library(ggrepel)   
library(scales)
library(ggplot2)
library(purrr)
#library(timetk)
#library(rsample)
#library(prophet)
#library(ggplotly)
#--------------#

source("R_functions/aux_functions.R")
print("this is electricity_price.R file")

#### define functions ####
my_data_read_distrib_costs_observed_data <- function(filepath = "_static_data/cena_distribuce.xlsx") {
  print(paste0("Reading grid cost observations from: ", filepath))
  distribution_costs <- readxl::read_excel(filepath, sheet = 1, range = "A7:D29") %>%
    rename(year = Rok, distr_costs = distribuce_prumer, service_costs = regulovane_slozky, valid_from = platnost_od) %>%
    mutate(grid_cost = distr_costs+service_costs
           , year = year %>% as.integer
           , valid_from = lubridate::dmy(valid_from)) %>% 
    group_by(year) %>% summarize(distr_costs = mean(distr_costs, na.rm = TRUE)
                                 , service_costs = mean(service_costs, na.rm = TRUE)
                                 , grid_cost = mean(grid_cost, na.rm = TRUE)
    )
  return(distribution_costs)
}#endfunction my_data_read_distrib_costs_observed_data

#wholesale prices ! 
my_data_read_elprice_observed_data <- function(multiply_wholesale_by = 1.2 #wholesale prices -> actual prices usually higher (profit margin)
                                               , filepath = "_static_data/Czechia.csv"
                                               , filepath_fx = "_static_data/ECB_Data_Portal_long_20250207083750.xlsx") {
  # read el prices for CZ from eurostat
  print(paste0("Reading el. price observations from: ", filepath))
  elprice_cz <- readr::read_csv(file = filepath, show_col_types = FALSE) %>%
    mutate(
      # for datetime, use fixed timezone (no summer time, because with summer time, I get 2 observations for same hour or a missing obs.):
      # "Etc/GMT-1" time zone offset is UTC/GMT +1 hours
      datetime_fixed = with_tz(`Datetime (UTC)`, tzone = "Etc/GMT-1")
      , date = as.Date(datetime_fixed)
      , hour = format(as.POSIXct(datetime_fixed), format = "%H") %>% as.integer()
      , year = year(date)
      , month = month(date)
      , day = day(date)
      , weekday = wday(date, week_start = 1)
    )
  
  # read EURxCZK fx rate
  fxrate <- readxl::read_excel(filepath_fx
                               , sheet = 2) %>% mutate(date = as.Date(DATE)
                                                       , EURCZK = OBS.VALUE) %>%
    fill(EURCZK, .direction = c("down"))
  
  # convert EUR to CZK prices
  elprice_czk <- elprice_cz %>% left_join(fxrate %>% select(date, EURCZK), by = "date") %>%
    mutate(price_CZK_kWh = `Price (EUR/MWhe)`* EURCZK / 10^3
           , price_CZK_kWh_retail = price_CZK_kWh * multiply_wholesale_by) %>%
    rename(datetime = datetime_fixed) %>%
    select(datetime, date, year, month, day, hour, price_CZK_kWh_retail) %>%
    rename(price = price_CZK_kWh_retail)
  
  # check duplicates
  if ( elprice_czk %>% count(datetime) %>% filter(n > 1) %>% nrow() >1) {warning("Duplicated values found!")}
  #rm(elprice_cz, fxrate)

  return(elprice_czk)
}#endfunction my_data_read_elprice_observed_data 

my_gridcost <- function(df = my_data_read_distrib_costs_observed_data()
                          , years = 20
                          , annual_growth = 0.04
                          , startdate = '2025-01-01'
                          , method = "linear" # "static", "linear", "last_w_growth"
                          , replace2023 = TRUE
                          , fixed_seed = TRUE
                          , orig_data_where_available = TRUE
                          
) {
  print(paste0("My grid cost started"))
  
  if (fixed_seed) {set.seed(123)}
  #check for method
  if (!method %in% c("static", "linear", "last_w_growth")) {stop("Unknown method")}#endif
  
  startdate <- my_check_date(startdate)
  startyear <- substring(startdate, 1,4) %>% as.integer()  #start_date %>% lubridate::floor_date(start_date %>% lubridate::ymd())
  endyear <- startyear + years

  
  startdate_orig <- startdate
  startdate <- startdate %>% as.Date() %>% with_tz(`Datetime (UTC)`, tzone = "Etc/GMT-1") 
  if (fixed_seed) {set.seed(123)}
  if (!all(c("year", "grid_cost") %in% names(df))) {
    stop("Input dataframe probably wrong, expected 'year' and 'grid_cost' columns")
  }
  if (replace2023 == TRUE) {
    prev_val <- df$grid_cost[df$year == 2022]
    next_val <- df$grid_cost[df$year == 2024]
    
    df <- df %>% mutate(grid_cost = case_when(
      year == 2023 ~ {  (prev_val + next_val) / 2 }
      , TRUE ~ grid_cost
    )
    )
                          
    }#replace 2023 value with avg of 2024 and 2022
  
  # some calculation of average vals first
  avg_grid_cost <- df %>%
    summarise(mean = mean(grid_cost, na.rm = TRUE),
              median = median(grid_cost, na.rm = TRUE),
              sd = sd(grid_cost, na.rm = TRUE), 
              quantile_025 = quantile(grid_cost, probs = 0.025, na.rm = TRUE),
              quantile_050 = quantile(grid_cost, probs = 0.05, na.rm = TRUE),
              quantile_250 = quantile(grid_cost, probs = 0.25, na.rm = TRUE),
              quantile_500 = quantile(grid_cost, probs = 0.5, na.rm = TRUE),
              quantile_750 = quantile(grid_cost, probs = 0.75, na.rm = TRUE),
              quantile_950 = quantile(grid_cost, probs = 0.95, na.rm = TRUE),
              quantile_975 = quantile(grid_cost, probs = 0.975, na.rm = TRUE)
    )
  
  
  # create future timestamps of hourly vals - use left join to apply the forecast for hourly values
  future_timestamps <- seq( startdate, by = "1 hour", length.out = years * 8760)
  # create df for future (like new_data)
  future_prices_step0 <- tibble(datetime = future_timestamps) %>%
    mutate(
      date = date(datetime)
      , year = year(datetime) %>% as.integer()
      , month = month(datetime) %>% as.integer()
      , day = day(datetime) %>% as.integer()
      , hour = hour(datetime) %>% as.integer()
      , weekday = wday(datetime, week_start = 1)
      , is_weekend = weekday %in% c(6,7)
    )
  future_years <- tibble(year = seq( startdate %>% year(), by = 1, length.out = years))
    
  if (method %in% c("static")) {
    mu <- mean(df$grid_cost, na.rm = TRUE)
    
    future_prices_step2 <- future_years %>% mutate(grid_cost_fcast = mu)
    
  } else if (method %in% c("linear")) {
    
    model1 <- df %>% as_tsibble(index = year) %>%
      model(
        ts_fit = TSLM(grid_cost ~ trend())
      )
    
    fitted_vals <- model1 %>% forecast(new_data = df %>% as_tsibble(index = year)) %>%
                      rename(grid_cost_fcast = .mean) %>%
                      select(-`.model`, -`grid_cost`)
    
    future_prices_step2 <- model1 %>% forecast(new_data = future_years %>% as_tsibble(index = year)) %>%  #new_data has to be tsibble!
      rename(grid_cost_fcast = .mean) %>%
      select(-`.model`, -`grid_cost`)
    
  } else if (method %in% c("last_w_growth")) {
    # take the last value from observations in range
    lastval <- df %>% #filter(year <= year(startdate)) %>% 
               tail(1) %>% select(grid_cost) %>% pull(1)
    future_prices_step2 <- future_years %>% mutate(grid_cost_fcast = lastval * (1 + annual_growth)^(0:(years-1)) )
  }
  

  
  
  # use original data instead of forecasts for data in observed range
  if (orig_data_where_available) {
    future_prices_step2 <- future_prices_step2 %>% left_join(df %>% select(year, grid_cost) %>% rename(orig_grid_cost = grid_cost), by ="year") %>%
      mutate(grid_cost_fcast = ifelse( orig_grid_cost %>% is.na(), yes = grid_cost_fcast, no = orig_grid_cost)) %>%
      select(-orig_grid_cost)
    
  }
  
  # Generate future prices in hourly basis
  future_prices <- future_prices_step0 %>% left_join(future_prices_step2, by = "year") %>%
    mutate(# noise_multiplier1 = sample(c(-1, 1), size = n(), replace = TRUE)
      grid_cost = grid_cost_fcast ) %>%
      select(-grid_cost_fcast)
  
  
  
  
  plt <- ggplot() + 
    geom_line(data = df, aes(x=year, y = grid_cost), alpha = 0.5) +
    geom_line(data = future_prices_step2, aes(x=year, y = grid_cost_fcast), alpha = 0.5, color = "red") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_minimal()
  if (method %in% c("linear")) {
    
    plt <- plt + geom_line(data = fitted_vals, aes(x = year, y = grid_cost_fcast), alpha = 0.5, color = "blue")
  }#add fitted for linear
  
  output <- list(plot = plt, grid_cost = future_prices)
  return(output)
}#endfunction my_gridcost




my_feed_in <- function( years = 20
                      , annual_growth = 0.04
                      , startdate = '2025-01-01'
                      , method = "last_w_growth" 
                      , fixed_seed = TRUE
                      , lastval #last value of price in kWh 
) {
  print(paste0("My feed-in started"))
  startdate <- my_check_date(startdate)
  startyear <- substring(startdate, 1,4) %>% as.integer()  #start_date %>% lubridate::floor_date(start_date %>% lubridate::ymd())
  endyear <- startyear + years
  
  startdate_orig <- startdate
  startdate <- startdate %>% as.Date() %>% with_tz(`Datetime (UTC)`, tzone = "Etc/GMT-1") 
  if (fixed_seed) {set.seed(123)}

  
  # create future timestamps of hourly vals - use left join to apply the forecast for hourly values
  future_timestamps <- seq( startdate, by = "1 hour", length.out = years * 8760)
  # create df for future (like new_data)
  future_prices_step0 <- tibble(datetime = future_timestamps) %>%
    mutate(
      date = date(datetime)
      , year = year(datetime) %>% as.integer()
      , month = month(datetime) %>% as.integer()
      , day = day(datetime) %>% as.integer()
      , hour = hour(datetime) %>% as.integer()
      , weekday = wday(datetime, week_start = 1)
      , is_weekend = weekday %in% c(6,7)
    )
  future_years <- tibble(year = seq( startdate %>% year(), by = 1, length.out = years))
  
 if (method %in% c("last_w_growth")) {

    future_prices_step2 <- future_years %>% mutate(feed_in = lastval * (1 + annual_growth)^(0:(years-1)) )
  }
  

  # Generate future prices in hourly basis
  future_prices <- future_prices_step0 %>% left_join(future_prices_step2, by = "year")
  
  
  plt <- ggplot() + 
    geom_line(data = future_prices_step2, aes(x=year, y = feed_in), alpha = 0.5, color = "red") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_minimal()
  
  output <- list(plot = plt, feed_in = future_prices)
  return(output)
}#endfunction my_feed_in

my_elprice <- function(df
                          , years = 20
                          , annual_growth = 0.04
                          , startdate = '2025-01-01'
                          , method = "linear" # "static", "linear", "historical_w_growth", "random_walk", "random_walk_trend", "mean_reverting_rw"
                          , fixed_seed = TRUE
                          , theta = 0.05
                          , add_intraday_variability = TRUE
                          , add_intraweek_variability = TRUE
) {
  print(paste0("My el. price started"))
  #check for method
  if (!method %in% c("static", "linear", "historical_w_growth", "random_walk", "random_walk_trend", "mean_reverting_rw")) {stop("Unknown method")}#endif
  
  startdate <- my_check_date(startdate)
  startyear <- substring(startdate, 1,4) %>% as.integer()  #start_date %>% lubridate::floor_date(start_date %>% lubridate::ymd())
  endyear <- startyear + years
  
  # last known price (avg. of 24 hours)
  last_price <- mean(tail(df$price, 24), na.rm = TRUE)
  print(paste0("last price: ", last_price %>% round(3)))
  
  startdate_orig <- startdate
  startdate <- startdate %>% as.Date() %>% with_tz(`Datetime (UTC)`, tzone = "Etc/GMT-1") 
  if (fixed_seed) {set.seed(123)}
  if (!all(c("datetime", "price") %in% names(df))) {
    stop("Input dataframe probably wrong, expected 'datetime' and 'price' columns")
  }
  
  # some calculation of average vals first
  avg_actual_price <- df %>% group_by(month, day, hour) %>%
    summarise(mean_price = mean(price, na.rm = TRUE),
              median_price = median(price, na.rm = TRUE),
              sd_price = sd(price, na.rm = TRUE), 
              quantile_025 = quantile(price, probs = 0.025, na.rm = TRUE),
              quantile_050 = quantile(price, probs = 0.05, na.rm = TRUE),
              quantile_250 = quantile(price, probs = 0.25, na.rm = TRUE),
              quantile_500 = quantile(price, probs = 0.5, na.rm = TRUE),
              quantile_750 = quantile(price, probs = 0.75, na.rm = TRUE),
              quantile_950 = quantile(price, probs = 0.95, na.rm = TRUE),
              quantile_975 = quantile(price, probs = 0.975, na.rm = TRUE)
    )
  
  # SLT decomposition
  decomposed <- df %>% as_tsibble(index = datetime) %>% 
    #filter(datetime<'2024-01-01') %>%
    model(STL(price ~ season(period = "day") + season(period = 168) #+ season(period = "year")
    )) %>%  #season is "day" because pattern repeats daily
    components() %>% 
    mutate( month = month(datetime) %>% as.integer()
            , day = day(datetime) %>% as.integer()
            , hour = hour(datetime) %>% as.integer()
            , weekday = wday(datetime, week_start = 1))
  
  decomposed %>% filter(datetime >='2020-01-01' & datetime <='2020-04-30')   %>% autoplot()
  
  
  
  # df %>% filter(year==2020) %>% as_tsibble(index = datetime) %>% 
  #   model(STL(price ~ season(period = "day") + season(period = "month") + season(period = "year"))) %>% components() %>% autoplot()
  # 
  # df %>% filter(year==2020) %>% as_tsibble(index = datetime) %>% 
  #   model(STL(price ~  season(period = "year"))) %>% components() %>% autoplot()
  # 
  # x1<- df %>% filter(year==2020) %>% as_tsibble(index = datetime) %>% 
  #   model(STL(price ~  season(period = "day"))) %>% components() 
  # x1 %>% filter(datetime <='2020-03-31') %>% autoplot()
  # 
  # x2<- df %>% filter(year==2020) %>% as_tsibble(index = datetime) %>% 
  #   model(STL(price ~  season(period = "month"))) %>% components() 
  # x2 %>% filter(datetime <='2020-03-31') %>% autoplot()
  # x<- df %>% filter(year==2020) %>% as_tsibble(index = datetime) %>% 
  #   model(STL(price ~  season(period = "month") + season(period = "day"))) %>% components() 
  # x %>% filter(datetime <='2020-03-31') %>% autoplot()
  # 
  # x4 <- df %>% filter(year<=2017) %>% as_tsibble(index = datetime) %>% 
  #   model(STL(price ~ season(period = "year"))) %>% components() 
  # x4 %>% filter(datetime <='2020-03-31') %>% autoplot()
  # 
  # df %>% filter(year==2020) %>% filter(datetime <='2020-03-31') %>% mutate(date = as.Date(datetime)) %>% group_by(date) %>% summarize(price = mean(price,na.rm=TRUE)) %>%
  # ggplot(aes(x=date, y=price)) +geom_line()
  # 
  
  decomposed_agg <- decomposed %>% as_tibble() %>%
    mutate(year = year(datetime) %>% as.integer()
           , month = month(datetime) %>% as.integer()
           , day = day(datetime) %>% as.integer()
           , hour = hour(datetime) %>% as.integer()
           , weekday = wday(datetime, week_start = 1)
    ) %>%
    group_by(month, hour, weekday) %>%
    summarise(mean_season_day = mean(season_day, na.rm = TRUE)
              , sd_season_day = sd(season_day, na.rm = TRUE)
              , mean_season_168 = mean(season_168, na.rm = TRUE)
              , sd_season_168 = sd(season_168, na.rm = TRUE)
              , mean_trend = mean(trend, na.rm = TRUE)
              , sd_trend = sd(trend, na.rm = TRUE)
              , season_day_050 = quantile(season_day, probs = 0.05, na.rm = TRUE)
              , season_day_250 = quantile(season_day, probs = 0.25, na.rm = TRUE)
              , season_day_500 = quantile(season_day, probs = 0.5, na.rm = TRUE)
              , season_day_750 = quantile(season_day, probs = 0.75, na.rm = TRUE)
              , season_day_950 = quantile(season_day, probs = 0.95, na.rm = TRUE)
              , season_week_050 = quantile(season_168, probs = 0.05, na.rm = TRUE)
              , season_week_250 = quantile(season_168, probs = 0.25, na.rm = TRUE)
              , season_week_500 = quantile(season_168, probs = 0.5, na.rm = TRUE)
              , season_week_750 = quantile(season_168, probs = 0.75, na.rm = TRUE)
              , season_week_950 = quantile(season_168, probs = 0.95, na.rm = TRUE)
    )
  
  
  mean_trend <- decomposed %>% as_tibble() %>% ungroup() %>% summarise(mean_trend = mean(trend, na.rm = TRUE)
                                                                       , sd_trend = sd(trend, na.rm = TRUE))
  
  # create future timestamps
  future_timestamps <- seq( startdate, by = "1 hour", length.out = years * 8760)
  # create df for future (like new_data)
  future_prices_step0 <- tibble(datetime = future_timestamps) %>%
    mutate(
      date = date(datetime)
      , year = year(datetime) %>% as.integer()
      , month = month(datetime) %>% as.integer()
      , day = day(datetime) %>% as.integer()
      , hour = hour(datetime) %>% as.integer()
      , weekday = wday(datetime, week_start = 1)
      , is_weekend = weekday %in% c(6,7)
    )
  
  if (!( method %in% c("static", "linear", "historical", "historical_w_growth", "random_walk", "random_walk_trend", "mean_reverting_rw"))) {
    stop("Unknown method, stopping")
  }
  
  if (method %in% c("static")) {
    mu <- mean(df$price, na.rm = TRUE)
    
    future_prices_step2 <- future_prices_step0 %>% mutate(price_method = mu)
    
  } else if (method %in% c("linear")) {
    
    tslm_model <- df %>% as_tsibble(index = datetime) %>%
      model(
        ts_fit = TSLM(price ~ trend() + season())
      )
    
    future_prices_step2 <- tslm_model %>% forecast(new_data = future_prices_step0 %>% as_tsibble(index = datetime)) %>%  #new_data has to be tsibble!
      rename(price_method = .mean) %>%
      select(-`.model`, -`price`)
  } else if (method %in% c("historical_w_growth")) { 
    #apply just historical trend (without seasonality)
    future_prices_step2 <- future_prices_step0 %>%
      left_join(decomposed_agg %>% select(month, weekday, hour, mean_trend)
                , by = c("month" = "month", "weekday" = "weekday", "hour" = "hour")) %>%
      rename( 
        price_method = mean_trend ) %>% 
      mutate(price_method = price_method * exp(annual_growth * as.numeric(difftime(datetime, max(df$datetime)
                                                                                   , units = "days") / 365)))
    
  } else if (method %in% c("random_walk", "random_walk_trend", "mean_reverting_rw")) {
    
    # Compute historical mean price
    mu <- mean(df$price, na.rm = TRUE)
    
    #create innovations for random walk, using sd from hourly prices from observations
    future_prices_step1 <- future_prices_step0 %>%
      left_join(avg_actual_price, by = c("month" = "month", "day" = "day", "hour" = "hour") ) %>%
      mutate(
        #note: the random walk step (innovation) is kept as an extra column for random walk methods
        innovation = rnorm(n(), mean = 0, sd = sd_price * 0.1),  # sd_price scaled down to avoid large fluctuations
      ) %>%
      #drop unneeded cols
      select(-(mean_price:quantile_975))
    
    if (method %in% c("random_walk")) {
      future_prices_step2 <- future_prices_step1 %>% 
        mutate(
          price_method =  last_price + cumsum(innovation) 
        )
      
    } else if (method %in% c("random_walk_trend")) {
      drift_per_hour <- (annual_growth * last_price) / (365 * 24)
      future_prices_step2 <- future_prices_step1 %>% 
        mutate(
          price_method =  last_price + cumsum(drift_per_hour + innovation) 
        )
    } else if (method %in% c("mean_reverting_rw")) {
      innovations <- future_prices_step1$innovation
      future_prices_step2 <- future_prices_step1 %>%
        mutate(
          price_method = accumulate(innovations, ~ .x + theta * (mu - .x) + .y, .init = last_price)[-1]
        )
    } else {
      stop("Unknown method, stopping.")
    }
  }#end for trend methods
  
  # Generate future prices
  
  future_prices <- future_prices_step2
  if (add_intraday_variability) {
    future_prices <- future_prices %>%
      left_join(decomposed_agg, by = c("hour" = "hour", "month" = "month", "weekday" = "weekday") ) %>%
      # uniform distribution between quantiles, not so great
      #mutate(generated_hour_component = map_dbl(1:n(), ~ runif(1, min = quantile_025[.x], max = quantile_975[.x])))
      # use piecewise linear and more narrow (10% CI)
      mutate(generated_hour_component = map_dbl(1:n(), function(.x) {
        runif(1, min = season_day_250[.x], max = season_day_750[.x])
      })
      ) %>% 
      # add to price_method
      # mutate(price = price_method + generated_hour_component) %>%
      select(-c(mean_season_day:season_week_950)) 
    
  } else {
    future_prices <- future_prices %>% mutate(generated_hour_component = 0)
  } #end add_intraday_variability
  
  if (add_intraweek_variability) {
    future_prices <- future_prices %>%
      left_join(decomposed_agg, by = c("hour" = "hour", "month" = "month", "weekday" = "weekday") ) %>%
      # uniform distribution between quantiles, not so great
      #mutate(generated_hour_component = map_dbl(1:n(), ~ runif(1, min = quantile_025[.x], max = quantile_975[.x])))
      # use piecewise linear and more narrow (10% CI)
      mutate(generated_week_component = map_dbl(1:n(), function(.x) {
        runif(1, min = season_week_250[.x], max = season_week_750[.x])
      })
      ) %>% 
      # add to price_method
      #mutate(price = price_method + generated_week_component) %>%
      select(-c(mean_season_day:season_week_950)) 
  } else {
    future_prices <- future_prices %>% mutate(generated_week_component = 0)
  }#end add_intraweek_variability
  
  future_prices <- future_prices %>%
    mutate(# noise_multiplier1 = sample(c(-1, 1), size = n(), replace = TRUE)
      price = price_method + generated_hour_component + generated_week_component)
  
  
  # plt <- ggplot() + 
  #   geom_line(data = , aes(x=datetime, y = price), alpha = 0.3) +
  #   theme_minimal()
  
  plt_boxplot_per_year <- future_prices %>% ggplot(aes(x=factor(year), y = price)) + geom_boxplot()+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  
  # this is too slow to run:
  # plt_ elprice$price_data %>% ggplot(aes(x = datetime, y = price)) +
  #   geom_smooth(method = "loess", se = TRUE) + 
  #   facet_wrap(~year, scales = "free_x") + 
  #   labs(title = "Hourly Value Trend by Year",
  #              +          x = "Date Time",
  #              +          y = "Value") +
  #   theme_minimal()
  
  plt_heatmap <- future_prices %>%
    group_by(year, day, hour) %>%
    summarise(mean = mean(price, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = hour, y = day, fill = mean)) +
    geom_tile() +
    facet_wrap(~year) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Heatmap of Avg Hourly Price by Day of Year",
         x = "Hour of Day",
         y = "Day of Year",
         fill = "Mean Price") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    theme_minimal()
  
    plt_monthly <- future_prices %>% mutate(year_month = format(datetime, "%Y-%m")) %>%
      group_by(year, month, year_month) %>%
      summarise(mean = mean(price, na.rm = TRUE)
                , q10 = quantile(price, 0.10, na.rm = TRUE)
                , q90 = quantile(price, 0.90, na.rm = TRUE)
                , .groups = "drop") %>% 
      ggplot(aes(x = month, y = mean)) +
      geom_line(group = 1) + # group = 1 to prevent ggplot from trying to group lines
      #geom_point() +
      geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.2) + # Add ribbon for quantiles
      labs(
        title = "Monthly Average Price",
        x = "Year-Month",
        y = "Price"
      ) +
      facet_grid(year ~ .) +
      theme_minimal() +
      #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      # x axis as integers with step 1
      scale_x_continuous(breaks = 1:12) + 
      # Integer y-axis labels
      scale_y_continuous(labels = scales::number_format(accuracy = 1))
    
    
  
  output <- list(plot = plt_boxplot_per_year
                 , plot_monthly = plt_monthly
                 , plot_heatmap = plt_heatmap
                 , price_data = future_prices)
  
  return(output)
}#endfunction my_elprice


# # using Meta's prophet algorithm 
# my_elprice_2 <- function(df
#                           , years = 20
#                           , startdate = '2025-01-01' %>% as.Date() %>% with_tz(`Datetime (UTC)`, tzone = "Etc/GMT-1") 
#                           , method = "prophet"
#                           , fixed_seed = TRUE
#                           , changepoint_prior = 0.05
#                           , seasonality_mode = "multiplicative"
#                           , interval_width = 0.8
#                           , custom_seasonality = TRUE
# ) {
#   if (fixed_seed) {set.seed(123)}
#   
#   if (!all(c("datetime", "price") %in% names(df))) {
#     stop("Input dataframe probably wrong, expected 'datetime' and 'price' columns")
#   }
#   
#   last_price < mean(tail(df$price, 24), na.rm = TRUE)
#   #last_price <- tail(df$price, 1)
#   future_timestamps <- seq( startdate, by = "1 hour", length.out = years * 8760)
#   future_prices_step0 <- tibble(datetime = future_timestamps) %>%
#     mutate(year = year(datetime)
#            , month = month(datetime)
#            , day = day(datetime)
#            , hour = hour(datetime)
#            , weekday = wday(datetime, week_start = 1))
#   
#   if (method == "prophet") {
#     df_prophet <- df %>%
#       rename(ds = datetime, y = price) %>%  #required names by prophet package
#       select(ds, y)
#     
#     model <- prophet(
#       changepoint.prior.scale = changepoint_prior,
#       seasonality.mode = seasonality_mode,
#       interval.width = interval_width
#     )
#     
#     # add custom seasonality?
#     if (custom_seasonality) {
#       model <- add_seasonality(
#         model, 
#         name = "daily", 
#         period = 24, 
#         fourier.order = 10
#       ) %>% 
#         #add_seasonality(name = "yearly", period = 365.25, fourier.order = 10) %>%
#         add_seasonality(name = "weekly", period = 24*7, fourier.order = 10)
#     }#end custom seasonality
#     
#     model <- fit.prophet(model, df_prophet)
#     
#     future_df <- tibble(ds = future_timestamps)
#     forecast <- predict(model, future_df) # %>% select(ds, yhat)
#     
#     future_prices <- future_prices_step0 %>%
#       left_join(forecast, by = c("datetime" = "ds")) %>%
#       rename(price = yhat)
#     
#   } else {
#     stop("Unknown method, stopping.")
#   }
#   
#   return(future_prices)
# }#endfunction my_forecast_2


