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
  distribution_costs <- openxlsx::read.xlsx(filepath, sheet = 1, rows = 7:29, cols = 1:4) %>%
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


prepare_elprice_observed <- function(filepath = "../_data/OTE_elektrina/OTE_data/"
                              #, filepath_fx = "_static_data/ECB_Data_Portal_long_20250207083750.xlsx"
                              , save_to_path = "_static_data/elprices_Czechia.Rds") {
  print("this is prepare_elprice_observed()") 
  what_files <- list.files(filepath, pattern = "\\.xls[x]?$", full.names = TRUE)
  #if OTE Excel files are causing problems opening in R, open the files in Excel and re-save them as xlsx
  # data for 2024 only up to 2024-11-30 -> filter for dates below
  # filter out 2025 (because 2024 incomplete) 
  what_files <- what_files[!grepl("2025", what_files)]
  # filter out 2014 and below, to reduce shinyapps.io resource consumption
  what_files <- what_files[as.numeric(str_extract(basename(what_files), "\\d{4}")) >= 2016]
  
  out1 <-lapply(what_files, function(myfile) {
    tryCatch({

      sheets <- openxlsx::getSheetNames(myfile)
      
      year <- basename(myfile) %>% str_extract(pattern = "trhu_([0-9]{4})") %>% str_replace("trhu_", replacement = "") %>% as.integer()

      #what sheet
      if (year == 2008) {
        target_sheet <- sheets[grep("DT \\(Kč\\)", sheets)]
      } else if (year == 2009) {
        target_sheet <- sheets[grep("DT ČR \\(EUR\\)", sheets)]
      } else {
        target_sheet <- sheets[grep("DT ČR$", sheets)] # $ ensures exact match at the end
      }#endif
      
      #target_sheet <- sheets[grep("DT", sheets)]
    
      if (length(target_sheet) == 0) {
        warning(paste("Cannot determine 'DT' sheet in: ", myfile))
        return(NULL)
      }#endif
      
      #data <- readxl::read_excel(myfile, sheet = target_sheet, range = "A6:M10000") 
      data <- openxlsx::read.xlsx(myfile, sheet = target_sheet, rows = 6:10000, cols = 1:14, skipEmptyCols = FALSE, skipEmptyRows = TRUE, detectDates = TRUE)
      
      # remove empty rows at the end ( all values in the row are NA)
      data <- data[rowSums(is.na(data)) < ncol(data), ]
      
      #only the first table
      empty_col <- which(apply(data, 2, function(x) all(is.na(x))))[1]
      selected_data <- data[, 1:(empty_col - 1)]
    
      # only the value in col containing "Marginální cena" and "EUR/MWh"
      target_col1 <- grep("Marginální.*EUR/MWh", names(selected_data))
      target_col2 <- grep("Marginální.*Kč/MWh", names(selected_data))
      target_col3 <- grep("Kurz", names(selected_data))
      
      if (length(target_col1) == 0) {
        warning(paste("Target col+ not found in: ", myfile))
        return(NULL)
      }
      
      result <- selected_data[, c(1,2, target_col1,target_col2,target_col3)]
      first_date <- selected_data[1,1][[1]]
      if (first_date %>% is.character() == TRUE) {
        if (first_date %>% str_detect(".")) {
          parse_as_text <- TRUE    #format like 31.12.2009
        } else { parse_as_text <- FALSE}
      } else {
        parse_as_text <- FALSE
      }
      
      names(result) <- c("date", "hour", "price_EUR_MWh", "price_CZK_MWh", "FX_CZK_EUR")
      if (parse_as_text) {
        result <- result %>% mutate( date = date %>% lubridate::dmy() %>% format("%Y-%m-%d")
                                     #, datetime = as.POSIXct(paste0(date, " ", hour), format = "%Y-%m-%d %H", tz = "Etc/GMT-1") 
        )
      } else {

        result <- result %>% mutate( date = date %>% format("%Y-%m-%d")
        #, datetime = as.POSIXct(paste0(date, " ", hour), format = "%Y-%m-%d %H", tz = "Etc/GMT-1") 
                  )
      }#endif
      
    
      return(result)
    }, error = function(e) {
      warning(paste("Error processing: ", myfile, ":", e))
      return(NULL)
    })
  
    })#end lapply
  
  final_result <- bind_rows(out1) %>%
    #filter(date == "2015-03-29" | date == "2015-03-30" | date == "2015-10-25" | date == "2015-10-26" ) %>%
    mutate(hour = hour - 1   # OTE uses hours 1-24, but I use 0-23, but anyway, OTE has consecutive hours observed
           , x_prague =  as.POSIXct(paste0(date, " ", 0), format = "%Y-%m-%d %H", tz = "Europe/Prague") 
           , x_utc =  with_tz(x_prague, tzone = "UTC")
           , x_utc_addhours = x_utc + lubridate::hours(hour)
           , x_utc_addhours_gmt = with_tz(x_utc_addhours, tzone = "Etc/GMT-1")
           )
  #final_result %>% select(date, hour, hour_local, datetime_CET , datetime_GMT) %>% filter(date == "2015-03-29") %>% View()
  #openxlsx::write.xlsx(x = final_result, file = save_to_path)
  
  saveRDS(final_result, file = save_to_path )
 print(paste0("file written to: ", save_to_path ))
}#endfunction prepare_elprice_observed_dataset

#wholesale prices! use multiply_wholesale_by multiplier
my_data_read_elprice_observed_data <- function(multiply_wholesale_by = 1.0 #wholesale prices -> retail prices usually higher (profit margin), but spot prices for HH usually based on wholesale anyway
                                               , filepath = "_static_data/elprices_Czechia.Rds"
                                               #, filepath_fx = "_static_data/ECB_Data_Portal_long_20250207083750.xlsx"
                                               ) {
  # read el prices for CZ from eurostat
  print(paste0("Reading el. price observations from: ", filepath))
  
  elprice_czk_raw <- readRDS(filepath) %>%
    mutate(
      date = lubridate::ymd(date)
      , hour = hour %>% as.integer()
      # for datetime, use fixed timezone (no summer time, because with summer time, I would get 2 observations for same hour or a missing obs.):
      # "Etc/GMT-1" time zone offset is UTC/GMT +1 hours (like CET but without NAs for when the summer/winter time changes)
      , x_prague =  as.POSIXct(paste0(date, " ", 0), format = "%Y-%m-%d %H", tz = "Europe/Prague") 
      , x_utc =  with_tz(x_prague, tzone = "UTC")
      , x_utc_addhours = x_utc + lubridate::hours(hour)
      , datetime = with_tz(x_utc_addhours, tzone = "Etc/GMT-1")
      , year = year(date) %>% as.integer()
      , month = month(date) %>% as.integer()
      , day = day(date) %>% as.integer()
      , weekday = wday(date, week_start = 1) %>% as.integer()
    )
  elprice_czk <- elprice_czk_raw %>% 
    mutate(price_CZK_kWh = price_CZK_MWh / 10^3
           , price = price_CZK_kWh * multiply_wholesale_by) %>%
    select(datetime, date, year, month, day, hour, price)
  
 
  # check duplicates
  if ( elprice_czk %>% count(date, hour) %>% filter(n > 1) %>% nrow() >1) {
    warning("Duplicated values found!")
    duplicates <- elprice_czk %>% group_by(date, hour) %>% mutate(n=n()) %>% filter(n>1) %>% ungroup()
    glimpse(duplicates)
    }
  #rm(elprice_cz, fxrate)
  print("Reading el. price observations done")
  return(elprice_czk)
}#endfunction my_data_read_elprice_observed_data 

my_gridcost <- function(df = my_data_read_distrib_costs_observed_data()
                          , years = 20
                          , annual_growth = 0.04
                          , startdate = '2025-01-01'
                          , method = "linear" # "static", "linear", "last_w_growth", "historical_w_growth"
                          , replace2023 = TRUE
                          , fixed_seed = TRUE
                          , orig_data_where_available = TRUE
                          , lastval # if method == last_w_growth and lastval not set, it uses the last available data as in historical_w_growth
) {
  print(paste0("My grid cost started"))
  
  if (fixed_seed) {set.seed(123)}
  #check for method
  if (!method %in% c("static", "linear", "last_w_growth", "historical_w_growth")) {stop("Unknown method for grid cost calculation")}#endif
  
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
  future_timestamps <- seq( startdate, by = "1 hour", length.out = years * 24*365.25 %>% round(0) -1)
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
    
  } else if (method %in% c("historical_w_growth")) {
    # take the last value from observations in range
    lastval <- df %>% #filter(year <= year(startdate)) %>% 
               tail(1) %>% select(grid_cost) %>% pull(1)
    future_prices_step2 <- future_years %>% mutate(grid_cost_fcast = lastval * (1 + annual_growth)^(0:(years-1)) )
  } else if (method %in% c("last_w_growth")) {
    #if lastval missing, set the same way as in "historical_w_growth" method
    if (missing(lastval)) {lastval <- df %>% #filter(year <= year(startdate)) %>% 
      tail(1) %>% select(grid_cost) %>% pull(1)}
    future_prices_step2 <- future_years %>% mutate(grid_cost_fcast = lastval * (1 + annual_growth)^(0:(years-1)) )
    }#endif
  

  
  
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
  future_timestamps <- seq( startdate, by = "1 hour", length.out = years * 24*365.25 %>% round(0) -1)
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
                          , method = "linear" # "static", "linear", "last_w_growth", "historical_w_growth", "random_walk", "random_walk_trend", "mean_reverting_rw", "selected_year" 
                          , fixed_seed = TRUE
                          , theta = 0.05
                          , add_intraday_variability = TRUE
                          , add_intraweek_variability = TRUE
                          , lastval
                          , selected_year = 2023
                          , add_random_noise = 0  # multiplier of the original used to add some noise to the price values (0.2 means some value from 0.8*price to 1.2*price)
) {
  print(paste0("My el. price started"))
  #check for method
  if (!method %in% c("static", "linear", "last_w_growth", "historical_w_growth", "random_walk", "random_walk_trend", "mean_reverting_rw", "selected_year" )) {stop("Unknown method for el. price calculation")}#endif
  
  startdate <- my_check_date(startdate)
  startyear <- substring(startdate, 1,4) %>% as.integer()  #start_date %>% lubridate::floor_date(start_date %>% lubridate::ymd())
  endyear <- startyear + years
  
  # last known price (avg. of 24 hours)
  last_price <- mean(tail(df$price, 24), na.rm = TRUE)
  print(paste0("last price: ", last_price %>% round(3)))
  
  if (missing(lastval)) {
    lastval <- last_price
  }#
  
  if (missing(selected_year)) {
    selected_year <- 2023
  }#
  
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
  if (add_intraday_variability || add_intraweek_variability || method == "historical_w_growth") { 
    
  decomposed <- df %>% as_tsibble(index = datetime) %>% 
    #filter(datetime<'2024-01-01') %>%
    model(STL(price ~ trend() + season(period = "day") + season(period = 168) #+ season(period = "year")
    )) %>%  #season is "day" because pattern repeats daily
    components() %>% 
    mutate( month = month(datetime) %>% as.integer()
            , day = day(datetime) %>% as.integer()
            , hour = hour(datetime) %>% as.integer()
            , weekday = wday(datetime, week_start = 1))
  
  #decomposed %>% filter(datetime >='2020-01-01' & datetime <='2020-04-30')   %>% autoplot()
  
  
  
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
  
  print("decomposed done")
  }#end decomposing
  
  mean_trend <- decomposed %>% as_tibble() %>% ungroup() %>% summarise(mean_trend = mean(trend, na.rm = TRUE)
                                                                       , sd_trend = sd(trend, na.rm = TRUE))
  
  # create future timestamps
  future_timestamps <- seq( startdate, by = "1 hour", length.out = years * 24*365.25 %>% round(0) -1)
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
  
  print("future_prices_step0 done")

  if (method %in% c("static")) {
    mu <- mean(df$price, na.rm = TRUE)
    
    future_prices_step2 <- future_prices_step0 %>% mutate(price_method = mu)
    print("future_prices_step2 - static done")
    rm(future_prices_step0)
  } else if (method %in% c("linear")) {
    
    tslm_model <- df %>% as_tsibble(index = datetime) %>%
      model(
        ts_fit = TSLM(price ~ trend()  + season(period = "day") + season(period = 168))
      )
    
    future_prices_step2 <- tslm_model %>% forecast(new_data = future_prices_step0 %>% as_tsibble(index = datetime)) %>%  #new_data has to be tsibble!
      rename(price_method = .mean) %>%
      select(-`.model`, -`price`)
    print("future_prices_step2 - time series model done")
    rm(future_prices_step0)
    #future_prices_step2 %>% head(150) %>% ggplot(aes(x=datetime, y = price_method)) + geom_line()
    
  } else if (method %in% c("last_w_growth")) { 
    #apply lastval and growth 
    future_prices_step2 <- future_prices_step0 %>%
      mutate(price_method = lastval * exp(annual_growth * as.numeric(difftime(datetime, max(df$datetime)
                                                                                   , units = "days") / 365)))
    print("future_prices_step2 - last_w_growth done")
  } else if (method %in% c("historical_w_growth")) { 
    #apply just historical trend (without seasonality)
    future_prices_step2 <- future_prices_step0 %>%
      left_join(decomposed_agg %>% select(month, weekday, hour, mean_trend)
                , by = c("month" = "month", "weekday" = "weekday", "hour" = "hour")) %>%
      rename( 
        price_method = mean_trend ) %>% 
      mutate(price_method = price_method * exp(annual_growth * as.numeric(difftime(datetime, max(df$datetime)
                                                                                   , units = "days") / 365)))
    print("future_prices_step2 - historical_w_growth done")
    rm(future_prices_step0)
  } else if (method %in% c("selected_year")) {
    
    # check if base_year exists in data
    if ( !(selected_year %in% (df$year %>% unique() ))) {
      stop(paste("Stopping: selected year", selected_year, "not found in data"))
    }#
    
    base_data <- df %>% filter(year == selected_year) %>% select(-datetime, -year, -date)
    
    if (nrow(base_data) < 365*24 ) {
      warning("Selected year has incomplete underlying data, switching to 2023")
      selected_year <- 2023
      base_data <- df %>% filter(year == selected_year) %>% select(-datetime, -year, -date)
    }#
    
    future_prices_step2 <- future_prices_step0 %>% left_join(base_data, by = c("month" = "month"
                                                                               , "day" = "day"
                                                                               , "hour" = "hour")) %>%
      #fill Feb 29 in leap years with data from previous day
      mutate(price = ifelse(is.na(price), yes = lag(price, 24), no = price)) %>%
      rename(price_method = price)
    print("future_prices_step2 - selected_year done")
    rm(future_prices_step0)
  } else if (method %in% c("random_walk", "random_walk_trend", "mean_reverting_rw")) {
    
    # Compute historical mean price
    mu <- mean(df$price, na.rm = TRUE)
    
    #create innovations for random walk, using sd from hourly prices from observations
    future_prices_step1 <- future_prices_step0 %>%
      left_join(avg_actual_price, by = c("month" = "month", "day" = "day", "hour" = "hour") ) %>%
      mutate(
        #note: the random walk step (innovation) is kept as an extra column for random walk methods
        innovation = rnorm(n(), mean = 0, sd = sd_price * 0.5),  # sd_price scaled down to avoid large fluctuations
      ) %>%
      #drop unneeded cols
      select(-(mean_price:quantile_975))
     print("future_prices_step1 - rand walk done")
     rm(future_prices_step0)
    if (method %in% c("random_walk")) {
      future_prices_step2 <- future_prices_step1 %>% 
        mutate(
          price_method =  lastval + cumsum(innovation) 
        )
      print("future_prices_step2 - rand walk done")
      rm(future_prices_step1)
    } else if (method %in% c("random_walk_trend")) {
      drift_per_hour <- (annual_growth * lastval) / (365 * 24)
      future_prices_step2 <- future_prices_step1 %>% 
        mutate(
          price_method =  lastval + cumsum(drift_per_hour + innovation) 
        )
      print("future_prices_step2 - rand walk done")
      rm(future_prices_step1)
    } else if (method %in% c("mean_reverting_rw")) {
      innovations <- future_prices_step1$innovation
      future_prices_step2 <- future_prices_step1 %>%
        mutate(
          price_method = accumulate(innovations, ~ .x + theta * (mu - .x) + .y, .init = lastval)[-1]
        )
      print("future_prices_step2 - rand walk done")
      rm(future_prices_step1)
    } else {
      stop("Unknown method, my_elprice() is stopping")
    }
  }#end for trend methods
  gc(full = TRUE)
  # Generate future prices, add random noise if desired
  future_prices <- future_prices_step2 %>%
      mutate(noise_range = runif(n(), 1 - add_random_noise, 1 + add_random_noise)
             , price_method = price_method * noise_range
            ) %>%
    select(-noise_range)
  rm(future_prices_step2)
  gc(full = TRUE)
  print("future_prices - before adding variability done")
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
  
  # variability already included in "linear" method, so if/else here to avoid adding the intraday/intraweek components twice
  if (method %in% c("linear", "selected_year")) {
    future_prices <- future_prices %>%
      mutate(# noise_multiplier1 = sample(c(-1, 1), size = n(), replace = TRUE)
        price = price_method)  
  } else {
  future_prices <- future_prices %>%
    mutate(# noise_multiplier1 = sample(c(-1, 1), size = n(), replace = TRUE)
      price = price_method + generated_hour_component + generated_week_component)
  }#endif
  
  
  #future_prices %>% head(150) %>% ggplot() + geom_line(aes(x=datetime, y = price, color = "red"))
  #future_prices %>% head(150) %>% ggplot() + geom_line(aes(x=datetime, y = price_method)) + geom_line(aes(x=datetime, y = price, color = "red"))
  
  # plt <- ggplot() + 
  #   geom_line(data = , aes(x=datetime, y = price), alpha = 0.3) +
  #   theme_minimal()
  
  plt_boxplot_per_year <- future_prices %>% ggplot(aes(x=factor(year), y = price)) + geom_boxplot()+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(x = "Year",
         y = "Price"
         )
  
  
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




