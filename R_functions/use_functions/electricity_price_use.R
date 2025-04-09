source("R_functions/electricity_price.R")
library(ggrepel)
print("this is electricity_price_use.R file")

#TODO convert plots to a function
plot_charts <- TRUE


#### prepare data #### 
# read electricity prices from file

print("about to read elprice_czk within electricity_price_use.R file")
if (file.exists("_static_data/elprices_Czechia.Rds") == FALSE) {
  prepare_elprice_observed()
}#


if (plot_charts) {
# read cross country data (for a chart only)
prices_multicountry <- "../_data/OTE_elektrina/european_wholesale_electricity_price_data_hourly/european_wholesale_electricity_price_data_hourly/all_countries.csv" %>%
 readr::read_csv() %>% mutate(datetime = with_tz(`Datetime (UTC)` , tzone = "Etc/GMT-1")
                              , year = year(datetime)
                              , month = month(datetime)
                              , day = day(datetime)
                              , hour = hour(datetime)
                              , date = date(datetime)
                              )
prices_multi1 <- prices_multicountry %>% 
  filter(`ISO3 Code` %in% c("CZE", "AUT", "DEU", "SVK", "POL")) 

price_multi2 <- prices_multi1 %>%
  filter(year == 2024) %>%
  filter(month == 2) %>%
  filter(day %in% c(20:25))
 
prices_multi1_day <- prices_multi1 %>% 
  filter(year == "2024") %>%
  group_by(date, `ISO3 Code`, `Country`) %>% 
  summarize(price = mean(`Price (EUR/MWhe)`, na.rm = TRUE)
            , median = median(`Price (EUR/MWhe)`, na.rm = TRUE)
            , q10 = quantile(`Price (EUR/MWhe)`, 0.1, na.rm = TRUE)
            , q90 = quantile(`Price (EUR/MWhe)`, 0.9, na.rm = TRUE))
#daily prices
plot1_daily_crosscuntry <-  ggplot() +
  geom_line(data = prices_multi1_day, aes(x = date, y = price, color = `Country`), alpha = 0.5) +
  geom_line(data = prices_multi1_day %>% filter(`ISO3 Code` %in% c("CZE")) 
            , aes(x = date, y = price), alpha= 0.2, size=3, color = "yellow" ) +
  labs(title = "Electricity prices (daily) in Czechia and nearby countries "
       , x = "Date"
       , y = "EUR / MWh") +
  theme_minimal()
plot1_daily_crosscuntry
my_ggsave("../grafy_atp/elprice/cross_country/01_prices_2024_multicountry_daily.png")


#hourly
 ggplot() +
  geom_line(data =price_multi2, aes(x = datetime, y = `Price (EUR/MWhe)`, color = `Country`), alpha = 0.5) +
  geom_line(data = price_multi2 %>% filter(`ISO3 Code` %in% c("CZE")) 
            , aes(x = datetime, y = `Price (EUR/MWhe)`), alpha= 0.2, size=3, color = "yellow" ) +
  labs(title = "Electricity prices (hourly) in Czechia and nearby countries "
       , x = "Date"
       , y = "EUR / MWh") +
  theme_minimal()
my_ggsave("../grafy_atp/elprice/cross_country/02_prices_2024_02_multicountry_daily.png")
}#endif plot charts cross country comparison


# actual observed prices
elprice_czk <- my_data_read_elprice_observed_data(multiply_wholesale_by = 1.0)

# read distribution_costs - actual observation data - from file
distribution_costs <- my_data_read_distrib_costs_observed_data()
#plot grid costs:
if (plot_charts) {
first_value <- distribution_costs$grid_cost %>% head(1)
last_value <- distribution_costs$grid_cost %>% tail(1)
nyear <- distribution_costs$year %>% max() -  distribution_costs$year %>% min() 
cagr <- (last_value / first_value)^(1 / nyear) - 1
print(paste0("The CAGR rate of grid cost increase over ", nyear, " years is about ", round(cagr,3)*100, " %"))


  distribution_costs %>% ggplot(aes(x = year, y = grid_cost)) +
    geom_line() +
    geom_smooth(method = "loess" # Explicitly use loess
                , se = FALSE
                # , span = 3/ nrow(distribution_costs) #0.05,  # Adjust span for rolling average window
                #, n = nrow(distribution_costs)  #n is crucial for loess to work correctly with date)
    )+
    labs(
      title = "Development of Grid Cost",
      x = "Year",
      y = "CZK / kWh"
    ) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove the legend
  
  my_ggsave("../grafy_atp/gridcost/00_grid_cost_by_year.png")
}#endif plotcharts

# generate dataset for distribution/grid costs - based on observed data - in hourly frequency
print("about to read grid_cost within electricity_price_use.R file")
grid_cost <- my_gridcost(df = my_data_read_distrib_costs_observed_data()
                         , startdate = '2025-01-01'
                         , years = 20
                         , annual_growth = 0.02 # observed data: annual CAGR 0.02104641
                         , method = "historical_w_growth" #   # "static", "linear", "last_w_growth", "historical_w_growth"
                         , orig_data_where_available = TRUE
                         , lastval = 2.0
                        )

grid_cost$plot
grid_cost$grid_cost$grid_cost %>% tail(1)
if (plot_charts) {
my_ggsave("../grafy_atp/gridcost/01_expected_gridcost_historical_w_growth.png")
}#endif plot_charts


grid_cost <- my_gridcost(df = my_data_read_distrib_costs_observed_data()
                         , startdate = '2025-01-01'
                         , years = 20
                         , annual_growth = 0.02 # observed data: annual CAGR 0.02104641
                         , method = "linear" # "last_w_growth"
                         , orig_data_where_available = TRUE
                         , lastval = 2.0
)
grid_cost$plot
grid_cost$grid_cost$grid_cost %>% tail(1)
if (plot_charts) {
my_ggsave("../grafy_atp/gridcost/01b_expected_gridcost_timeseries_model.png")
}#endif plot_charts

grid_cost_data <- grid_cost$grid_cost #hourly data only
#save( x= grid_cost_data, file = "_cache/grid_cost.Rdata")
saveRDS(grid_cost, file = "_cache/grid_cost.Rds")

# generate dataset for feed in tariff in hourly frequency
feed_in <- my_feed_in( years = 20
                       , annual_growth = -0.1 # -0.05
                       , startdate = '2022-01-01'
                       , method = "last_w_growth" 
                       , fixed_seed = TRUE
                       , lastval = 1.1 #last value of price in kWh 
)
if (plot_charts) {
feed_in$plot
my_ggsave("../grafy_atp/feedin/01_expected_feedin.png")
}#endif plot_charts

#feed_in_data <- feed_in$feed_in
#save( x= feed_in_data, file = "_cache/feed_in_price.Rdata")
saveRDS(feed_in, file = "_cache/feed_in_price.Rds")

# static forecast
elprice <- my_elprice(df = my_data_read_elprice_observed_data(multiply_wholesale_by = 1.0)
                         , startdate = '2022-01-01'
                         , years = 2
                         , annual_growth = 0.04
                         , method ="static"
                         , add_intraday_variability = FALSE
                         , add_intraweek_variability = FALSE
                      , lastval = 3.5
                      , selected_year = 2023
                      , add_random_noise = 0) 

#save(elprice , file = "_cache/elprice_static.Rdata")
saveRDS(elprice, file = "_cache/elprice.Rds" )

# # static forecast - with or without daily variability
# elprice1 <- my_elprice(df= elprice_czk
#                           , years = 5
#                           , annual_growth = 0.04
#                           , method ="static"
#                           , add_intraday_variability = FALSE
#                           , add_intraweek_variability = FALSE) %>% mutate(series="static only")
# 
# 
# elprice2 <- my_elprice(df= elprice_czk
#                           , years = 5
#                           , annual_growth = 0.04
#                           , method ="static"
#                           , add_intraday_variability = TRUE
#                           , add_intraweek_variability = FALSE) %>% mutate(series="static + variab_day")
# 
# elprice3 <- my_elprice(df= elprice_czk
#                           , years = 5
#                           , annual_growth = 0.04
#                           , method ="static"
#                           , add_intraday_variability = TRUE
#                           , add_intraweek_variability = TRUE) %>% mutate(series="static + variab_both")
# 
# elprice4 <- my_elprice(df= elprice_czk
#                           , years = 5
#                           , annual_growth = 0.04
#                           , method ="linear"
#                           , add_intraday_variability = TRUE
#                           , add_intraweek_variability = TRUE) %>% mutate(series="linear + variab_both")
# 
# elprice <- bind_rows(elprice1, elprice2, elprice3, elprice4)
# elpriceF <- elprice %>% filter(datetime >= '2027-01-01' & datetime <= '2027-01-30') %>% filter(series != "static only")
# plt1 <- elpriceF%>% ggplot(aes(x=datetime, y=price, color = series)) +
#   geom_line(alpha =.7)+
#   theme_minimal()
# plt1
# 



#### observe daily patterns in data of el. price ####

# daily ACTUAL PRICE pattern
print(paste0("Min: ", elprice_czk$datetime %>% min(), " max: ", elprice_czk$datetime %>% max()))
intraday_pattern_actualprice <- elprice_czk %>% select(datetime, price)

intraday_distribution_per_year_actualprice <- intraday_pattern_actualprice %>% as_tibble() %>%
  mutate(hour = hour(datetime) %>% as.integer()
         , year = year(datetime)) %>%
  group_by(hour, year) %>%
  summarise(mean_price = mean(price, na.rm = TRUE),
            sd_price = sd(price, na.rm = TRUE))
# avg daily ACTUAL PRICe
intraday_distribution_avg_actualprice <- intraday_pattern_actualprice %>% as_tibble() %>%
  mutate(hour = hour(datetime) %>% as.integer()
         , year = year(datetime)) %>%
  group_by(hour) %>%
  summarise(mean_price = mean(price, na.rm = TRUE),
            sd_price = sd(price, na.rm = TRUE), 
            quantile_025 = quantile(price, probs = 0.025, na.rm = TRUE),
            quantile_050 = quantile(price, probs = 0.05, na.rm = TRUE),
            quantile_500 = quantile(price, probs = 0.5, na.rm = TRUE),
            quantile_950 = quantile(price, probs = 0.95, na.rm = TRUE),
            quantile_975 = quantile(price, probs = 0.975, na.rm = TRUE)
  )

# add average ACTUAL PRICE to indiv years
intraday_distribution_actualprice <- bind_rows(intraday_distribution_per_year_actualprice, intraday_distribution_avg_actualprice) %>%
  mutate( year = as.character(year),
          year_ = replace_na(year, "average"))

#### display charts for actual price over years ####
if (plot_charts) {
  # observe the mean and quantiles of ACTUAL PRICE individual hour of the day in individual years -> recent years higher volatility
  intraday_distribution_actualprice %>% ggplot(aes(x = hour, y = mean_price, color = as.factor(year_))) +
    geom_line() +
    ggrepel::geom_text_repel(data = intraday_distribution_actualprice %>%
                      group_by(year_) %>%
                      filter(hour == max(hour)), 
                    aes(label = as.factor(year_)), 
                    box.padding = 0.5,  # Adjust the padding around text
                    max.overlaps = 20,  # Control number of overlaps
                    nudge_x = 4,  # Nudging the text slightly to the right
                    nudge_y = 0,  # No nudging vertically
                    direction = "y",  # Keep text on the y-axis
                    hjust = 0, vjust = 0) +  # Text alignment
    labs(
      title = "Development of Mean Price - Daily Pattern Over the Years",
      x = "Hour of Day",
      y = "Price (CZK / kWh)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove the legend
  
  my_ggsave("../grafy_atp/elprice/01_price_actual_hourly_pattern_by_year.png")
  
  # plot the standard deviation of the daily pattern - higher volatility
  ggplot(intraday_distribution_actualprice, aes(x = hour, y = sd_price, color = as.factor(year_))) +
    geom_line() +
    geom_text_repel(data = intraday_distribution_actualprice %>%
                      group_by(year_) %>%
                      filter(hour == max(hour)), 
                    aes(label = as.factor(year_)), 
                    box.padding = 0.5,  # Adjust the padding around text
                    max.overlaps = 20,  # Control number of overlaps
                    nudge_x = 0.5,  # Nudging the text slightly to the right
                    nudge_y = 0,  # No nudging vertically
                    direction = "y",  # Keep text on the y-axis
                    hjust = 0, vjust = 0) +  # Text alignment
    labs(
      title = "Development of Standard Deviation of Price \nDaily Pattern Over the Years",
      x = "Hour of Day",
      y = "Standard Deviation of Price (CZK / kWh)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove the legend
  my_ggsave("../grafy_atp/elprice/02_price_actual_stdev_hourly_pattern_by_year.png")
  
  # plot average and empirical quantiles
  intraday_distribution_avg_actualprice %>% ggplot(aes(x = hour, y = mean_price)) +
    geom_line(color = "blue", size = 1) +   
    geom_ribbon(aes(ymin = quantile_025, ymax = quantile_975), fill = "blue", alpha = 0.2) + 
    labs(
      title = "Mean and Empirical 95% Confidence Interval of Price in CZK/kWh",
      x = "Hour of Day",
      y = "Price (CZK / kWh)"
    ) +
    ggrepel::geom_label_repel(aes(label = round(mean_price,2), size = 3)) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(legend.position = "none") 
  my_ggsave("../grafy_atp/elprice/03_price_actual_mean_and_quantiles_hourly_pattern.png")
  
  # timeseries chart of price over time
  daily_median_actualprice <- elprice_czk %>% group_by(date) %>% 
    summarize(median = median(price, na.rm = TRUE)
              , mean = mean(price, na.rm = TRUE)
              , sd = sd(price, na.rm = TRUE) ) %>%
    ungroup() %>%
    arrange(date) %>%  # Ensure data is sorted by date (rollmean requires sorted data)
    mutate(mean_24h = zoo::rollmean(mean, k = 24, fill = NA, align = "right")
           , mean_1month = zoo::rollmean(mean, k = 24*30, fill = NA, align = "right")
           , year = year(date))
  
  monthly_median_actualprice <- elprice_czk %>% group_by(year,month) %>% 
    summarize(median = median(price, na.rm = TRUE)
              , mean = mean(price, na.rm = TRUE)
              , sd = sd(price, na.rm = TRUE) ) %>%
    ungroup() %>%
    arrange(year,month) 
  
  # plot price development
  daily_median_actualprice %>% ggplot(aes(x = date)) +
    # geom_line(aes(y = median, colour = "green", alpha=.05)) + 
    geom_line(aes(y = mean), alpha=0.3, color = "blue") +
    #geom_line(aes(y = mean_24h, alpha=.05), color = "green") +
    #geom_line(aes(y = mean_1month, colour = "darkgreen", alpha=.05)) +
    geom_smooth(aes(y = mean), color = "red",  alpha=.1,
                method = "loess",  # Explicitly use loess
                se = FALSE,
                span = 60/nrow(daily_median_actualprice),  # Adjust span for rolling average window
                n = nrow(daily_median_actualprice)) + #n is crucial for loess to work correctly with date
    labs(
      title = "Daily mean of wholesale electricity price",
      x = "Date",
      y = "CZK / kWh"
    ) +
    #ggrepel::geom_label_repel(aes(label = round(mean_price,2), size = 3)) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(legend.position = "none") 
  my_ggsave("../grafy_atp/elprice/04_price_actual_timeseries_hourly_mean_rollmean_loess.png")
  
  # facet by year 
  daily_median_actualprice %>% #filter(date>='2017-01-01' & date<'2021-01-01') %>% 
    mutate(year =  year(date),
           date1 = as_date(paste(1901, str_pad(month(date),2, pad = "0"), str_pad(day(date),2, pad = "0"), sep = "-"))
    ) %>%
    ggplot(aes(x = date1)) +
    facet_grid(year ~ ., scales = "free_y") +  
    scale_x_date(labels = scales::date_format("%b")
                 #, breaks = "1 month"
                 , date_breaks = "1 month"
                 #, limits = as.Date(c(paste0("1901-01-01"), paste0("1901-12-31")))
    ) +
    geom_line(aes(y = mean, alpha=.05), colour = "blue") +
    geom_hline(yintercept = daily_median_actualprice %>% summarize(mean=mean(mean,na.rm=TRUE) %>% round(3)) %>% pull(1)
               , linetype = "dashed") +
    # geom_smooth(aes(y = mean, ), colour = "red",
    #             method = "loess",  # Explicitly use loess
    #             se = FALSE,
    #             span = 0.1,  # Adjust span for rolling average window
    #             n = nrow(daily_median_actualprice)) + #n is crucial for loess to work correctly with date
    labs(
      title = paste0("Daily mean of price \n(dashed line shows the average price of ", daily_median_actualprice %>% summarize(mean=mean(mean,na.rm=TRUE) %>% round(2)) %>% pull(1)," CZK/kWh \nover the whole sample)"),
      x = "Date",
      y = "Price (CZK / kWh)"
    ) +
    #ggrepel::geom_label_repel(aes(label = round(mean_price,2), size = 3)) +
    geom_hline(yintercept = 0, color = "black") +
    theme_minimal() +
    theme(legend.position = "none") 
  my_ggsave("../grafy_atp/elprice/04_price_actual_timeseries_daily_by_year_facet.png", height = 200)
  
  # price and distr. costs
  daily_median_actualprice %>% left_join(distribution_costs, by = "year") %>%
    mutate(price_w_distr = grid_cost + mean) %>%
    ggplot(aes(x = date)) +
    # geom_line(aes(y = median, colour = "green", alpha=.05)) + 
    geom_line(aes(y = mean), color = "blue", alpha=.1) +
    geom_line(aes(y = grid_cost), color = "green", alpha=.8, size = 2) +
    geom_line(aes(y = price_w_distr), color = "black", alpha=.7) +
    geom_smooth(aes(y = price_w_distr), color = "red",  alpha=.1,
                method = "loess",  # Explicitly use loess
                se = FALSE,
                span = 60/nrow(daily_median_actualprice),  # Adjust span for rolling average window
                n = nrow(daily_median_actualprice)) +
    labs(
      title = "El. price (blue), grid costs (green) and total (black)",
      x = "Date",
      y = "Price (CZK / kWh)"
    ) +
    #ggrepel::geom_label_repel(aes(label = round(mean_price,2), size = 3)) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(legend.position = "none") 
  my_ggsave("../grafy_atp/elprice/04_price_actual_timeseries_and_distr_costs.png")
}# shows mean and sd in individual years


# seasonality components from ""Seasonal-Trend decomposition using Loess" (STL):  Trend, Seasonal Component, Remainder (residual); 
decomposed <- elprice_czk %>% as_tsibble(index = datetime) %>% 
  filter(datetime<'2024-01-01') %>%
  model(STL(price ~ season(period = "day") + season(period = 24*7))) %>%  #season is "day" because pattern repeats daily
  components()

# daily SEASONAL PATTERN
intraday_pattern <- decomposed %>% select(datetime, season_day)
monthly_pattern <- decomposed %>% select(datetime, season_168)

intraday_distribution_per_year <- intraday_pattern %>% as_tibble() %>%
  mutate(hour = hour(datetime) %>% as.integer()
         , year = year(datetime)) %>%
  group_by(hour, year) %>%
  summarise(mean_price = mean(season_day, na.rm = TRUE),
            sd_price = sd(season_day, na.rm = TRUE))
# avg daily SEASONAL PATTERN
intraday_distribution_avg <- intraday_pattern %>% as_tibble() %>%
  mutate(hour = hour(datetime) %>% as.integer()
         , year = year(datetime)) %>%
  group_by(hour) %>%
  summarise(mean_price = mean(season_day, na.rm = TRUE),
            sd_price = sd(season_day, na.rm = TRUE), 
            quantile_025 = quantile(season_day, probs = 0.025, na.rm = TRUE),
            quantile_050 = quantile(season_day, probs = 0.05, na.rm = TRUE),
            quantile_250 = quantile(season_day, probs = 0.25, na.rm = TRUE),
            quantile_500 = quantile(season_day, probs = 0.5, na.rm = TRUE),
            quantile_750 = quantile(season_day, probs = 0.75, na.rm = TRUE),
            quantile_950 = quantile(season_day, probs = 0.95, na.rm = TRUE),
            quantile_975 = quantile(season_day, probs = 0.975, na.rm = TRUE)
  )

# add average SEASONAL PATTERN to indiv years
intraday_distribution <- bind_rows(intraday_distribution_per_year,intraday_distribution_avg) %>%
  mutate( year = as.character(year),
          year_ = replace_na(year, "average"))

# monthly component
monthly_distribution_per_year <- monthly_pattern %>% as_tibble() %>%
  mutate(month = month(datetime) %>% as.integer()
         , year = year(datetime)) %>%
  group_by(month, year) %>%
  summarise(mean_price = mean(season_168, na.rm = TRUE),
            sd_price = sd(season_168, na.rm = TRUE))
# avg daily SEASONAL PATTERN
monthly_distribution_avg <- monthly_pattern %>% as_tibble() %>%
  mutate(month = month(datetime) %>% as.integer()
         , year = year(datetime)) %>%
  group_by(month) %>%
  summarise(mean_price = mean(season_168, na.rm = TRUE),
            sd_price = sd(season_168, na.rm = TRUE), 
            quantile_025 = quantile(season_168, probs = 0.025, na.rm = TRUE),
            quantile_050 = quantile(season_168, probs = 0.05, na.rm = TRUE),
            quantile_500 = quantile(season_168, probs = 0.5, na.rm = TRUE),
            quantile_950 = quantile(season_168, probs = 0.95, na.rm = TRUE),
            quantile_975 = quantile(season_168, probs = 0.975, na.rm = TRUE)
  )

# add average SEASONAL PATTERN to indiv years
monthly_distribution <- bind_rows(monthly_distribution_per_year, monthly_distribution_avg) %>%
  mutate( year = as.character(year),
          year_ = replace_na(year, "average"))

#### display charts for intraday seasonal component of the price over years ####
if (plot_charts) {
  # observe the mean and sd of SEASONAL PATTERN individual hour of the day in individual years -> recent years higher volatility
  ggplot(monthly_distribution, aes(x = month, y = mean_price, color = as.factor(year_))) +
    geom_line() +
    geom_text_repel(data = monthly_distribution %>%
                      group_by(year_) %>%
                      filter(month == max(month)), 
                    aes(label = as.factor(year_)), 
                    arrow = arrow(type = 'closed', length = unit(0.2, 'cm')),
                    box.padding = 0.5,  # Adjust the padding around text
                    max.overlaps = 20,  # Control number of overlaps
                    nudge_x = 0.5,  # Nudging the text slightly to the right
                    nudge_y = 0,  # No nudging vertically
                    direction = "y",  # Keep text on the y-axis
                    hjust = 0, vjust = 0) +  # Text alignment
    labs(
      title = "Development of Mean of Monthly Seasonal ComponentOver the Years",
      x = "Month",
      y = "CZK / kWh"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = pretty_breaks()) +
    theme(legend.position = "none")  # Remove the legend
  my_ggsave("../grafy_atp/elprice/05_price_seasonal_hour_hourly_pattern_by_year.png")
  
  
  # plot the standard deviation of the daily pattern - higher volatility
  ggplot(intraday_distribution, aes(x = hour, y = sd_price, color = as.factor(year_))) +
    geom_line() +
    geom_text_repel(data = intraday_distribution %>%
                      group_by(year_) %>%
                      filter(hour == max(hour)), 
                    aes(label = as.factor(year_)), 
                    arrow = arrow(type = 'closed'),
                    box.padding = 0.5,  # Adjust the padding around text
                    max.overlaps = 20,  # Control number of overlaps
                    nudge_x = 0.5,  # Nudging the text slightly to the right
                    nudge_y = 0,  # No nudging vertically
                    direction = "y",  # Keep text on the y-axis
                    hjust = 0, vjust = 0) +  # Text alignment
    labs(
      title = "Development of Standard Deviation of Seasonal Component Daily Pattern Over the Years",
      x = "Hour of Day",
      y = "Standard Deviation of Seasonality"
    ) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove the legend
  my_ggsave("../grafy_atp/elprice/06_price_stdev_seasonal_hour_hourly_pattern_by_year.png")
  
  # plot average and empirical quantiles for intraday seasonal component
  intraday_distribution_avg %>% ggplot(aes(x = hour, y = mean_price)) +
    geom_line(color = "blue", size = 1) +  # Plot the mean line
    geom_ribbon(aes(ymin = quantile_025, ymax = quantile_975), fill = "blue", alpha = 0.2) +  # Confidence interval
    labs(
      title = "Mean and Empirical 95% CI of Seasonal Component",
      x = "Hour of Day",
      y = "Mean Seasonality with Confidence Interval"
    ) +
    ggrepel::geom_label_repel(aes(label = round(mean_price,2)), size = 3) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(legend.position = "none")
  
  my_ggsave("../grafy_atp/elprice/07_price_seasonal_mean_and_quantiles_hourly_pattern_by_year.png")
  
  
}#end plot seasonal components 



#### for 2023, observe whether the intraday pattern of actual price changes in various months of the year ####
if (plot_charts & FALSE) {
  # daily ACTUAL PRICe pattern
  intraday_pattern_actualprice <- elprice_czk %>% filter(year == 2023) %>% select(datetime, price, year, month)
  
  intraday_distribution_per_year_actualprice <- intraday_pattern_actualprice %>% as_tibble() %>%
    mutate(hour = hour(datetime) %>% as.integer()) %>%
    group_by(hour, year, month) %>%
    summarise(mean_price = mean(price, na.rm = TRUE),
              sd_price = sd(price, na.rm = TRUE))
  # avg daily ACTUAL PRICe
  intraday_distribution_avg_actualprice <- intraday_pattern_actualprice %>% as_tibble() %>%
    mutate(hour = hour(datetime) %>% as.integer()
    ) %>%
    group_by(hour) %>%
    summarise(mean_price = mean(price, na.rm = TRUE),
              sd_price = sd(price, na.rm = TRUE), 
              quantile_025 = quantile(price, probs = 0.025, na.rm = TRUE),
              quantile_050 = quantile(price, probs = 0.05, na.rm = TRUE),
              quantile_500 = quantile(price, probs = 0.5, na.rm = TRUE),
              quantile_950 = quantile(price, probs = 0.95, na.rm = TRUE),
              quantile_975 = quantile(price, probs = 0.975, na.rm = TRUE)
    )
  
  # add average ACTUAL PRICE to indiv years
  intraday_distribution_actualprice <- bind_rows(intraday_distribution_per_year_actualprice, intraday_distribution_avg_actualprice) %>%
    mutate( month = as.character(month),
            month_ = replace_na(month, "average"))
  
  # observe the mean and quantiles of ACTUAL PRICE individual hour of the day in individual years -> recent years higher volatility
  ggplot(intraday_distribution_actualprice, aes(x = hour, y = mean_price, color = as.factor(month_))) +
    geom_line() +
    geom_text_repel(data = intraday_distribution_actualprice %>%
                      group_by(month_) %>%
                      filter(hour == max(hour)), 
                    aes(label = as.factor(month_)), 
                    box.padding = 0.5,  # Adjust the padding around text
                    max.overlaps = 20,  # Control number of overlaps
                    nudge_x = 0.5,  # Nudging the text slightly to the right
                    nudge_y = 0,  # No nudging vertically
                    direction = "y",  # Keep text on the y-axis
                    hjust = 0, vjust = 0) +  # Text alignment
    labs(
      title = "Development of Mean Price - Daily Pattern Over Month in 2023",
      x = "Hour of Day",
      y = "CZK / kWh"
    ) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove the legend
  
  # plot the standard deviation of the daily pattern - higher volatility
  ggplot(intraday_distribution_actualprice, aes(x = hour, y = sd_price, color = as.factor(month_))) +
    geom_line() +
    geom_text_repel(data = intraday_distribution_actualprice %>%
                      group_by(month_) %>%
                      filter(hour == max(hour)), 
                    aes(label = as.factor(month_)), 
                    box.padding = 0.5,  # Adjust the padding around text
                    max.overlaps = 20,  # Control number of overlaps
                    nudge_x = 0.5,  # Nudging the text slightly to the right
                    nudge_y = 0,  # No nudging vertically
                    direction = "y",  # Keep text on the y-axis
                    hjust = 0, vjust = 0) +  # Text alignment
    labs(
      title = "Development of Standard Deviation of Price - Daily Pattern Over Month in 2023",
      x = "Hour of Day",
      y = "Standard Deviation of Seasonality"
    ) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove the legend
  
  # plot average and empirical quantiles
  intraday_distribution_avg_actualprice %>% ggplot(aes(x = hour, y = mean_price)) +
    geom_line(color = "blue", size = 1) +   
    geom_ribbon(aes(ymin = quantile_025, ymax = quantile_975), fill = "blue", alpha = 0.2) + 
    labs(
      title = "Mean and Empirical 95% Confidence Interval of Price in CZK/kWh",
      x = "Hour of Day",
      y = "CZK/kWh"
    ) +
    ggrepel::geom_label_repel(aes(label = round(mean_price,2), size = 3)) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(legend.position = "none") 
  
  
}# show mean and sd in 2023 by month



#### observe seasonality patterns ####

if (plot_charts) {
  timetk::plot_stl_diagnostics(elprice_czk, .date_var = datetime, .value = price)
  my_ggsave("../grafy_atp/elprice/08_price_seasonal_slt_diagnostics.png")
  
  timetk::plot_seasonal_diagnostics(elprice_czk, .date_var = datetime, .value = price)
  my_ggsave("../grafy_atp/elprice/09_price_seasonal_diagnostics.png", height = 200)
}

#### define training vs testing split ####
elprice_czk_split <- rsample::initial_time_split(elprice_czk, prop =   1- (365*24/ nrow(elprice_czk)))
elprice_czk_train <- rsample::training(elprice_czk_split) %>%  as_tsibble(index = datetime)
elprice_czk_test <- rsample::testing(elprice_czk_split) %>%  as_tsibble(index = datetime)

#is_tsibble(elprice_czk)
#is_tsibble(elprice_czk_train)

tslm_fit <- elprice_czk_train %>% 
  fabletools::model(trend_fit = TSLM(price ~ trend())
                    , ts_fit = TSLM(price ~ trend() + season())
                    #ts_2_fit = TSLM(price ~ trend() + season() + I(trend()^2))
  )

tidy(tslm_fit) %>% View()

#see fit metrics
tslm_fit %>% 
  accuracy() %>% 
  arrange(MAPE)

# plot forecast 
tmp1 <- tslm_fit %>% 
  forecast(h = "3 years") 
tmp1 %>% 
  autoplot(elprice_czk %>% as_tsibble(index = datetime))



#### show projected price 
#"static", "linear", "last_w_growth", "historical_w_growth", "random_walk_trend", "selected_year" 
elprice1_linear <- my_elprice(df = my_data_read_elprice_observed_data()
                      , startdate = '2025-01-01'
                      , years = 20
                      , annual_growth = 0.05
                      , method ="linear"
                      , add_intraday_variability = FALSE
                      , add_intraweek_variability = FALSE
                      , lastval = 3.5
                      , selected_year = 2023
                      , add_random_noise = 0)

elprice2_historical_w_growth <- my_elprice(df = my_data_read_elprice_observed_data()
                      , startdate = '2025-01-01'
                      , years = 20
                      , annual_growth = 0.05
                      , method ="historical_w_growth"
                      , add_intraday_variability = TRUE
                      , add_intraweek_variability = TRUE
                      , lastval = 3.5
                      , selected_year = 2023
                      , add_random_noise = 0)

elprice3_randwalk_trend <- my_elprice(df = my_data_read_elprice_observed_data()
                                           , startdate = '2025-01-01'
                                           , years = 20
                                           , annual_growth = 0.05
                                           , method ="random_walk_trend"
                                           , add_intraday_variability = TRUE
                                           , add_intraweek_variability = TRUE
                                           , lastval = 3.5
                                           , selected_year = 2023
                                           , add_random_noise = 0)

elprice4_selectedyear <- my_elprice(df = my_data_read_elprice_observed_data()
                                      , startdate = '2025-01-01'
                                      , years = 20
                                      , annual_growth = 0.05
                                      , method ="selected_year"
                                      , add_intraday_variability = TRUE
                                      , add_intraweek_variability = TRUE
                                      , lastval = 3.5
                                      , selected_year = 2023
                                      , add_random_noise = 0)


save(file = "tmp/elpricecharts.RData"
     , elprice1_linear
     , elprice2_historical_w_growth
     , elprice3_randwalk_trend
     , elprice4_selectedyear)
load("tmp/elpricecharts.RData")

elpr <- elprice1_linear$price_data %>% 
  inner_join(elprice2_historical_w_growth$price_data %>% select(datetime, price) %>% rename(price_hist_w_growth = price), by = "datetime") %>%
  inner_join(elprice3_randwalk_trend$price_data %>% select(datetime, price) %>% rename(price_rw_w_growth = price), by = "datetime") %>%
  inner_join(elprice4_selectedyear$price_data %>% select(datetime, price) %>% rename(price_selyear_w_growth = price), by = "datetime") 

save(file = "tmp/elpr.RData"
     , elpr)
load("tmp/elpr.RData")

color_mapping1 <- c("Time series model" = "orange"
                   , "Historical w growth" = "red"
                   , "Rand Walk w growth" = "purple"
                   , "Selected Year" = "gray"
                   )

elpr %>% filter(year == 2025 & month %in% c(3:6)) %>% ggplot() +
geom_line( aes(x = datetime, y = price, color =  "Time series model"), alpha = 0.4 ) +  
geom_line( aes(x = datetime, y = price_hist_w_growth, color = "Historical w growth"), alpha = 0.4 ) +  
geom_line( aes(x = datetime, y = price_rw_w_growth, color =  "Rand Walk w growth"), alpha = 0.4 ) +  
geom_line( aes(x = datetime, y = price_selyear_w_growth, color = "Selected Year"), alpha = 0.4 ) +  
scale_color_manual(values = color_mapping1) +
theme_minimal()

elpr %>% filter(year == 2044 & month %in% c(1:7) ) %>% ggplot() +
  geom_line( aes(x = datetime, y = price, color =  "Time series model"), alpha = 0.4, ) +  
  geom_line( aes(x = datetime, y = price_hist_w_growth, color = "Historical w growth"), alpha = 0.4 ) +  
  geom_line( aes(x = datetime, y = price_rw_w_growth, color =  "Rand Walk w growth"), alpha = 0.4 ) +  
  geom_line( aes(x = datetime, y = price_selyear_w_growth, color = "Selected Year"), alpha = 0.4 ) +  
  scale_color_manual(values = color_mapping1) +
  theme_minimal()
  