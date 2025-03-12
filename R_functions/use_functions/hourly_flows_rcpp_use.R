source("R_functions/hourly_flows_rcpp.R")

system_params <- list(
  # for dev work only:
  use_cache_data = FALSE     # load example cached data
  , fixed_seed = TRUE  # if some random values are to be used
  # system params
  , battery_capacity_kwh = 10  # battery capacity in kWh
  , battery_charge_efficiency = 0.95     # when charging the battery with E kWh, the stored energy is E * battery_charge_efficiency 
  , battery_discharge_efficiency = 0.95  # when discharging the battery with E kWh, the released energy is E * battery_discharge_efficiency 
  , battery_initial_soc = 0.5            # initial state of charge of the battery in terms of battery_capacity_kwh (0.5 means 50%)
  , battery_min_soc = 0.1      # to avoid battery damage, do not discharge below battery_min_soc (0.1 = 10% of max battery capacity)
  , battery_max_soc = 1.0      # cannot charge over full capacity, maybe I want to charge to lower than max capacity (longer battery life)  (1.0 = 100% of max battery capacity)
  , battery_degradation = 0.01 # rate of degradation of battery (lower capacity) per year
  , PV_degradation = 0.01      # rate of degradation of PV panels per year
  , PV_system_own_consumption = 0.030 # constant consumption of the PV system installation to power inverter etc, in kWh per hour
  # financial params
  , installation_cost = 200000 # in CZK
  , discount_rate = 0.03       # 3% discount rate
  , annual_maintenance_cost = 4000  # expected cost paid every year in CZK
  # date range
  , start_date = "2022-01-01" # from what date system starts operating 
  , system_lifetime = 20       # in years
  #, end_date = ((start_date %>% as.Date()) + lubridate::years(system_params$system_lifetime) ) %>% lubridate::ceiling_date(unit = "year")
  # PV installation parameters
  , lat =  49.278          # Latitude  
  , lon = 16.998           # Longitude
  , PV_system_loss = 14    # in percent
  , PV_angle = 30          # inclination angle (0° = horizontal)
  , PV_aspect = 0          # azimuth angle (0° = South, 90° = West)
  , PV_peakpower = 4.5     # in kWp
  , PV_angle = 30          #  Inclination angle (0° = horizontal)
  , PV_aspect = 0           # Azimuth angle (0° = South, 90° = West)
  #, PV_optimalangles = 0   # optimal angle and inclination
  #, PV_trackingtype = 0    # Type of suntracking used, 0=fixed
  , PV_add_PV_noise = 0.0  # multiplier of the original used to add some noise to the P values (0.2 means some value from 0.8P to 1.2P)
  # household characteristic
  , HH_annual_consumption = 3         # in MWh annually
  , HH_add_cons_multiplier = 0.0      # multiplier of the original used to add some noise to the cons_kWh values (0.2 means some value from 0.8cons_kWh to 1.2cons_kWh)
  # electricity price parameters
  , elprice_method = "historical_w_growth"    # methods are  "static", "linear", "historical", "historical_w_growth", "random_walk", "random_walk_trend", "mean_reverting_rw"
  , elprice_annual_growth = 0.05              # annual growth of price, e.g. 0.005 , if method like "last_w_growth" is used
  , elprice_add_intraday_variability = TRUE   # add intraday seasonality pattern
  , elprice_add_intraweek_variability = TRUE  # add intraweek (weekday) seasonality pattern
  # feed-in tariff parameters
  , feedin_method = "last_w_growth"   # only method is "last_w_growth" 
  , feedin_lastval = 1.1              #last value of feed-in tarrif in CZK per kWh 
  , feedin_annual_growth = 0.01       # annual growth of feed-in tarrif
  # gridcost parameters
  , gridcost_method = "last_w_growth" # methods are "static", "linear", "last_w_growth"
  , gridcost_annual_growth = 0.04     # annual growth of grid cost
)

#### load initial data for load and production ####

# adjust loaded data to include the same structure: datetime, date, year, month, day, weekday, hour

if (system_params$use_cache_data) {

#get solar data (API call) (replace with function call) - returns list with data and metadata, data in solar$solar_data
# datetime, date, year, month, day, weekday, hour, P  (P is in Watts, change to kWh later)
solar <- readRDS("_cache/solar.Rds")  # this is for 4.5 kWp capacity installed   #2022-2024
solar_data <- solar$solar_data
solar$plot
solar_data %>% tail(100) %>% glimpse()

#get household el. consumption data (replace with function call)
# datetime, date, year, month, day, weekday, hour, cons_kWh
elcons <- readRDS("_cache/elcons_data.Rds")  # this is for a residential consumption of 3 MWh annually #2022-2024
elcons$plot
elcons_data <- elcons$elcons
elcons_data %>% glimpse()

#get grid distribution costs 
# datetime, date, year, month, day, weekday, hour, grid_cost
grid_cost <- readRDS("_cache/grid_cost.Rds")  # this is STATIC feed-in price for each hour, range 2022-2024
grid_cost$plot
grid_cost_data <- grid_cost$grid_cost
grid_cost_data %>% glimpse()

#get feedin prices (replace with function call)
# datetime, date, year, month, day, weekday, hour, feedin
feed_in <-  readRDS("_cache/feed_in_price.Rds")   # this is STATIC feed-in price for each hour, range 2022-2024
feed_in$plot
feed_in_data <- feed_in$feed_in
feed_in_data %>% glimpse()

#get electricty prices (replace with function call)
# datetime, date, year, month, day, weekday, hour, price
elprice <- readRDS("_cache/elprice.Rds")  # this is STATIC price for each hour, range 2022-2024
elprice$plot
elprice_data <- elprice$price_data
elprice_data %>% glimpse()

} else {
  #source the data and call the functions 
  source("R_functions/read_irradiance_api.R")
  source("R_functions/residential_electricity_load_profile.R")
  source("R_functions/electricity_price.R")
  
  #get solar data (API call) - returns list with data and metadata, hourly data in solar$solar_data
  # datetime, date, year, month, day, weekday, hour, P  (P is in Watts, change to kWh later)
  # solar data is kept as list, as function myCombineConsAndSolar() handles the conversion to kWh and selection of important cols
  solar <- getSolarData(lat =  system_params$lat  
                        , lon =  system_params$lon
                        , start_date =  system_params$start_date
                        , system_lifetime = system_params$system_lifetime
                        , loss = system_params$PV_system_loss  # in percent, e.g. 14 means 14 %
                        , angle = system_params$PV_angle # Inclination angle (0° = horizontal)
                        , aspect = system_params$PV_aspect # Azimuth angle (0° = South, 90° = West)
                        , peakpower = system_params$PV_peakpower  #in kWp
                        , add_PV_noise = system_params$PV_add_PV_noise # multiplier of the original used to add some noise to the P values (0.2 means some value from 0.8P to 1.2P)
                        , fixed_seed = system_params$fixed_seed
  ) 
  solar_data <- solar$solar_data
  #solar_data$solar_data %>% tail(100) %>% glimpse()
  print(paste0("solar_data nrow: ", nrow(solar_data), " minyear: ", min(solar_data$year), " maxyear: ", max(solar_data$year) ) )
  
  #get household el. consumption data (replace with function call)
  # datetime, date, year, month, day, weekday, hour, cons_kWh
  elcons <- get_load_data(start_date = system_params$start_date
                          , system_lifetime = system_params$system_lifetime
                          , annual_consumption = system_params$HH_annual_consumption
                          , fixed_seed = system_params$fixed_seed 
                          , add_HH_cons_noise = system_params$HH_add_cons_multiplier )
  #elcons %>% glimpse()
  elcons_data <- elcons$elcons
  print(paste0("elcons nrow: ", nrow(elcons_data)))
  
  #get grid distribution costs 
  # datetime, date, year, month, day, weekday, hour, grid_cost
  grid_cost <- my_gridcost(df = my_data_read_distrib_costs_observed_data()
                           , startdate = system_params$start_date
                           , years = system_params$system_lifetime
                           , annual_growth = system_params$gridcost_annual_growth #
                           , method = system_params$gridcost_method
  )
  grid_cost_data <- grid_cost$grid_cost #hourly data only
  #grid_cost_data %>% glimpse()
  print(paste0("grid_cost_data nrow: ", nrow(grid_cost_data)))
  
  #get feedin prices (replace with function call)
  # datetime, date, year, month, day, weekday, hour, feedin
  feed_in <- my_feed_in( years = system_params$system_lifetime
                              , annual_growth = system_params$feedin_annual_growth # e.g. -0.05
                              , startdate = system_params$start_date
                              , method = system_params$feedin_method
                              , fixed_seed = system_params$fixed_seed 
                              , lastval = system_params$feedin_lastval #last value of price in kWh 
  )
  feed_in_data <- feed_in$feed_in
  #feed_in_data %>% glimpse()
  print(paste0("feed_in_data nrow: ", nrow(feed_in_data)))
  
  #get electricty prices (replace with function call)
  # datetime, date, year, month, day, weekday, hour, price
  elprice <- my_elprice(df = my_data_read_elprice_observed_data()
                        , startdate = system_params$start_date
                        , years = system_params$system_lifetime
                        , annual_growth = system_params$elprice_annual_growth
                        , method = system_params$elprice_method 
                        , add_intraday_variability = system_params$elprice_add_intraday_variability 
                        , add_intraweek_variability = system_params$elprice_add_intraweek_variability 
                        ) 
  elprice_data <- elprice$price_data
  #elprice_data %>% glimpse()
  print(paste0("elprice nrow: ", nrow(elprice_data)))

}#endif use_cache_data

# prepare dataset to serve as input for energy flows
df1 <- myCombineConsAndSolar(solar_data
                             , elcons_data
)
df1 %>% nrow()
df1$datetime %>% min()
df1$datetime %>% max()

# calculate energy flows
energy_flows <- CalculateEnergyFlows(df1,
                                       system_params)
energy_flows$date %>% min()
energy_flows$date %>% max()
# debugonce(CalculateFinancials)
energy_flows_enh <- CalculateFinancials(energy_flows
                                         , elprice_data
                                         , feed_in_data
                                         , grid_cost_data
                                         , params = system_params)
energy_flows_enh$summary_vals %>% glimpse()

# check financial values calcs
if (TRUE==FALSE) {
  energy_flows_enh$df_hourly %>% filter(PV_available>0 & ( grid_export >0.1)) %>% head(10) %>% 
    select(maintenance_costs, feed_in, grid_cost, price, cons_kWh, PV_available, excess_solar, deficit
           , grid_export, grid_import, net_cashflow_without_PV
           , revenue_from_feed_in, cost_from_grid, net_cashflow_explicit, net_cashflow_explicit_w_maintenance
           , benefit_wo_maintenance, benefit, benefit_alt
           , hourly_elcons_savings_kwh, hourly_elcons_savings_value) %>% View()
}#end disabled block

estim_breakeven_feed_in <- FindBreakevenFeedIn(hourly_energy_flows = energy_flows_enh$df_hourly, params = system_params)
cat("Breakeven Feed-in Tariff:", round(estim_breakeven_feed_in, 3), "CZK/kWh\n"
    , " (with given el. prices (mean price: ", mean(energy_flows_enh$df_hourly$price, na.rm = TRUE)  ,"CZK/kWh))\n")
estim_breakeven_price <- FindBreakevenPrice(hourly_energy_flows = energy_flows_enh$df_hourly, params = system_params)
cat("Breakeven Electricity Price :", round(estim_breakeven_price, 3), "CZK/kWh\n"
    , " (with given feed-in tariff (mean feed-in: ", mean(energy_flows_enh$df_hourly$feed_in, na.rm = TRUE)  ,"CZK/kWh))\n")



# inspect plots
plotDay("2024-07-15", energy_flows_enh$df_hourly, system_params)
plotWeek("2024-07-10", energy_flows_enh$df_hourly, system_params)
plotPrices("2024-07-10", range = 5, energy_flows_enh$df_hourly, params = system_params)

enflows_grp_day <- energy_flows_enh$df_hourly %>% group_by(date) %>% 
  summarize(total_demand = total_demand %>% sum(na.rm = TRUE)
            , PV_available = PV_available %>% sum(na.rm = TRUE)
            , price = price %>% mean(na.rm = TRUE)
            , grid_cost = grid_cost %>% mean(na.rm = TRUE)
            , feed_in = feed_in %>% mean(na.rm = TRUE)
             )
plotPrices("2024-07-10", range = 500, enflows_grp_day, params = system_params)
