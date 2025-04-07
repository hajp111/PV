library(tidyverse)
#library(tictoc)
library(Rcpp)
source("R_functions/aux_functions.R")

print("this is hourly_flows_rcpp.R file")

#combine solar and elcons data to calculate energy flows (financial value to be calculated in a separate function)
myCombineConsAndSolar <- function(solar_data
                          , elcons
                          # , elprice_data
                          # , feed_in_data
                          # , grid_cost_data
                          ) {
print("myCombineConsAndSolar started")  
solar_data <- solar_data %>% select(datetime, date, hour, P_kWh) 
elcons <- elcons %>% select(datetime, date, hour, cons_kWh)
# elprice_data <- elprice_data %>% select(datetime, date, hour, price)
# feed_in_data <- feed_in_data %>% select(datetime, date, hour, feed_in)
# grid_cost_data <- grid_cost_data %>% select(datetime, date, hour, grid_cost)

out <- solar_data %>% 
  inner_join(elcons, by = c("datetime", "date", "hour")) %>%
  # inner_join(elprice_data, by = c("datetime", "date", "hour") ) %>%
  # inner_join(feed_in_data, by = c("datetime", "date", "hour")) %>%
  # inner_join(grid_cost_data, by = c("datetime", "date", "hour")) %>%
  mutate(# datetime
         # date
         # hour 
         day_of_year = (date - lubridate::floor_date(date, "year")) %>% as.numeric() +1
         , year = lubridate::year(date)
         , quarter = lubridate::quarter(date)
         , month = lubridate::month(date)
         , day = lubridate::day(date)
         , weekday = lubridate::wday(date, week_start = getOption("lubridate.week.start", 1))
         , is_weekend = weekday %in% c(6,7)
         )
return(out)  
}#endfunction myCombineConsAndSolar

#combine energy flows with prices (easier to use different prices then)
myCombineFlowsPrices <- function(energy_flows
                          , elprice_data
                          , feed_in_data
                          , grid_cost_data
) {
  # solar_data <- solar_data$solar_data %>% mutate(P_kWh = P / 10^3) %>% select(datetime, date, hour, P_kWh) 
  # elcons <- elcons %>% select(datetime, date, hour, cons_kWh)
  elprice_data <- elprice_data %>% select(datetime, date, hour, price)
  feed_in_data <- feed_in_data %>% select(datetime, date, hour, feed_in)
  grid_cost_data <- grid_cost_data %>% select(datetime, date, hour, grid_cost)
  
  cat("Dimensions before joins: energy_flows:", dim(energy_flows),  "\n"
      , "elprice:", dim(elprice_data),  "\n"
      , "feed_in:", dim(feed_in_data),  "\n"
      , "grid_cost:", dim(grid_cost_data), "\n")
  
  # Check for matching keys
  cat("Date range in energy_flows:", min(energy_flows$date), "to", max(energy_flows$date), "\n")
  cat("Date range in elprice:", min(elprice_data$date), "to", max(elprice_data$date), "\n")
  cat("Date range in feed_in:", min(feed_in_data$date), "to", max(feed_in_data$date), "\n")
  cat("Date range in grid_cost:", min(grid_cost_data$date), "to", max(grid_cost_data$date), "\n")
  
  
  out <- energy_flows %>%
    inner_join(elprice_data, by = c("datetime", "date", "hour") ) %>%
    inner_join(feed_in_data, by = c("datetime", "date", "hour")) %>%
    inner_join(grid_cost_data, by = c("datetime", "date", "hour")) #%>%
    # mutate(# datetime
    #   # date
    #   # hour 
    #   day_of_year = (date - lubridate::floor_date(date, "year")) %>% as.numeric() +1
    #   , year = lubridate::year(date)
    #   , quarter = lubridate::quarter(date)
    #   , month = lubridate::month(date)
    #   , day = lubridate::day(date)
    #   , weekday = lubridate::wday(date, week_start = getOption("lubridate.week.start", 1))
    #   , is_weekend = weekday %in% c(6,7)
    # )
  
  cat("Dimensions after joins:", dim(out), "\n")
  return(out)  
}#endfunction myCombineFlowsPrices


#### define functions for hourly flows ####
# the function calculates each year separately to improve performance
CalculateEnergyFlows <- function(dat, params) {
  #tic()
  print("CalculateEnergyFlows() started")
  #load C code
  sourceCpp("R_functions/yearly_calc.cpp") 
  
  PV_installation_start_date <- min(dat$datetime)
  
  dat <- dat %>% mutate(
    time_elapsed_years = as.numeric(difftime(datetime, PV_installation_start_date, units = "secs")) / (60 * 60 * 24 * 365.25)
    , PV_degradation_factor = (1 - params$PV_degradation)^time_elapsed_years
    , battery_degradation_factor = (1 - params$battery_degradation)^time_elapsed_years
    , battery_min_capacity = params$battery_capacity_kwh * params$battery_min_soc   #min charge at x% of nominal capacity (min charge does not degrade)
    , battery_max_capacity = pmax( params$battery_capacity_kwh * battery_degradation_factor, battery_min_capacity)
    , battery_storable_capacity = battery_max_capacity - battery_min_capacity  # total capacity
    # these values are defined here but will be filled in the loop
    , total_demand = cons_kWh + params$PV_system_own_consumption,  # hourly demand including system's own consumption
    , PV_available = pmax(P_kWh * PV_degradation_factor, 0)
    , excess_solar =  pmax(PV_available - total_demand, 0)
    , deficit =   pmax(total_demand - PV_available, 0)
    # these values are just pre-defined here but will be filled in the loop
    , battery_available_charge_capacity = 0  # depends on SoC in the previous hour, i.e. "I can store up to N kWh in this hour"
    , battery_charge_raw = 0
    , battery_charge = 0
    , battery_charge_loss = 0
    , battery_discharge_raw = 0
    , battery_discharge = 0
    , battery_discharge_loss = 0
    , grid_import = 0
    , grid_export = 0
    # initial battery state of charge (SOC) from params
    , battery_soc =  params$battery_capacity_kwh * params$battery_min_soc 
    , battery_soc_change = 0 
  )
  
  years <- unique(year(dat$datetime))
  # preallocate list for results
  results_list <- vector("list", length(years))  
  
  #disable lapply, try for loop instaed
  if (FALSE == TRUE) {
  print("starting the lapply")
  results_list <- lapply(years, function(y) {
    year_data <- dat %>% filter(year(datetime) == y)
    initial_soc <- params$battery_initial_soc * params$battery_capacity_kwh # this is the initial SOC for each year
    calculate_year_energy(year_data, params, initial_soc) # call Rcpp function on single year data
  })
  print("lapply done")
  }#end disabled block
  
  # use for instaed of lapply (because of shinyapps.io)
  print("start for loop")
  for (i in seq_along(years)) {
    y <- years[i]
    year_data <- dat %>% filter(year(datetime) == y)
    initial_soc <- params$battery_initial_soc * params$battery_capacity_kwh  # this is the initial SOC for each year
    results_list[[i]] <- calculate_year_energy(year_data, params, initial_soc)  # call Rcpp function on single year data
  }#end for loop
  print("For loop done")

  
  final_results <- bind_rows(results_list)
  final_results <- final_results %>% 
    mutate(
      grid_balance = grid_import - grid_export
      , hourly_elcons_savings_kwh = cons_kWh - grid_import # positive when PV is beneficial, negative when it is worse than no installation
      , self_sufficiency_ratio = (1 - (grid_import/total_demand)) %>% round(3)
    )
  
  #toc()
  return(final_results)
}#endfunction CalculateEnergyFlows

#this ensures that the same mutate is applied in CalculateFinancials() and in BreakevenFeedIn() and BreakevenPrice()
CalculateFinancials_auxMutate <- function(energy_flows_w_prices, params) {
  energy_flows_w_prices <- energy_flows_w_prices %>%
    mutate(discount_factor = 1 / (1 + params$discount_rate/8760)^(row_number()-1) 
           , discounted_PV_available = PV_available * discount_factor
           , theo_cost_from_grid_without_PV = (price + grid_cost) * cons_kWh   #cost without PV installation, not yet with negative sign
           , discounted_theo_cost_from_grid_without_PV = discount_factor * theo_cost_from_grid_without_PV # not yet with negative sign
           , net_cashflow_without_PV = 0 - theo_cost_from_grid_without_PV # already with negative sign
           , discounted_net_cashflow_without_PV = discount_factor * net_cashflow_without_PV # already with negative sign
           # value of kWh saved by having PV
           , hourly_elcons_savings_value = hourly_elcons_savings_kwh * (price + grid_cost) # the value of saved electricity at that hour, i.e. implicit CF 
           # include maintenance costs spread to each hour
           , maintenance_costs = params$annual_maintenance_cost / (365.25*24) # not yet with negative sign
           , discounted_maintenance_costs = discount_factor * maintenance_costs #not yet with negative sign
           # aux cols not yet discounted
           , revenue_from_feed_in = feed_in * grid_export
           , cost_from_grid = (price + grid_cost) * grid_import    #cost with PV installation
           , net_cashflow_explicit = revenue_from_feed_in - cost_from_grid # only explicit cashflows
           , net_cashflow_explicit_w_maintenance = net_cashflow_explicit - maintenance_costs # explicit cashflows and maintenance one has to pay
           #
           , benefit_wo_maintenance = net_cashflow_explicit - net_cashflow_without_PV # compare CF with PV (without maintenance) and without PV
           , benefit = net_cashflow_explicit_w_maintenance - net_cashflow_without_PV # compare CF with PV (incl. maintenance) and without PV
           , benefit_alt = (hourly_elcons_savings_value - maintenance_costs) + revenue_from_feed_in # alternative way to express the benefit
           # discounted values here:
           #, DEBUG_discounted_net_cashflow_explicit = net_cashflow_explicit * discount_factor
           , discounted_net_cashflow_explicit = net_cashflow_explicit * discount_factor
           , discounted_net_cashflow_explicit_w_maintenance = net_cashflow_explicit_w_maintenance * discount_factor
           , discounted_benefit_wo_maintenance = benefit_wo_maintenance * discount_factor
           , discounted_benefit = benefit * discount_factor
           , discounted_benefit_alt = benefit_alt * discount_factor
           #, DEBUG_discounted_revenue_from_feed_in = revenue_from_feed_in * discount_factor
           #, DEBUG_discounted_cost_from_grid = cost_from_grid * discount_factor)
           , cumsum_benefit = cumsum(benefit)
           , cumsum_discounted_benefit = cumsum(discounted_benefit)
          )
  
  return(energy_flows_w_prices)
}#endfunction CalculateFinancials_auxMutate

CalculateFinancials <- function(energy_flows
                                 , elprice_data
                                 , feed_in_data
                                 , grid_cost_data
                                 , params
                                 ) {
  
  #DEBUG change elprice
  # elprice_data <- elprice_data %>% mutate(price = 50)
  
  # enhance energy flows with prices and calculate financials
  energy_flows_w_prices <- myCombineFlowsPrices(energy_flows = energy_flows, elprice_data = elprice_data, feed_in_data = feed_in_data, grid_cost_data = grid_cost_data)
  # energy_flows_w_prices %>% glimpse()
  
  # calculate present value
  energy_flows_w_prices <- energy_flows_w_prices %>% 
                          CalculateFinancials_auxMutate(params = params) 
     
  if (TRUE==FALSE) {
    #view the new cols in data
  energy_flows_w_prices %>% filter(PV_available>0 & ( grid_export >0.1)) %>% head(10) %>% 
    select(maintenance_costs, feed_in, grid_cost, price, cons_kWh, PV_available, excess_solar, deficit
           , grid_export, grid_import, net_cashflow_without_PV
           , revenue_from_feed_in, cost_from_grid, net_cashflow_explicit, net_cashflow_explicit_w_maintenance
           , benefit_wo_maintenance, benefit, benefit_alt
           , hourly_elcons_savings_kwh, hourly_elcons_savings_value) %>% View()
  }#end disabledblock
  
  #payback period
 payback_datetime <- energy_flows_w_prices %>% select(datetime, cumsum_benefit) %>%
    filter(cumsum_benefit > params$installation_cost) %>%
    slice(1) %>%
    pull(datetime)
 if (length(payback_datetime) == 0) {
   payback_datetime <- NA_Date_
   payback_period <- NA_real_
 } else {
 payback_period <- ((difftime(time1 = payback_datetime, time2 = min(energy_flows_w_prices$date), units = "days") %>% as.numeric())/365.25) %>% round(1)
 }#endif 
 #discounted payback period
 discounted_payback_datetime <- energy_flows_w_prices %>% select(datetime, cumsum_discounted_benefit) %>%
   filter(cumsum_discounted_benefit > params$installation_cost) %>%
   slice(1) %>%
   pull(datetime)
 if (length(discounted_payback_datetime) == 0) {
   discounted_payback_datetime <- NA_Date_
   discounted_payback_period <- NA_real_
 } else {
 discounted_payback_period <- ((difftime(time1 = discounted_payback_datetime, time2 = min(energy_flows_w_prices$date), units = "days") %>% as.numeric())/365.25) %>% round(1)
 }#endif
 
    
  # create summary to be returned along with the hourly data
  summary_vals <- tibble(
           date_range = paste0( energy_flows_w_prices$date %>% min() %>% format("%Y-%m-%d"), " - ",  energy_flows_w_prices$date %>% max() %>% format("%Y-%m-%d"))
           , date_range_years = (difftime(energy_flows_w_prices$date %>% max(), energy_flows_w_prices$date %>% min(), units = "days") %>% as.numeric() / 365.25) %>% round(2)
           , discounted_net_cashflow_without_PV = sum(energy_flows_w_prices$discounted_net_cashflow_without_PV) %>% round(1)
           , discounted_net_cashflow_explicit = sum(energy_flows_w_prices$discounted_net_cashflow_explicit)
           , discounted_maintenance_costs =  sum(energy_flows_w_prices$discounted_maintenance_costs) %>% round(1)
           , discounted_net_cashflow_explicit_w_maintenance = sum(energy_flows_w_prices$discounted_net_cashflow_explicit_w_maintenance) %>% round(1)
           #
           , discounted_benefit_wo_maintenance = sum(energy_flows_w_prices$discounted_benefit_wo_maintenance) %>% round(1)
           , discounted_benefit = sum(energy_flows_w_prices$discounted_benefit) %>% round(1)
           , discounted_benefit_alt = sum(energy_flows_w_prices$discounted_benefit_alt) %>% round(1) #should be the same as discounted_benefit
           , NPV = (sum(energy_flows_w_prices$discounted_benefit) - params$installation_cost) %>% round(0)  #benefit has maintenance costs included, compares with baseline scenario of no PV installation
           , NPV_alt = (sum(energy_flows_w_prices$discounted_benefit_wo_maintenance) - params$installation_cost - sum(energy_flows_w_prices$discounted_maintenance_costs)) %>% round(0)  #should be the same as NPV
           , profitable_investment = ifelse(NPV>=0, yes = TRUE, no = FALSE)
           #
           , total_electricity_generated = sum(energy_flows_w_prices$PV_available) %>% round(1)
           , grid_export = sum(energy_flows_w_prices$grid_export) %>% round(1)
           , grid_import = sum(energy_flows_w_prices$grid_import) %>% round(1)
           , household_el_consumption = sum(energy_flows_w_prices$cons_kWh) %>% round(1)
           , elcons_saved = household_el_consumption - grid_import
           , feed_in_revenue_nominal = sum(energy_flows_w_prices$revenue_from_feed_in) %>% round(1)
           # for LOCE calculation
           , installation_cost = params$installation_cost 
           , Present_Value_maintenance_costs = sum(energy_flows_w_prices$discounted_maintenance_costs) %>% round(0)
           , Present_Value_total_cost = ((params$installation_cost + sum(energy_flows_w_prices$discounted_maintenance_costs))) %>% round(0)
           , discounted_total_el_produced = sum(energy_flows_w_prices$discounted_PV_available)
           , LCOE = (Present_Value_total_cost / discounted_total_el_produced) %>% round(2)
           , annualized_rate_of_return = (-1 + ( (1+ NPV/installation_cost)^(1/date_range_years) ) * (1+params$discount_rate)) %>% round(4)  # output like 0.03 means annualized rate of return of 3%
           , FV = ((installation_cost + NPV)*(1+params$discount_rate)^date_range_years) %>% round(1)
           , annualized_rate_of_return_alt = (-1 + ( FV/installation_cost ) ^ (1/date_range_years)) %>% round(4)  #CAGR formula: -1+ (end_vale / start_value)^(1/years),  should be the same as annualized_rate_of_return
           , breakeven_feedin = FindBreakevenFeedIn(hourly_energy_flows = energy_flows_w_prices, params = params) %>% round(2)
           , breakeven_price = FindBreakevenPrice(hourly_energy_flows = energy_flows_w_prices, params = params) %>% round(2)
           , payback_period  
           , discounted_payback_period = discounted_payback_period
           , self_sufficiency_ratio =  (1 - (sum(energy_flows_w_prices$grid_import)/ sum(energy_flows_w_prices$total_demand))) %>% round(3)
           , avg_solar_capture_rate = ( sum(energy_flows_w_prices$revenue_from_feed_in) / sum(energy_flows_w_prices$grid_export) ) %>% round(2)
           , annual_savings = ( discounted_benefit / date_range_years) %>% round(0)
      #     , DEBUG_elcons_x_price = mean(energy_flows_w_prices$price) * elcons_saved
      #     , DEBUG_elprice = paste0("Min: ", min(energy_flows_w_prices$price) %>% round(1), " Max: ", max(energy_flows_w_prices$price) %>% round(1), " Mean: ", mean(energy_flows_w_prices$price) %>% round(1))
      #     , DEBUG_feed_in =  paste0("Min: ", min(energy_flows_w_prices$feed_in) %>% round(1), " Max: ", max(energy_flows_w_prices$feed_in) %>% round(1), " Mean: ", mean(energy_flows_w_prices$feed_in) %>% round(1))
      #     , DEBUG_grid_cost = paste0("Min: ", min(energy_flows_w_prices$grid_cost) %>% round(1), " Max: ", max(energy_flows_w_prices$grid_cost) %>% round(1), " Mean: ", mean(energy_flows_w_prices$grid_cost) %>% round(1))
          )

# summary_vals %>% glimpse()
  #prepare output
  output = list(
          summary_vals = summary_vals
          , df_hourly = energy_flows_w_prices
          )
  return(output)
}#endfunction CalculateFinancials


# breakeven feed-in tariff main
BreakevenFeedIn <- function(new_fixed_val
                             , hourly_energy_flows
                             , params) {
  hourly_energy_flows <- hourly_energy_flows %>% mutate(feed_in = new_fixed_val) %>%
                                    # repeat mutate to reflect the new feed_in  
                                  CalculateFinancials_auxMutate(params = params) 
                                    
  
  new_NPV <- sum(hourly_energy_flows$discounted_benefit) - params$installation_cost
  return(new_NPV)
}#endfunction BreakevenFeedIn

# breakeven electricity price main
BreakevenPrice <- function(new_fixed_val
                            , hourly_energy_flows
                            , params) {
  
  hourly_energy_flows <- hourly_energy_flows %>% mutate(price = new_fixed_val) %>%
                                          # repeat mutate to reflect the new price
                                  CalculateFinancials_auxMutate(params = params)
  
  new_NPV <- sum(hourly_energy_flows$discounted_benefit) - params$installation_cost
  return(new_NPV)
}#endfunction BreakevenPrice

# auxiliary function to provide closure (to pass more than one parameter)
FindBreakevenFeedIn <- function(hourly_energy_flows, params) {
  print("running FindBreakevenFeedIn()")
  # closure to allow uniroot to read energy_flows and system_params
  BreakevenFeedIn_fixed <- function(new_fixed_val) {
    BreakevenFeedIn(new_fixed_val, hourly_energy_flows, params)
  }
  
  #use uniroot with closure
  result <- uniroot(BreakevenFeedIn_fixed, lower = -1*10^3, upper = 10^4)
  return(result$root)  # Return the root directly
}#endfunction FindBreakevenFeedIn

# auxiliary function to provide closure (to pass more than one parameter)
FindBreakevenPrice <- function(hourly_energy_flows, params) {
  print("running FindBreakevenPrice()")
  # closure to allow uniroot to read energy_flows and system_params
  BreakevenPrice_fixed <- function(new_fixed_val) {
    BreakevenPrice(new_fixed_val, hourly_energy_flows, params)
  }

  #use uniroot with closure
  result <- uniroot(BreakevenPrice_fixed, lower = -1*10^3, upper = 10^4)
  return(result$root)  # Return the root directly
}#endfunction FindBreakevenPrice


# aux scaling functions
# secondary y-axis to match primary y-axis scale
scale_secondary_to_primary <- function(x, range_primary_y, range_secondary_y) {
  (x - range_secondary_y[1]) / (range_secondary_y[2] - range_secondary_y[1]) * 
    (range_primary_y[2] - range_primary_y[1]) + range_primary_y[1]
}#endfunction scale_secondary_to_primary

# inverse scaling for the secondary y-axis
scale_primary_to_secondary <- function(y, range_primary_y, range_secondary_y) {
  (y - range_primary_y[1]) / (range_primary_y[2] - range_primary_y[1]) * 
    (range_secondary_y[2] - range_secondary_y[1]) + range_secondary_y[1]
}#endfunction scale_primary_to_secondary

# plot values in selected day (hourly)
plotDay <- function(which_day, hourly_energy_flows, params) {
  which_day <- ConvertTextToDate(which_day)
  df <- hourly_energy_flows %>% filter(date == which_day)
  
  range_primary_y <- range(c(df$PV_available, df$total_demand, df$grid_import, df$grid_export), na.rm = TRUE)
  range_secondary_y <- c(0, params$battery_capacity_kwh)  # Min 0, Max battery capacity
  
  if (nrow(hourly_energy_flows) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data for selected day") +
             theme_void())
  }#endif
  
  ggplot() +
    geom_line(data = df, aes(x = hour, y = PV_available, color = "PV Production")) +
    geom_line(data = df, aes(x = hour, y = total_demand, color = "Total Demand")) +
    geom_line(data = df, aes(x = hour, y = grid_import, color = "Grid Import"), linetype = "dashed") +
    geom_line(data = df, aes(x = hour, y = grid_export, color = "Grid Export"), linetype = "dotdash") +
    #rescaled Battery SoC
    geom_line(data = df, aes(x = hour, y = scale_secondary_to_primary(battery_soc, range_primary_y, range_secondary_y), 
                  color = "Battery SoC"), linetype = "dotted") +
    labs(title = paste("Expected energy flows on", which_day), x = "Hour", y = "kWh") +
    scale_color_manual(values = c("PV Production" = "orange", "Total Demand" = "red",
                                  "Battery SoC" = "blue", "Grid Import" = "black", "Grid Export" = "darkgreen")) +
    scale_y_continuous(
      sec.axis = sec_axis(~ scale_primary_to_secondary(., range_primary_y, range_secondary_y), name = "Battery SoC (kWh)")
    ) +
    theme_minimal()
}#endfunction plotDay

# plot selected week (hourly)
plotWeek <- function(start_day, hourly_energy_flows, params) {
  start_day <- ConvertTextToDate(start_day)
  df <- hourly_energy_flows %>% filter(date >= start_day & date < (start_day + days(7)))
  
  if (nrow(df) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data for selected day") +
             theme_void())
  }#endif
  
  range_primary_y <- range(c(df$PV_available, df$total_demand, df$grid_import, df$grid_export), na.rm = TRUE)
  range_secondary_y <- c(0, params$battery_capacity_kwh)  # Min 0, Max battery capacity
  
  ggplot() +
    geom_line(data = df, aes(x = datetime, y = PV_available, color = "PV Production")) +
    geom_line(data = df, aes(x = datetime, y = total_demand, color = "Total Demand")) +
    geom_line(data = df, aes(x = datetime, y = grid_import, color = "Grid Import"), linetype = "dashed") +
    geom_line(data = df, aes(x = datetime, y = grid_export, color = "Grid Export"), linetype = "dotdash") +
    geom_line(data = df, aes(x = datetime, y = scale_secondary_to_primary(battery_soc, range_primary_y, range_secondary_y), 
                             color = "Battery SoC"), linetype = "dotted") +
    labs(title = paste("Energy Flow from", start_day, "to", start_day + days(6)),
         x = "Date-Time", y = "kWh") +
    labs(title = paste("Exp. energy flows from ", start_day), x = "Hour", y = "kWh") +
    scale_color_manual(values = c("PV Production" = "orange", "Total Demand" = "red",
                                  "Battery SoC" = "blue", "Grid Import" = "black", "Grid Export" = "darkgreen")) +
    scale_y_continuous(
      sec.axis = sec_axis(~ scale_primary_to_secondary(., range_primary_y, range_secondary_y), name = "Battery SoC (kWh)")
    ) +
    theme_minimal()
}#endfunction plotWeek


#plotPrices
plotPrices <- function(start_day, range = 5, energy_flows, params) {
  start_day <- ConvertTextToDate(start_day)
  df <- energy_flows %>% filter(date >= start_day & date<= start_day +lubridate::days(range))
  
  if (!("datetime" %in% names(df)) & ("date" %in% names(df))) { df <- df %>% rename(datetime = date)}
  
  x_max <- max(df$datetime)  
  x_label_pos <- x_max + lubridate::days(ceiling(range/(10)))  # 
  
  range_primary_y <- c(
    ifelse(all(c(df$price, df$grid_cost, df$feed_in) >= 0, na.rm = TRUE), yes = 0
           , no = min(c(df$price, df$grid_cost, df$feed_in), na.rm = TRUE)),
    max(c(df$price, df$grid_cost, df$feed_in), na.rm = TRUE)
  )
  range_secondary_y <- c(0, max(c(df$PV_available, df$total_demand), na.rm = TRUE))
  
  
  y_min <- min(0, range_primary_y[1])  # Ensures it includes 0 if all values are positive
  y_max <- range_primary_y[2]
  

  
  #for labels, get last values:
  last_values <- df %>%
    arrange(datetime) %>%
    summarise(
      datetime = tail(datetime, 1)
      , `PV Production` = tail(PV_available, 1)
      , `Total Demand` = tail(total_demand, 1)
      , `El. price` = tail(price, 1)
      , `Grid Cost` = tail(grid_cost, 1)
      , `Feed-in Tariff` = tail(feed_in, 1)
    ) %>%
    pivot_longer(-datetime, names_to = "series", values_to = "value") %>%
    #because of different axis, adjust position values:
    mutate(value_scaled = case_when(
      series %in% c("PV Production", "Total Demand") ~ scale_secondary_to_primary(value, range_primary_y, range_secondary_y)
      ,  TRUE ~ value 
    ))
  color_mapping <- c("PV Production" = "orange"
                     , "Total Demand" = "red"
                     , "El. price" = "purple"
                     , "Grid Cost" = "gray"
                     , "Feed-in Tariff" = "brown")
  
  ggplot() +
    #kWh values (for right-hand side y-axis)
    geom_line(data = df, aes(x = datetime, y = scale_secondary_to_primary(PV_available, range_primary_y, range_secondary_y), color = "PV Production"), alpha=0.7) +
    geom_line(data = df, aes(x = datetime, y = scale_secondary_to_primary(total_demand, range_primary_y, range_secondary_y), color = "Total Demand"), alpha=0.7) +
   #prices (for main axis)
    geom_line(data = df, aes(x = datetime, y = price, color = "El. price"), alpha=0.7) +
    geom_line(data = df, aes(x = datetime, y = grid_cost, color = "Grid Cost"), linetype = "longdash", alpha=0.7) +
    geom_line(data = df, aes(x = datetime, y = feed_in, color = "Feed-in Tariff"), linetype = "dashed", alpha=0.7) +
    labs(x = "Date", y = "CZK / kWh") +
    scale_color_manual(values = color_mapping) +
    scale_y_continuous(limits = c(y_min, y_max),  
                       sec.axis = sec_axis(~ scale_primary_to_secondary(., range_primary_y, range_secondary_y), name = "kWh")) +
    # add text labels for last values
    #geom_point(data = last_values, aes(x = datetime, y = value_scaled, color = series), size = 3) +
    geom_text_repel(data = last_values, 
                    aes(x = x_label_pos,, y = value_scaled
                        , label = paste0(series, ": ", round(value, 1))
                        , color = series),
                    hjust = 0, nudge_x = 0.05, direction = "y"
                    , size = 3) +
    theme(legend.position = "none")+
    theme_minimal()
}#endfunction plotPrices

