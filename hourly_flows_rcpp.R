library(tidyverse)
library(tictoc)
library(Rcpp)
source("aux_functions.R")

#combine solar and elcons data to calculate energy flows
myCombineConsAndSolar <- function(solar_data
                          , elcons
                          # , elprice
                          # , feed_in_data
                          # , grid_cost_data
                          ) {
  
solar_data <- solar_data$solar_data %>% mutate(P_kWh = P / 10^3) %>% select(datetime, date, hour, P_kWh) 
elcons <- elcons %>% select(datetime, date, hour, cons_kWh)
# elprice <- elprice %>% select(datetime, date, hour, price)
# feed_in_data <- feed_in_data %>% select(datetime, date, hour, feed_in)
# grid_cost_data <- grid_cost_data %>% select(datetime, date, hour, grid_cost)

out <- solar_data %>% 
  inner_join(elcons, by = c("datetime", "date", "hour")) %>%
  # inner_join(elprice, by = c("datetime", "date", "hour") ) %>%
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

#combine energy flows with prices (user can vary the prices)
myCombineFlowsPrices <- function(energy_flows
                          , elprice
                          , feed_in_data
                          , grid_cost_data
) {
  # solar_data <- solar_data$solar_data %>% mutate(P_kWh = P / 10^3) %>% select(datetime, date, hour, P_kWh) 
  # elcons <- elcons %>% select(datetime, date, hour, cons_kWh)
  elprice <- elprice %>% select(datetime, date, hour, price)
  feed_in_data <- feed_in_data %>% select(datetime, date, hour, feed_in)
  grid_cost_data <- grid_cost_data %>% select(datetime, date, hour, grid_cost)
  
  out <- energy_flows %>%
    inner_join(elprice, by = c("datetime", "date", "hour") ) %>%
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
  return(out)  
}#endfunction myCombineFlowsPrices


#### define functions for hourly flows ####
sourceCpp("yearly_calc.cpp") 
# the function calculates each year separately to improve performance
calculate_energy_flows <- function(dat, params = system_params) {
  tic()
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
  results_list <- vector("list", length(years))
  
  # for (y in seq_along(years)) {
  #   year_data <- dat %>% filter(year(datetime) == years[y])
  #   
  #   initial_soc <- params$battery_initial_soc * params$battery_capacity_kwh # Initial SOC for each year
  #   year_results <- calculate_year_energy(year_data, params, initial_soc)
  #   results_list[[y]] <- year_results
  # }
  
  results_list <- lapply(years, function(y) {
    year_data <- dat %>% filter(year(datetime) == y)
    initial_soc <- params$battery_initial_soc * params$battery_capacity_kwh # this is the initial SOC for each year
    calculate_year_energy(year_data, params, initial_soc) # call Rcpp function on single year data
  })
  
  final_results <- bind_rows(results_list)
  final_results <- final_results %>% 
    mutate(
      grid_balance = grid_import - grid_export
      , hourly_elcons_savings_kwh = cons_kWh - grid_import # positive when PV is beneficial, negative when it is worse than no installation
      , discount_factor = 1 / (1 + params$discount_rate/8760)^(row_number()-1) 
      , discounted_PV_available = PV_available * discount_factor
    )
  toc()
  return(final_results)
}#endfunction calculate_energy_flows





calculate_financials <- function(energy_flows
                                 , elprice
                                 , feed_in_data
                                 , grid_cost_data
                                 , params = system_params
                                 ) {
  
  #DEBUG change elprice
  # elprice <- elprice %>% mutate(price = 50)
  
  # enhance energy flows with prices and calculate financials
  energy_flows_w_prices <- myCombineFlowsPrices(energy_flows = energy_flows, elprice = elprice, feed_in_data = feed_in_data, grid_cost_data = grid_cost_data)
  energy_flows_w_prices %>% glimpse()
  
  # calculate present value
  energy_flows_w_prices <- energy_flows_w_prices %>%
    mutate(
     theo_cost_from_grid_without_PV = (price + grid_cost) * cons_kWh   #cost without PV installation, not yet with negative sign
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
      , net_cashflow = net_cashflow_explicit + hourly_elcons_savings_value  # hourly net CF without maintenance, costs now with negative sign, includes implicit CF, i.e. the value of hourly savings
      , net_cashflow_w_maintenace = net_cashflow - maintenance_costs  # hourly net CF with maintenance cost reflected
      , savings = net_cashflow_explicit - maintenance_costs - net_cashflow_without_PV # compare CF with PV (incl. maintenance) and without PV
      # discounted values here:
        #, DEBUG_discounted_net_cashflow_explicit = net_cashflow_explicit * discount_factor
      , discounted_net_cashflow_explicit = net_cashflow_explicit * discount_factor
      , discounted_net_cashflow = net_cashflow * discount_factor
      , discounted_net_cashflow_w_maintenace = net_cashflow_w_maintenace * discount_factor
      , discounted_savings = savings * discount_factor
       #, DEBUG_discounted_revenue_from_feed_in = revenue_from_feed_in * discount_factor
       #, DEBUG_discounted_cost_from_grid = cost_from_grid * discount_factor
      #
    )
  if (TRUE==FALSE) {
    #view the new cols in data
  energy_flows_w_prices %>% filter(grid_import >0) %>% head(10) %>% 
    select(maintenance_costs, feed_in, grid_cost, price, cons_kWh, grid_export, grid_import, net_cashflow_without_PV
           ,  revenue_from_feed_in, cost_from_grid, net_cashflow_explicit, net_cashflow
            , savings, hourly_elcons_savings_kwh, hourly_elcons_savings_value) %>% View()
  }#end disabledblock
  
  # create summary to be returned along with the hourly data
  summary_vals <- tibble(
           mindate = energy_flows_w_prices$date %>% min()
           , maxdate = energy_flows_w_prices$date %>% max()
           , sum_discounted_net_cashflow = sum(energy_flows_w_prices$discounted_net_cashflow)
           , NPV = sum(energy_flows_w_prices$discounted_net_cashflow) - params$installation_cost - sum(energy_flows_w_prices$discounted_maintenance_costs)
           , discounted_el_cost_without_PV = sum(energy_flows_w_prices$discounted_net_cashflow_without_PV)
           , total_energy_produced = sum(energy_flows_w_prices$PV_available)
           , grid_export = sum(energy_flows_w_prices$grid_export)
           , grid_import = sum(energy_flows_w_prices$grid_import)
           , household_el_consumption = sum(energy_flows_w_prices$cons_kWh)
           , elcons_saved = household_el_consumption - grid_import
           , feed_in_revenue_nominal = sum(energy_flows_w_prices$revenue_from_feed_in)
           # for LOCE calculation
           , discounted_total_el_produced = sum(energy_flows_w_prices$discounted_PV_available)
           , PV_total_cost = (params$installation_cost + sum(energy_flows_w_prices$discounted_maintenance_costs))
           , LCOE = PV_total_cost / discounted_total_el_produced
           , DEBUG_elcons_x_price = mean(energy_flows_w_prices$price) * elcons_saved
          )
summary_vals %>% glimpse()
  #prepare output
  output = list(
          summary_vals = summary_vals
          , df_hourly = energy_flows_w_prices
          )
  return(output)
}#endfunction calculate_financials





# breakeven feed-in tariff main
breakeven_feed_in <- function(new_fixed_val
                             , energy_flows
                             , params = system_params) {
  energy_flows <- energy_flows %>% mutate(feed_in = new_fixed_val
                                          # repeat mutate to reflect the new feed_in
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
                                          , net_cashflow = net_cashflow_explicit + hourly_elcons_savings_value  # hourly net CF without maintenance, costs now with negative sign, includes implicit CF, i.e. the value of hourly savings
                                          , net_cashflow_w_maintenace = net_cashflow - maintenance_costs  # hourly net CF with maintenance cost reflected
                                          , savings =  net_cashflow_explicit - maintenance_costs - net_cashflow_without_PV # compare CF with PV and without PV
                                          # discounted values here:
                                          #, DEBUG_discounted_net_cashflow_explicit = net_cashflow_explicit * discount_factor
                                          , discounted_net_cashflow_explicit = net_cashflow_explicit * discount_factor
                                          , discounted_net_cashflow = net_cashflow * discount_factor
                                          , discounted_net_cashflow_w_maintenace = net_cashflow_w_maintenace * discount_factor
                                          , discounted_savings = savings * discount_factor
                                          )
  
  new_NPV <- sum(energy_flows$discounted_net_cashflow) - params$installation_cost - sum(energy_flows$discounted_maintenance_costs)
  return(new_NPV)
}#endfunction breakeven_feed_in

# breakeven electricity price main
breakeven_price <- function(new_fixed_val
                            , hourly_energy_flows
                            , params = system_params) {
  
  hourly_energy_flows <- hourly_energy_flows %>% mutate(price = new_fixed_val
                                          # repeat mutate to reflect the new price
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
                                          , net_cashflow = net_cashflow_explicit + hourly_elcons_savings_value  # hourly net CF without maintenance, costs now with negative sign, includes implicit CF, i.e. the value of hourly savings
                                          , net_cashflow_w_maintenace = net_cashflow - maintenance_costs  # hourly net CF with maintenance cost reflected
                                          , savings =  net_cashflow_explicit - maintenance_costs - net_cashflow_without_PV # compare CF with PV and without PV
                                          # discounted values here:
                                          #, DEBUG_discounted_net_cashflow_explicit = net_cashflow_explicit * discount_factor
                                          , discounted_net_cashflow_explicit = net_cashflow_explicit * discount_factor
                                          , discounted_net_cashflow = net_cashflow * discount_factor
                                          , discounted_net_cashflow_w_maintenace = net_cashflow_w_maintenace * discount_factor
                                          , discounted_savings = savings * discount_factor
  )

  
  new_NPV <- sum(hourly_energy_flows$discounted_net_cashflow) - params$installation_cost - sum(hourly_energy_flows$discounted_maintenance_costs)
  return(new_NPV)
}

# auxiliary function to provide closure (to pass more than one parameter)
find_breakeven_feed_in <- function(hourly_energy_flows, params = system_params) {
  # closure to allow uniroot to read energy_flows and system_params
  breakeven_feed_in_fixed <- function(new_fixed_val) {
    breakeven_feed_in(new_fixed_val, hourly_energy_flows, params)
  }
  
  #use uniroot with closure
  result <- uniroot(breakeven_feed_in_fixed, lower = -1*10^3, upper = 10^4)
  return(result$root)  # Return the root directly
}#endfunction find_breakeven_feed_in

# auxiliary function to provide closure (to pass more than one parameter)
find_breakeven_price <- function(hourly_energy_flows, params = system_params) {
  # closure to allow uniroot to read energy_flows and system_params
  breakeven_price_fixed <- function(new_fixed_val) {
    breakeven_price(new_fixed_val, hourly_energy_flows, params)
  }

  #use uniroot with closure
  result <- uniroot(breakeven_price_fixed, lower = -1*10^3, upper = 10^4)
  return(result$root)  # Return the root directly
}#endfunction find_breakeven_price


# plot values in selected day (hourly)
plot_selected_day <- function(day, hourly_energy_flows = energy_flows_enh$df_hourly) {
  df <- energy_flows %>% filter(date == day)
  
  ggplot(df, aes(x = hour)) +
    geom_line(aes(y = PV_available, color = "PV Production")) +
    geom_line(aes(y = total_demand, color = "Total Demand")) +
    geom_line(aes(y = battery_soc, color = "Battery SoC")) +
    geom_line(aes(y = grid_import, color = "Grid Import"), linetype = "dashed") +
    geom_line(aes(y = grid_export, color = "Grid Export"), linetype = "dotted") +
    labs(title = paste("Energy Flow on", day), x = "Hour", y = "kWh") +
    scale_color_manual(values = c("PV Production" = "orange", "Total Demand" = "red",
                                  "Battery SoC" = "blue", "Grid Import" = "black", "Grid Export" = "green")) +
    theme_minimal()
}

# plot selected week (hourly)
plot_selected_week <- function(start_day) {
  df <- energy_flows %>% filter(date >= start_day & date < (start_day + days(7)))
  
  ggplot(df, aes(x = datetime)) +
    geom_line(aes(y = PV_available, color = "PV Production")) +
    geom_line(aes(y = total_demand, color = "Total Demand")) +
    geom_line(aes(y = battery_soc, color = "Battery SoC")) +
    labs(title = paste("Energy Flow from", start_day, "to", start_day + days(6)),
         x = "Date-Time", y = "kWh") +
    scale_color_manual(values = c("PV Production" = "orange", "Total Demand" = "red", "Battery SoC" = "blue")) +
    theme_minimal()
}


