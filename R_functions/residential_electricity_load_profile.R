#### electricity load profile - generation
library(standardlastprofile)
library(tidyverse)
source("R_functions/aux_functions.R")

print("this is residential_electricity_load_profile.R file")

get_load_data <- function(start_date = "2022-01-01"
                          , system_lifetime = 20
                          , annual_consumption = 3 # in MWh
                          , fixed_seed = FALSE 
                          , add_HH_cons_noise = 0 # multiplier of the original used to add some noise to the P values (0.2 means some value from 0.8P to 1.2P)
                          ) {

if (fixed_seed) {set.seed(123)}

start_date <- my_check_date(start_date)
startyear <- substring(start_date, 1,4) %>% as.integer()
endyear <- startyear + system_lifetime
end_date <- paste0(endyear, "-12-31")
  
#generate H0 profile
H0 <- standardlastprofile::slp_generate(
  profile_id = "H0",
  start_date = start_date,
  end_date = end_date
) %>% 
  # adjust by the user-inputed value of annual consumption of the household
  mutate(watts = watts * annual_consumption)


#aggregate from 15min window to hourly data  - use this format in next steps
H0_hourly <- H0 %>% mutate(
  date = as.Date(start_time)
  , day_of_year = (date - lubridate::floor_date(date, "year")) %>% as.numeric() +1
  , year = lubridate::year(start_time)
  , quarter = lubridate::quarter(start_time)
  , month = lubridate::month(start_time)
  , day = lubridate::day(start_time)
  , hour = lubridate::hour(start_time)
  , weekday = lubridate::wday(start_time, week_start = getOption("lubridate.week.start", 1))
  , is_weekend = weekday %in% c(6,7)
  , period = case_when( (month == 5 & day >= 15) | (month > 5 & month < 9) | (month == 9 & day <= 14) ~ "summer",
                        (month == 11) | (month == 12) | (month >= 1 & month < 3) | (month == 3 & day <= 20) ~ "winter",
                        TRUE ~ "transition")
) %>% group_by(profile_id, date, day_of_year, year, quarter, period, month, day, hour, weekday, is_weekend) %>% 
  summarize(cons_kWh = sum(watts/4) / 1000) %>% 
  ungroup() %>%
  mutate(datetime = make_datetime( year = year, month = month, day = day, hour = hour, tz = "Etc/GMT-1")
         , noise_range =  add_HH_cons_noise * cons_kWh
         , cons_kWh = pmax(0, cons_kWh + runif(n(), -noise_range, noise_range))
  ) %>%
  select(-noise_range)

H0_daily <- H0_hourly %>% group_by(profile_id, date, day_of_year, year, quarter, period, month, day, weekday, is_weekend) %>% 
  summarize(cons_kWh = sum(cons_kWh))

plt <- ggplot() + 
  geom_line(data = H0_daily, aes(x=date, y = cons_kWh), alpha = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_minimal()

output <- list(plot = plt, elcons = H0_hourly, elcons_daily = H0_daily)

return(output)
}#endfunction get_load_data


