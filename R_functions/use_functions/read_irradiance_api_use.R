source("R_functions/read_irradiance_api.R")
print("this is read_irradiance_api_use.R file")

# API only works for range 2005 - 2023, ranges beyond API are generated from existing years
solar <- getSolarData(lat =  49.277780 
                             , lon = 17.013359
                             , start_date = '2002-01-01'
                             , system_lifetime = 23
                             , loss = 14  # in percent
                             , angle = 40 # Inclination angle (0° = horizontal)
                             , aspect = 50 # Azimuth angle (0° = South, 90° = West)
                             , peakpower = 4.5 #in kWp
                             , add_PV_noise = 0.0 # multiplier of the original used to add some noise to the P values (0.2 means some value from 0.8P to 1.2P)
                             , fixed_seed = FALSE 
                             ) 


glimpse(solar$solar_data)
solar$solar_data %>% group_by(year) %>% tally()
#save(solar_data, file = "_cache/solar_data.Rdata")
saveRDS(solar, file = "_cache/solar.Rds")


#solar1 - only observed
soldata1 <- solar$solar_data %>% filter(year >= 2005 & year <= 2023) %>%
  mutate(day_of_year =  as.numeric(format(date, "%j"))
         , day_of_year_fct = day_of_year %>% as_factor())

soldata1_daily <- soldata1 %>% group_by(year, month, day, date, day_of_year) %>% summarize(P_kWh = sum(P_kWh, na.rm = TRUE)) %>%
  ungroup() 
  
soldata1_by_dayofyear <- soldata1_daily %>% 
  filter(day_of_year <= 365) %>%
  group_by(day_of_year) %>% summarize(P_mean = mean(P_kWh, na.rm = TRUE)
                                      , P_q10 = quantile(P_kWh, 0.1)
                                      , P_q50 = quantile(P_kWh, 0.5)
                                      , P_q90 = quantile(P_kWh, 0.9))


soldata1_by_dayofyear %>% ggplot() +
 geom_line(aes(x = day_of_year, y = P_mean), color = "orange", alpha = .9)+
  geom_line(aes(x = day_of_year, y = P_q10), alpha = .4, linetype  = "solid", color = "brown")+
#  geom_line(aes(x = day_of_year, y = P_q50), alpha = .7, , linetype  = "dashed", color = "grey")+
  geom_line(aes(x = day_of_year, y = P_q90), alpha = .4, linetype  = "solid", color = "brown")+
  geom_smooth(aes(x = day_of_year, y = P_q10), color = "brown", alpha = .5, se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = day_of_year, y = P_mean), color = "orange", alpha = .9, se = FALSE)+
  geom_smooth(aes(x = day_of_year, y = P_q90), color = "brown", alpha = .5, se = FALSE, linetype = "dashed")+
 labs(title = "Expected mean (orange) solar production of 4.5 kWp installation\nquantile 10 and 90 (brown)" # \nat coordinates [49.278, 16.998]\nmean, q10 and q90 (2005-2023)"
      , x = "Day of year"
      , y = "kWh") +
  theme_minimal()
  
my_ggsave("../grafy_atp/solar/01_solar_expected_by_dayofyear.png")


#read comparison data
fve <- readxl::read_excel("../batterybox_data/FVE_stats.xlsx") %>% 
  mutate(P_kWh = `dnes jste vyrobili Wh`/ 10^3
         , cons_kWh = `spotreba celkem` / 10^3
         , grid_import = `odber Wh (ze site)` / 10^3
         , grid_export = `dodavka (sit)` / 10^3
         , date = datum %>% as.Date()
         , day_of_year = as.numeric(format(datum, "%j"))
         ) %>% 
  filter(rok >= 2024) 

fve %>% ggplot() +
  geom_line(aes(x = date, y = P_kWh), alpha = .7)+
  theme_minimal()

fve_by_dayofyear <- fve %>%
  group_by(day_of_year) %>% summarize(n = n()
                                      , P_kWh = mean(P_kWh, na.rm = TRUE)
                                      , cons_kWh = mean(cons_kWh, na.rm = TRUE)
                                      , grid_import = mean(grid_import, na.rm = TRUE)
                                      , grid_export = mean(grid_export, na.rm = TRUE)) %>%
 filter(day_of_year <= 365) %>%
  left_join(soldata1_by_dayofyear, by = c("day_of_year" = "day_of_year"))


fve_with_expected <- fve %>% left_join(soldata1_by_dayofyear, by = c("day_of_year" = "day_of_year"))


fve_with_expected %>% ggplot() +
  geom_line(aes(x = day_of_year, y = P_kWh), alpha = .7, color = "red")+
  geom_smooth(aes(x = day_of_year, y = P_kWh), color = "red", alpha = .1, se = FALSE)+
#  geom_line(aes(x = day_of_year, y = P_mean), color = "black", alpha = .9)+
#  geom_line(aes(x = day_of_year, y = P_q10), alpha = .7, linetype  = "dotted")+
  #  geom_line(aes(x = day_of_year, y = P_q50), alpha = .7, , linetype  = "dashed")+
#  geom_line(aes(x = day_of_year, y = P_q90), alpha = .7, linetype  = "dotted")+
  geom_smooth(aes(x = day_of_year, y = P_q10), color = "brown", alpha = .5, se = FALSE, linetype = "dashed")+
  geom_smooth(aes(x = day_of_year, y = P_mean), color = "orange", alpha = .9, se = FALSE)+
  geom_smooth(aes(x = day_of_year, y = P_q90), color = "brown", alpha = .5, se = FALSE, linetype = "dashed")+
  labs(title = "Actual (red) vs expected (orange) solar production\n4.5 kWp installation"
       , x = "Day of year"
       , y = "kWh") +
  facet_grid(rok ~.) +
theme_minimal()

my_ggsave("../grafy_atp/solar/02_actual_vs_expected_solar_by_dayofyear.png")

#RMSE
rmse <- fve_with_expected %>% 
  filter(P_kWh %>% is.na() == FALSE & P_kWh>0) %>%
  summarise(
    stdev_data = sd(P_kWh, na.rm = TRUE)
    , mean_data = mean(P_kWh, na.rm = TRUE)
    , RMSE = sqrt(mean((P_kWh - P_mean)^2, na.rm = TRUE))
    , MAE = mean(abs(P_kWh - P_mean), na.rm = TRUE)
    , MAPE = mean(abs((P_kWh - P_mean) / P_kWh), na.rm = TRUE) * 100
    , RMSE_baseline <- sqrt(mean((P_kWh - mean_data)^2, na.rm = TRUE))
  )
print(rmse)