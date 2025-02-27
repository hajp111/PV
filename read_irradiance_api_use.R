source("read_irradiance_api.R")


solar_data <- get_solar_data(lat =  49.278
                             , lon = 16.998
                             , start_date = '2022-01-01'
                             , system_lifetime = 2
                             , loss = 14  # in percent
                             , angle = 30 # Inclination angle (0° = horizontal)
                             , aspect = 0 # Azimuth angle (0° = South, 90° = West)
                             , peakpower = 4.5 #in kWp
                             , add_PV_noise = 0.2 # multiplier of the original used to add some noise to the P values (0.2 means some value from 0.8P to 1.2P)
                             , fixed_seed = FALSE 
                             ) 
# only works for range 2005 - 2023 
# to project into the future -> pick a random year and repeat the values 

glimpse(solar_data$solar_data)
solar_data$solar_data %>% group_by(year) %>% tally()

save(solar_data, file = "_cache/solar_data.Rdata")