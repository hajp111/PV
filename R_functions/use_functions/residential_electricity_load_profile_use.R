source("R_functions/residential_electricity_load_profile.R")
print("WTF: this is residential_electricity_load_profile_use.R file")

# inspect the data structure
a <- standardlastprofile::slp
a %>% filter(profile_id=="H0") %>% group_by(profile_id, period, day) %>% tally()
# 24*4 observations for each period and type of day 
# meaning:
# summer: May 15 to September 14
# winter: November 1 to March 20
# transition: March 21 to May 14, and September 15 to October 31 

# description of the load profile:
standardlastprofile::slp_info("H0")
# the load profile is defined so that annual consumption is 1000 kWh -> multiply load profile by expected annual consumption


elcons <- get_load_data(start_date = "2022-01-01"
              , system_lifetime = 2
              , annual_consumption =3
              , fixed_seed = TRUE 
              , add_HH_cons_noise = 0 
)
#save(elcons, file = "_cache/elcons_data.Rdata")
saveRDS(elcons, file = "_cache/elcons_data.Rds")

###-- generate charts for the methodology
H0_2024_hourly <- get_load_data(start_date = "2024-01-01"
                                , system_lifetime = 2
                                , annual_consumption =3
                                , fixed_seed = TRUE 
                                , add_HH_cons_noise = 0 
                                )

# calculate hour mean and q10, q90 -> every month, there are 24 hourly averages
H0_2024_agg_by_year_month_hour <- H0_2024_hourly %>%
  group_by(profile_id, year, quarter, month, hour) %>%
  summarize(
    mean = mean(kWh, na.rm = TRUE)
    , median = median(kWh, na.rm = TRUE)
    , q10 = quantile(kWh, 0.1)
    , q90 = quantile(kWh, 0.9)
    , n = n()
    , q10_pct = q10/mean
    , q90_pct = q90/mean
  )

H0_2024_agg_by_year_month_day <- H0_2024_hourly %>%
  group_by(profile_id, date, year, quarter, month, day, is_weekend) %>%
  summarize(
    kWh = sum(kWh, na.rm = TRUE)
    , n = n()
  )
H0_2024_agg_by_year_month_day %>% group_by(profile_id, year) %>% summarize(kWh = sum(kWh))

# lineplot
H0_2024_agg_by_year_month_hour %>% filter(month == 1) %>% ggplot(., aes(x = hour, y = mean)) +
  geom_line() +  # Line for median
  geom_line(aes(y = q10), color = "blue", linetype = "dashed", alpha = .3) +
  geom_line(aes(y = q90), color = "red", linetype = "dashed", alpha = .3) +
  theme_light()  +
  facet_grid(. ~ as.factor(month) ) +
  ylim(0, NA)



H0_2024_agg_by_year_month_day %>% ggplot(., aes(x = date, y = kWh)) +
  geom_line()  +
  theme_light()  +
  ylim(0, NA)

H0_2024_agg_by_year_month_day %>% filter(month == 7) %>%  ggplot(., aes(x = date, y = kWh)) +
  geom_line()  +
  theme_light()  +
  ylim(0, NA)

H0_2024_agg_by_year_month_day %>% filter(month == 2) %>% ggplot(., aes(x = date, y = kWh)) +
  geom_line()  +
  theme_light()  +
  ylim(0, NA)
