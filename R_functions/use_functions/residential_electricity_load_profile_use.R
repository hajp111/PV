source("R_functions/residential_electricity_load_profile.R")
print("this is residential_electricity_load_profile_use.R file")

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


elcons <- get_load_data(start_date = "2024-01-01"
              , system_lifetime = 20
              , annual_consumption = 2.8
              , fixed_seed = TRUE 
              , add_HH_cons_noise = 0 
)
elcons$elcons %>% filter(year == max(year)) %>%
  group_by(date, year) %>% 
  summarize(cons_kWh = sum(cons_kWh)) %>%
  ggplot() + 
  geom_line(aes(x=date, y = cons_kWh), alpha = 0.5) +
  #geom_smooth(aes(x=date, y = cons_kWh), se = FALSE) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(title = "H0 profile, 3 MWh annual consumption"
      , x = "Date"
      , y = "kWh") +
  theme_minimal()

my_ggsave("../grafy_atp/hh_cons/01_H0profile.png")


#save(elcons, file = "_cache/elcons_data.Rdata")
saveRDS(elcons, file = "_cache/elcons_data.Rds")

###-- generate charts for the methodology
H0_2024_hourly <- get_load_data(start_date = "2024-01-01"
                                , system_lifetime = 5
                                , annual_consumption = 3.0
                                , fixed_seed = TRUE 
                                , add_HH_cons_noise = 0.0
                                )

H0_2024_hourly_w_noise <- get_load_data(start_date = "2024-01-01"
                                , system_lifetime = 5
                                , annual_consumption = 3.0
                                , fixed_seed = TRUE 
                                , add_HH_cons_noise = 0.8
)

# calculate hour mean and q10, q90 -> every month, there are 24 hourly averages
H0_2024_agg_by_year_month_hour <- H0_2024_hourly$elcons %>%
  group_by(profile_id, year, quarter, month, hour) %>%
  summarize(
    mean = mean(cons_kWh, na.rm = TRUE)
    , median = median(cons_kWh, na.rm = TRUE)
    , q10 = quantile(cons_kWh, 0.1)
    , q90 = quantile(cons_kWh, 0.9)
    , n = n()
    , q10_pct = q10/mean
    , q90_pct = q90/mean
  )

H0_2024_agg_by_year_month_day <- H0_2024_hourly$elcons  %>%
  group_by(profile_id, date, year, quarter, month, day, is_weekend) %>%
  summarize(
    cons_kWh = sum(cons_kWh, na.rm = TRUE)
    , n = n()
  )
# how much per year
H0_2024_agg_by_year_month_day %>% group_by(profile_id, year) %>% summarize(cons_kWh = sum(cons_kWh))



H0noise_2024_agg_by_year_month_day <- H0_2024_hourly_w_noise$elcons  %>%
  group_by(profile_id, date, year, quarter, month, day, is_weekend) %>%
  summarize(
    cons_kWh = sum(cons_kWh, na.rm = TRUE)
    , n = n()
  )
# how much per year
H0noise_2024_agg_by_year_month_day %>% group_by(profile_id, year) %>% summarize(cons_kWh = sum(cons_kWh))





# lineplot
H0_2024_agg_by_year_month_hour %>% filter(month == 1 & year ==2024) %>% ggplot(., aes(x = hour, y = mean)) +
  geom_line() +  # Line for median
  geom_line(aes(y = q10), color = "brown", linetype = "dashed", alpha = .7) +
  geom_line(aes(y = q90), color = "brown", linetype = "dashed", alpha = .7) +
  theme_light()  +
  facet_grid(. ~ as.factor(month) ) +
  labs(title = "H0 profile expected load\n3 MWh annual consumption"
       , x = "Hour"
       , y = "kWh") +
  ylim(0, NA)

my_ggsave("../grafy_atp/hh_cons/02_hourly_pattern_H0_january.png")

# lineplot
H0_2024_agg_by_year_month_hour %>% filter(month == 7 & year ==2024) %>% ggplot(., aes(x = hour, y = mean)) +
  geom_line() +  # Line for median
  geom_line(aes(y = q10), color = "brown", linetype = "dashed", alpha = .7) +
  geom_line(aes(y = q90), color = "brown", linetype = "dashed", alpha = .7) +
  theme_light()  +
  facet_grid(. ~ as.factor(month) ) +
  labs(title = "H0 profile expected load\n3 MWh annual consumption"
       , x = "Hour"
       , y = "kWh") +
  ylim(0, NA)

my_ggsave("../grafy_atp/hh_cons/02_hourly_pattern_H0_july.png")


H0_2024_agg_by_year_month_day %>% ggplot(., aes(x = date, y = cons_kWh)) +
  geom_line()  +
  theme_light()  +
  ylim(0, NA)

H0_2024_agg_by_year_month_day %>% filter(month == 7 & year == 2024) %>%  ggplot(., aes(x = date, y = cons_kWh)) +
  geom_line()  +
  theme_light()  +
  ylim(0, NA)

H0_2024_agg_by_year_month_day %>% filter(month == 2 & year == 2024) %>% ggplot(., aes(x = date, y = cons_kWh)) +
  geom_line()  +
  theme_light()  +
  ylim(0, NA)

#### observed

#read comparison data
fve <- readxl::read_excel("../batterybox_data/FVE_stats.xlsx") %>% 
  mutate(P_kWh = `dnes jste vyrobili Wh`/ 10^3
         , cons_kWh = `spotreba celkem` / 10^3
         , grid_import = `odber Wh (ze site)` / 10^3
         , grid_export = `dodavka (sit)` / 10^3
         , date = datum %>% as.Date()
         , day_of_year = as.numeric(format(datum, "%j"))
  ) %>% 
  filter(rok >= 2024 & date <= '2025-03-09') 

fve %>% group_by(day_of_year) %>% summarise(cons_kWh = sum(cons_kWh, na.rm=TRUE)) %>% ungroup() %>% summarise(cons_kWh = sum(cons_kWh, na.rm=TRUE))

fve_with_expected <- fve %>% left_join(H0_2024_agg_by_year_month_day %>% rename(cons_kWh_expected = cons_kWh), by = c("date" = "date"))

fve_with_expected %>% ggplot() +
  geom_line(aes(x = day_of_year, y = cons_kWh), alpha = .7, color = "red")+
  geom_smooth(aes(x = day_of_year, y = cons_kWh), color = "red", alpha = .1, se = FALSE)+
    geom_line(aes(x = day_of_year, y = cons_kWh_expected), color = "blue", alpha = .9)+
  #  geom_line(aes(x = day_of_year, y = P_q10), alpha = .7, linetype  = "dotted")+
  #  geom_line(aes(x = day_of_year, y = P_q50), alpha = .7, , linetype  = "dashed")+
  #  geom_line(aes(x = day_of_year, y = P_q90), alpha = .7, linetype  = "dotted")+
  # geom_smooth(aes(x = day_of_year, y = P_q10), color = "brown", alpha = .5, se = FALSE, linetype = "dashed")+
  # geom_smooth(aes(x = day_of_year, y = P_mean), color = "orange", alpha = .9, se = FALSE)+
  # geom_smooth(aes(x = day_of_year, y = P_q90), color = "brown", alpha = .5, se = FALSE, linetype = "dashed")+
  labs(title = "Actual (red) vs expected (blue) HH consumption"
       , x = "Day of year"
       , y = "kWh") +
  facet_grid(rok ~.) +
  ylim(0, NA) +
  theme_minimal()

my_ggsave("../grafy_atp/hh_cons/03_actual_vs_expected_elcons_by_dayofyear.png")

fve_with_expected_noise <- fve %>% left_join(H0noise_2024_agg_by_year_month_day %>% rename(cons_kWh_expected = cons_kWh), by = c("date" = "date"))

fve_with_expected_noise %>% ggplot() +
  geom_line(aes(x = day_of_year, y = cons_kWh), alpha = .7, color = "red")+
  geom_smooth(aes(x = day_of_year, y = cons_kWh), color = "red", alpha = .1, se = FALSE)+
  geom_line(aes(x = day_of_year, y = cons_kWh_expected), color = "blue", alpha = .9)+
  #  geom_line(aes(x = day_of_year, y = P_q10), alpha = .7, linetype  = "dotted")+
  #  geom_line(aes(x = day_of_year, y = P_q50), alpha = .7, , linetype  = "dashed")+
  #  geom_line(aes(x = day_of_year, y = P_q90), alpha = .7, linetype  = "dotted")+
  # geom_smooth(aes(x = day_of_year, y = P_q10), color = "brown", alpha = .5, se = FALSE, linetype = "dashed")+
  # geom_smooth(aes(x = day_of_year, y = P_mean), color = "orange", alpha = .9, se = FALSE)+
  # geom_smooth(aes(x = day_of_year, y = P_q90), color = "brown", alpha = .5, se = FALSE, linetype = "dashed")+
  labs(title = "Actual (red) vs expected (blue) HH consumption"
       , x = "Day of year"
       , y = "kWh") +
  facet_grid(rok ~.) +
  ylim(0, NA) +
  theme_minimal() 

my_ggsave("../grafy_atp/hh_cons/03b_actual_vs_expected_w_noise_elcons_by_dayofyear.png")
