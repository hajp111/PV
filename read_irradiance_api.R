library(httr)
library(readr)
library(tidyverse)
source("aux_functions.R")

#info: https://joint-research-centre.ec.europa.eu/photovoltaic-geographical-information-system-pvgis/getting-started-pvgis/api-non-interactive-service_en

solar_data_cache_file_path <- "_cache/solar_data_backup/unavailable_API_example_data.Rds"

get_solar_data <- function(lat =  49.278
                           , lon = 16.998
                           , start_date = '2022-01-01'
                           , system_lifetime = 20
                           #, startyear = 2022
                           #, endyear = 2023
                           , loss = 14  # in percent
                           , angle = 30 # Inclination angle (0° = horizontal)
                           , aspect = 0  # Azimuth angle (0° = South, 90° = West)
                           , peakpower = 1 # in kWp
                           , add_PV_noise = 0.2 # multiplier of the original used to add some noise to the P values (0.2 means some value from 0.8P to 1.2P)
                           , fixed_seed = FALSE 
                           ) {
  
#API endpoint
url <- "https://re.jrc.ec.europa.eu/api/v5_3/seriescalc"
url_TMY <-  "https://re.jrc.ec.europa.eu/api/tmy"
  
# INFO: Czech Republic approximately within:
# Latitude: 48.5° to 51.1°
# Longitude: 12° to 19°
if ( ((48.5 <= lat & lat <= 51.1) & (12 <= lon & lon <= 19))==FALSE) {warning("Position not in CZ, please select another location.")
  return(NULL)
  }#endif check for location within CZ
  
if (fixed_seed) {set.seed(123)}
  
start_date <- my_check_date(start_date)
startyear <- substring(start_date, 1,4) %>% as.integer()  #start_date %>% lubridate::floor_date(start_date %>% lubridate::ymd())
endyear <- startyear + system_lifetime
orig_startyear <- startyear
orig_endyear <- endyear

if (startyear < 2005) {
  print("Start year lower than 2005, data only from 2005")
  orig_startyear <- startyear
  startyear <- 2005
} 
if (endyear > 2023) {
    print("End year larger than 2023, data only until 2023")
  orig_endyear <- endyear
  endyear <- 2023
  }

orig_range <- seq(from = orig_startyear, to = orig_endyear, by = 1)
available_years <- seq(from = 2005, to = 2023, by = 1)
years_to_fill <- setdiff(orig_range, available_years)


#### get data via API  
params <- list(
  #raddatabase = "PVGIS-SARAH",  # Radiation database, API selects it automatically based on location (lat, lon)
  lat =  lat                 # Latitude  
  , lon = lon                # Longitude
  , startyear = startyear      # Start year - but I will get all years
  , endyear = endyear        # End year - but I will get all years
  , loss = loss              # Sum of system losses, in percent.
  , usehorizon = 1           # Include horizon data
  , pvcalculation = 1        # get PV calculation (otherwise get solar radiation only)
  , peakpower = peakpower    # Nominal power of the PV system, in kW.
  # , pvtechchoice = "crystSi"  # default choice, not needed
  , mountingplace = "building"  # can use "free" for free-standing, not needed
  , optimalangles = 0        #optimal angle and inclination
  , trackingtype = 0         # Type of suntracking used, 0=fixed, 1=single horizontal axis aligned north-south, 2=two-axis tracking, 3=vertical axis tracking, 4=single horizontal axis aligned east-west, 5=single inclined axis aligned north-south.
  , angle = angle            # Inclination angle (0° = horizontal)
  , aspect = aspect          # Azimuth angle (0° = South, 90° = West)
  , outputformat = "csv"     # Request CSV format
)
# if some years need to be filled (not in API raw data, get all years available from the API)
if (length(years_to_fill) > 0) {
  params$startyear <- 2005
  params$endyear <- 2023
}

tryCatch({
response <- httr::GET(url, query = params)
if (httr::http_status(response)$category == "Success") {
  print(paste0("API request: ", httr::http_status(response)$message ))
} else {
  stop(paste("API request failed:", httr::http_status(response)$message))
}

content_text <- httr::content(response, "text", encoding = "UTF-8")

# Split text into lines and find the header
lines <- unlist(strsplit(content_text, "\n"))
header_line <- grep("time,P,", lines)[1]  # where headers start
last_line <-  grep("P: PV system power", lines)[1]  -1


#write.table(x = lines[header_line:last_line], file = "tmp.csv"
#            , row.names = FALSE, col.names=FALSE, quote = FALSE)

#get old warning levels
oldw <- getOption("warn")
#supress warning for separate() in metadata
options(warn = -1)

solar_data <- lines[header_line:last_line] %>% paste(collapse = "\n") %>% read_csv() %>% 
  separate(time, into = c("date", "hour"), sep = ":") %>%
  mutate(date = lubridate::ymd(date)
         , hour = substr(hour,1,2) %>% as.integer()
         , year = year(date)
         , month = month(date)
         , day = day(date) 
         , weekday = lubridate::wday(date, week_start = getOption("lubridate.week.start", 1))
         , is_weekend = weekday %in% c(6,7)
         , datetime = make_datetime( year = year, month = month, day = day, hour = hour, tz = "Etc/GMT-1"))

metadata <- c(lines[1:(header_line-1)], lines[last_line:length(lines)]) %>% gsub(pattern = "[\t\n\r]", 
                                                                                 replacement=" ") %>%
  as_tibble() %>% filter(!(value %in% c("", " "))) %>% separate(col = value, into = c("key", "value"), sep = ":") %>%
  mutate(value = trimws(value, which = "both"))

# enable warnings again 
options(warn = oldw)

#available years of data from API
minyear <- solar_data$year %>% min()
maxyear <- solar_data$year %>% max()

if (length(years_to_fill) > 0) {
  print(paste0("Years to fill: ", paste0(years_to_fill, collapse = ", ")))
  
  x <- lapply(years_to_fill, function(year) {
    # pick a year at random 
    random_year <-  random_year <- sample(x = c(minyear, maxyear), size = 1, replace = TRUE) 
    random_year_data <- solar_data %>% filter(year == random_year)
    
    # Check whether the year to fill is a leap year 
    if (lubridate::leap_year(year) && !any(random_year_data$month == 2 & random_year_data$day == 29)) {
      # If the target year is a leap year but the random year doesn't have Feb 29 then duplicate Feb 28 
      feb_28_data <- random_year_data %>% filter(month == 2, day == 28)
      
      if(nrow(feb_28_data) > 0){
        feb_29_data <- feb_28_data %>% mutate(day = 29)
        random_year_data <- bind_rows(random_year_data, feb_29_data) %>% arrange(year, month, day, hour)
      } else {
        #if feb 28 missing in source data (shouldn't happen)
        stop(paste("Random year", random_year, "is missing Feb 28", year_to_fill))
      }
    }
    
    
    random_year_data <- random_year_data %>% mutate( noise_range =  add_PV_noise * P
                                        , P = pmax(0, P + runif(n(), -noise_range, noise_range))
                                        ) %>%
                                select(-noise_range)
                                         
    
    return(random_year_data)
    
  })#end lapply
  names(x) <- years_to_fill
  y <- bind_rows(x, .id = "newyear") %>% mutate(yearorig = year
                                                , year = newyear %>% as.integer()
                                                , data_from_year = yearorig # identifies the year of observation of such data
                                                , date = lubridate::ymd(paste0(year,"-",month,"-",str_pad(day,2, pad = "0")))
                                                , datetime = make_datetime( year = year, month = month, day = day, hour = hour, tz = "Etc/GMT-1")
                                                ) %>% 
    select(-yearorig, -newyear)
  
  solar_data <- bind_rows(solar_data, y)
  }#endif

solar_data <- solar_data %>% filter(year >= orig_startyear & year <= orig_endyear)

#write_csv(metadata, file = "tmp_metadata.csv")
print(metadata)
return(list(solar_data = solar_data, metadata = metadata))


# if errors when reading from API, try to use backup data instead
}, error = function(e) {
  message("API request encountered problems: ", e$message)
  message("Attempting to load data from locally stored cache")
  
  solar_data <- readRDS(solar_data_cache_file_path)
  solar_data$solar_data <- solar_data$solar_data %>% mutate(P = P * peakpower) %>%
                           filter(year >= orig_startyear & year <= orig_endyear)
  
  solar_data$metadata$value[solar_data$metadata$key == "Nominal power of the PV system (c-Si) (kWp)"] <- peakpower %>% as.character() 
  solar_data$metadata$value[solar_data$metadata$key == "Latitude (decimal degrees)"] <- 49.278 %>% as.character() 
  solar_data$metadata$value[solar_data$metadata$key == "Longitude (decimal degrees)"] <- 16.998 %>% as.character() 

  return(solar_data)
  
})# tryCatch end
}#ENDFUNCTION get_solar_data

if (file.exists("_cache/solar_data_backup/unavailable_API_example_data.Rds") == FALSE) {
  backup_solar_data <- get_solar_data(lat =  49.278
                                   , lon = 16.998
                                   , start_date = '2005-01-01'
                                   , system_lifetime = 40
                                   #, startyear = 2022
                                   #, endyear = 2023
                                   , loss = 14  # in percent
                                   , angle = 30 # Inclination angle (0° = horizontal)
                                   , aspect = 0  # Azimuth angle (0° = South, 90° = West)
                                   , peakpower = 1 # in kWp
                                   , add_PV_noise = 0.2 # multiplier of the original used to add some noise to the P values (0.2 means some value from 0.8P to 1.2P)
                                   , fixed_seed = FALSE 
                                        ) 
  dir.create(dirname(solar_data_cache_file_path), showWarnings = FALSE)
  saveRDS(backup_solar_data, file = solar_data_cache_file_path)
  rm(backup_solar_data)
  ## to assign from readRDS
  # solar_data <- readRDS(solar_data_cache_file_path)
  
}#
