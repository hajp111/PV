#### server.R
source("global.R")

server <- function(input, output, session) {
    # Map rendering and click handling
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = 16.998, lat = 49.278, zoom = 8) #%>% addMarkers(lng = 16.998, lat = 49.278)
    })
    
    observeEvent(input$map_click, {
        click <- input$map_click
        if (!is.null(click)) {
        updateNumericInput(session, "lat", value = click$lat)
        updateNumericInput(session, "lon", value = click$lng)
       
        # Clear existing markers and add a new marker
        leafletProxy("map") %>%
            clearMarkers() %>%
            addMarkers(lng = click$lng, lat = click$lat)
     }#endif 
    })
    
    
 
    
    # Reactive expression for calculations
    results <- eventReactive(input$calculate, {
        # Create system_params list from inputs
        system_params <- list(
            use_cache_data = input$use_cache_data,
            fixed_seed = input$fixed_seed,
            battery_capacity_kwh = input$battery_capacity_kwh,
            battery_charge_efficiency = input$battery_charge_efficiency,
            battery_discharge_efficiency = input$battery_discharge_efficiency,
            battery_initial_soc = input$battery_initial_soc / 100,
            battery_min_soc = input$battery_min_soc / 100,
            battery_max_soc = input$battery_max_soc / 100,
            battery_degradation = input$battery_degradation,
            PV_degradation = input$PV_degradation,
            PV_system_own_consumption = input$PV_system_own_consumption,
            installation_cost = input$installation_cost,
            discount_rate = input$discount_rate,
            annual_maintenance_cost = input$annual_maintenance_cost,
            start_date = input$start_date,
            system_lifetime = input$system_lifetime,
            lat = input$lat,
            lon = input$lon,
            PV_system_loss = input$PV_system_loss,
            PV_angle = input$PV_angle,
            PV_aspect = input$PV_aspect,
            PV_peakpower = input$PV_peakpower,
            PV_add_PV_noise = input$PV_add_PV_noise,
            HH_annual_consumption = input$HH_annual_consumption,
            HH_add_cons_multiplier = input$HH_add_cons_multiplier,
            elprice_method = input$elprice_method,
            elprice_annual_growth = input$elprice_annual_growth,
            elprice_add_intraday_variability = input$elprice_add_intraday_variability,
            elprice_add_intraweek_variability = input$elprice_add_intraweek_variability,
            feedin_method = input$feedin_method,
            feedin_lastval = input$feedin_lastval,
            feedin_annual_growth = input$feedin_annual_growth,
            gridcost_method = input$gridcost_method,
            gridcost_annual_growth = input$gridcost_annual_growth
        )
        
        # Your existing data processing code
        if (system_params$use_cache_data) {
            # Load cached data
            #get solar data (API call) (replace with function call) - returns list with data and metadata, data in solar$solar_data
            # datetime, date, year, month, day, weekday, hour, P  (P is in Watts, change to kWh later)
            solar <- readRDS("_cache/solar.Rds")  # this is for 4.5 kWp capacity installed   #2022-2024
            solar_data <- solar$solar_data
            #solar$plot
            #solar_data %>% tail(100) %>% glimpse()
            
            #get household el. consumption data (replace with function call)
            # datetime, date, year, month, day, weekday, hour, cons_kWh
            elcons <- readRDS("_cache/elcons_data.Rds")  # this is for a residential consumption of 3 MWh annually #2022-2024
            #elcons$plot
            elcons_data <- elcons$elcons
            #elcons_data %>% glimpse()
            
            #get grid distribution costs 
            # datetime, date, year, month, day, weekday, hour, grid_cost
            grid_cost <- readRDS("_cache/grid_cost.Rds")  # this is STATIC feed-in price for each hour, range 2022-2024
            #grid_cost$plot
            grid_cost_data <- grid_cost$grid_cost
            #grid_cost_data %>% glimpse()
            
            #get feedin prices (replace with function call)
            # datetime, date, year, month, day, weekday, hour, feedin
            feed_in <-  readRDS("_cache/feed_in_price.Rds")   # this is STATIC feed-in price for each hour, range 2022-2024
            #feed_in$plot
            feed_in_data <- feed_in$feed_in
            #feed_in_data %>% glimpse()
            
            #get electricty prices (replace with function call)
            # datetime, date, year, month, day, weekday, hour, price
            elprice <- readRDS("_cache/elprice.Rds")  # this is STATIC price for each hour, range 2022-2024
            #elprice$plot
            elprice_data <- elprice_data$price_data
            #elprice_data %>% glimpse()
            
        } else {
            #get solar data (API call) - returns list with data and metadata, hourly data in solar$solar_data
            # datetime, date, year, month, day, weekday, hour, P  (P is in Watts, change to kWh later)
            # solar data is kept as list, as function myCombineConsAndSolar() handles the conversion to kWh and selection of important cols
            print("Loading data solar")
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
            #print(paste0("solar_data nrow: ", nrow(solar_data), " minyear: ", min(solar_data$year), " maxyear: ", max(solar_data$year) ) )
            
            print("Loading data elcons")
            #get household el. consumption data (replace with function call)
            # datetime, date, year, month, day, weekday, hour, cons_kWh
            elcons <- get_load_data(start_date = system_params$start_date
                                    , system_lifetime = system_params$system_lifetime
                                    , annual_consumption = system_params$HH_annual_consumption
                                    , fixed_seed = system_params$fixed_seed 
                                    , add_HH_cons_noise = system_params$HH_add_cons_multiplier )
            #elcons %>% glimpse()
            elcons_data <- elcons$elcons
            #print(paste0("elcons nrow: ", nrow(elcons_data)))
            
            print("Loading data grid_cost")
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
            #print(paste0("grid_cost_data nrow: ", nrow(grid_cost_data)))
            
            print("Loading data feed_in")
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
            #print(paste0("feed_in_data nrow: ", nrow(feed_in_data)))
            
            print("Loading data elprice")
            #get electricty prices (replace with function call)
            # datetime, date, year, month, day, weekday, hour, price
            elprice <- my_elprice(df = my_data_read_elprice_observed_data()
                                  , startdate = system_params$start_date
                                  , years = system_params$system_lifetime
                                  , annual_growth = system_params$elprice_annual_growth
                                  , method = system_params$elprice_method 
                                  , add_intraday_variability = system_params$elprice_add_intraday_variability #FALSE
                                  , add_intraweek_variability = system_params$elprice_add_intraweek_variability #FALSE
            ) 
            elprice_data <- elprice$price_data
            #elprice_data %>% glimpse()
            #print(paste0("elprice nrow: ", nrow(elprice_data)))
        }#end load data
        
       # Perform calculations
        df1 <- myCombineConsAndSolar(solar_data, elcons_data)
        energy_flows <- CalculateEnergyFlows(df1, system_params)
        energy_flows_enh <- CalculateFinancials(
            energy_flows,
            elprice_data,
            feed_in_data,
            grid_cost_data,
            params = system_params
        )
        
        out1 <- list(
            summary = energy_flows_enh$summary_vals,
            hourly = energy_flows_enh$df_hourly
        )
        return(out1)
    })#end results <- eventReactive(input$calculate, {})
    
    # Render summary table
    output$summary_table <- renderTable({
        req(results())
        results()$summary
    })
    
    # Render interactive plot
    output$energy_plot <- renderPlotly({
        req(results())
        df <- results()$hourly %>%
            filter(date == as.Date(input$plot_date))
        
        p <- plotDay(input$plot_date, df)  # Modified your plot function to return ggplot
        ggplotly(p) %>% 
            layout(
                hovermode = "x unified",
                yaxis2 = list(
                    overlaying = "y",
                    side = "right",
                    title = "Battery SoC (kWh)"
                )
            )
    })
}

