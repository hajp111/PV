####  server.R
library(shinyjs)

server <- function(input, output, session) {
    
    print("hiding calculate_financials button")
    shinyjs::hide("calculate_financials")
    # hide the results tabs 
    shinyjs::hide("chartsTab")
    shinyjs::hide("resultsTab")

    # Show modal on app start
    showModal(modalDialog(
        title = "Welcome to the PV evaluator",
        "Please follow these steps:",
        tags$ol(
            tags$li("Browse the tabs and set the values of parameters."),
            tags$li("Click the 'Load Energy Data' button and wait until the data is loaded."),
            tags$li("Click the 'Calculate Financials' button and wait until the calculations are done"),
            tags$li("See the results on the Results tab.")
        ),
        footer = modalButton("Got it!")
    ))#end modal
    
    # Reactive values to store intermediate results
    system_params <- reactiveVal(NULL)
    energy_flows <- reactiveVal(NULL)
    final_results <- reactiveVal(NULL)
    # Reactive flag to see if 2nd step is done
    calculations_done <- reactiveVal(FALSE)
    
    solar <- reactiveVal(NULL)
    elcons <- reactiveVal(NULL)
    
    grid_cost <- reactiveVal(NULL)
    feed_in <- reactiveVal(NULL)
    elprice <- reactiveVal(NULL)
    
    # Map rendering and click handling
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = 16.998, lat = 49.278, zoom = 8)
    })
    
    observeEvent(input$map_click, {
        click <- input$map_click
        if (!is.null(click)) {
            updateNumericInput(session, "lat", value = click$lat)
            updateNumericInput(session, "lon", value = click$lng)
            
            leafletProxy("map") %>%
                clearMarkers() %>%
                addMarkers(lng = click$lng, lat = click$lat)
        }
    })
    
    # Input validation
    observe({
        # Validate percentage inputs
        validate_min_max <- function(input_id, min_val, max_val, error_msg) {
            if (input[[input_id]] < min_val || input[[input_id]] > max_val) {
                showNotification(
                    error_msg,
                    type = "error",
                    duration = 5
                )
                return(FALSE)
            }
            return(TRUE)
        }
        
        # Battery percentage validations
        battery_init_valid <- validate_min_max(
            "battery_initial_soc", 0, 100,
            "Initial SOC must be between 0% and 100%"
        )
        
        battery_min_valid <- validate_min_max(
            "battery_min_soc", 0, 100,
            "Min SOC must be between 0% and 100%"
        )
        
        battery_max_valid <- validate_min_max(
            "battery_max_soc", 0, 100,
            "Max SOC must be between 0% and 100%"
        )
        
        # Check min < initial < max
        if (battery_min_valid && battery_init_valid && battery_max_valid) {
            if (input$battery_min_soc > input$battery_initial_soc) {
                showNotification(
                    "Initial SOC must be greater than or equal to Min SOC",
                    type = "error",
                    duration = 5
                )
            }
            
            if (input$battery_initial_soc > input$battery_max_soc) {
                showNotification(
                    "Initial SOC must be less than or equal to Max SOC",
                    type = "error",
                    duration = 5
                )
            }
            
            if (input$battery_min_soc > input$battery_max_soc) {
                showNotification(
                    "Min SOC must be less than Max SOC",
                    type = "error",
                    duration = 5
                )
            }
        }
        
        # Validate efficiency values
        validate_min_max(
            "battery_charge_efficiency", 0, 1, 
            "Charge efficiency must be between 0 and 1"
        )
        
        validate_min_max(
            "battery_discharge_efficiency", 0, 1,
            "Discharge efficiency must be between 0 and 1"
        )
        
        # Validate PV parameters
        validate_min_max(
            "PV_system_loss", 0, 100,
            "PV system loss must be between 0% and 100%"
        )
        
        # Validate date range
        if (!is.null(input$start_date) && !is.null(input$system_lifetime)) {
            if (input$system_lifetime <= 0) {
                showNotification(
                    "System lifetime must be positive",
                    type = "error",
                    duration = 5
                )
            }
        }
    })
    
    # First button - Load Energy Data
    observeEvent(input$load_data, {
        showModal(modalDialog("Loading energy data...", footer = NULL))
        
        tryCatch({
            # Convert percentage inputs to decimal form for processing
            sp <- list(
                use_cache_data = input$use_cache_data,
                fixed_seed = input$fixed_seed,
                
                # Battery parameters
                battery_capacity_kwh = input$battery_capacity_kwh,
                battery_charge_efficiency = input$battery_charge_efficiency,
                battery_discharge_efficiency = input$battery_discharge_efficiency,
                battery_initial_soc = input$battery_initial_soc / 100, # Convert from percentage to decimal
                battery_min_soc = input$battery_min_soc / 100, # Convert from percentage to decimal
                battery_max_soc = input$battery_max_soc / 100, # Convert from percentage to decimal
                battery_degradation = input$battery_degradation,
                
                # PV parameters
                PV_degradation = input$PV_degradation,
                PV_system_own_consumption = input$PV_system_own_consumption,
                PV_system_loss = input$PV_system_loss, # Already in percentage format for the API
                PV_angle = input$PV_angle,
                PV_aspect = input$PV_aspect,
                PV_peakpower = input$PV_peakpower,
                PV_add_PV_noise = input$PV_add_PV_noise,
                
                # Financial parameters
                installation_cost = input$installation_cost,
                discount_rate = input$discount_rate,
                annual_maintenance_cost = input$annual_maintenance_cost,
                
                # Date range
                start_date = as.character(input$start_date), # Ensure it's a string in YYYY-MM-DD format
                system_lifetime = input$system_lifetime,
                lat = input$lat,
                lon = input$lon,
                
                # Household consumption
                HH_annual_consumption = input$HH_annual_consumption,
                HH_add_cons_multiplier = input$HH_add_cons_multiplier,
                
                # Electricity price parameters
                elprice_method = input$elprice_method,
                elprice_annual_growth = input$elprice_annual_growth,
                elprice_add_intraday_variability = input$elprice_add_intraday_variability,
                elprice_add_intraweek_variability = input$elprice_add_intraweek_variability,
                
                # Feed-in tariff
                feedin_method = input$feedin_method,
                feedin_lastval = input$feedin_lastval,
                feedin_annual_growth = input$feedin_annual_growth,
                
                # Grid cost parameters
                gridcost_method = input$gridcost_method,
                gridcost_annual_growth = input$gridcost_annual_growth
            )
            
            system_params(sp)
            
            if (sp$use_cache_data) {
                solar <- solar(readRDS("_cache/solar.Rds"))
                elcons <- elcons(readRDS("_cache/elcons_data.Rds"))
            } else {
                solar( getSolarData(
                    lat = sp$lat, lon = sp$lon, start_date = sp$start_date,
                    system_lifetime = sp$system_lifetime, loss = sp$PV_system_loss,
                    angle = sp$PV_angle, aspect = sp$PV_aspect, peakpower = sp$PV_peakpower,
                    add_PV_noise = sp$PV_add_PV_noise, fixed_seed = sp$fixed_seed
                ))
                elcons( get_load_data(
                    start_date = sp$start_date, system_lifetime = sp$system_lifetime,
                    annual_consumption = sp$HH_annual_consumption, fixed_seed = sp$fixed_seed,
                    add_HH_cons_noise = sp$HH_add_cons_multiplier
                ))
            }
            
            df1 <- myCombineConsAndSolar(solar()$solar_data, elcons()$elcons)
            print("df1: ")
            glimpse(df1)
            energy_flows <- CalculateEnergyFlows(df1, sp)
            print("energy_flows: ")
            glimpse(energy_flows)
            energy_flows(energy_flows)
            
            showNotification("Energy data loaded successfully", type = "message")
            shinyjs::hide("load_data")
            print("hiding load data button")
            shinyjs::show("calculate_financials")
            
           
        }, error = function(e) {
            showNotification(paste("Error loading data:", e$message), type = "error")
        }, finally = {
            removeModal()
        })
    })
    
    # Second button - Load Financial Data and Calculate
    observeEvent(input$calculate_financials, {
        req(system_params(), energy_flows())
        showModal(modalDialog("Calculating financials...", footer = NULL))
        
        tryCatch({
            sp <- system_params()
            energy_flows <- energy_flows()
            
            if (sp$use_cache_data) {
                grid_cost <- grid_cost(readRDS("_cache/grid_cost.Rds") )
                feed_in <- feed_in( readRDS("_cache/feed_in_price.Rds") )
                elprice <- elprice( readRDS("_cache/elprice.Rds") )
            } else {
                grid_cost( my_gridcost(
                    my_data_read_distrib_costs_observed_data(), startdate = sp$start_date,
                    years = sp$system_lifetime, annual_growth = sp$gridcost_annual_growth,
                    method = sp$gridcost_method
                ))
                feed_in( my_feed_in(
                    years = sp$system_lifetime, annual_growth = sp$feedin_annual_growth,
                    startdate = sp$start_date, method = sp$feedin_method,
                    fixed_seed = sp$fixed_seed, lastval = sp$feedin_lastval
                ))
                elprice( my_elprice(
                    my_data_read_elprice_observed_data(), startdate = sp$start_date,
                    years = sp$system_lifetime, annual_growth = sp$elprice_annual_growth,
                    method = sp$elprice_method,
                    add_intraday_variability = sp$elprice_add_intraday_variability,
                    add_intraweek_variability = sp$elprice_add_intraweek_variability
                ))
            }
            
            #DEBUG
            # cat("Data types check:\n")
            # cat("energy_flows datetime:", class(energy_flows$datetime), " ", max(energy_flows$datetime), "\n")
            # cat("elprice datetime:", class(elprice$price_data$datetime), " ", max(elprice$price_data$datetime), "\n")
            # cat("feed_in datetime:", class(feed_in$feed_in$datetime), " ", max(elprice$price_data$datetime), "\n")
            # cat("grid_cost datetime:", class(grid_cost$grid_cost$datetime), " ", max(grid_cost$grid_cost$datetime), "\n")
            # cat("----")
            # cat("Energy flows date range:", min(energy_flows$date), "to", max(energy_flows$date), "\n")
            # cat("Elprice date range:", min(elprice$price_data$date), "to", max(elprice$price_data$date), "\n")
            # cat("Feed-in date range:", min(feed_in$feed_in$date), "to", max(feed_in$feed_in$date), "\n")
            # cat("Grid cost date range:", min(grid_cost$grid_cost$date), "to", max(grid_cost$grid_cost$date), "\n")
            # 
            # print("---")
            # print("energy_flows: ")
            # glimpse(energy_flows)
            # print("elprice: ")
            # glimpse(elprice$price_data)
            # print("feed_in: ")
            # glimpse(feed_in$feed_in)
            # print("grid_cost: ")
            # glimpse(grid_cost$grid_cost)
            
            energy_flows_enh <- CalculateFinancials(
                energy_flows, elprice()$price_data, feed_in()$feed_in, grid_cost()$grid_cost, params = sp
            )
            print("energy_flows_enh: ")
            glimpse(energy_flows_enh)
            
            final_results(list(
                summary = energy_flows_enh$summary_vals,
                hourly = energy_flows_enh$df_hourly
            ))
            calculations_done(TRUE)
            showNotification("Financial calculations complete", type = "message")
            
            
        }, error = function(e) {
            showNotification(paste("Error in financial calculations:", e$message), type = "error")
        }, finally = {
            
            shinyjs::show("chartsTab")
            shinyjs::show("resultsTab")
            removeModal()
        })
    })
    
    # Render summary table
    output$summary_table <- renderTable({
        req(final_results())
        final_results()$summary
    })
    
    # Render interactive plot
    output$energy_plot <- renderPlotly({
        req(final_results(), input$plot_date)
        
        df <- final_results()$hourly %>%
            filter(as.Date(date) == as.Date(input$plot_date))
        
        validate(
            need(nrow(df) > 0, "No data available for selected date")
        )
        
        p <- plotDay(input$plot_date, df, system_params())  # Ensure plotDay() returns a ggplot
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
    
    
    # render plots of inputs
    output$elconsPlot <- renderPlot({
        req(elcons())
        elcons()$plot
    })
    
    output$solarPlot <- renderPlot({
        req(solar())
        solar()$plot
    })
    
    output$gridCostPlot <- renderPlot({
        req(grid_cost())
        grid_cost()$plot
    })
    
    output$feedInPlot <- renderPlot({
        req(feed_in())
        feed_in()$plot
    })
    
    output$elpricePlot <- renderPlot({
        req(elprice())
        elprice()$plot
    })
    
    
    #DEBUG 
    # observeEvent(input$testSwitch, {
    #     updateTabsetPanel(session, "mainPanelTabs", selected = "Results")
    #     print("Test switch tab")
    # })
    
    # navigate to Results
    observe({
        print(paste("observe final_results: final_results is", ifelse(is.null(final_results()), "NULL", "NOT NULL"))) 
        if (!is.null(final_results())) {
            updateTabsetPanel(session, "mainPanelTabs", selected = "Results")
        }
    })
    
    
    # Reset App button using session$reload()
    observeEvent(input$reset_app, {
        session$reload()
    })
    
    
  
    
    #for downloads of data:
    output$download_summary <- downloadHandler(
        filename = function() {
            paste("summary_results.xlsx", sep = "")
        },
        content = function(file) {
            req(final_results())  # Ensure data is available
            
            # Get the summary data
            summary_data <- final_results()$summary
            
            # Write to an Excel file
            writexl::write_xlsx(list("Summary" = summary_data), path = file)
        }
    )
    
    # Download Hourly Data
    output$download_hourly <- downloadHandler(
        filename = function() {
            paste("hourly_data.xlsx", sep = "")
        },
        content = function(file) {
            req(final_results())  # Ensure data is available
            
            # Get the hourly data
            hourly_data <- final_results()$hourly
            
            # Write to an Excel file
            writexl::write_xlsx(list("Hourly Data" = hourly_data), path = file)
        }
    )
}