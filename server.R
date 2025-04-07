####  server.R
library(shinyjs)
library(shiny.i18n)
#gcinfo(TRUE)

server <- function(input, output, session) {
  # Initialize i18n translator
  i18n <- Translator$new(translation_json_path = "translations.json")
  i18n$set_translation_language("en") 
  
    #### hide/show elements at start
    print("hiding calculate_financials button")
    shinyjs::hide("calculate_financials")

    # observe language changes
    lang_reactive <- reactiveVal("en")
    observeEvent(input$selected_language, {
      lang_reactive(input$selected_language)
      i18n$set_translation_language(input$selected_language)
      
      # DEBUG: check language change is noticed
      message("Language changed to: ", input$selected_language)
      test_translation <- i18n$t("This is a test")
      message("Check text: ", test_translation)
      
      # change UI elements
      output$title <- renderText({ i18n$t("PV Analyzer") })
      output$title_panel <- renderText({ i18n$t("PV Analyzer") })
      output$load_btn <- renderText({ i18n$t("1. Load Energy Data") })
      output$calc_btn <- renderText({ i18n$t("2. Calculate Financials") })
      output$reset_btn <- renderText({ i18n$t("Reset App") })
      
      output$controls_header <- renderText({ i18n$t("Controls") })
      output$fixed_seed_label <- renderText({ i18n$t("Fixed Seed") })
      output$location_header <- renderText({ i18n$t("Location") })
      output$location_help <- renderText({ i18n$t("Click on the map to set your location or enter coordinates manually") })
      output$latitude_label <- renderText({ i18n$t("Latitude") })
      output$longitude_label <- renderText({ i18n$t("Longitude") })
      output$date_range_header <- renderText({ i18n$t("Date Range") })
      output$start_date_label <- renderText({ i18n$t("Start Date") })
      output$system_lifetime_label <- renderText({ i18n$t("System Lifetime (years)") })
      
    })
    
    observe({
      req(lang_reactive())  
      # modal with instructions on app start
      showModal(modalDialog(
        title = i18n$t("Welcome to the PV Analyzer"),
        i18n$t("Please follow these steps:"),
        tags$ol(
          tags$li(i18n$t("Browse the input tabs and set the values of parameters.")),
          tags$li(i18n$t("Click the 'Load Energy Data' button and wait until the data is loaded.")),
          tags$li(i18n$t("Click the 'Calculate Financials' button and wait until the calculations are done")),
          tags$li(i18n$t("See the results on the Results tab."))
        ),
        footer = modalButton("OK")
      ))#end modal
    })
    
    #### dynamic UI updates
    
    # entire tabs section
    
    # Create reactive versions of the choice lists
    elprice_choices_list <- reactive({
      req(lang_reactive())
      list(
        "Provided Value with Growth" = "last_w_growth",
        "Historical with Growth" = "historical_w_growth",
        "Static" = "static",
        "Time Series Model" = "linear",
        "Random Walk with Trend" = "random_walk_trend",
        "Repeat Selected Year" = "selected_year"
      ) %>% 
        setNames(., sapply(names(.), function(x) i18n$t(x)))
    })
    
    feedin_choices_list <- reactive({
      req(lang_reactive())
      list("Provided Value with Growth" = "last_w_growth") %>% 
        setNames(., sapply(names(.), function(x) i18n$t(x)))
    })
    
    gridcost_choices_list <- reactive({
      req(lang_reactive())
      list(
        "Provided Value with Growth" = "last_w_growth",
        "Static" = "static",
        "Time Series Model" = "linear",
        "Historical with Growth" = "historical_w_growth"
      ) %>% 
        setNames(., sapply(names(.), function(x) i18n$t(x)))
    })
    
    
    output$translated_tabs <- renderUI({
      req(lang_reactive())
      tabsetPanel(id = "mainPanelTabs",
                  tabPanel(i18n$t("Battery Parameters"), value = "batteryTab",
                           numericInput("battery_capacity_kwh", i18n$t("Capacity (kWh)"), min = 0, value = if (!is.null(system_params()$battery_capacity_kwh)) system_params()$battery_capacity_kwh else 10),
                           numericInput("battery_charge_efficiency", i18n$t("Charge Efficiency"), min = 0, max = 1, step = 0.01, value = if (!is.null(system_params()$battery_charge_efficiency)) system_params()$battery_charge_efficiency else 0.95),
                           helpText(i18n$t("Value between 0 and 1. When charging with E kWh, stored energy is E × efficiency")),
                           numericInput("battery_discharge_efficiency", i18n$t("Discharge Efficiency"), min = 0, max = 1, step = 0.01, value = if (!is.null(system_params()$battery_discharge_efficiency)) system_params()$battery_discharge_efficiency else 0.95),
                           helpText(i18n$t("Value between 0 and 1. When discharging E kWh, released energy is E × efficiency")),
                           numericInput("battery_initial_soc", i18n$t("Initial SOC (%)"), min = 0, max = 100, step = 1, value = if (!is.null(system_params()$battery_initial_soc)) system_params()$battery_initial_soc*100 else 20),
                           numericInput("battery_min_soc", i18n$t("Min SOC (%)"), min = 0, max = 100, step = 1, value = if (!is.null(system_params()$battery_min_soc)) system_params()$battery_min_soc*100 else 10),
                           helpText(i18n$t("Minimum state of charge to avoid battery damage")),
                           numericInput("battery_max_soc", i18n$t("Max SOC (%)"), min = 0, max = 100, step = 1, value = if (!is.null(system_params()$battery_max_soc)) system_params()$battery_max_soc*100 else 100),
                           helpText(i18n$t("Maximum state of charge (100% = full capacity)")),
                           #degradation of 1.1% annualy corresponds to decrease to 80% in 20 years
                           numericInput("battery_degradation", i18n$t("Degradation Rate/year"), min = 0, max = 1, step = 0.01, value = if (!is.null(system_params()$battery_degradation)) system_params()$battery_degradation else 0.01),
                           helpText(i18n$t("Annual battery capacity degradation rate (decimal, e.g., 0.01 = 1%)")),
                           helpText(i18n$t("Degredation to 80% in 20 years corresponds to about 1.1% annual degredation rate"))
                  ),
                  
                  tabPanel(i18n$t("PV Parameters"), value = "pvTab",
                           numericInput("PV_peakpower", i18n$t("Peak Power (kWp)"), min = 0, step = 0.1, max = 20, value = if (!is.null(system_params()$PV_peakpower)) system_params()$PV_peakpower else 4.5),
                           numericInput("PV_system_loss", i18n$t("System Loss (%)"), min = 0, max = 100, value = if (!is.null(system_params()$PV_system_loss)) system_params()$PV_system_loss else 14),
                           helpText(i18n$t("Overall system losses in percent")),
                           numericInput("PV_angle", i18n$t("Inclination Angle"),  min = 0, max = 90, value = if (!is.null(system_params()$PV_angle)) system_params()$PV_angle else 30),
                           helpText(i18n$t("Panel inclination (0° = horizontal, 90° = vertical)")),
                           numericInput("PV_aspect", i18n$t("Azimuth Angle"), min = -180, max = 180, value = if (!is.null(system_params()$PV_aspect)) system_params()$PV_aspect else 0),
                           helpText(i18n$t("Panel orientation (0° = South, 90° = West, -90° = East)")),
                           numericInput("PV_degradation", i18n$t("Degradation Rate/year"),  min = 0, max = 1, step = 0.01, value = if (!is.null(system_params()$PV_degradation)) system_params()$PV_degradation else 0.01),
                           helpText(i18n$t("Annual PV panel degradation rate (decimal, e.g., 0.01 = 1%)")),
                           helpText(i18n$t("Degredation to 80% in 20 years corresponds to about 1.1% annual degredation rate")),
                           numericInput("PV_system_own_consumption", i18n$t("System Consumption (kWh/h)"), min = 0, step = 0.01, value = if (!is.null(system_params()$PV_system_own_consumption)) system_params()$PV_system_own_consumption else 0.03),
                           helpText(i18n$t("Constant energy consumption of the PV system (inverter, etc.)")),
                           numericInput("PV_add_PV_noise", i18n$t("PV Noise Multiplier"), min = 0, step = 0.1, value = if (!is.null(system_params()$PV_add_PV_noise)) system_params()$PV_add_PV_noise else 0.0),
                           helpText(i18n$t("Adds random variation to PV output (0.2 means ±20% variation)"))
                  ),
                  
                  tabPanel(i18n$t("Household"), value = "householdTab",
                           numericInput("HH_annual_consumption", i18n$t("Annual Consumption (MWh)"), min = 0, step = 0.1, value = if (!is.null(system_params()$HH_annual_consumption)) system_params()$HH_annual_consumption else 3),
                           numericInput("HH_add_cons_noise", i18n$t("Consumption Noise Multiplier"), min = 0, step = 0.1, value = if (!is.null(system_params()$HH_add_cons_noise)) system_params()$HH_add_cons_noise else 0.8),
                           helpText(i18n$t("Adds random variation to consumption (0.2 means ±20% variation)"))
                  ),
                  
                  tabPanel(i18n$t("Financials"), value = "financialsTab",
                           numericInput("installation_cost", i18n$t("Installation Cost (CZK)"), min = 0, value = if (!is.null(system_params()$installation_cost)) system_params()$installation_cost else 220000),
                           numericInput("annual_maintenance_cost", i18n$t("Annual Maintenance (CZK)"), min = 0, value = if (!is.null(system_params()$annual_maintenance_cost)) system_params()$annual_maintenance_cost else 4000),
                           numericInput("discount_rate", i18n$t("Discount Rate"), min = 0, max = 100, step = 0.1, value = if (!is.null(system_params()$discount_rate)) system_params()$discount_rate*100 else 3),
                           helpText(i18n$t("Annual discount rate for NPV calculations (e.g. 3.5 means 3.5 %)"))
                  ),
                  
                  tabPanel(i18n$t("Electricity Prices"), value = "elpriceTab",
                           selectInput(
                             inputId = "elprice_method",
                             i18n$t("Price Method"),
                             choices = elprice_choices_list(),
                             selected = if (!is.null(system_params()$elprice_method)) system_params()$elprice_method else "selected_year"
                           ),
                           helpText(i18n$t("Method for electricity price projection")),
                           numericInput(inputId = "add_random_noise", i18n$t("Add Random Noise to Data"), step = 0.05, min = 0, max = 1, value = if (!is.null(system_params()$add_random_noise)) system_params()$add_random_noise else 0.0),
                           helpText(i18n$t("Adds random variation to el. price (0.2 means ±20% variation)")),
                           
                           conditionalPanel(
                             condition = "['last_w_growth', 'random_walk_trend'].includes(input.elprice_method)",
                             numericInput(inputId = "elprice_lastval", i18n$t("Provided El. Price (CZK/kWh) to apply"), step = 0.1, value = if (!is.null(system_params()$elprice_lastval)) system_params()$elprice_lastval else 3.5),
                             helpText(i18n$t("Electricity price to apply the growth rate to"))
                           ),
                           conditionalPanel(
                             condition = "input.elprice_method == 'selected_year'",
                             selectInput(inputId = "selected_year", i18n$t("Repeat Year from Observations"), choices = c(2016:2023), selected = if (!is.null(system_params()$selected_year)) system_params()$selected_year else 2023)
                           ),
                           conditionalPanel(
                             condition = "['last_w_growth', 'historical_w_growth', 'random_walk_trend'].includes(input.elprice_method)",
                             numericInput(inputId = "elprice_annual_growth", i18n$t("Annual Growth"), step = 0.01, value = if (!is.null(system_params()$elprice_annual_growth)) system_params()$elprice_annual_growth else 0.05),
                             helpText(i18n$t("Annual price growth rate (decimal, e.g., 0.05 = 5%)"))
                           ),
                           
                           checkboxInput(inputId = "elprice_add_intraday_variability", i18n$t("Intraday Variability"), value = if (!is.null(system_params()$elprice_add_intraday_variability)) system_params()$elprice_add_intraday_variability else TRUE),
                           helpText(i18n$t("Add daily price patterns")),
                           checkboxInput(inputId = "elprice_add_intraweek_variability", i18n$t("Intraweek Variability"), value = if (!is.null(system_params()$elprice_add_intraweek_variability)) system_params()$elprice_add_intraweek_variability else TRUE),
                           helpText(i18n$t("Add weekday price patterns"))
                  ),
                  
                  tabPanel(i18n$t("Feed-in Tariff"), value = "feedinTab",
                           selectInput(inputId = "feedin_method", i18n$t("Method"),
                                       choices = feedin_choices_list(),
                                       selected = if (!is.null(system_params()$feedin_method)) system_params()$feedin_method else "last_w_growth"
                           ),
                           numericInput(inputId = "feedin_lastval", i18n$t("Provided Value (CZK/kWh)"), min = 0, step = 0.1, value = if (!is.null(system_params()$feedin_lastval)) system_params()$feedin_lastval else 1.1),
                           numericInput(inputId = "feedin_annual_growth", i18n$t("Annual Growth"), step = 0.01, value = if (!is.null(system_params()$feedin_annual_growth)) system_params()$feedin_annual_growth else -0.1),
                           helpText(i18n$t("Annual feed-in tariff growth rate (decimal, e.g., 0.01 = 1%)"))
                  ),
                  
                  tabPanel(i18n$t("Grid Costs"), value = "gridcostTab",
                           
                           
                           selectInput(inputId = "gridcost_method", i18n$t("Method"),
                                       choices = gridcost_choices_list(),
                                       selected = if (!is.null(system_params()$gridcost_method)) system_params()$gridcost_method else "last_w_growth"
                           ),
                           conditionalPanel(
                             condition = "input.gridcost_method == 'last_w_growth'",
                             numericInput("gridcost_lastval", i18n$t("Provided Grid Cost (CZK/kWh) to apply as initial observation"), step = 0.1, value = if (!is.null(system_params()$gridcost_lastval)) system_params()$gridcost_lastval else 2.8),
                             helpText(i18n$t("Grid cost to apply the growth rate to (in CZK/kWh)"))
                           ),
                           
                           conditionalPanel(
                             condition = "['last_w_growth', 'historical_w_growth'].includes(input.gridcost_method)",
                             numericInput("gridcost_annual_growth", i18n$t("Annual Growth"), step = 0.01, value = if (!is.null(system_params()$gridcost_annual_growth)) system_params()$gridcost_annual_growth else 0.02),
                             helpText(i18n$t("Annual grid cost growth rate (decimal, e.g., 0.04 = 4%)"))
                           )
                  ),
                  if (calculations_done()) {
                  tabPanel(i18n$t("Observe Input Data Charts"), value = "chartsTab",
                           plotlyOutput("elconsPlot"),
                           plotlyOutput("solarPlot"),
                           plotlyOutput("gridCostPlot"),
                           plotlyOutput("feedInPlot"),
                           plotlyOutput("elpricePlot")
                  )},
                  if (calculations_done()) {
                  tabPanel(i18n$t("Results"), value = "resultsTab",
                           h3(i18n$t("Financial Summary")),
                           tableOutput("summary_table"),
                           h3(i18n$t("Hourly Energy Flows")),
                           dateInput("plot_date", i18n$t("Select Date"), value = Sys.Date()),
                           plotlyOutput("energy_plot"),
                           helpText(i18n$t("Plot shows energy flows and battery state for the selected date")),
                           downloadButton("download_summary", i18n$t("Download Summary (CSV)"), class = "btn btn-primary"),
                           downloadButton("download_hourly", i18n$t("Download Hourly Data (CSV)"), class = "btn btn-success"),
                           downloadButton("download_params", i18n$t("Download Input Parameters (CSV)"), class = "btn btn-success")
                  )
                  }#endif calculations_done()
      )#end tabsetPanel
    })#end translated_tabs
    
   
    
    #### reactive values to store intermediate results
    system_params <- reactiveVal(NULL)
    # solar <- reactiveVal(NULL)
    # elcons <- reactiveVal(NULL)
    # #
    # grid_cost <- reactiveVal(NULL)
    # feed_in <- reactiveVal(NULL)
    # elprice <- reactiveVal(NULL)
    # #
    energy_flows <- reactiveVal(NULL)
    final_results <- reactiveVal(NULL)
    
    # flag to see if the 1st setp is done
    endata_loaded <- reactiveVal(FALSE)
    # flag to see if the 2nd step is done
    calculations_done <- reactiveVal(FALSE)
    
    
    #### functionality for reset button 
    observeEvent(input$reset_app, {
      session$reload()
    })
    
    #### render map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = 16.998, lat = 49.278, zoom = 8)
    })
    
    #set marker in map
    observeEvent(input$map_click, {
      # only update map marker when data not yet loaded
      if (!endata_loaded()) {  
        click <- input$map_click
        if (!is.null(click)) {
            updateNumericInput(session, "lat", value = click$lat)
            updateNumericInput(session, "lon", value = click$lng)
            
            leafletProxy("map") %>%
                clearMarkers() %>%
                addMarkers(lng = click$lng, lat = click$lat)
        }
      }
    })

    #### replace values with 0 if input negative 
    # grid_cost 
    observeEvent(input$gridcost_lastval, {
      if (input$gridcost_lastval < 0) {
        updateNumericInput(session, "gridcost_lastval", value = 0)
        showNotification(i18n$t("Grid cost must be non-negative. Value reset to 0."), type = "warning")
      }#endif
    })
    # feed_in
    observeEvent(input$feedin_lastval, {
      if (input$feedin_lastval < 0) {
        updateNumericInput(session, "feedin_lastval", value = 0) 
        showNotification(i18n$t("Feed-in must be non-negative. Value reset to 0."), type = "warning") 
      }#endif
    })
    
    # battery capacity
    observeEvent(input$battery_capacity_kwh, {
      if (input$battery_capacity_kwh < 0) {
        updateNumericInput(session, "battery_capacity_kwh", value = 0) 
        showNotification(i18n$t("Battery capacity must be non-negative. Value reset to 0."), type = "warning") 
      }#endif
    })
    # PV capacity
    observeEvent(input$PV_peakpower, {
      if (input$PV_peakpower < 0) {
        updateNumericInput(session, "PV_peakpower", value = 1) 
        showNotification(i18n$t("PV power must be non-negative. Value reset to 1."), type = "warning") 
      }#endif
    })
    # HH cons
    observeEvent(input$HH_annual_consumption, {
      if (input$HH_annual_consumption < 0) {
        updateNumericInput(session, "HH_annual_consumption", value = 1) 
        showNotification(i18n$t("Household consumption must be non-negative. Value reset to 1."), type = "warning") 
      }#endif
    })
    # installation cost
    observeEvent(input$installation_cost, {
      if (input$installation_cost < 0) {
        updateNumericInput(session, "installation_cost", value = 1) 
        showNotification(i18n$t("Installation cost must be non-negative. Value reset to 0."), type = "warning") 
      }#endif
    })
    
    
    #### navigate to Results tab
    observe({
      print(paste("observe final_results: final_results is", ifelse(is.null(final_results()), "NULL", "calculated"))) 
      if (!is.null(final_results())) {
        updateTabsetPanel(session, "mainPanelTabs", selected = "resultsTab")
      } 
    })
    
    
    #### logic for the first button - Load Energy Data
    observeEvent(input$load_data, {
        #showModal(modalDialog(i18n$t("Estimating energy data..."), footer = NULL))
        
      # valid range for system lifetime
      min_val <- 1
      max_val <- 25
      user_val <- input$system_lifetime
      if (user_val < min_val) {
        updateNumericInput(session, "system_lifetime", value = min_val)
        validsyslife <- min_val
        showNotification(i18n$t("System Lifetime enterd was out of bounds, set to minimum allowed."), type = "warning")
      } else if (user_val > max_val) {
        updateNumericInput(session, "system_lifetime", value = max_val)
        validsyslife <- max_val
        showNotification(i18n$t("System Lifetime enterd was out of bounds, set to maximum allowed."), type = "warning")
      } else {
        validsyslife <- input$system_lifetime
      }#endif
      
      if (input$start_date <= '2026-01-01' %>% as.Date() & input$start_date >= '2020-01-01' %>% as.Date() ) { 
        validstartdate <- format(input$start_date, "%Y-%m-%d") # ensure a string (in YYYY-MM-DD format)
      } else {
         validstartdate <- '2026-01-01'
      }#endif
      
    
        tryCatch({
          # in sp, convert percentage inputs to decimal form for processing
          sp <- list(
               # use_cache_data = input$use_cache_data,
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
                PV_system_loss = input$PV_system_loss, # is like 10 for 10% - used like "10" in the API call
                PV_angle = input$PV_angle,
                PV_aspect = input$PV_aspect,
                PV_peakpower = input$PV_peakpower,
                PV_add_PV_noise = input$PV_add_PV_noise,
                
                # Financial parameters
                installation_cost = input$installation_cost,
                discount_rate = input$discount_rate/ 100, # Convert from percentage to decimal,
                annual_maintenance_cost = input$annual_maintenance_cost,
                
                # Date range
                start_date = validstartdate,
                system_lifetime = validsyslife,
                lat = input$lat,
                lon = input$lon,
                
                # Household consumption
                HH_annual_consumption = input$HH_annual_consumption,
                HH_add_cons_multiplier = input$HH_add_cons_noise,
                
                # Electricity price parameters
                elprice_method = input$elprice_method,
                elprice_annual_growth = input$elprice_annual_growth,
                elprice_lastval = input$elprice_lastval,
                elprice_selected_year = input$selected_year,
                elprice_add_random_noise = input$add_random_noise,
                elprice_add_intraday_variability = input$elprice_add_intraday_variability,
                elprice_add_intraweek_variability = input$elprice_add_intraweek_variability,
                
                # Feed-in tariff
                feedin_method = input$feedin_method,
                feedin_lastval = input$feedin_lastval,
                feedin_annual_growth = input$feedin_annual_growth,
                
                # Grid cost parameters
                gridcost_method = input$gridcost_method,
                gridcost_annual_growth = input$gridcost_annual_growth,
                gridcost_lastval = input$gridcost_lastval
            )
            
            system_params(sp)
            glimpse(sp)
            print("sys params loaded")
            
            
            showModal(modalDialog(i18n$t("Estimating solar and energy consumption data..."), footer = NULL))
            #removeModal()
            
            df1 <- myCombineConsAndSolar(
                #solar
                getSolarData(
                  lat = sp$lat
                  , lon = sp$lon
                  , start_date = sp$start_date
                  , system_lifetime = sp$system_lifetime
                  , loss = sp$PV_system_loss
                  , angle = sp$PV_angle
                  , aspect = sp$PV_aspect
                  , peakpower = sp$PV_peakpower
                  , add_PV_noise = sp$PV_add_PV_noise
                  , fixed_seed = sp$fixed_seed
                )$solar_data
                #elcons
                , get_load_data(
                  start_date = sp$start_date
                  , system_lifetime = sp$system_lifetime
                  , annual_consumption = sp$HH_annual_consumption
                  , fixed_seed = sp$fixed_seed
                  , add_HH_cons_noise = sp$HH_add_cons_multiplier
                )$elcons
                )
              
            gc(full = TRUE)
            print("df1 done")
            #glimpse(df1)
            #energy_flows <- CalculateEnergyFlows(df1, sp)
            
            removeModal()
            showModal(modalDialog(i18n$t("Calculating expected energy flows..."), footer = NULL))
            
            
            energy_flows(CalculateEnergyFlows(df1, sp)) 
            #df1 not needed anymore
            rm(df1)
            print("energy_flows done")
            gc(full = TRUE)
            #glimpse(energy_flows)
            
            showNotification(i18n$t("Energy data loaded successfully"), type = "message")
            #hide load data button
            shinyjs::hide("load_data")
            print("hiding load data button")
            
            #disable inputs affecting inputs (dates, PV and HH cons) that shouldn't be editable anymore
            shinyjs::delay(100, { #delay by 100 milliseconds
              shinyjs::disable("HH_annual_consumption")
              shinyjs::disable("HH_add_cons_noise")
              shinyjs::disable("PV_peakpower")
              shinyjs::disable("PV_system_loss")
              shinyjs::disable("PV_angle")
              shinyjs::disable("PV_aspect")
              shinyjs::disable("PV_degradation")
              shinyjs::disable("PV_system_own_consumption")
              shinyjs::disable("PV_add_PV_noise")
              shinyjs::disable("battery_capacity_kwh")
              shinyjs::disable("battery_charge_efficiency")
              shinyjs::disable("battery_discharge_efficiency")
              shinyjs::disable("battery_initial_soc")
              shinyjs::disable("battery_min_soc")
              shinyjs::disable("battery_max_soc")
              shinyjs::disable("battery_degradation")
              shinyjs::disable("system_lifetime")
              shinyjs::disable("lat")
              shinyjs::disable("lon")
              shinyjs::disable("start_date")
              shinyjs::disable("selected_language")
            })
            
            # show button for 2nd step
            shinyjs::show("calculate_financials")
           
        }, error = function(e) {
            showNotification(paste(i18n$t("Error loading data:"), e$message), type = "error", duration = 15)
        }, finally = {
            removeModal()
          endata_loaded(TRUE)  # Set flag to block future clicks
          print("step1 done")
        })
    })
    
    #### logic for the second button - Calculate Financial Data 
    observeEvent(input$calculate_financials, {
      shiny::req(system_params(), energy_flows())
        #showModal(modalDialog("Calculating financials...", footer = NULL))
        calculations_done(FALSE)
        
        tryCatch({
          
          sp <- list(
           # use_cache_data = input$use_cache_data,
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
            PV_system_loss = input$PV_system_loss, # is like 10 for 10% - used like "10" in the API call
            PV_angle = input$PV_angle,
            PV_aspect = input$PV_aspect,
            PV_peakpower = input$PV_peakpower,
            PV_add_PV_noise = input$PV_add_PV_noise,
            
            # Financial parameters
            installation_cost = input$installation_cost,
            discount_rate = input$discount_rate/ 100, # Convert from percentage to decimal,
            annual_maintenance_cost = input$annual_maintenance_cost,
            
            # Date range
            start_date = format(input$start_date, "%Y-%m-%d"), # ensure a string (in YYYY-MM-DD format)
            system_lifetime = input$system_lifetime,
            lat = input$lat,
            lon = input$lon,
            
            # Household consumption
            HH_annual_consumption = input$HH_annual_consumption,
            HH_add_cons_multiplier = input$HH_add_cons_noise,
            
            # Electricity price parameters
            elprice_method = input$elprice_method,
            elprice_annual_growth = input$elprice_annual_growth,
            elprice_lastval = input$elprice_lastval,
            elprice_selected_year = input$selected_year,
            elprice_add_random_noise = input$add_random_noise,
            elprice_add_intraday_variability = input$elprice_add_intraday_variability,
            elprice_add_intraweek_variability = input$elprice_add_intraweek_variability,
            
            # Feed-in tariff
            feedin_method = input$feedin_method,
            feedin_lastval = input$feedin_lastval,
            feedin_annual_growth = input$feedin_annual_growth,
            
            # Grid cost parameters
            gridcost_method = input$gridcost_method,
            gridcost_annual_growth = input$gridcost_annual_growth,
            gridcost_lastval = input$gridcost_lastval
          )
          
          system_params(sp)
          glimpse(sp)
          print("sys params loaded")
          
          showModal(modalDialog(i18n$t("Estimating price-related data..."), footer = NULL))
            
            
          grid_cost <- my_gridcost(
                    my_data_read_distrib_costs_observed_data()
                    , startdate = sp$start_date
                    , years = sp$system_lifetime
                    , annual_growth = sp$gridcost_annual_growth
                    , method = sp$gridcost_method
                    , lastval = sp$gridcost_lastval
                    )
          print("grid_cost loaded")
          feed_in <- my_feed_in(
                    years = sp$system_lifetime
                    , annual_growth = sp$feedin_annual_growth
                    , startdate = sp$start_date
                    , method = sp$feedin_method
                    , fixed_seed = sp$fixed_seed
                    , lastval = sp$feedin_lastval
                    )
          print("feed_in loaded")  
          elprice <- my_elprice(
                    my_data_read_elprice_observed_data()
                    , startdate = sp$start_date
                    , years = sp$system_lifetime, annual_growth = sp$elprice_annual_growth
                    , method = sp$elprice_method
                    , add_intraday_variability = sp$elprice_add_intraday_variability
                    , add_intraweek_variability = sp$elprice_add_intraweek_variability
                    , lastval = sp$elprice_lastval
                    , selected_year = sp$elprice_selected_year 
                    , add_random_noise = sp$elprice_add_random_noise
                    )
          print("elprice loaded")  
          gc(full = TRUE)
                
            
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
            
          removeModal()
          showModal(modalDialog(i18n$t("Calculating financials..."), footer = NULL))
          
          final_results( CalculateFinancials(
                energy_flows(), elprice$price_data, feed_in$feed_in, grid_cost$grid_cost, params = sp
          ))
            
          print("final_results done")
          rm(elprice, feed_in, grid_cost)
          gc(full = TRUE)
          calculations_done(TRUE)
          showNotification(i18n$t("Financial calculations complete"), type = "message")
            
        }, error = function(e) {
            showNotification(paste(i18n$t("Error in financial calculations:"), e$message), type = "error", duration = 15)
        }, finally = {
            
            #showTab(inputId = "mainPanelTabs", target = "chartsTab")
            #showTab(inputId = "mainPanelTabs", target = "resultsTab")
            # shinyjs::runjs("
            #   $('#mainPanelTabs li a[data-value=\"chartsTab\"]').parent().show();
            #   $('#mainPanelTabs li a[data-value=\"resultsTab\"]').parent().show();
            #   ")
            
            removeModal()
            print("step2 done")
        })
    })
    
    observeEvent(calculations_done(), {
      message("calculations_done() is: ", calculations_done())
      if (calculations_done()) {
        shinyjs::delay(100, { #delay by 100 milliseconds
          shinyjs::disable("HH_annual_consumption")
          shinyjs::disable("HH_add_cons_noise")
          shinyjs::disable("PV_peakpower")
          shinyjs::disable("PV_system_loss")
          shinyjs::disable("PV_angle")
          shinyjs::disable("PV_aspect")
          shinyjs::disable("PV_degradation")
          shinyjs::disable("PV_system_own_consumption")
          shinyjs::disable("PV_add_PV_noise")
          shinyjs::disable("battery_capacity_kwh")
          shinyjs::disable("battery_charge_efficiency")
          shinyjs::disable("battery_discharge_efficiency")
          shinyjs::disable("battery_initial_soc")
          shinyjs::disable("battery_min_soc")
          shinyjs::disable("battery_max_soc")
          shinyjs::disable("battery_degradation")
          shinyjs::disable("system_lifetime")
          shinyjs::disable("lat")
          shinyjs::disable("lon")
          shinyjs::disable("start_date")
          shinyjs::disable("selected_language")
        })
      }
    })
    
    #### render summary table in results
    output$summary_table <- renderTable({
      shiny::req(final_results())
      
    transpose_this <- final_results()$summary_vals %>% select( date_range
                                                              # , discounted_net_cashflow_without_PV
                                                              # , discounted_net_cashflow_explicit
                                                              # , discounted_maintenance_costs
                                                              # , discounted_net_cashflow_explicit_w_maintenance
                                                               #
                                                              # , discounted_benefit_wo_maintenance
                                                               , discounted_benefit
                                                              # , discounted_benefit_alt
                                                               , NPV
                                                              # , NPV_alt
                                                               #
                                                               , total_electricity_generated
                                                              # , grid_export 
                                                              # , grid_import 
                                                              # , household_el_consumption 
                                                              # , elcons_saved
                                                              # , feed_in_revenue_nominal
                                                               # for LCOE calculation
                                                              # , installation_cost
                                                              # , Present_Value_maintenance_costs
                                                               , Present_Value_total_cost
                                                              # , discounted_total_el_produced
                                                               , LCOE
                                                               , annualized_rate_of_return
                                                              # , FV
                                                              # , annualized_rate_of_return_alt #should be the same as annualized_rate_of_return
                                                              , breakeven_feedin
                                                              , breakeven_price
                                                              , payback_period
                                                              , discounted_payback_period
                                                              , self_sufficiency_ratio
                                                              , avg_solar_capture_rate
                                                              ) %>% 
      rename(
          "Date range" = date_range
          , "Discounted benefit (CZK)" = discounted_benefit
          , "Net Present Value (CZK)" = NPV
          , "Total electricity generated (kWh)" = total_electricity_generated
          , "Present value of Total Cost (CZK)" = Present_Value_total_cost
          , "Levelized Cost of Electricity (CZK/kWh)" = LCOE
          , "Annualized rate of return" = annualized_rate_of_return
          , "Break-even feed-in tariff (CZK/kWh)" = breakeven_feedin
          , "Break-even price (CZK/kWh)" = breakeven_price
          , "Payback Period (years)" = payback_period
          , "Discounted Payback Period (years)" = discounted_payback_period
          , "Self-Sufficiency Ratio" = self_sufficiency_ratio
          , "Average Solar Capture Rate (CZK/kWh)" = avg_solar_capture_rate
           ) %>%
    setNames(., sapply(names(.), function(x) i18n$t(x))) 
    
    #show this as a table:
    transpose_this %>% mutate(across(everything(), as.character)) %>%
     pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
      setNames(., sapply(names(.), function(x) i18n$t(x))) 
    })#end renderTable summary_table
    
    #### interactive plot for daily overview:
    output$energy_plot <- renderPlotly({
      shiny::req(final_results(), input$plot_date)
        
        df <- final_results()$df_hourly %>%
            filter(as.Date(date) == as.Date(input$plot_date))
        
        validate(
            shiny::need(nrow(df) > 0, "No data available for selected date")
        )
        
        colors <- list(
          "PV Available" = "orange",
          "Total Demand" = "red",
          "Grid Import" = "black",
          "Grid Export" = "darkgreen",
          "Battery SoC" = "blue"
        ) %>% 
          setNames(., sapply(names(.), function(x) i18n$t(x)))
        
        ply <- plot_ly(df, x = ~hour) %>%
          # primariy axis
          add_trace(y = ~PV_available, name = i18n$t("PV Available"), type = "scatter", mode = "lines",
                    line = list(color = colors[[i18n$t("PV Available")]])) %>%
          add_trace(y = ~total_demand, name = i18n$t("Total Demand"), type = "scatter", mode = "lines",
                    line = list(color = colors[[i18n$t("Total Demand")]])) %>%
          add_trace(y = ~grid_import, name = i18n$t("Grid Import"), type = "scatter", mode = "lines",
                    line = list(color = colors[[i18n$t("Grid Import")]])) %>%
          add_trace(y = ~grid_export, name = i18n$t("Grid Export"), type = "scatter", mode = "lines",
                    line = list(color = colors[[i18n$t("Grid Export")]])) %>%
          # secondary axis (for battery SoC)
          add_trace(y = ~battery_soc, name = i18n$t("Battery SoC"), type = "scatter", mode = "lines",
                    line = list(color = colors[[i18n$t("Battery SoC")]], dash = "dot"),
                    yaxis = "y2") %>%
          #labs
          layout(
            title = list(text = i18n$t("Energy Flow (Hourly)"), font = list(size = 12)),
            xaxis = list(title = i18n$t("Hour"), tickmode = "linear", dtick = 1), 
            yaxis = list(title = i18n$t("Energy (kWh)")
                         , side = "left", showgrid = TRUE
                         , titlefont = list(size = 10),
                         tickfont = list(size = 8)),
            yaxis2 = list(
              title = i18n$t("Battery SoC (kWh)"),
              side = "right",
              overlaying = "y",
              range = c(0, system_params()$battery_capacity_kwh), 
              titlefont = list(size = 10),
              tickfont = list(size = 8),
              showgrid = FALSE
            ),
            legend = list(
              # transparent background
              bgcolor = "rgba(0,0,0,0)",  
              font = list(size = 11),  
              orientation = "h",  
              x = 0.5, y = -0.15, xanchor = "center"
            ),
            hovermode = "x unified"
          )
        
        return(ply)
    })#end renderPlotly 
    
    
    #### render plots of inputs
    output$elconsPlot <- renderPlotly({
        shiny::req( final_results()$df_hourly )
      
      final_results()$df_hourly %>%
        group_by(date, year) %>% 
        summarize(cons_kWh = sum(cons_kWh), .groups = "drop") %>%
        plot_ly(x = ~date, y = ~cons_kWh, type = 'scatter', mode = 'lines',
                line = list(color = 'rgba(0, 0, 0, 0.5)')) %>%
        layout(
          title = list(text = i18n$t("Household Consumption"), x = 0.5), 
          yaxis = list(title = i18n$t("kWh"), range = c(0, NA), zeroline = TRUE),
          xaxis = list(title = i18n$t("Date")),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF"
        )
    })
    
    output$solarPlot <- renderPlotly({
         shiny::req( final_results()$df_hourly )
         
      final_results()$df_hourly %>%
        group_by(date, year, month, day, weekday, is_weekend) %>% 
        summarize(PV_available = sum(PV_available, na.rm = TRUE), .groups = "drop") %>%
        plot_ly(
          x = ~date, 
          y = ~PV_available, 
          type = 'scatter', 
          mode = 'lines',
          line = list(color = 'rgba(0, 0, 0, 0.5)')  # semi-transparent line
        ) %>%
        layout(
          title = list(text = i18n$t("Available Solar Power"), x = 0.5),  # optional title
          yaxis = list(title = i18n$t("kWh"), range = c(0, NA), zeroline = TRUE),
          xaxis = list(title = i18n$t("Date")),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF"
        )
         
    })
    
    output$gridCostPlot <- renderPlotly({
      shiny::req( final_results()$df_hourly)
      
      
      final_results()$df_hourly %>%
        group_by(year) %>% 
        summarize(grid_cost = mean(grid_cost, na.rm = TRUE), .groups = "drop") %>%
        plot_ly(
          x = ~year, 
          y = ~grid_cost, 
          type = 'scatter', 
          mode = 'lines',
          line = list(color = 'rgba(0, 0, 0, 0.5)')  # match ggplot alpha = 0.5
        ) %>%
        layout(
          title = list(text = i18n$t("Grid Cost"), x = 0.5),
          yaxis = list(title = i18n$t("Grid Cost"), range = c(0, NA), zeroline = TRUE),
          xaxis = list(
            title = i18n$t("Year"),
            tickmode = "linear",  
            tickformat = "%d",     # format ticks as integers 
            dtick = 1              # interval between ticks
          ),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF"
        )
    })
    
    output$feedInPlot <- renderPlotly({
      shiny::req( final_results()$df_hourly)
      
      final_results()$df_hourly %>%
        group_by(year) %>% 
        summarize(feed_in = mean(feed_in, na.rm = TRUE), .groups = "drop") %>%
        plot_ly(
          x = ~year, 
          y = ~feed_in, 
          type = 'scatter', 
          mode = 'lines',
          line = list(color = 'rgba(0, 0, 0, 0.5)')  # semi-transparent like alpha=0.5
        ) %>%
        layout(
          title = list(text = i18n$t("Average Feed-In per Year"), x = 0.5),
          yaxis = list(title = i18n$t("Feed-In"), range = c(0, NA), zeroline = TRUE),
          xaxis = list(
            title = i18n$t("Year"),
            tickmode = "linear",  
            tickformat = "%d",     # format ticks as integers 
            dtick = 1              # interval between ticks
          ),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF"
        )
    })
    
    output$elpricePlot <- renderPlotly({
      shiny::req( final_results()$df_hourly)
      
      # final_results()$df_hourly %>%
      #   group_by(year) %>% 
      #   summarize(price = price %>% mean(na.rm = TRUE)
      #             , price_q10 = price %>% quantile(0.1, na.rm = TRUE)
      #             , price_q50 = price %>% quantile(0.5, na.rm = TRUE)
      #             , price_q90 = price %>% quantile(0.9, na.rm = TRUE)
      #   ) %>% 
      #   ggplot() +
      #   geom_line(aes(x=date, y = price), alpha = 0.7, color = "red") +
      #   geom_line(aes(x=date, y = price_q10), alpha = 0.7, color = "grey") +
      #   geom_line(aes(x=date, y = price_q90), alpha = 0.7, color = "grey") +
      #   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      #   theme_minimal()  
      
      final_results()$df_hourly %>%
        plot_ly(
          x = ~factor(year), 
          y = ~price, 
          type = "box"
        ) %>%
        layout(
          title = list(text =  i18n$t("Electricity Price per Year"), x = 0.5),
          xaxis = list(title =  i18n$t("Year"), tickangle = 90),
          yaxis = list(title =  i18n$t("CZK/kWh")),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF"
        )
    })
    
    
    #DEBUG 
    # observeEvent(input$testSwitch, {
    #     updateTabsetPanel(session, "mainPanelTabs", selected = "Results")
    #     print("Test switch tab")
    # })
    
   
    
    #### buttons for downloads of data:
    #summary data
    output$download_summary <- downloadHandler(
        filename = function() {
            paste("summary_results.csv", sep = "")
        },
        content = function(file) {
          shiny::req(final_results())  
            summary_data <- final_results()$summary_vals
            #writexl::write_xlsx(list("Summary" = summary_data), path = file)
            #openxlsx::write.xlsx(x = list("Summary" = summary_data), file = file)
            readr::write_csv(summary_data, file = file)
        }
    )
    
    #hourly data
    output$download_hourly <- downloadHandler(
        filename = function() {
            paste("hourly_data.csv", sep = "")
        },
        content = function(file) {
          shiny::req(final_results()) 
            hourly_data <- final_results()$df_hourly
            #writexl::write_xlsx(list("Hourly Data" = hourly_data), path = file)
            #openxlsx::write.xlsx(x = list("Hourly Data" = hourly_data), file = file)
            readr::write_csv(hourly_data, file = file)
        }
    )
    
    #params data
    output$download_params <- downloadHandler(
      filename = function() {
        paste("input_params.csv", sep = "")
      },
      content = function(file) {
        shiny::req(system_params()) 
        par <- tibble(
          key = names(system_params()),
          value = unlist(system_params()) # unlist to ensure all values are in a single vector
        )
        #writexl::write_xlsx(list("Hourly Data" = hourly_data), path = file)
        #openxlsx::write.xlsx(x = list("Input Parameters" = par), file = file)
        readr::write_csv(par, file = file)
      }
    )
    
}#end of server