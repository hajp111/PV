####  ui.R
# This is the user-interface definition of a Shiny web application. You can run the application by clicking 'Run App' above.
library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)



ui <- fluidPage(
    shinyjs::useShinyjs(),  # Add this line
    
    # Add error display area
    tags$div(id = "error-space", style = "color: red; margin: 10px;"),
    
    theme = shinytheme("flatly"),
    titlePanel("PV Analyzer"),
    sidebarLayout(
        sidebarPanel(
            h4("Controls"),
            
            actionButton(inputId = "load_data", "1. Load Energy Data", class = "btn-primary"),
            actionButton(inputId = "calculate_financials", "2. Calculate Financials", class = "btn-success"),
            
            #DEBUG change tab
            #actionButton("testSwitch", "Test Tab Switch"),
            
            #button for session reload
            actionButton("reset_app", "Reset App"),
            
            
            checkboxInput("use_cache_data", "Use Cached Data", value = FALSE),
            checkboxInput("fixed_seed", "Fixed Seed", value = TRUE),
            
            h4("Location"),
            leafletOutput("map"),
            helpText("Click on the map to set your location or enter coordinates manually"),
            numericInput("lat", "Latitude", 49.278),
            numericInput("lon", "Longitude", 16.998),
            
            h4("Date Range"),
            dateInput("start_date", "Start Date", value = "2025-01-01", max = "2028-01-01"),  #max range for HH energy data is 2073-12-31
            numericInput("system_lifetime", "System Lifetime (years)",
                         value = 20, min = 1, step = 1, max = 25)
        ),
        
        mainPanel(
            tabsetPanel( id = "mainPanelTabs"
                , tabPanel("Battery Parameters", value = "batteryTab",
                         numericInput("battery_capacity_kwh", "Capacity (kWh)", 10, min = 0),
                         numericInput("battery_charge_efficiency", "Charge Efficiency", 0.95, min = 0, max = 1, step = 0.01),
                         helpText("Value between 0 and 1. When charging with E kWh, stored energy is E × efficiency"),
                         numericInput("battery_discharge_efficiency", "Discharge Efficiency", 0.95, min = 0, max = 1, step = 0.01),
                         helpText("Value between 0 and 1. When discharging E kWh, released energy is E × efficiency"),
                         numericInput("battery_initial_soc", "Initial SOC (%)", 20, min = 0, max = 100, step = 1),
                         helpText("Initial state of charge as percentage of battery capacity"),
                         numericInput("battery_min_soc", "Min SOC (%)", 10, min = 0, max = 100, step = 1),
                         helpText("Minimum state of charge to avoid battery damage"),
                         numericInput("battery_max_soc", "Max SOC (%)", 100, min = 0, max = 100, step = 1),
                         helpText("Maximum state of charge (100% = full capacity)"),
                         numericInput("battery_degradation", "Degradation Rate/year", 0.01, min = 0, max = 1, step = 0.01),  #degradation of 1.1% annualy corresponds to decrease to 80% in 20 years
                         helpText("Annual battery capacity degradation rate (decimal, e.g., 0.01 = 1%)"),
                         helpText("Degredation to 80% in 20 years corresponds to about 1.1% annual degredation rate"))
                
                , tabPanel("PV Parameters", value = "pvTab",
                         numericInput("PV_peakpower", "Peak Power (kWp)", 4.5, min = 0, max = 20),
                         numericInput("PV_system_loss", "System Loss (%)", 14, min = 0, max = 100),
                         helpText("Overall system losses in percent"),
                         numericInput("PV_angle", "Inclination Angle", 30, min = 0, max = 90),
                         helpText("Panel inclination (0° = horizontal, 90° = vertical)"),
                         numericInput("PV_aspect", "Azimuth Angle", 0, min = -180, max = 180),
                         helpText("Panel orientation (0° = South, 90° = West, -90° = East)"),
                         numericInput("PV_degradation", "Degradation Rate/year", 0.01, min = 0, max = 1, step = 0.01),
                         helpText("Annual PV panel degradation rate (decimal, e.g., 0.01 = 1%)"),
                         helpText("Degredation to 80% in 20 years corresponds to about 1.1% annual degredation rate"),
                         numericInput("PV_system_own_consumption", "System Consumption (kWh/h)", 0.03, min = 0),
                         helpText("Constant energy consumption of the PV system (inverter, etc.)"),
                         numericInput("PV_add_PV_noise", "PV Noise Multiplier", 0.0, min = 0),
                         helpText("Adds random variation to PV output (0.2 means ±20% variation)"))
                
                , tabPanel("Financials", value = "financialsTab",
                         numericInput("installation_cost", "Installation Cost (CZK)", 200000, min = 0),
                         numericInput("annual_maintenance_cost", "Annual Maintenance (CZK)", 4000, min = 0),
                         numericInput("discount_rate", "Discount Rate", 0.03, min = 0, max = 1, step = 0.01),
                         helpText("Annual discount rate for NPV calculations (decimal, e.g., 0.03 = 3%)"))
                
                , tabPanel("Household", value = "householdTab",
                         numericInput("HH_annual_consumption", "Annual Consumption (MWh)", 3, min = 0),
                         numericInput("HH_add_cons_multiplier", "Consumption Noise Multiplier", 0.0, min = 0),
                         helpText("Adds random variation to consumption (0.2 means ±20% variation)"))
                
                , tabPanel("Electricity Prices", value = "elpriceTab",
                         selectInput("elprice_method", "Price Method",
                                     choices = c("Static" = "static", 
                                                 "Time Series Model" = "linear", 
                                                 "Provided Value with Growth" = "last_w_growth",
                                                 "Historical with Growth" = "historical_w_growth",
                                                 "Random Walk" = "random_walk", 
                                                 "Random Walk with Trend" = "random_walk_trend", 
                                                 "Mean Reverting Random Walk" = "mean_reverting_rw",
                                                 "Repeat Selected Year" = "selected_year")),
                         helpText("Method for electricity price projection"),
                         numericInput("elprice_annual_growth", "Annual Growth", 0.05, step = 0.01),
                         helpText("Annual price growth rate (decimal, e.g., 0.05 = 5%)"),
                         numericInput("add_random_noise", "Add Random Noise to Data", 0.00, step = 0.05, min = 0, max = 1),
                         helpText("Adds random variation to el. price (0.2 means ±20% variation)"),
                         selectInput("selected_year", "Repeat Year from Observations",
                                     choices = c(2008:2023)),
                         numericInput("elprice_lastval", "Provided El. Price (CZK/kWh) to apply (only for Provided Value with Growth)", 3.5, step = 0.1),
                         helpText("Electricity price to apply the growth rate to"),
                         checkboxInput("elprice_add_intraday_variability", "Intraday Variability", TRUE),
                         helpText("Add daily price patterns"),
                         checkboxInput("elprice_add_intraweek_variability", "Intraweek Variability", TRUE),
                         helpText("Add weekday price patterns"))
                
                , tabPanel("Feed-in Tariff", value = "feedinTab",
                         selectInput("feedin_method", "Method", choices = c("Provided Value with Growth" = "last_w_growth")),
                         numericInput("feedin_lastval", "Provided Value (CZK/kWh)", 1.1, min = 0),
                         numericInput("feedin_annual_growth", "Annual Growth", 0.01, step = 0.01),
                         helpText("Annual feed-in tariff growth rate (decimal, e.g., 0.01 = 1%)"))
                
                , tabPanel("Grid Costs", value = "gridcostTab",
                         selectInput("gridcost_method", "Method", 
                                     choices = c("Static" = "static", 
                                                 "Linear" = "linear",
                                                 "Provided Value with Growth" = "last_w_growth",
                                                 "Historical with Growth" = "historical_w_growth" )),
                         numericInput("gridcost_annual_growth", "Annual Growth", 0.04, step = 0.01),
                         helpText("Annual grid cost growth rate (decimal, e.g., 0.04 = 4%)"),
                         numericInput("gridcost_lastval", "Provided Grid Cost (CZK/kWh) to apply (only for Provided Value with Growth)", 2.0, step = 0.1),
                         helpText("Grid cost to apply the growth rate to (in CZK/kWh)")
                         )
           
                , tabPanel("Observe Input Data Charts", value = "chartsTab",
                         plotOutput("elconsPlot"),
                         plotOutput("solarPlot"),
                         plotOutput("gridCostPlot"),
                         plotOutput("feedInPlot"),
                         plotOutput("elpricePlot")
                )
                , tabPanel("Results", value = "resultsTab",
                         h3("Financial Summary"),
                         tableOutput("summary_table"),
                         h3("Hourly Energy Flows"),
                         dateInput("plot_date", "Select Date", value = Sys.Date()),
                         plotlyOutput("energy_plot"),
                         helpText("Plot shows energy flows and battery state for the selected date"),
                         downloadButton("download_summary", "Download Summary (Excel)", class = "btn btn-primary"),
                         downloadButton("download_hourly", "Download Hourly Data (Excel)", class = "btn btn-success")
                )
         
            )
        )
    )
)#end fluidPage