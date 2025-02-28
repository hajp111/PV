####  ui.R
# This is the user-interface definition of a Shiny web application. You can run the application by clicking 'Run App' above.
library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)

ui <- fluidPage(
    # Add error display area
    tags$div(id = "error-space", style = "color: red; margin: 10px;"),
    
    theme = shinytheme("flatly"),
    titlePanel("Solar Battery System Analyzer"),
    sidebarLayout(
        sidebarPanel(
            h4("Controls"),
            
            actionButton("load_data", "1. Load Energy Data", class = "btn-primary"),
            actionButton("calculate_financials", "2. Calculate Financials", class = "btn-success"),
            
            checkboxInput("use_cache_data", "Use Cached Data", value = FALSE),
            checkboxInput("fixed_seed", "Fixed Seed", value = TRUE),
            
            h4("Location"),
            leafletOutput("map"),
            helpText("Click on the map to set your location or enter coordinates manually"),
            numericInput("lat", "Latitude", 49.278),
            numericInput("lon", "Longitude", 16.998),
            
            h4("Date Range"),
            dateInput("start_date", "Start Date", value = "2022-01-01"),
            numericInput("system_lifetime", "System Lifetime (years)",
                         value = 2, min = 1, step = 1)
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Battery Parameters",
                         numericInput("battery_capacity_kwh", "Capacity (kWh)", 10, min = 0),
                         numericInput("battery_charge_efficiency", "Charge Efficiency", 0.95, min = 0, max = 1, step = 0.01),
                         helpText("Value between 0 and 1. When charging with E kWh, stored energy is E × efficiency"),
                         numericInput("battery_discharge_efficiency", "Discharge Efficiency", 0.95, min = 0, max = 1, step = 0.01),
                         helpText("Value between 0 and 1. When discharging E kWh, released energy is E × efficiency"),
                         numericInput("battery_initial_soc", "Initial SOC (%)", 50, min = 0, max = 100),
                         helpText("Initial state of charge as percentage of battery capacity"),
                         numericInput("battery_min_soc", "Min SOC (%)", 10, min = 0, max = 100),
                         helpText("Minimum state of charge to avoid battery damage"),
                         numericInput("battery_max_soc", "Max SOC (%)", 100, min = 0, max = 100),
                         helpText("Maximum state of charge (100% = full capacity)"),
                         numericInput("battery_degradation", "Degradation Rate/year", 0.01, min = 0, max = 1, step = 0.001),
                         helpText("Annual battery capacity degradation rate (decimal, e.g., 0.01 = 1%)")),
                
                tabPanel("PV Parameters",
                         numericInput("PV_peakpower", "Peak Power (kWp)", 4.5, min = 0),
                         numericInput("PV_system_loss", "System Loss (%)", 14, min = 0, max = 100),
                         helpText("Overall system losses in percent"),
                         numericInput("PV_angle", "Inclination Angle", 30, min = 0, max = 90),
                         helpText("Panel inclination (0° = horizontal, 90° = vertical)"),
                         numericInput("PV_aspect", "Azimuth Angle", 0, min = -180, max = 180),
                         helpText("Panel orientation (0° = South, 90° = West, -90° = East)"),
                         numericInput("PV_degradation", "Degradation Rate/year", 0.01, min = 0, max = 1, step = 0.001),
                         helpText("Annual PV panel degradation rate (decimal, e.g., 0.01 = 1%)"),
                         numericInput("PV_system_own_consumption", "System Consumption (kWh/h)", 0.03, min = 0),
                         helpText("Constant energy consumption of the PV system (inverter, etc.)"),
                         numericInput("PV_add_PV_noise", "PV Noise Multiplier", 0.0, min = 0),
                         helpText("Adds random variation to PV output (0.2 means ±20% variation)")),
                
                tabPanel("Financials",
                         numericInput("installation_cost", "Installation Cost (CZK)", 200000, min = 0),
                         numericInput("annual_maintenance_cost", "Annual Maintenance (CZK)", 4000, min = 0),
                         numericInput("discount_rate", "Discount Rate", 0.03, min = 0, max = 1, step = 0.01),
                         helpText("Annual discount rate for NPV calculations (decimal, e.g., 0.03 = 3%)")),
                
                tabPanel("Household",
                         numericInput("HH_annual_consumption", "Annual Consumption (MWh)", 3, min = 0),
                         numericInput("HH_add_cons_multiplier", "Consumption Noise Multiplier", 0.0, min = 0),
                         helpText("Adds random variation to consumption (0.2 means ±20% variation)")),
                
                tabPanel("Electricity Prices",
                         selectInput("elprice_method", "Price Method",
                                     choices = c("static", "linear", "historical", "historical_w_growth",
                                                 "random_walk", "random_walk_trend", "mean_reverting_rw")),
                         helpText("Method for electricity price projection"),
                         numericInput("elprice_annual_growth", "Annual Growth", 0.05, step = 0.01),
                         helpText("Annual price growth rate (decimal, e.g., 0.05 = 5%)"),
                         checkboxInput("elprice_add_intraday_variability", "Intraday Variability", TRUE),
                         helpText("Add daily price patterns"),
                         checkboxInput("elprice_add_intraweek_variability", "Intraweek Variability", TRUE),
                         helpText("Add weekday price patterns")),
                
                tabPanel("Feed-in Tariff",
                         selectInput("feedin_method", "Method", choices = c("last_w_growth")),
                         numericInput("feedin_lastval", "Last Value (CZK/kWh)", 1.1, min = 0),
                         numericInput("feedin_annual_growth", "Annual Growth", 0.01, step = 0.01),
                         helpText("Annual feed-in tariff growth rate (decimal, e.g., 0.01 = 1%)")),
                
                tabPanel("Grid Costs",
                         selectInput("gridcost_method", "Method", 
                                     choices = c("static", "linear", "last_w_growth")),
                         numericInput("gridcost_annual_growth", "Annual Growth", 0.04, step = 0.01),
                         helpText("Annual grid cost growth rate (decimal, e.g., 0.04 = 4%)")),
                
                # output results
                tabPanel("Results",
                         dateInput("plot_date", "Select Date", value = Sys.Date()),
                         h3("Financial Summary"),
                         tableOutput("summary_table"),
                         h3("Hourly Energy Flows"),
                         plotlyOutput("energy_plot"),
                         helpText("Plot shows energy flows and battery state for the selected date"))
            )
        )
    )
)#end fluidPage