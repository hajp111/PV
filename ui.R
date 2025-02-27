####  ui.R
# This is the user-interface definition of a Shiny web application. You can run the application by clicking 'Run App' above.
library(shiny)
library(leaflet)
#
source("global.R")

ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Solar Battery System Analyzer"),
    sidebarLayout(
        sidebarPanel(
            h4("Controls"),
            actionButton("calculate", "Calculate", class = "btn-primary"),
            checkboxInput("use_cache_data", "Use Cached Data", value = FALSE),
            checkboxInput("fixed_seed", "Fixed Seed", value = TRUE),
            
            h4("Location"),
            leafletOutput("map"),
            numericInput("lat", "Latitude", 49.278),
            numericInput("lon", "Longitude", 16.998),
            
            h4("Date Range"),
            dateInput("start_date", "Start Date", value = "2022-01-01"),
            numericInput("system_lifetime", "System Lifetime (years)"
                         , value = 2, min = 1, step = 1)
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Battery Parameters",
                         numericInput("battery_capacity_kwh", "Capacity (kWh)", 10),
                         numericInput("battery_charge_efficiency", "Charge Efficiency", 0.95, step = 0.01),
                         numericInput("battery_discharge_efficiency", "Discharge Efficiency", 0.95, step = 0.01),
                         numericInput("battery_initial_soc", "Initial SOC (%)", 50, min = 0, max = 100),
                         numericInput("battery_min_soc", "Min SOC (%)", 10),
                         numericInput("battery_max_soc", "Max SOC (%)", 100),
                         numericInput("battery_degradation", "Degradation Rate/year", 0.01, step = 0.001)),
                
                tabPanel("PV Parameters",
                         numericInput("PV_peakpower", "Peak Power (kWp)", 4.5),
                         numericInput("PV_system_loss", "System Loss (%)", 14),
                         numericInput("PV_angle", "Inclination Angle", 30),
                         numericInput("PV_aspect", "Azimuth Angle", 0),
                         numericInput("PV_degradation", "Degradation Rate/year", 0.01, step = 0.001),
                         numericInput("PV_system_own_consumption", "System Consumption (kWh/h)", 0.03),
                         numericInput("PV_add_PV_noise", "PV Noise Multiplier", 0.0)),
                
                tabPanel("Financials",
                         numericInput("installation_cost", "Installation Cost (CZK)", 200000),
                         numericInput("annual_maintenance_cost", "Annual Maintenance (CZK)", 4000),
                         numericInput("discount_rate", "Discount Rate", 0.03, step = 0.01)),
                
                tabPanel("Household",
                         numericInput("HH_annual_consumption", "Annual Consumption (MWh)", 3),
                         numericInput("HH_add_cons_multiplier", "Consumption Noise Multiplier", 0.0)),
                
                tabPanel("Electricity Prices",
                         selectInput("elprice_method", "Price Method",
                                     choices = c("static", "linear", "historical", "historical_w_growth",
                                                 "random_walk", "random_walk_trend", "mean_reverting_rw")),
                         numericInput("elprice_annual_growth", "Annual Growth", 0.05),
                         checkboxInput("elprice_add_intraday_variability", "Intraday Variability", TRUE),
                         checkboxInput("elprice_add_intraweek_variability", "Intraweek Variability", TRUE)),
                
                tabPanel("Feed-in Tariff",
                         selectInput("feedin_method", "Method", choices = c("last_w_growth")),
                         numericInput("feedin_lastval", "Last Value (CZK/kWh)", 1.1),
                         numericInput("feedin_annual_growth", "Annual Growth", 0.01)),
                
                tabPanel("Grid Costs",
                         selectInput("gridcost_method", "Method", 
                                     choices = c("static", "linear", "last_w_growth")),
                         numericInput("gridcost_annual_growth", "Annual Growth", 0.04)),
                # output results
                tabPanel("Results",
                         dateInput("plot_date", "Select Date", value = Sys.Date()),
                         tableOutput("summary_table"),
                         plotlyOutput("energy_plot")
                )
            )
        )
    )
)#end fluidPage
