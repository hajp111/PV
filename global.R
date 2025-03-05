# global.R
#options(verbose = TRUE)  # Show detailed loading messages

library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
#library(writexl)

print("this is global.R")
# Source your existing code
#print("global: sourcing hourly_flows_rcpp.R")
source("R_functions/hourly_flows_rcpp.R")
#print("global: sourcing read_irradiance_api.R")
source("R_functions/read_irradiance_api.R")
#print("global: sourcing residential_electricity_load_profile.R")
source("R_functions/residential_electricity_load_profile.R")
#print("global: sourcing electricity_price.R")
source("R_functions/electricity_price.R")

# Helper function to convert ggplot to plotly with secondary axis
ggplotly_with_secondary <- function(p) {
    # Create plotly object
    ply <- ggplotly(p)
    
    # Add secondary axis
    ply %>% layout(
        yaxis2 = list(
            overlaying = "y",
            side = "right",
            title = "Battery SoC (kWh)"
        )
    )
}#endfunction ggplotly_with_secondary

# Validate percentage inputs
validate_min_max <- function(input_id, min_val, max_val, error_msg) {
    if (input[[input_id]] < min_val || input[[input_id]] > max_val) {
        showNotification(
            error_msg,
            type = "error",
            duration = 5
        )
        return(FALSE)
    }#endif
    return(TRUE)
}#endfunction validate_min_max
