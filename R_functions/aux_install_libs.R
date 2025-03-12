required_libs <- setdiff(c("fable", "feasts",  "forecast", "ggplot2", "httr", "leaflet"
                           , "lubridate", "plotly", "purrr", "Rcpp", "readr", "scales"
                           , "shiny", "shinyjs", "shinythemes", "standardlastprofile"
                           , "tidyverse", "tsibble", "writexl",  "readxl")
                          ,  installed.packages())

install.packages(required_libs)
