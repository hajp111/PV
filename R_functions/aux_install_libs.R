required_libs <- setdiff(c("fable", "feasts",  "forecast", "tsibble"
                           , "ggplot2", "plotly", "scales"
                           , "shiny", "shinyjs", "shinythemes", "leaflet"
                           , "standardlastprofile"
                           , "tidyverse"
                           , "httr", "lubridate",  "purrr", "Rcpp", "readr"
                           ,  "openxlsx"
                           , "shiny.i18n"
                           #, "writexl",  "readxl"
                           )
                          ,  installed.packages())

install.packages(required_libs, dependencies = TRUE)
