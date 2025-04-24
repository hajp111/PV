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

# This file is part of the PV Analyzer project
# Copyright (c) 2025 Pavel HAJKO
# See the license.txt file in the project root