####  ui.R
# This is the user-interface definition of a Shiny web application. You can run the application by clicking 'Run App' above.
library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)
library(shinyjs)
library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translations.json")
shinyjs::useShinyjs()
ui <- fluidPage(
   
    tags$footer(style = "position: fixed; 
                bottom: 0; 
                width: 100%; 
                padding: 10px; 
                padding-right: 40px;
                background-color: #f0f0f0; 
                text-align: right; 
                font-size: 0.8em; 
                color: #808080;"
                , "Created by Pavel HAJKO"
    ),
    
    shinyjs::useShinyjs(),
    shiny.i18n::usei18n(i18n), 
    # error display area
    tags$div(id = "error-space", style = "color: red; margin: 10px;"),
    
    # select language input
    div(style = "float: right;",
        selectInput("selected_language", 
                    label = NULL,
                    choices = c("English" = "en", "Česky" = "cs"),
                    selected = "en")
    ),
    
    
    theme = shinytheme("flatly"),
    titlePanel("PV Analyzer"),
    sidebarLayout(
        sidebarPanel(
            h4(textOutput("controls_header")),
            actionButton(inputId = "load_data", label = textOutput("load_btn"), class = "btn-primary"),
            actionButton(inputId = "calculate_financials", label = textOutput("calc_btn"), class = "btn-success"),
            
            #DEBUG change tab
            #actionButton("testSwitch", "Test Tab Switch"),
            
            #button for session reload
            actionButton("reset_app", label = textOutput("reset_btn")),
            
            #checkboxInput("use_cache_data", "Use Cached Data", value = FALSE),
            checkboxInput("fixed_seed", textOutput("fixed_seed_label"), value = TRUE),
            
          
            h4(textOutput("location_header")),
            leafletOutput("map"),
            helpText(textOutput("location_help")),
            numericInput("lat", textOutput("latitude_label"), 49.278),
            numericInput("lon", textOutput("longitude_label"), 16.998),
            
            h4(textOutput("date_range_header")),
            dateInput("start_date", textOutput("start_date_label"), value = "2025-01-01", max = "2028-01-01"),  #max range for HH energy data is 2073-12-31
            numericInput("system_lifetime", textOutput("system_lifetime_label"), value = 20, min = 1, step = 1, max = 25)
        ),
        
        mainPanel(
            #everything in server.R
            uiOutput("translated_tabs") 
            
        )
        
    )
)#end fluidPage