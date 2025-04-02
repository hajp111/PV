####  server.R
library(shinyjs)
library(shiny.i18n)
#gcinfo(TRUE)

# Initialize i18n translator
i18n <- Translator$new(translation_json_path = "translations.json")
i18n$set_translation_language("en") 

server <- function(input, output, session) {
    
    print("hiding calculate_financials button")
  
    shinyjs::hide("calculate_financials")
    # hide the results tabs 
    hideTab(inputId = "mainPanelTabs", target = "chartsTab")
    hideTab(inputId = "mainPanelTabs", target = "resultsTab")

    # observe language changes
    lang_reactive <- reactiveVal("en")
    observeEvent(input$selected_language, {
      lang_reactive(input$selected_language)
      i18n$set_translation_language(input$selected_language)
    })
    
    observe({
      req(lang_reactive())  
      # modal with instructions on app start
      showModal(modalDialog(
        title = i18n$t("Welcome to the PV Analyzer"),
        i18n$t("Please follow these steps:"),
        tags$ol(
          tags$li(i18n$t("Browse the tabs and set the values of parameters.")),
          tags$li(i18n$t("Click the 'Load Energy Data' button and wait until the data is loaded.")),
          tags$li(i18n$t("Click the 'Calculate Financials' button and wait until the calculations are done")),
          tags$li(i18n$t("See the results on the Results tab."))
        ),
        footer = modalButton("OK")
      ))#end modal
    })
    
    # dynamic UI updates
    output$title <- renderText({ i18n$t("PV Analyzer") })
    output$load_btn <- renderText({ i18n$t("1. Load Energy Data") })
    output$calc_btn <- renderText({ i18n$t("2. Calculate Financials") })
    output$reset_btn <- renderText({ i18n$t("Reset App") })
    
    
    
    # reset button 
    observeEvent(input$reset_app, {
      session$reload()
    })
    
    
    # reactive values to store intermediate results
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
    
    
    
    # render map
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

    
    # replace grid_cost last value with 0 if negative
    observeEvent(input$gridcost_lastval, {
      if (input$gridcost_lastval < 0) {
        updateNumericInput(session, "gridcost_lastval", value = 0)
        showNotification("Grid cost must be non-negative. Value reset to 0.", type = "warning")
      }#endif
    })
    #replace feed_in last value with 0 if negative
    observeEvent(input$feedin_lastval, {
      if (input$feedin_lastval < 0) {
        updateNumericInput(session, "feedin_lastval", value = 0) 
        showNotification("Feed-in must be non-negative. Value reset to 0.", type = "warning") 
      }#endif
    })
    
    # first button - Load Energy Data
    observeEvent(input$load_data, {
        #showModal(modalDialog("Loading energy data...", footer = NULL))
        
      # valid range for system lifetime
      min_val <- 1
      max_val <- 25
      user_val <- input$system_lifetime
      if (user_val < min_val) {
        updateNumericInput(session, "system_lifetime", value = min_val)
        validsyslife <- min_val
        showNotification("System Lifetime enterd was out of bounds, set to minimum allowed.", type = "warning")
      } else if (user_val > max_val) {
        updateNumericInput(session, "system_lifetime", value = max_val)
        validsyslife <- max_val
        showNotification("System Lifetime enterd was out of bounds, set to maximum allowed.", type = "warning")
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
                discount_rate = input$discount_rate,
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
            
            
            showModal(modalDialog("Loading solar and energy consumption data...", footer = NULL))
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
            showModal(modalDialog("Calculating expected energy flows...", footer = NULL))
            
            
            energy_flows(CalculateEnergyFlows(df1, sp)) 
            #df1 not needed anymore
            rm(df1)
            print("energy_flows done")
            gc(full = TRUE)
            #glimpse(energy_flows)
            
            showNotification("Energy data loaded successfully", type = "message")
            #hide load data button
            shinyjs::hide("load_data")
            print("hiding load data button")
            #hide parameters affecting PV and HH cons
            removeTab("mainPanelTabs", target = "batteryTab")
            removeTab("mainPanelTabs", target = "pvTab")
            removeTab("mainPanelTabs", target = "householdTab")
            #disable inputs that shouldn't be editable anymore
            shinyjs::disable("system_lifetime")
            shinyjs::disable("lat")
            shinyjs::disable("lon")
            shinyjs::disable("start_date")
            
            # show button for 2nd step
            shinyjs::show("calculate_financials")
           
        }, error = function(e) {
            showNotification(paste("Error loading data:", e$message), type = "error", duration = 15)
        }, finally = {
            removeModal()
          endata_loaded(TRUE)  # Set flag to block future clicks
          print("step1 done")
        })
    })
    
    # Second button - Load Financial Data and Calculate
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
            discount_rate = input$discount_rate,
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
          print("sys params loaded")
          
          showModal(modalDialog("Loading price-related data...", footer = NULL))
            
            
                grid_cost <- my_gridcost(
                    my_data_read_distrib_costs_observed_data()
                    , startdate = sp$start_date,
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
          showModal(modalDialog("Calculating financials...", footer = NULL))
          
            final_results( CalculateFinancials(
                energy_flows(), elprice$price_data, feed_in$feed_in, grid_cost$grid_cost, params = sp
            ))
            
            print("final_results done")
            rm(elprice, feed_in, grid_cost)
            gc(full = TRUE)
            calculations_done(TRUE)
            showNotification("Financial calculations complete", type = "message")
            
            
        }, error = function(e) {
            showNotification(paste("Error in financial calculations:", e$message), type = "error", duration = 15)
        }, finally = {
            
            showTab(inputId = "mainPanelTabs", target = "chartsTab")
            showTab(inputId = "mainPanelTabs", target = "resultsTab")
            removeModal()
            print("step2 done")
        })
    })
    
    # Render summary table
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
                                                               , annualized_rate_of_return_alt #should be the same as annualized_rate_of_return
                                                              , breakeven_feedin
                                                              , breakeven_price
                                                              )
    #show this as a table:
    transpose_this %>% mutate(across(everything(), as.character)) %>%
     pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
    })#end renderTable summary_table
    
    # interactive plot for daily overview:
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
        )
        
        ply <- plot_ly(df, x = ~hour) %>%
          # primariy axis
          add_trace(y = ~PV_available, name = "PV Available", type = "scatter", mode = "lines",
                    line = list(color = colors[["PV Available"]])) %>%
          add_trace(y = ~total_demand, name = "Total Demand", type = "scatter", mode = "lines",
                    line = list(color = colors[["Total Demand"]])) %>%
          add_trace(y = ~grid_import, name = "Grid Import", type = "scatter", mode = "lines",
                    line = list(color = colors[["Grid Import"]])) %>%
          add_trace(y = ~grid_export, name = "Grid Export", type = "scatter", mode = "lines",
                    line = list(color = colors[["Grid Export"]])) %>%
          # secondary axis (for battery SoC)
          add_trace(y = ~battery_soc, name = "Battery SoC", type = "scatter", mode = "lines",
                    line = list(color = colors[["Battery SoC"]], dash = "dot"),
                    yaxis = "y2") %>%
          #labs
          layout(
            title = list(text = "Energy Flow (Hourly)", font = list(size = 12)),
            xaxis = list(title = "Hour", tickmode = "linear", dtick = 1), 
            yaxis = list(title = "Energy (kWh)"
                         , side = "left", showgrid = TRUE
                         , titlefont = list(size = 10),
                         tickfont = list(size = 8)),
            yaxis2 = list(
              title = "Battery SoC (kWh)",
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
    
    
    # render plots of inputs
    output$elconsPlot <- renderPlot({
        shiny::req( final_results()$df_hourly )
      
      final_results()$df_hourly %>%
        group_by(date, year) %>% 
        summarize(cons_kWh = sum(cons_kWh)) %>%
        ggplot() + 
        geom_line(aes(x=date, y = cons_kWh), alpha = 0.5) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
        theme_minimal()
    })
    
    output$solarPlot <- renderPlot({
         shiny::req( final_results()$df_hourly )
         
         final_results()$df_hourly %>%
           group_by(date, year, month, day, weekday, is_weekend) %>% 
           summarize(P_kWh = P_kWh %>% sum(na.rm = TRUE)
           ) %>% 
           ggplot() +
           geom_line(aes(x=date, y = P_kWh), alpha = 0.5) +
           scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
           theme_minimal()
         
    })
    
    output$gridCostPlot <- renderPlot({
      shiny::req( final_results()$df_hourly)
      
      
      final_results()$df_hourly %>%
        group_by(year) %>% 
        summarize(grid_cost = grid_cost %>% mean(na.rm = TRUE)
        ) %>% 
        ggplot() +
        geom_line(aes(x=year, y = grid_cost), alpha = 0.5) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
        theme_minimal()
        
      
    })
    
    output$feedInPlot <- renderPlot({
      shiny::req( final_results()$df_hourly)
      
      final_results()$df_hourly %>%
        group_by(year) %>% 
        summarize(feed_in = feed_in %>% mean(na.rm = TRUE)
        ) %>% 
        ggplot() +
        geom_line(aes(x=year, y = feed_in), alpha = 0.5) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
        theme_minimal()
    })
    
    output$elpricePlot <- renderPlot({
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
        ggplot() +
        geom_boxplot(aes( x= year %>% factor(), y = price)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(x = "Year",
             y = "Price"
        )
      
    })
    
    
    #DEBUG 
    # observeEvent(input$testSwitch, {
    #     updateTabsetPanel(session, "mainPanelTabs", selected = "Results")
    #     print("Test switch tab")
    # })
    
    # navigate to Results tab
    observe({
        print(paste("observe final_results: final_results is", ifelse(is.null(final_results()), "NULL", "calculated"))) 
        if (!is.null(final_results())) {
            updateTabsetPanel(session, "mainPanelTabs", selected = "resultsTab")
        }
    })
    
    
    #buttons for downloads of data:
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
}