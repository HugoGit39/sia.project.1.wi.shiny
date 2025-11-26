############################################################################################
#
#  Function nodule for feauture filter (extensive)
#
#############################################################################################

mod_feat_fil_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        bs4Card(
          title = "Feature Filter",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          div(
            style = "text-align: center; margin-bottom: 10px;",
            downloadButton(ns("download_filter_settings"), "Download Filter Settings")
          ),
          div(
            style = "text-align: center; margin-bottom: 10px;",
            actionButton(
              inputId = ns("reset_feat_filter"),
              label = "Reset Filter",
              status = "danger",
              outline = TRUE,
              size = "sm",
              flat = TRUE,
              icon = NULL,
              block = TRUE,
              width = "50%",
              style = "border-width: 2px"
            )
          ),

          # * SiA Expert Scores ----
          bs4Card(
            title = "SiA Expert Score",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            collapsible = FALSE,
            sliderInput(
              ns("long_term_all_score"),
              label = "Long-Term Expert Score",
              min   = 0,
              max   = 10,
              value = c(0, 10),
              step  = 0.1
            ),
            sliderInput(
              ns("short_term_all_score"),
              label = "Short-Term Expert Score",
              min   = 0,
              max   = 10,
              value = c(0, 10),
              step  = 0.1
            ),
            tags$div(
              tags$label("Exclude missing SiA scores"),
              switchInput(
                inputId = ns("exclude_na_sia"),
                onLabel = "YES", offLabel = "NO",
                value = FALSE, size = "sm",
                onStatus = "secondary", offStatus = "primary"
              )
            )
          ),

          # * General Device Information ----
          bs4Card(
            title = "General Device Information",
            width = 12,
            status = "secondary",
            collapsible = FALSE,
            selectInput(ns("manufacturer"), "Manufacturer", choices = NULL, multiple = TRUE),
            selectInput(ns("model"), "Model", choices = NULL, multiple = TRUE),
            airDatepickerInput(
              ns("release_year"),
              "Release Year Range",
              range = TRUE,
              view = "years",
              minView = "years",
              dateFormat = "yyyy",
              value = c(min(sia_df$release_year, na.rm = TRUE),
                        max(sia_df$release_year, na.rm = TRUE))
            ),
            selectInput(ns("market_status"), "Market Status", choices = NULL, multiple = TRUE),
            selectInput(ns("main_use"), "Main Use", choices = NULL, multiple = TRUE),
            sliderInput(
              ns("device_cost"),
              label = "Device Cost (€)",
              min   = 0,
              max   = max(sia_df$device_cost, na.rm = TRUE),
              value = c(0, max(sia_df$device_cost, na.rm = TRUE)),
              step  = 1
            ),
            selectInput(ns("wearable_type"), "Type", choices = NULL, multiple = TRUE),
            selectInput(ns("location"), "Location", choices = NULL, multiple = TRUE)
          ),

          # * Technical Specifications ----
          bs4Card(
            title = "Technical Specifications",
            width = 12,
            status = "secondary",
            collapsible = FALSE,
            prettyCheckbox(ns("water_resistance_spec_boel_value"), label = "Water Resistant", icon = icon("check"), status = "primary"),
            sliderInput(
              ns("battery_life_spec_num_value"),
              label = "Battery Life (hrs)",
              min   = 0,
              max   = max(sia_df$battery_life_spec_num_value, na.rm = TRUE),
              value = c(0, max(sia_df$battery_life_spec_num_value, na.rm = TRUE)),
              step  = 1
            ),
            sliderInput(
              ns("charging_duration_spec_num_value"),
              label = "Charging Duration (min)",
              min   = 0,
              max   = max(sia_df$charging_duration_spec_num_value, na.rm = TRUE),
              value = c(0, max(sia_df$charging_duration_spec_num_value, na.rm = TRUE)),
              step  = 1
            ),
            prettyCheckbox(ns("bio_cueing_spec_boel_value"), label = "Bio Cueing", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("bio_feedback_spec_boel_value"), label = "Bio Feedback", icon = icon("check"), status = "primary")
          ),

          # * Signals ----
          bs4Card(
            title = "Signals",
            width = 12,
            status = "secondary",
            collapsible = FALSE,
            prettyCheckbox(ns("accelerometer_available"), label = "Accelerometer", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("bp_available"), label = "Blood Pressure", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("ecg_available"), label = "Electrocardiogram (ECG)", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("eda_available"), label = "Electrodermal Activity (EDA)", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("eeg_available"), label = "Electroencephalography (EEG)", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("emg_available"), label = "Electromyography (EMG)", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("gps_available"), label = "Global Positioning System (GPS)", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("gyroscope_available"), label = "Gyroscope", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("icg_available"), label = "Impedance Cardiography (ICG)", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("ppg_available"), label = "Photoplethysmogram (PPG)", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("respiration_available"), label = "Respiration", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("skin_temperature_available"), label = "Skin Temperature", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("other_signals_available"), label = "Other Signals", icon = icon("check"), status = "primary")
          ),

          # * Data Access ----
          bs4Card(
            title = "Data Access",
            width = 12,
            status = "secondary",
            collapsible = FALSE,
            prettyCheckbox(ns("raw_data_available_spec_boel_value"), label = "Raw Data Available", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("int_storage_met_spec_boel_value"), label = "Internal Storage", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("server_data_storage_spec_boel_value"), label = "Server Storage", icon = icon("check"), status = "primary"),
            sliderInput(
              ns("dev_storage_cap_mb_spec_num_value"),
              label = "Storage Capacity (MB)",
              min   = 0,
              max   = max(sia_df$dev_storage_cap_mb_spec_num_value, na.rm = TRUE),
              value = c(0, max(sia_df$dev_storage_cap_mb_spec_num_value, na.rm = TRUE)),
              step  = 1
            ),
            sliderInput(
              ns("dev_storage_cap_hr_spec_num_value"),
              label = "Storage Capacity (hrs)",
              min   = 0,
              max   = max(sia_df$dev_storage_cap_hr_spec_num_value, na.rm = TRUE),
              value = c(0, max(sia_df$dev_storage_cap_hr_spec_num_value, na.rm = TRUE)),
              step  = 1
            ),
            prettyCheckbox(ns("gdpr_compliance_spec_boel_value"), label = "GDPR Compliant", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("fda_clearance_spec_boel_value"), label = "FDA Cleared", icon = icon("check"), status = "primary"),
            prettyCheckbox(ns("ce_marking_spec_boel_value"), label = "CE Marked", icon = icon("check"), status = "primary")
          ),

          # * Validation, Reliability & Usability ----
          bs4Card(
            title = "Validation, Reliability & Usability",
            width = 12,
            status = "secondary",
            collapsible = FALSE,
            sliderInput(
              ns("usability_n_of_studies"),
              label = "# Usability Studies",
              min   = 0,
              max   = max(sia_df$usability_n_of_studies, na.rm = TRUE),
              value = c(0, max(sia_df$usability_n_of_studies, na.rm = TRUE)),
              step  = 1
            ),
            sliderInput(
              ns("validity_and_reliability_n_of_studies"),
              label = "# Validity & Reliability Studies",
              min   = 0,
              max   = max(sia_df$validity_and_reliability_n_of_studies, na.rm = TRUE),
              value = c(0, max(sia_df$validity_and_reliability_n_of_studies, na.rm = TRUE)),
              step  = 1
            ),
            selectInput(ns("usability_evidence_level"), "Usability Evidence Level", choices = NULL, multiple = TRUE),
            selectInput(ns("validity_and_reliability_evidence_level"), "Validity & Reliability Evidence Level", choices = NULL, multiple = TRUE)
          )
        )
      ),

      # * Filtered results panel ----
      column(
        width = 9,
        bs4Card(
          title = "Filtered Results",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          div(
            style = "text-align: center; margin-bottom: 10px;",
            downloadButton(ns("download_data"), "Download Filtered Results")
          ),
          reactableOutput(ns("feat_filtered_table")) %>% withSpinner(),
          footer = tags$div(
            "Source: Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. ",
            "Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information. ",
            tags$em("Behav Res Methods."),
            " 2025 May 13;57(6):171. doi: ",
            tags$a(href = "https://link.springer.com/article/10.3758/s13428-025-02685-4",
                   target = "_blank", "10.3758/s13428-025-02685-4"),
            "; PMID: 40360861; PMCID: ",
            tags$a(href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12075381/",
                   target = "_blank", "PMC12075381"),
            style = "font-family: sans-serif; font-size: 10pt; color: #8C8C8C;"
          )
        )
      )
    )
  )
}


############################################################################################
#
#  Module for feauture filter (extensive)
#
#############################################################################################

mod_feat_fil_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. Variable groups (reuse from global.R) ----
    range_vars   <- setdiff(c(bar_vars, numeric_vars), "weight_gr")
    checkbox_vars <- yn_vars
    select_inputs <- setdiff(intersect(names(sia_df), char_vars), c("release_year", "size_mm"))

    # --- 2. Filter for dropdowns ----
    filtered_for_dropdowns <- reactive({
      df <- data()

      # Apply range filters
      for (var in range_vars) {
        df <- range_filter(df, var, input[[var]])
      }

      # Release year range
      if (!is.null(input$release_year[1]) && !is.null(input$release_year[2])) {
        df <- df %>%
          filter(
            is.na(release_year) |
              (year(release_year) >= year(input$release_year[1]) &
                 year(release_year) <= year(input$release_year[2]))
          )
      }

      # Apply checkboxes
      for (var in checkbox_vars) {
        df <- checkbox_filter(df, var, input[[var]])
      }

      # Exclude missing SiA scores if needed
      if (isTRUE(input$exclude_na_sia)) {
        df <- df %>%
          filter(!is.na(short_term_all_score), !is.na(long_term_all_score))
      }

      df
    })

    # --- 3. Dynamically update selectInput options ----
    observe({
      df <- filtered_for_dropdowns()
      for (input_id in select_inputs) {
        df_filtered <- df
        for (other_id in setdiff(select_inputs, input_id)) {
          selected <- input[[other_id]]
          if (!is.null(selected)) {
            df_filtered <- select_filter(df_filtered, other_id, selected)
          }
        }
        valid_choices <- sort(unique(df_filtered[[input_id]]))
        selected_now <- input[[input_id]]
        updateSelectInput(
          session, input_id,
          choices = valid_choices,
          selected = selected_now[selected_now %in% valid_choices]
        )
      }
    })

    # --- 4. Apply full filtering ----
    filtered_data <- reactive({
      df <- filtered_for_dropdowns()
      for (var in select_inputs) {
        df <- select_filter(df, var, input[[var]])
      }
      df
    })

    # --- 5. Reset filters ----
    observeEvent(input$reset_feat_filter, {
      for (var in range_vars) {
        if (var %in% c("long_term_all_score", "short_term_all_score")) {
          updateSliderInput(session, var, value = c(0, 10))
        } else {
          updateSliderInput(session, var, value = c(0, max(sia_df[[var]], na.rm = TRUE)))
        }
      }

      updateAirDateInput(session, "release_year",
                         value = c(
                           min(sia_df$release_year, na.rm = TRUE),
                           max(sia_df$release_year, na.rm = TRUE)
                         ))

      lapply(checkbox_vars, function(id) updatePrettyCheckbox(session, id, value = FALSE))
      lapply(select_inputs, function(id) updateSelectInput(session, id, selected = character(0)))
      updateSwitchInput(session, "exclude_na_sia", value = FALSE)
    })

    # --- 6. Render filtered reactable ----
    output$feat_filtered_table <- renderReactable({
      df <- filtered_data()

      # Format release year
      if ("release_year" %in% names(df)) {
        df$release_year <- format(df$release_year, "%Y")
      }

      # Create reactable column definitions
      bar_column_defs     <- func_bar_column_defs(df, bar_vars, rename_map)
      yn_column_defs      <- func_yn_column_defs(yn_vars, rename_map)
      numeric_column_defs <- func_numeric_column_defs(df, numeric_vars, rename_map, numeric_var_ranges)
      char_column_defs    <- func_char_column_defs(char_vars, rename_map)

      # Render table
      reactable(
        df,
        columns = c(
          list(
            manufacturer = colDef(
              name = "Manufacturer",
              sticky = "left",
              style = sticky_style,
              headerStyle = sticky_style,
              minWidth = 180
            ),
            model = colDef(
              name = "Model",
              sticky = "left",
              style = sticky_style,
              headerStyle = sticky_style,
              minWidth = 180
            )
          ),
          bar_column_defs,
          yn_column_defs,
          numeric_column_defs,
          char_column_defs
        ),
        bordered = TRUE,
        highlight = TRUE,
        pagination = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        fullWidth = TRUE,
        striped = FALSE,
        defaultSorted = "manufacturer",
        style = list(maxHeight = "1000px", overflowY = "auto")
      )
    })

    # --- 7. Download filtered data ----
    output$download_data <- downloadHandler(
      filename = function() paste0("sia_feature_filter_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE, na = "")
        cat(
          "\n# Citation terms.\n",
          "# Thank you for using the Stress-in-Action Wearable Database!\n",
          "# If you use the SiA-WD and/or this web app you must cite:\n",
          "# Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical reliability validity and usability information. Behav Res Methods. 2025 May 13 57(6):171. doi: 10.3758/s13428-025-02685-4. PMID: 40360861 PMCID: PMC12075381.\n",
          file = file, append = TRUE, sep = ""
        )
      },
      contentType = "text/csv"
    )

    # --- 8. Download filter settings ----
    output$download_filter_settings <- downloadHandler(
      filename = function() paste0("sia_filter_settings_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        settings <- list()

        # Sliders
        for (var in range_vars) {
          range_vals <- as.integer(round(input[[var]]))
          settings[[var]] <- paste(range_vals[1], range_vals[2], sep = "|")
        }

        # Checkboxes
        for (var in checkbox_vars) {
          settings[[var]] <- if (isTRUE(input[[var]])) "YES" else "YES|NO"
        }

        # SelectInputs
        for (var in select_inputs) {
          settings[[var]] <- paste(input[[var]], collapse = "|")
        }

        # Date range
        settings[["release_year"]] <- paste(
          format(input$release_year[1], "%Y"),
          format(input$release_year[2], "%Y"),
          sep = "|"
        )

        # Exclude NA SiA
        settings[["exclude_na_sia"]] <- if (isTRUE(input$exclude_na_sia)) "YES" else "YES|NO"

        # Convert to data frame
        df <- data.frame(t(unlist(settings)), check.names = FALSE)
        names(df) <- names(settings)

        write.table(df, file, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
        cat(
          "\n# Citation terms.\n",
          "# Thank you for using the SiA-WD!\n",
          "# If you use the database and/or this web app, you must cite:\n",
          "# Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical reliability, validity, and usability information. Behav Res Methods. 2025 May 13;57(6):171. doi: 10.3758/s13428-025-02685-4.\n",
          file = file, append = TRUE, sep = ""
        )
      },
      contentType = "text/csv"
    )
  })
}



