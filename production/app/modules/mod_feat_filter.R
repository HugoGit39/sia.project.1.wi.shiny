############################################################################################
#
# Module for feature filter
#
# Stress in Action 2025
#
#############################################################################################

# Feature Filter Module (UI)
mod_feat_fil_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        div(
          style = "
            max-height: calc(100vh - 0px);  /* adjust if you have navbar/footer */
            overflow-y: auto;
          ",
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
              div(
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
                value = c(
                  min(df_sia_shiny_filters$release_year, na.rm = TRUE),
                  max(df_sia_shiny_filters$release_year, na.rm = TRUE)
                )
              ),
              selectInput(ns("market_status"), "Market Status", choices = NULL, multiple = TRUE),
              selectInput(ns("main_use"), "Main Use", choices = NULL, multiple = TRUE),
              sliderInput(
                ns("device_cost"),
                label = "Device Cost (€)",
                min   = 0,
                max   = max(df_sia_shiny_filters$device_cost, na.rm = TRUE),
                value = c(0, max(df_sia_shiny_filters$device_cost, na.rm = TRUE)),
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
                max   = max(df_sia_shiny_filters$battery_life_spec_num_value, na.rm = TRUE),
                value = c(0, max(df_sia_shiny_filters$battery_life_spec_num_value, na.rm = TRUE)),
                step  = 1
              ),
              sliderInput(
                ns("charging_duration_spec_num_value"),
                label = "Charging Duration (min)",
                min   = 0,
                max   = max(df_sia_shiny_filters$charging_duration_spec_num_value, na.rm = TRUE),
                value = c(0, max(df_sia_shiny_filters$charging_duration_spec_num_value, na.rm = TRUE)),
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
              prettyCheckbox(ns("ecg_available"), label = "ECG", icon = icon("check"), status = "primary"),
              prettyCheckbox(ns("eda_available"), label = "EDA", icon = icon("check"), status = "primary"),
              prettyCheckbox(ns("eeg_available"), label = "EEG", icon = icon("check"), status = "primary"),
              prettyCheckbox(ns("emg_available"), label = "EMG", icon = icon("check"), status = "primary"),
              prettyCheckbox(ns("gps_available"), label = "GPS", icon = icon("check"), status = "primary"),
              prettyCheckbox(ns("gyroscope_available"), label = "Gyroscope", icon = icon("check"), status = "primary"),
              prettyCheckbox(ns("icg_available"), label = "ICG", icon = icon("check"), status = "primary"),
              prettyCheckbox(ns("ppg_available"), label = "PPG", icon = icon("check"), status = "primary"),
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
                max   = max(df_sia_shiny_filters$dev_storage_cap_mb_spec_num_value, na.rm = TRUE),
                value = c(0, max(df_sia_shiny_filters$dev_storage_cap_mb_spec_num_value, na.rm = TRUE)),
                step  = 1
              ),
              sliderInput(
                ns("dev_storage_cap_hr_spec_num_value"),
                label = "Storage Capacity (hrs)",
                min   = 0,
                max   = max(df_sia_shiny_filters$dev_storage_cap_hr_spec_num_value, na.rm = TRUE),
                value = c(0, max(df_sia_shiny_filters$dev_storage_cap_hr_spec_num_value, na.rm = TRUE)),
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
                label = "Usability Studies (n)",
                min   = 0,
                max   = max(df_sia_shiny_filters$usability_n_of_studies, na.rm = TRUE),
                value = c(0, max(df_sia_shiny_filters$usability_n_of_studies, na.rm = TRUE)),
                step  = 1
              ),
              sliderInput(
                ns("validity_and_reliability_n_of_studies"),
                label = "Validity & Reliability Studies (n)",
                min   = 0,
                max   = max(df_sia_shiny_filters$validity_and_reliability_n_of_studies, na.rm = TRUE),
                value = c(0, max(df_sia_shiny_filters$validity_and_reliability_n_of_studies, na.rm = TRUE)),
                step  = 1
              ),
              selectInput(ns("usability_evidence_level"), "Usability Evidence Level", choices = NULL, multiple = TRUE),
              selectInput(ns("validity_and_reliability_evidence_level"), "Validity & Reliability Evidence Level", choices = NULL, multiple = TRUE)
            )
          )
        )
      ),
      column(
        width = 9,
        bs4Card(
          title = "Filtered Results",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          div(
            style = "display: flex; justify-content: center; gap: 10px; margin-bottom: 15px;",
            actionButton(
              inputId = ns("glossary_info"),
              label   = tagList(
                icon("info-circle", style = "color: #1c75bc;"),
                "Table Information"
              ),
              status  = "success",
              outline = TRUE,
              size    = "sm",
              flat    = TRUE,
              width   = "20%",
              class   = "glossary-info-btn",
              style   = "border-width: 2px;"
            ),
            downloadButton(
              outputId = ns("download_data"),
              "Download Filtered Results"
            )
          ),
          reactableOutput(ns("feat_filtered_table")) %>% withSpinner(),
          footer = div(
            "Source: Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. ",
            "Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information. ",
            em("Behav Res Methods."),
            " 2025 May 13;57(6):171. doi: ",
            a(href = "https://link.springer.com/article/10.3758/s13428-025-02685-4",
              target = "_blank", "10.3758/s13428-025-02685-4"),
            "; PMID: 40360861; PMCID: ",
            a(href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12075381/",
              target = "_blank", "PMC12075381"),
            style = "font-family: sans-serif; font-size: 10pt; color: #8C8C8C;"
          )
        )
      )
    )
  )
}


# Feature Filter Module (Server)
mod_feat_fil_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Columns that live in df_sia_shiny_info (everything except device_id)
    info_cols <- setdiff(names(df_sia_shiny_info),c("device_id"))

    # --- 1. Variable groups (reuse from global.R) ----
    range_vars   <- setdiff(c(bar_vars, numeric_vars), "weight_gr")
    checkbox_vars <- yn_vars
    select_inputs <- setdiff(intersect(names(df_sia_shiny_filters), char_vars), c("release_year", "size_mm"))

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
          updateSliderInput(session, var, value = c(0, max(df_sia_shiny_filters[[var]], na.rm = TRUE)))
        }
      }

      updateAirDateInput(session, "release_year",
                         value = c(
                           min(df_sia_shiny_filters$release_year, na.rm = TRUE),
                           max(df_sia_shiny_filters$release_year, na.rm = TRUE)
                         ))

      lapply(checkbox_vars, function(id) updatePrettyCheckbox(session, id, value = FALSE))
      lapply(select_inputs, function(id) updateSelectInput(session, id, selected = character(0)))
      updateSwitchInput(session, "exclude_na_sia", value = FALSE)
    })

    # --- 6. Render filtered reactable ----
    output$feat_filtered_table <- renderReactable({

      # 1) Start from filtered filter-table data
      df <- filtered_data()

      # 2) Merge in info columns by device_id (now df has BOTH filter + info cols)
      df <- df %>%
        left_join(df_sia_shiny_info, by = "device_id")

      # 3) Format release year if present
      df$release_year <- format(df$release_year, "%Y")

      # 4) Add the 'details' column for the button
      df$details <- NA_character_

      # 5) Reorder columns: manufacturer, model, details first
      front_cols <- c(
        "manufacturer",
        "model",
        "long_term_all_score",
        "short_term_all_score",
        "details",
        "website"
      )
      other_cols <- setdiff(names(df), front_cols)
      df <- df[, c(front_cols, other_cols)]

      # 6) Column definitions from your existing helpers (only for filter cols)
      bar_column_defs     <- func_bar_column_defs(df, bar_vars, rename_map)
      yn_column_defs      <- func_yn_column_defs(yn_vars, rename_map)
      numeric_column_defs <- func_numeric_column_defs(df, numeric_vars, rename_map, numeric_var_ranges)
      char_column_defs    <- func_char_column_defs(char_vars, rename_map)

      # 7) Hide the info columns in the table (but keep them in rowInfo.values)
      info_column_defs <- lapply(info_cols, function(x) colDef(show = FALSE))
      names(info_column_defs) <- info_cols

      # Build JS array of info column names for popup
      info_cols_js <- paste0("['", paste(info_cols, collapse = "','"), "']")

      reactable(
        df,
        columns = c(
          list(
            # hide device_id but keep for popup use
            device_id = colDef(show = FALSE),

            manufacturer = colDef(
              name = "Manufacturer",
              sticky = "left",
              style = sticky_style,
              headerStyle = sticky_style,
              minWidth = 141
            ),
            model = colDef(
              name = "Model",
              sticky = "left",
              style = sticky_style,
              headerStyle = sticky_style,
              minWidth = 141
            ),
            details = colDef(
              name = "Details",
              sortable = FALSE,
              filterable = FALSE,
              minWidth = 141,
              cell = function() tags$button(
                "More details",
                class = "btn btn-sm btn-outline-primary"
              )
            ),
            website = colDef(
              name = "Website",
              sortable = FALSE,
              minWidth = 141,
              cell = function(value) {
                if (!is.na(value) && nzchar(value)) {
                  a(
                    href = value,
                    target = "_blank",
                    "Visit website"
                  )
                } else {
                  ""
                }
              }
            )
          ),
          bar_column_defs,
          yn_column_defs,
          numeric_column_defs,
          char_column_defs,
          info_column_defs   # hidden info columns
        ),
        bordered      = TRUE,
        highlight     = TRUE,
        pagination    = TRUE,
        searchable    = TRUE,
        resizable     = TRUE,
        fullWidth     = TRUE,
        striped       = FALSE,
        defaultSorted = "manufacturer",
        defaultColDef = colDef(
          footer = function(values, name) {
            if (nrow(df) > 6)
              strong(unname(rename_map[name]))
            else NULL
          }
        ),
        style         = list(maxHeight = "1000px", overflowY = "auto"),

        # --- Custom click handler for popup details ---
        onClick = JS(sprintf("
  function(rowInfo, column) {
    if (column.id !== 'details') return;

    const values   = rowInfo.values;
    const infoCols = %s;
    let lines = [];

    infoCols.forEach(function(col) {
      if (values[col] !== undefined && values[col] !== null && values[col] !== '') {
        lines.push(col + ': ' + values[col]);
      }
    });

    if (lines.length === 0) {
      window.alert('No additional details available for this device.');
      return;
    }

    // ---- create modal once ----
    let modal = document.getElementById('details-modal');
    if (!modal) {
      modal = document.createElement('div');
      modal.id = 'details-modal';
      modal.style.position = 'fixed';
      modal.style.top = '0';
      modal.style.left = '0';
      modal.style.width = '100%%';
      modal.style.height = '100%%';
      modal.style.backgroundColor = 'rgba(0,0,0,0.4)';
      modal.style.zIndex = '9999';
      modal.style.display = 'flex';
      modal.style.alignItems = 'center';
      modal.style.justifyContent = 'center';
      modal.style.fontFamily = 'inherit';

      const box = document.createElement('div');
      box.id = 'details-modal-box';
      box.style.backgroundColor = 'white';
      box.style.maxWidth = '700px';
      box.style.width = '90%%';
      box.style.maxHeight = '80%%';
      box.style.padding = '20px 20px 16px 20px';
      box.style.borderRadius = '8px';
      box.style.boxShadow = '0 2px 10px rgba(0,0,0,0.3)';
      box.style.display = 'flex';
      box.style.flexDirection = 'column';
      box.style.position = 'relative';
      box.style.fontFamily = 'inherit';

      const header = document.createElement('div');
      header.style.display = 'flex';
      header.style.justifyContent = 'flex-start';
      header.style.alignItems = 'center';

      const title = document.createElement('h3');
      title.id = 'details-modal-title';
      title.style.margin = '0';
      title.style.paddingRight = '40px';

      header.appendChild(title);
      box.appendChild(header);

      const body = document.createElement('pre');
      body.id = 'details-modal-body';
      body.style.marginTop = '12px';
      body.style.whiteSpace = 'pre-wrap';
      body.style.overflowY = 'auto';  // vertical scrollbar
      body.style.flex = '1';
      body.style.fontFamily = 'inherit';

      box.appendChild(body);

      // ---- footer with static SiA blue Close button (bottom-right) ----
      const footer = document.createElement('div');
      footer.style.display = 'flex';
      footer.style.justifyContent = 'flex-end';
      footer.style.marginTop = '12px';

      const closeBtn = document.createElement('button');
      closeBtn.type = 'button';
      closeBtn.id = 'details-modal-close';
      closeBtn.textContent = 'Close';
      closeBtn.style.padding = '6px 16px';
      closeBtn.style.borderRadius = '4px';
      closeBtn.style.border = '1px solid #1c75bc';
      closeBtn.style.background = '#1c75bc';   // SiA blue
      closeBtn.style.color = 'white';          // white text
      closeBtn.style.cursor = 'pointer';
      closeBtn.style.fontSize = '14px';
      closeBtn.style.fontFamily = 'inherit';
      closeBtn.style.fontWeight = '500';
      closeBtn.style.boxShadow = '0 2px 4px rgba(0,0,0,0.15)';

      closeBtn.onclick = function() {
        modal.style.display = 'none';
      };

      footer.appendChild(closeBtn);
      box.appendChild(footer);

      modal.appendChild(box);
      document.body.appendChild(modal);

      // Close when clicking outside the box
      modal.addEventListener('click', function(e) {
        if (e.target === modal) modal.style.display = 'none';
      });
    }

    // ---- update content & show ----
    const titleText = 'Details for ' + (values.manufacturer || '') + ' \u2013 ' + (values.model || '');
    document.getElementById('details-modal-title').textContent = titleText;
    document.getElementById('details-modal-body').textContent  = lines.join('\\n');

    modal.style.display = 'flex';
  }
", info_cols_js))
      )
    })

    # --- 10. Download filtered results (Excel) ----
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("sia_feature_filter_data_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        # Get selected device IDs from the filtered data
        selected_ids <- filtered_data()$device_id

        # Export full OSF data for those devices
        export_df <- df_sia_osf %>%
          filter(device_id %in% selected_ids) %>%
          as.data.frame()

        # Format release year if present
        if ("release_year" %in% names(export_df)) {
          export_df$release_year <- format(export_df$release_year, "%Y")
        }

        # Write Excel with all tabs
        write_xlsx(
          list(
            "Selected Devices" = export_df,
            "Citations"        = df_citations,
            "Codebook"         = df_codebook
          ),
          path = file
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )


    # --- 11. Download filter settings (Excel) ----
    output$download_filter_settings <- downloadHandler(
      filename = function() {
        paste0("sia_filter_settings_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        settings <- list()

        # Sliders: store "min;max"
        for (var in range_vars) {
          range_vals <- as.integer(round(input[[var]]))
          settings[[var]] <- paste(range_vals[1], range_vals[2], sep = ";")
        }

        # Checkboxes: "YES" if selected, "YES;NO" if not (no restriction)
        for (var in checkbox_vars) {
          settings[[var]] <- if (isTRUE(input[[var]])) "yes" else "yes;no"
        }

        # SelectInputs: chosen values separated by ";"
        for (var in select_inputs) {
          settings[[var]] <- paste(input[[var]], collapse = ";")
        }

        # Release year range (YYYY;YYYY)
        settings[["release_year"]] <- paste(
          format(input$release_year[1], "%Y"),
          format(input$release_year[2], "%Y"),
          sep = ";"
        )

        # Exclude NA SiA (same YES / YES;NO logic)
        settings[["exclude_na_sia"]] <- if (isTRUE(input$exclude_na_sia)) "yes" else "yes;no"

        # Convert list → one-row data.frame
        df_settings <- data.frame(t(unlist(settings)), check.names = FALSE)
        names(df_settings) <- names(settings)

        write_xlsx(
          list(
            "Filter settings" = df_settings,
            "Citations"        = df_citations,
            "Glossary" = df_codebook
          ),
          path = file
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

  })
}





