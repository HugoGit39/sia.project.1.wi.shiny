############################################################################################
#
# Module for submit data
#
# Stress in Action 2025
#
############################################################################################

# Submit Data Module (UI)
mod_sub_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # -------------------------------------------------------------------
      # 1. DRAFT FORM COLUMN
      # -------------------------------------------------------------------
      column(
        width = 4,
        div(
          style = "
        max-height: calc(100vh - 0px);  /* adjust if you have navbar/footer */
        overflow-y: auto;
      ",
          bs4Card(
            title = "1. Draft Form",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            div(
              p(
                strong("Help us expand the Stress-in-Action Wearables Database."), br(),
                "You can contribute by submitting information about additional wearable devices not yet included. All submitted data will be thoroughly reviewed before inclusion.", br(), br(),
                "The draft updates live as you type. Complete the mandatory fields ",
                strong("*", style = "color: #CC6677;"),
                " to enable submission.",
                style = "text-align: justify;"
              )
            ),
            textOutput(ns("status")),

            # ---------------- Your Information ----------------
            bs4Card(
              title = "Your Information",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              textInput(ns("name"), labelMandatory("Name")),
              textInput(ns("email"), labelMandatory("Email")),
              uiOutput(ns("email_error")),
              textInput(ns("telephone"), "Telephone"),
              textInput(ns("institution"), "Institution")
            ),

            # ---------------- General Device Information ----------------
            bs4Card(
              title = "General Device Information",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              text_or_selectize(ns("manufacturer"), labelMandatory("Manufacturer"), df_sia_shiny_filters, "manufacturer"),
              uiOutput(ns("manufacturer_csv_error")),
              textInput(ns("model"), labelMandatory("Model"), placeholder = "Click and type model name"),
              textInput(ns("website"), "Website", placeholder = "Click and paste or type URL https://"),
              textInput(ns("release_year"), labelMandatory("Release Year"), placeholder = "Click and write Y, Y/M or Y/M/D"),
              text_or_selectize(ns("market_status"), labelMandatory("Market Status"), df_sia_shiny_filters, "market_status"),
              text_or_selectize(ns("main_use"), labelMandatory("Main Use"), df_sia_shiny_filters, "main_use"),
              textInput(ns("device_cost"), labelMandatory("Device Cost (€)"), placeholder = "Click and write costs + details"),
              text_or_selectize(ns("wearable_type"), labelMandatory("Type"), df_sia_shiny_filters, "wearable_type"),
              text_or_selectize(ns("location"), labelMandatory("Location"), df_sia_shiny_filters, "location"),
              numericInput(ns("weight_gr"), "Weight (gr)", value = NA),
              textInput(ns("size_mm"), "Size (mm)", placeholder = "Click and write LxWxH or DxH (mm)")
            ),

            # ---------------- Technical Specifications ----------------
            bs4Card(
              title = "Technical Specifications",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              checkboxInput(ns("water_resistance_spec_boel_value"), "Water Resistant", value = FALSE),
              numericInput(ns("battery_life_spec_num_value"), "Battery Life (hrs)", value = NA),
              numericInput(ns("charging_duration_spec_num_value"), "Charging Duration (min)", value = NA),
              checkboxInput(ns("bio_cueing_spec_boel_value"), "Bio Cueing", value = FALSE),
              checkboxInput(ns("bio_feedback_spec_boel_value"), "Bio Feedback", value = FALSE)
            ),

            # ---------------- Signals ----------------
            bs4Card(
              title = "Signals",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              checkboxInput(ns("accelerometer_available"), "Accelerometer", value = FALSE),
              checkboxInput(ns("bp_available"), "Blood Pressure", value = FALSE),
              checkboxInput(ns("ecg_available"), "ECG", value = FALSE),
              checkboxInput(ns("eda_available"), "EDA", value = FALSE),
              checkboxInput(ns("eeg_available"), "EEG", value = FALSE),
              checkboxInput(ns("emg_available"), "EMG", value = FALSE),
              checkboxInput(ns("gps_available"), "GPS", value = FALSE),
              checkboxInput(ns("gyroscope_available"), "Gyroscope", value = FALSE),
              checkboxInput(ns("icg_available"), "ICG", value = FALSE),
              checkboxInput(ns("ppg_available"), "PPG", value = FALSE),
              checkboxInput(ns("respiration_available"), "Respiration", value = FALSE),
              checkboxInput(ns("skin_temperature_available"), "Skin Temperature", value = FALSE)
            ),

            # ---------------- Data Access ----------------
            bs4Card(
              title = "Data Access",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              checkboxInput(ns("raw_data_available_spec_boel_value"), "Raw Data Available", value = FALSE),
              checkboxInput(ns("int_storage_met_spec_boel_value"), "Internal Storage", value = FALSE),
              checkboxInput(ns("server_data_storage_spec_boel_value"), "Server Storage", value = FALSE),
              numericInput(ns("dev_storage_cap_mb_spec_num_value"), "Storage Capacity (MB)", value = NA),
              numericInput(ns("dev_storage_cap_hr_spec_num_value"), "Storage Capacity (hrs)", value = NA),
              checkboxInput(ns("gdpr_compliance_spec_boel_value"), "GDPR Compliant", value = FALSE),
              checkboxInput(ns("fda_clearance_spec_boel_value"), "FDA Cleared", value = FALSE),
              checkboxInput(ns("ce_marking_spec_boel_value"), "CE Marked", value = FALSE)
            ),

            # ---------------- Validation, Reliability & Usability ----------------
            bs4Card(
              title = "Validation, Reliability & Usability",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              numericInput(ns("usability_n_of_studies"), "Usability Studies (n)", value = NA),
              numericInput(ns("validity_and_reliability_n_of_studies"), "Validity & Reliability Studies (n)", value = NA),
              text_or_selectize(ns("usability_evidence_level"), "Usability Evidence Level", df_sia_shiny_filters, "usability_evidence_level"),
              text_or_selectize(ns("validity_and_reliability_evidence_level"), "Validity & Reliability Evidence Level", df_sia_shiny_filters, "validity_and_reliability_evidence_level")
            ),

            # ---------------- Further Details ----------------
            bs4Card(
              title = "Further Details",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              div(
                style = "text-align: center; margin-top: 8px;",
                div(
                  style = "text-align: left; margin-top: 8px; margin-left: 3px;",
                  actionButton(
                    inputId = ns("additional_info_help"),
                    label   = tagList(
                      icon("info-circle", style = "color: #1c75bc;"),
                      "Details"
                    ),
                    status  = "success",
                    outline = TRUE,
                    size    = "sm",
                    flat    = TRUE,
                    width   = NULL,              # no fixed width → just fits 'Details'
                    class   = "addinfo-info-btn",
                    style   = "border-width: 2px;"
                  )
                )
              ),
              br(),
              textAreaInput(
                ns("additional_information"),
                "Additional Information",
                rows = 4,
                placeholder = "e.g. sampling rates of signals, other signals available, or references to existing reliability, validity, or usability studies, etc. See ℹ Details for more information."
              )
            )
          )
        )
      ),

      # -------------------------------------------------------------------
      # 2. CHECK DRAFT FORM COLUMN
      # -------------------------------------------------------------------
      column(
        width = 4,
        div(
          style = "
      max-height: calc(100vh - 100px);  /* adjust if you have navbar/footer */
      overflow-y: auto;
    ",
          bs4Card(
            title = "2. Check Draft Form",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            p(
              "Please verify that provided fields are correct, then slide the toggle to enable submission.",
              style = "text-align: justify;"
            ),
            p(
              switchInput(
                inputId   = ns("draft_ok"),
                onLabel   = "YES",
                offLabel  = "NO",
                value     = FALSE,
                size      = "sm",
                onStatus  = "secondary",
                offStatus = "primary"
              )
            ),
            p(strong("All required fields complete?")),
            bs4Card(
              title = "Draft Form Output",
              width = 12,
              status = "secondary",
              solidHeader = TRUE,
              collapsible = FALSE,
              reactableOutput(ns("draft_table")) %>% withSpinner()
            )
          )
        )
      ),

      # -------------------------------------------------------------------
      # 3. SEND FINAL FORM COLUMN
      # -------------------------------------------------------------------
      column(
        width = 4,
        bs4Card(
          title = "3. Send Final Form",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          p("When you approve your draft, the option to send it to us will become available.",
            style = "text-align: justify;"),
          p(actionButton(ns("submit_final"), "Submit", disabled = TRUE)),
          downloadLink(ns("dl_xlsx_submit"), "", style = "display:none;"),
          p("A copy will be downloaded automatically when submitting. We will reach out to you to discuss it in more detail.",
            style = "text-align: justify;")
        ),
        div(
          style = "text-align: center;",
          img(
            src = "iStock_submit_data.jpg",
            style = "width: 75%; height: auto; margin-top: 100px; border-radius: 5px;"
          )
        )
      )
    )
  )
}

# Submit Data Module (Server)
mod_sub_data__server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    disable("draft_ok")
    updateSwitchInput(session, "draft_ok", value = FALSE)

    last_submission <- reactiveVal(NULL)

    # --- validation and inline errors (email only) ---
    output$email_error <- renderUI({
      v <- input$email
      if (is.null(v) || !nzchar(v)) return(NULL)
      if (!grepl("@", v))
        div(style = "color:#CC6677; font-size:12px;", strong("Email must contain '@' (e.g., name@example.com)."))
    })

    # --- validate mandatory fields ---
    observe({
      valid_form <- mandatoryfields_check(fieldsMandatory_data, input)
      toggleState("draft_ok", condition = valid_form)
      if (!valid_form && isTRUE(input$draft_ok))
        updateSwitchInput(session, "draft_ok", value = FALSE)
    })

    # --- reactive builder for the form ---
    build_form <- reactive({
      data.frame(
        Variable = rename_subm,
        Value = vapply(rename_subm, function(varname) {
          val <- input[[varname]]
          if (varname %in% yn_vars) return(if (isTRUE(val)) "Yes" else "No")
          if (inherits(val, "Date")) return(if (!is.null(val) && !is.na(val)) format(val, "%Y-%m-%d") else NA_character_)
          if (is.null(val) || (is.character(val) && val == "")) return(NA_character_)
          as.character(val)
        }, character(1)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    # --- table preview ---
    output$draft_table <- renderReactable({
      df <- build_form()

      # remove internal-only variables
      exclude_vars <- c("details", "long_term_all_score", "short_term_all_score", "other_signals_available")
      df <- df[!df$Variable %in% exclude_vars, ]

      # rename display names
      df$DisplayName <- ifelse(df$Variable %in% names(rename_map), rename_map[df$Variable], df$Variable)

      # capitalize core user fields for display
      df$DisplayName <- recode(
        df$DisplayName,
        "name" = "Name",
        "email" = "Email",
        "telephone" = "Telephone",
        "institution" = "Institution",
        "additional_information" = "Additional Information",
        .default = df$DisplayName
      )

      reactable(
        df[, c("DisplayName", "Value")],
        rownames = FALSE,
        columns = list(
          DisplayName = colDef(
            name = "Variable",
            sticky = "left",
            minWidth = 260,
            cell = function(value) {
              div(
                style = list(display = "inline-flex", alignItems = "center"),
                if (value %in% rename_map[fieldsMandatory_data]) labelMandatory(value) else value
              )
            }
          ),
          Value = colDef(name = "Value", minWidth = 380)
        ),
        bordered = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        resizable = TRUE,
        fullWidth = TRUE
      )
    })

    # --- download + submit logic (now Excel only) ---
    output$dl_xlsx_submit <- downloadHandler(
      filename = function() {
        paste0("sia_data_submission", ".xlsx")
      },
      content = function(file) {
        df <- req(last_submission())
        write_xlsx(list("Submission" = df), path = file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    outputOptions(output, "dl_xlsx_submit", suspendWhenHidden = FALSE)

    observe({ toggleState("submit_final", condition = isTRUE(input$draft_ok)) })

    # --- submission event (send + reset) ---
    observeEvent(input$submit_final, {
      df <- build_form()
      last_submission(df)

      excel_path <- file.path(tempdir(),   paste0(
        "sia_data_submission_",
        input$email, "_",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      ))
      write_xlsx(list("Submission" = df), path = excel_path)

      session$onFlushed(function() {
        runjs(sprintf("document.getElementById('%s').click();", ns("dl_xlsx_submit")))
      }, once = TRUE)

      session$sendCustomMessage("dataSubmitted", "Thank you for your data submission! We will get back to you soon.")
      reset_inputs_sub_data(session, input)

      subject <- sprintf("SiA Wearables submission: %s", input$email)
      body <- paste0(
        "New submission received.\n\n",
        "Name: ", input$name, "\n",
        "Email: ", input$email, "\n",
        "Telephone: ", input$telephone, "\n",
        "Institution: ", input$institution, "\n"
      )

      send_email(body = body, subject = subject, attachment = excel_path)
    })
  })
}
