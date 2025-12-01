############################################################################################
#
#  Module for product filter
#
# Stress in Action 2025
############################################################################################

# Product Filter Module (UI)
mod_prod_fil_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # ---------------- LEFT FILTER PANEL ----------------
      column(
        width = 3,
        bs4Card(
          title = "Product Filter",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,

          # --- Product 1 ---
          selectInput(
            ns("product1"), "Product 1: Manufacturer",
            choices = sort(unique(df_sia_shiny_filters$manufacturer)),
            selected = "Apple"
          ),
          selectInput(ns("model1"), "Product 1: Model", choices = NULL),

          # --- Product 2 ---
          selectInput(
            ns("product2"), "Product 2: Manufacturer",
            choices = sort(unique(df_sia_shiny_filters$manufacturer)),
            selected = "Vrije Universiteit van Amsterdam"
          ),
          selectInput(ns("model2"), "Product 2: Model", choices = NULL),

          # --- Reset Product 3 ---
          div(
            style = "text-align: center; margin-bottom: 10px;",
            actionButton(
              inputId = ns("reset_prod_filter"),
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

          # --- Product 3 ---
          selectInput(
            ns("product3"), "Product 3: Manufacturer",
            choices = c("Choose a product" = "", sort(unique(df_sia_shiny_filters$manufacturer))),
            selected = ""
          ),
          selectInput(ns("model3"), "Product 3: Model", choices = NULL)
        )
      ),

      # ---------------- RIGHT RESULTS PANEL ----------------
      column(
        width = 9,
        bs4Card(
          title = "Comparison Table",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,

          # --- Glossary Info + Download Button ----
          div(
            style = "display: flex; justify-content: center; gap: 10px; margin-bottom: 15px;",
            bs4Dash::actionButton(
              inputId = ns("glossary_info"),
              label   = tagList(
                icon("info-circle", style = "color: #1c75bc;"),  # SiA blue icon
                "Table Information"
              ),
              status  = "success",      # teal color
              outline = TRUE,
              size    = "sm",
              flat    = TRUE,
              width   = "20%",
              class   = "glossary-info-btn",   # <--- ADD THIS BACK
              style   = "border-width: 2px;"
            ),
            downloadButton(ns("download_data"), "Download Filtered Results")
          ),

          # --- Table Output ----
          reactableOutput(ns("prod_filtered_table")) %>% withSpinner(),

          # --- Footer Citation ----
          footer = tags$div(
            "Source: Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. ",
            "Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information. ",
            tags$em("Behav Res Methods."),
            " 2025 May 13;57(6):171. doi: ",
            tags$a(
              href = "https://link.springer.com/article/10.3758/s13428-025-02685-4",
              target = "_blank",
              "10.3758/s13428-025-02685-4"
            ),
            "; PMID: 40360861; PMCID: ",
            tags$a(
              href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12075381/",
              target = "_blank",
              "PMC12075381"
            ),
            style = "font-family: sans-serif; font-size: 10pt; color: #8C8C8C;"
          )
        )
      )
    )
  )
}

# Product Filter Module (Server)
mod_prod_fil_server <- function(id, df_sia_shiny_filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    disable("model3")

    # ---------------- OBSERVERS FOR MANUFACTURERS ----------------

    observeEvent(input$product1, {
      df <- df_sia_shiny_filters()
      updateSelectInput(session, "model1",
                        choices = sort(unique(df$model[df$manufacturer == input$product1])))
    })

    observeEvent(input$product2, {
      df <- df_sia_shiny_filters()
      updateSelectInput(session, "model2",
                        choices = sort(unique(df$model[df$manufacturer == input$product2])))
    })

    observeEvent(input$product3, {
      df <- df_sia_shiny_filters()
      if (input$product3 == "None" || input$product3 == "") {
        disable("model3")
        updateSelectInput(session, "model3", choices = character(0), selected = "")
      } else {
        enable("model3")
        updateSelectInput(session, "model3",
                          choices = c("Choose a model" = "",
                                      sort(unique(df$model[df$manufacturer == input$product3]))))
      }
    })

    # ---------------- REACTIVE SELECTED PRODUCTS ----------------
    selected_products <- reactive({
      df <- df_sia_shiny_filters()

      rows <- list(
        df %>% filter(manufacturer == input$product1, model == input$model1),
        df %>% filter(manufacturer == input$product2, model == input$model2)
      )

      if (!is.null(input$model3) && nzchar(input$model3)) {
        rows <- c(rows,
                  list(df %>% filter(manufacturer == input$product3, model == input$model3)))
      }

      bind_rows(rows) %>% distinct(manufacturer, model, .keep_all = TRUE)
    })

    # ---------------- RENDER TABLE ----------------
    output$prod_filtered_table <- renderReactable({
      df <- selected_products()
      df$release_year <- format(df$release_year, "%Y")

      # Transpose: features as rows, models as columns
      df_t <- df %>%
        select(-manufacturer, -model, -device_id) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE)

      colnames(df_t) <- paste0(df$manufacturer, " – ", df$model)
      df_t <- cbind(Feature_internal = rownames(df_t), df_t)
      rownames(df_t) <- NULL
      df_t$Feature <- rename_map[df_t$Feature_internal] %||% df_t$Feature_internal

      transposed_cols <- setdiff(names(df_t), c("Feature", "Feature_internal"))

      col_defs <- list(
        Feature = colDef(name = "Product - Model", minWidth = 50, style = list(fontWeight = "bold"))
      )

      # Define rendering logic for each column
      for (col in transposed_cols) {
        col_defs[[col]] <- colDef(
          cell = function(value, index) {
            # clickable website link
            if (df_t$Feature_internal[index] == "website" &&
                !is.na(value) && nzchar(value)) {
              return(tags$a(href = value, target = "_blank", "Visit website"))
            }

            rendered <- func_bar_row_defs(value, index, df_t$Feature, bar_vars, rename_map)
            if (!identical(rendered, value)) return(rendered)

            rendered <- func_yn_row_defs(value, index, df_t$Feature, yn_vars, rename_map)
            if (!identical(rendered, value)) return(rendered)

            rendered <- func_numeric_row_defs(
              value, index,
              feature_internal    = df_t$Feature_internal,
              numeric_vars        = numeric_vars,
              numeric_var_ranges  = numeric_var_ranges,
              palette             = pal_num_scale
            )
            if (!identical(rendered, value)) return(rendered)

            value
          }
        )
      }

      reactable(
        df_t[, c("Feature", "Feature_internal", transposed_cols)],
        columns = c(
          col_defs,
          list(Feature_internal = colDef(show = FALSE))
        ),
        height = (nrow(df_t) * 0.5) * 40,
        bordered = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        searchable = TRUE,
        fullWidth = TRUE,
        resizable = TRUE
      )
    })

    # ---------------- RESET THIRD PRODUCT ----------------
    observeEvent(input$reset_prod_filter, {
      updateSelectInput(session, "product3", selected = "")
      updateSelectInput(session, "model3", choices = character(0), selected = "")
      disable("model3")
    })

    # ---------------- DOWNLOAD HANDLER ----------------
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("sia_product_filter_data_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        selected_ids <- selected_products()$device_id
        export_df <- df_sia_osf %>%
          filter(device_id %in% selected_ids) %>%
          as.data.frame()

        citation_text <- data.frame(
          Citation = c(
            "Thank you for using the Stress-in-Action Wearable Database!",
            "If you use the SiA-WD and/or this web app you must cite:",
            "Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E.",
            "Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information.",
            "Behav Res Methods. 2025 May 13;57(6):171.",
            "doi: 10.3758/s13428-025-02685-4. PMID: 40360861; PMCID: PMC12075381.",
            "[Shiny paper coming soon]"
          )
        )

        write_xlsx(
          list("Selected_Devices" = export_df, "Citation" = citation_text),
          path = file
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

  })
}
