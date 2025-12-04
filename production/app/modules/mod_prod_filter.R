############################################################################################
#
# Module for product filter
#
# Stress in Action 2025
#
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

          # --- Product 1 (always prefilled) ---
          selectInput(
            ns("product1"), "Product 1: Manufacturer",
            choices = sort(unique(df_sia_shiny_filters$manufacturer)),
            selected = "Apple"
          ),
          selectInput(ns("model1"), "Product 1: Model", choices = NULL),

          # --- Reset Product 2 & 3 ---
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

          # --- Product 2 (user selects) ---
          selectInput(
            ns("product2"), "Product 2: Manufacturer",
            choices  = c("Choose a product" = "", sort(unique(df_sia_shiny_filters$manufacturer))),
            selected = ""
          ),
          selectInput(ns("model2"), "Product 2: Model", choices = NULL),

          # --- Product 3 (unlocked after Model 2) ---
          selectInput(
            ns("product3"), "Product 3: Manufacturer",
            choices  = c("Choose a product" = "", sort(unique(df_sia_shiny_filters$manufacturer))),
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
            downloadButton(ns("download_data"), "Download Filtered Results")
          ),

          reactableOutput(ns("prod_filtered_table")) %>% withSpinner(),

          footer = div(
            "Source: Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. ",
            "Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information. ",
            em("Behav Res Methods."),
            " 2025 May 13;57(6):171. doi: ",
            a(
              href = "https://link.springer.com/article/10.3758/s13428-025-02685-4",
              target = "_blank",
              "10.3758/s13428-025-02685-4"
            ),
            "; PMID: 40360861; PMCID: ",
            a(
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

#Product Filter Module (Server)
mod_prod_fil_server <- function(id, df_sia_shiny_filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # dummy values to force-reset selectInputs
    dummy_model2 <- "__dummy_model2__"
    dummy_model3 <- "__dummy_model3__"

    # ---------- INITIAL DISABLE STATE ----------
    disable("model2")
    disable("product3")
    disable("model3")

    # ---------------- PRODUCT 1 ----------------
    observeEvent(input$product1, {
      df <- df_sia_shiny_filters()
      m_choices <- sort(unique(df$model[df$manufacturer == input$product1]))
      if (length(m_choices) == 0) {
        updateSelectInput(session, "model1", choices = character(0), selected = "")
      } else {
        sel <- if (!is.null(input$model1) && input$model1 %in% m_choices) input$model1 else m_choices[1]
        updateSelectInput(session, "model1", choices = m_choices, selected = sel)
      }
    }, ignoreInit = FALSE)

    # ---------------- PRODUCT 2 ----------------
    observeEvent(input$product2, {
      df <- df_sia_shiny_filters()

      if (is.null(input$product2) || input$product2 == "") {
        disable("model2")
        disable("product3")
        disable("model3")
        updateSelectInput(session, "model2", choices = character(0), selected = "")
        updateSelectInput(session, "product3", selected = "")
        updateSelectInput(session, "model3", choices = character(0), selected = "")
      } else {
        # 1) force-drop any old model2 value by using a dummy choice
        updateSelectInput(
          session, "model2",
          choices  = c("Resetting…" = dummy_model2),
          selected = dummy_model2
        )

        # 2) now set real choices with empty selection
        enable("model2")
        m2_choices <- sort(unique(df$model[df$manufacturer == input$product2]))
        updateSelectInput(
          session, "model2",
          choices  = c("Choose a model" = "", m2_choices),
          selected = ""   # user must choose manually
        )

        # ensure product3 is reset/disabled
        disable("product3")
        disable("model3")
        updateSelectInput(session, "product3", selected = "")
        updateSelectInput(session, "model3", choices = character(0), selected = "")
      }
    })

    # ---------------- MODEL 2 ----------------
    observeEvent(input$model2, {
      # ignore the internal dummy value used during reset
      if (identical(input$model2, dummy_model2)) return()

      if (!is.null(input$model2) && nzchar(input$model2)) {
        # now user really chose a model -> unlock product3
        enable("product3")
      } else {
        disable("product3")
        disable("model3")
        updateSelectInput(session, "product3", selected = "")
        updateSelectInput(session, "model3", choices = character(0), selected = "")
      }
    })

    # ---------------- PRODUCT 3 ----------------
    observeEvent(input$product3, {
      df <- df_sia_shiny_filters()
      if (is.null(input$product3) || input$product3 == "" || input$product3 == "None") {
        disable("model3")
        updateSelectInput(session, "model3", choices = character(0), selected = "")
      } else {
        # same dummy trick for model3 to avoid sticky values if you ever change product3
        updateSelectInput(
          session, "model3",
          choices  = c("Resetting…" = dummy_model3),
          selected = dummy_model3
        )

        enable("model3")
        m3_choices <- sort(unique(df$model[df$manufacturer == input$product3]))
        updateSelectInput(
          session, "model3",
          choices  = c("Choose a model" = "", m3_choices),
          selected = ""
        )
      }
    })

    # ---------------- MODEL 3 ----------------
    observeEvent(input$model3, {
      # ignore dummy internal reset value
      if (identical(input$model3, dummy_model3)) return()

      if (!is.null(input$model3) && nzchar(input$model3)) {
        # nothing extra to unlock here, but you could add logic later
        invisible(NULL)
      } else {
        # model3 cleared
        invisible(NULL)
      }
    })

    # ---------------- REACTIVE SELECTED PRODUCTS ----------------
    selected_products <- reactive({
      df <- df_sia_shiny_filters()
      rows <- list()

      if (!is.null(input$model1) && nzchar(input$model1))
        rows[[length(rows) + 1]] <- df %>%
        dplyr::filter(manufacturer == input$product1, model == input$model1)

      if (!is.null(input$product2) && nzchar(input$product2) &&
          !is.null(input$model2) && nzchar(input$model2))
        rows[[length(rows) + 1]] <- df %>%
        dplyr::filter(manufacturer == input$product2, model == input$model2)

      if (!is.null(input$product3) && nzchar(input$product3) &&
          !is.null(input$model3) && nzchar(input$model3))
        rows[[length(rows) + 1]] <- df %>%
        dplyr::filter(manufacturer == input$product3, model == input$model3)

      if (length(rows) == 0) return(df[0, , drop = FALSE])
      dplyr::bind_rows(rows) %>% dplyr::distinct(manufacturer, model, .keep_all = TRUE)
    })

    # ---------------- RENDER TABLE ----------------
    output$prod_filtered_table <- renderReactable({
      df <- selected_products()
      if (nrow(df) == 0) {
        return(
          reactable(
            data.frame(Message = "Please select at least one valid product/model."),
            searchable = FALSE,
            pagination = FALSE
          )
        )
      }
      df$release_year <- format(df$release_year, "%Y")

      df_t <- df %>%
        dplyr::select(-manufacturer, -model, -device_id) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE)
      colnames(df_t) <- paste0(df$manufacturer, " – ", df$model)
      df_t <- cbind(Feature_internal = rownames(df_t), df_t)
      rownames(df_t) <- NULL
      df_t$Feature <- rename_map[df_t$Feature_internal] %||% df_t$Feature_internal

      transposed_cols <- setdiff(names(df_t), c("Feature", "Feature_internal"))
      col_defs <- list(
        Feature = colDef(
          name = "Product - Model",
          minWidth = 50,
          style = list(fontWeight = "bold")
        )
      )

      for (col in transposed_cols) {
        col_defs[[col]] <- colDef(cell = function(value, index) {
          if (df_t$Feature_internal[index] == "website" && !is.na(value) && nzchar(value)) {
            thumb <- paste0("https://s.wordpress.com/mshots/v1/", value, "?w=400")
            return(a(
              href   = value, target = "_blank",
              div(
                class = "website-thumb-container",
                img(
                  src      = thumb,
                  loading  = "lazy",
                  onerror  = "this.style.display='none'; this.parentElement.textContent='Preview unavailable';",
                  class    = "website-thumb-img"
                )
              )
            ))
          }
          rendered <- func_bar_row_defs(value, index, df_t$Feature, bar_vars, rename_map)
          if (!identical(rendered, value)) return(rendered)
          rendered <- func_yn_row_defs(value, index, df_t$Feature, yn_vars, rename_map)
          if (!identical(rendered, value)) return(rendered)
          rendered <- func_numeric_row_defs(
            value, index,
            feature_internal   = df_t$Feature_internal,
            numeric_vars       = numeric_vars,
            numeric_var_ranges = numeric_var_ranges,
            palette            = pal_num_scale
          )
          if (!identical(rendered, value)) return(rendered)
          value
        })
      }

      reactable(
        df_t[, c("Feature", "Feature_internal", transposed_cols)],
        columns   = c(col_defs, list(Feature_internal = colDef(show = FALSE))),
        height    = (nrow(df_t) * 0.5) * 40,
        bordered  = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        searchable = TRUE,
        fullWidth = TRUE,
        resizable = TRUE
      )
    })

    # ---------------- RESET ----------------
    observeEvent(input$reset_prod_filter, {
      # Reset Product 2 + Model 2
      updateSelectInput(session, "product2", selected = "")
      updateSelectInput(session, "model2", choices = character(0), selected = "")
      disable("model2")

      # Reset Product 3 + Model 3
      updateSelectInput(session, "product3", selected = "")
      updateSelectInput(session, "model3", choices = character(0), selected = "")
      disable("product3")
      disable("model3")
    })

    # ---------------- DOWNLOAD ----------------
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("sia_product_filter_data_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        selected_ids <- selected_products()$device_id
        export_df <- df_sia_osf %>%
          dplyr::filter(device_id %in% selected_ids) %>%
          as.data.frame()

        write_xlsx(
          list(
            "Selected Devices" = export_df,
            "Citations"        = df_citations,
            "Glossary"         = df_codebook
          ),
          path = file
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
  })
}




