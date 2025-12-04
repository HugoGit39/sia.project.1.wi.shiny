############################################################################################
#
# Module for Article
#
# Stress in Action 2025
#
#############################################################################################

# Article Module (UI)
mod_article_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        bs4Card(
          title = "Summary of the Research",
          status = "secondary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          fluidRow(
            column(
              width = 3,
              bs4Dash::bs4Card(
                title = "Why Needed?",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                style = "height: 450px; overflow-y: auto;",
                p("• Rapid growth of available consumer and research-oriented wearables", style = "text-align: justify;"),
                p("• To systematically choose a wearable among the multitude of available devices, one should perform:", style = "text-align: justify;"),
                p("o Get a detailed overview of technical and other device specific information", style = "text-align: justify;"),
                p("o Retrieve synopsis of Reliability, Validity and Usability studies per wearable", style = "text-align: justify;"),
                p(strong("To reduce the time and effort required to choose the appropriate wearable, the Stress in Action Wearables Database provides this detailed overview."), style = "text-align: justify;")
              )
            ),
            column(
              width = 3,
              bs4Card(
                title = "Literature Research",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                style = "height: 450px; overflow-y: auto;",
                p(
                  "Device-specific information was obtained from the device manual, while reliability, validity, and usability were assessed through a structured literature search (see", icon("info-circle", style = "color:#1c75bc;"),"for info), using the following string:", style = "text-align: justify;"),
                  p("“((Device Name) AND (valid* OR reliab* OR compar* OR accur* OR verif* OR usab* OR 'user experience' OR 'user friend*' OR user-friend*)”.", style = "text-align: justify;"),
                  p("Included studies assessed parameter-level convergent validity, test-retest reliability, and/or usability, focused on convergent validity, and were published as peer-reviewed articles or conference proceedings in English. Excluded were studies on construct validity only, those involving machine learning-based detection of secondary outcomes, meta-analyses, reviews, theses, grey literature, and any non-peer-reviewed texts.",
                  style = "text-align: justify;"
                ),
                sidebar = cardSidebar(
                  id = "sb_kr",
                  width = 100,
                  background = "white",
                  icon = icon(
                    "info-circle",
                    class = "fa-lg",
                    style = "color: #1c75bc;"
                  ),
                  img(
                    src = "article_asreview_process.png",
                    style = "max-width: 100%; height: auto; display: block; margin: 0 auto; margin-bottom: 15px;"
                  )
                )
              )
            ),
            column(
              width = 3,
              bs4Card(
                title = "SiA Expert Scoring",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                style = "height: 450px; overflow-y: auto;",
                p(
                  "Different criteria were rank-ordered in importance by the curators to score a device for short-term (2-day) and long-term (2+ week) research use.", style = "text-align: justify;"),
                p("Based on these criteria (see", icon("info-circle", style = "color:#1c75bc;"), "for info), each device was independently scored from 0 to 10 by the three first co-authors, who were blinded to each other's scores; these scores were then averaged to generate the short- and long-term \"SiA expert scores.\" ", style = "text-align: justify;"),
                p("High interrater reliability was achieved for both short-term (r = .87, 95% CI = [.78, .92], F(50, 100) = 8.0, p < .001) and long-term use (r = .85, 95% CI = [.76, .91], F(50, 100) = 6.6, p < .001).",
                  style = "text-align: justify;"
                ),
                sidebar = cardSidebar(
                  id = "sb_sia_es",
                  width = 100,
                  background = "white",
                  icon = icon(
                    "info-circle",
                    class = "fa-lg",
                    style = "color: #1c75bc;"
                  ),

                  # ---- SHORT-TERM ----
                  strong("Short-term importance of criteria", style = "color:#000000;"), br(),
                  reactable::reactable(
                    data.frame(
                      Criteria = c(
                        "GDPR approval",
                        "CE approval",
                        "All Reliability-Validity Criteria",
                        "Number and type of different signals that can be measured by the device (e.g., PPG, ECG, accelerometer)",
                        "Raw data availability",
                        "Provided parameters",
                        "Parameter sampling window",
                        "Price",
                        "Wearable type, location, and weight and size",
                        "Usability outcome (if available)",
                        "Data Storage Capacity",
                        "Data Transfer Method",
                        "Data Storage Method",
                        "Battery life",
                        "Charging duration",
                        "Charging method",
                        "FDA approval",
                        "Waterproof",
                        "Bio-cueing",
                        "Biofeedback"
                      ),
                      Importance = c(
                        "High",
                        "High",
                        "High",
                        "High",
                        "High",
                        "Medium",
                        "Medium",
                        "Medium",
                        "Medium",
                        "Medium",
                        "Medium/Low",
                        "Medium/Low",
                        "Medium/Low",
                        "Low",
                        "Low",
                        "Low",
                        "Low",
                        "Low",
                        "Low",
                        "Low"
                      ),
                      stringsAsFactors = FALSE
                    ),
                    columns = list(
                      Criteria   = colDef(name = "Criteria", width = 250),
                      Importance = colDef(name = "Importance", width = 120, style = list(whiteSpace = "nowrap"))
                    ),
                    sortable   = FALSE,
                    searchable = FALSE,
                    pagination = FALSE,
                    resizable  = FALSE,
                    bordered   = TRUE,
                    highlight  = TRUE,
                    fullWidth  = TRUE,
                    theme = reactableTheme(
                      color           = "#000000",
                      backgroundColor = "#FFFFFF",
                      borderColor     = "#D9D9D9"
                    )
                  ),

                  br(),

                  # ---- LONG-TERM ----
                  strong("Long-term importance of criteria", style = "color:#000000;"), br(),
                  reactable(
                    data.frame(
                      Criteria = c(
                        "GDPR approval",
                        "CE approval",
                        "Price",
                        "Number of different physiological parameters that can be measured by the device (e.g., HR, Skin temperature)",
                        "Wearable type, location, and weight and size",
                        "Usability outcome (if available)",
                        "Battery life",
                        "All Validity-Reliability Criteria",
                        "Parameter sampling window",
                        "Data Storage Capacity",
                        "Data Transfer Method",
                        "Data Storage Method",
                        "Charging duration",
                        "Charging method",
                        "Raw data availability",
                        "Data Transfer Compatibility",
                        "Waterproof",
                        "Bio-cueing",
                        "Biofeedback",
                        "FDA approval"
                      ),
                      Importance = c(
                        "High",
                        "High",
                        "High",
                        "High",
                        "High",
                        "High",
                        "High",
                        "High/Medium",
                        "High/Medium",
                        "High/Medium",
                        "Medium",
                        "Medium",
                        "Medium",
                        "Medium",
                        "Medium",
                        "Medium",
                        "Medium/Low",
                        "Medium/Low",
                        "Medium/Low",
                        "Low"
                      ),
                      stringsAsFactors = FALSE
                    ),
                    columns = list(
                      Criteria   = colDef(name = "Criteria", width = 250),
                      Importance = colDef(name = "Importance", width = 120, style = list(whiteSpace = "nowrap"))
                    ),
                    sortable   = FALSE,
                    searchable = FALSE,
                    pagination = FALSE,
                    resizable  = FALSE,
                    bordered   = TRUE,
                    highlight  = TRUE,
                    fullWidth  = F,
                    theme = reactableTheme(
                      color           = "#000000",
                      backgroundColor = "#FFFFFF",
                      borderColor     = "#D9D9D9"
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              bs4Card(
                title = "Continuously Updated Database",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                style = "height: 450px; overflow-y: auto;",
                p(
                  "In contrast to the wearable inventories or review studies with a static document, the SiA wearables database will be continuously updated every 6 months. This enables the entry of newly released wearables and updating of the validity, reliability, and usability information on existing wearables.",
                  style = "text-align: justify;"
                ),
                img(
                  src = "article_update_loop.png",
                  style = "display: block; margin: 15px auto; max-width: 100%; height: auto;"
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        bs4Card(
          title = "Behavior Research Methods: Original Paper",
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          footer = NULL,
          solidHeader = TRUE,
          status = "primary",
          div(
            style = "text-align: center; margin-bottom: 1rem; display: flex; justify-content: center; align-items: center; gap: 1rem;",
            downloadButton(ns("download_pdf"), "Download PDF"),
            a(
              href = "https://link.springer.com/article/10.3758/s13428-025-02685-4",
              target = "_blank",
              img(
                src = "springer_link.png",
                height = "40px",  # adjust size as needed
                alt = "Springer Link"
              )
            )
          ),
          tags$iframe(
            src = "s13428-025-02685-4.pdf#toolbar=0&navpanes=0&scrollbar=0",
            style = "width:100%; height:842px; border:none;"
          )
        )
      ),
      column(
        width = 6,
        bs4Card(
          title = "Journal of Open Source Software Paper: Shiny App Details",
          height = "800px",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          p("Coming soon")
        )
      )
    )
  )
}

# Article Module (Server)
mod_article_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$download_pdf <- downloadHandler(
      filename = function() {
        "s13428-025-02685-4.pdf"
      },
      content = function(file) {
        file.copy("www/s13428-025-02685-4.pdf", file)
      }
    )
  })
}
