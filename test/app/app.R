############################################################################################
#
#  App file
#
# Stress in Action 2025
#
# Author: H. Klarenberg, PhD
#
#############################################################################################

source("global.R", local = FALSE)

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js")
  ),
  dashboardPage(
    dark = NULL,
    freshTheme = colours_fresh(),
    title = "Stress in Action Wearables Database App",
    fullscreen = FALSE,
    skin = "light",
    help = NULL,
    header = mod_header_ui("header"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      id = "body_app",
      useShinyjs(),
      useSever(),
      tabItems(
        tabItem(tabName = "app_info", mod_app_info_ui("app_info")),
        tabItem(tabName = "product_filter", mod_prod_fil_ui("product_comp")),
        tabItem(tabName = "feature_filter", mod_feat_fil_ui("feature_comp")),
        tabItem(tabName = "submit_data", mod_sub_data_ui("add_data")),
        tabItem(tabName = "article", mod_article_ui("art")),
        tabItem(tabName = "about", mod_about_ui("about")),
        tabItem(tabName = "contact_us", mod_contact_ui("contact"))
      )
    ),
    controlbar = mod_control_ui("controlbar"),
    footer = mod_footer_ui("footer"),
    scrollToTop = TRUE
  )
)

server <- function(input, output, session) {
  sia_df_reactive <- reactive({ df_sia_shiny_filters })
  mod_header_server("header")
  mod_prod_fil_server("product_comp", sia_df_reactive)
  mod_feat_fil_server("feature_comp", sia_df_reactive)
  mod_sub_data__server("add_data")
  mod_article_server("art")
  mod_contact_server("contact")
  mod_control__server("controlbar", sia_df_reactive)
  mod_timeout_server("timeout")
}

shinyApp(ui = ui, server = server)
