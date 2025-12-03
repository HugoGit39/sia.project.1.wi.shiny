############################################################################################
#
# Module for time out
#
# Stress in Action 2025
#
#############################################################################################

# Time Out Module (UI)
mod_timeout_ui <- function(id) {
  ns <- NS(id)

  tagList(

  )
}

# Time Out Module (Server)
mod_timeout_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    sever(html = disconnected, bg_color = "#DDDDDD75", color = "black", box = T)

  })
}


