############################################################################################
#
# Module  for controlbar
#
# Stress in Action 2025
#
#############################################################################################

# Controlbar Module (UI)
mod_control_ui <- function(id) {
  ns <- NS(id)

    dashboardControlbar(
      skin = "light",
      pinned = FALSE,
      collapsed = TRUE,
      overlay = TRUE,
      controlbarMenu(
        id = "controlbarmenu",
        type = "pills",
        controlbarItem(
          title = "Wearables",
          reactableOutput(ns("wearables_table")) %>% withSpinner()
        ),
        controlbarItem(
          title = "Glossary",
          accordion(
            id = "accordion_glossary",
            accordionI(div("A", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$A),
            accordionI(div("B", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$B),
            accordionI(div("C", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$C),
            accordionI(div("D", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$D),
            accordionI(div("E", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$E),
            accordionI(div("F", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$F),
            accordionI(div("G", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$G),
            accordionI(div("H", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$H),
            accordionI(div("I", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$I),
            accordionI(div("J", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$J),
            accordionI(div("K", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$K),
            accordionI(div("L", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$L),
            accordionI(div("M", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$M),
            accordionI(div("N", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$N),
            accordionI(div("O", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$O),
            accordionI(div("P", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$P),
            accordionI(div("Q", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$Q),
            accordionI(div("R", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$R),
            accordionI(div("S", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$S),
            accordionI(div("T", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$T),
            accordionI(div("U", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$U),
            accordionI(div("V", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$V),
            accordionI(div("W", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$W),
            accordionI(div("X", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$X),
            accordionI(div("Y", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$Y),
            accordionI(div("Z", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  glos$Z)
          )
        )
      )
    )
}

# Controlbar Module (Server)
mod_control__server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    output$wearables_table <- renderReactable({
      df <- data() %>%
        arrange(manufacturer)

      reactable(
        data.frame(
          manufacturer = df$manufacturer,
          model = df$model,
          stringsAsFactors = FALSE),
        columns = list(manufacturer = colDef(name = "Manufacturer", width = 200),
                       model = colDef(name = "Model")),
        searchable = TRUE,
        sortable = TRUE,
        defaultPageSize = 10,
        pagination = TRUE,
        bordered = TRUE,
        highlight = TRUE,
        resizable = TRUE,
        fullWidth = TRUE
      )
    })

  })
}





