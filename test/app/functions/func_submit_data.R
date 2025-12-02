############################################################################################
#
# Function for submit data
#
# Stress in Action 2025
#
#############################################################################################

text_or_selectize <- function(inputId, label, df, col) {
  vals <- sort(unique(na.omit(df[[col]])))
  selectizeInput(
    inputId,
    label,
    choices = vals,
    selected = "",  # nothing preselected
    multiple = FALSE,
    options = list(
      create = TRUE,             # allows free text input
      placeholder = "Click for options or type + press Enter",
      onInitialize = I('function() { this.clear(); }')  # ensure starts empty
    )
  )
}
