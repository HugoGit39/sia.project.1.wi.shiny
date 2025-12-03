############################################################################################
#
# Global file
#
# Stress in Action 2025
#
# Author: H. Klarenberg, PhD
# Email: h.klarenberg@vu.nl
#
#############################################################################################

# * 1 Load libraries ----------------------- -------------------------------

# list of required packages
required_packages <- c(
  "shiny", "bs4Dash", "here", "dplyr", "rlang", "scales", "fresh", "lubridate",
  "shinySearchbar", "emayili", "shinyjs", "sever", "shinycssloaders", "shinyWidgets",
  "reactablefmtr", "reactable", "htmltools", "htmlwidgets", "writexl"
)

#check if installed and load
invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# * 2 Load .Renviron -----------------------------------------------------------

readRenviron(here("test", "app", ".Renviron"))

# * 3 Load functions -----------------------------------------------------------

source(here("test", "app", "functions", "func_accordion.R"))
source(here("test", "app", "functions", "func_cell_layout.R"))
source(here("test", "app", "functions", "func_colours_fresh.R"))
source(here("test", "app", "functions", "func_email.R"))
source(here("test", "app", "functions", "func_filters.R"))
source(here("test", "app", "functions", "func_column_defs.R"))
source(here("test", "app", "functions", "func_row_defs.R"))
source(here("test", "app", "functions", "func_mandatory_fields.R"))
source(here("test", "app", "functions", "func_reset_fields.R"))
source(here("test", "app", "functions", "func_submit_data.R"))

# * 4 Load modules -----------------------------------------------------------

source(here("test", "app", "modules", "mod_controlbar.R"))
source(here("test", "app", "modules", "mod_header.R"))
source(here("test", "app", "modules", "mod_app_info.R"))
source(here("test", "app", "modules", "mod_prod_filter.R"))
source(here("test", "app", "modules", "mod_feat_filter.R"))
source(here("test", "app", "modules", "mod_sub_data.R"))
source(here("test", "app", "modules", "mod_article.R"))
source(here("test", "app", "modules", "mod_about.R"))
source(here("test", "app", "modules", "mod_contact.R"))
source(here("test", "app", "modules", "mod_footer.R"))
source(here("test", "app", "modules", "mod_timeout.R"))

#  * 5 load data -----------------------------------------------
df_sia_shiny_filters <- readRDS(here("test", "app", "data", "df_shiny_sia_wd_filter.rds"))
df_sia_shiny_info <- readRDS(here("test", "app", "data", "df_shiny_sia_wd_info.rds"))
df_sia_osf <- readRDS(here("test", "app", "data", "df_osf_sia_wd_shiny.rds"))

glos <- readRDS(here("test", "app", "www","glos.rds"))

#  * 6 calculate no of wearables for home page -----------------------------------------------
n_wearables <- nrow(df_sia_shiny_filters)

#  * 7 set spinner table -----------------------------------------------
options(spinner.type = 5, spinner.color = "#f15a29", spinner.size = 0.5, hide.ui = FALSE)

#  * 8 reactable layout -----------------------------------------------

#  * * 8.1 colours -----------------------------------------------

#  sticky columns color
sticky_style <- list(backgroundColor = "#f7f7f7")

# base color palette numerical columns
pal_num_scale <- generate_alpha_palette("#1c75bc", 100)

#  * * 8.2 cells -----------------------------------------------

# 1. Define SiA score bars
bar_vars <- c("long_term_all_score", "short_term_all_score")

# 2. Numerical columns (exclude bar vars + device_id)
numeric_vars <- names(df_sia_shiny_filters)[
  sapply(df_sia_shiny_filters, is.numeric) &
    !names(df_sia_shiny_filters) %in% c(bar_vars, "device_id")
]

# 3. Compute min/max ranges for each numeric var
numeric_var_ranges <- suppressWarnings(
  lapply(numeric_vars, function(var) {
    range(df_sia_shiny_filters[[var]], na.rm = TRUE)
  })
)
names(numeric_var_ranges) <- numeric_vars

# 4. Yes/No columns (lowercase "yes"/"no")
yn_vars <- names(df_sia_shiny_filters)[
  sapply(df_sia_shiny_filters, is.character) &
    sapply(df_sia_shiny_filters, function(x) any(x %in% c("yes", "no"), na.rm = TRUE))
]

# 5. Character (categorical) columns
char_vars <- setdiff(
  names(df_sia_shiny_filters),
  c(bar_vars, yn_vars, numeric_vars, "device_id")
)

#  * 9 Mandatory fields ---------------------------

# 1. data
fieldsMandatory_data <- c(
  "name", "email", "manufacturer", "model",
  "market_status", "main_use", "device_cost",
  "wearable_type", "location", "weight_gr", "size_mm"
)

# 2. email
fieldsMandatory_email <- c("name", "email", "message")

# * 10 Rename table variables ---------------------------

# * * 10.1 Filters ---------------------------

rename_map <- c(
  "long_term_all_score" = "Long-Term SiA Score",
  "short_term_all_score" = "Short-Term SiA Score",
  "details" = "Details",
  "manufacturer" = "Manufacturer",
  "model" = "Model",
  "website" = "Website",
  "release_year" = "Release Year",
  "market_status" = "Market Status",
  "main_use" = "Main Use",
  "device_cost" = "Cost (€)",
  "wearable_type" = "Type",
  "location" = "Location",
  "size_mm" = "Size (mm)",
  "weight_gr" = "Weight (gr)",
  "bio_cueing_spec_boel_value" = "Bio Cueing",
  "bio_feedback_spec_boel_value" = "Bio Feedback",
  "water_resistance_spec_boel_value" = "Water Resistance",
  "battery_life_spec_num_value" = "Battery Life (h)",
  "charging_duration_spec_num_value" = "Charging Duration (min)",
  "accelerometer_available" = "Accelerometer",
  "bp_available" = "Blood Pressure",
  "ecg_available" = "ECG",
  "eda_available" = "EDA",
  "eeg_available" = "EEG",
  "emg_available" = "EMG",
  "gps_available" = "GPS",
  "gyroscope_available" = "Gyroscope",
  "icg_available" = "ICG",
  "other_signals_available" = "Other Signals",
  "ppg_available" = "PPG",
  "respiration_available" = "Respiration",
  "skin_temperature_available" = "Skin Temperature",
  "fda_clearance_spec_boel_value" = "FDA Clearance",
  "gdpr_compliance_spec_boel_value" = "GDPR Compliance",
  "ce_marking_spec_boel_value" = "CE Marking",
  "int_storage_met_spec_boel_value" = "Internal Storage",
  "raw_data_available_spec_boel_value" = "Raw Data Available",
  "server_data_storage_spec_boel_value" = "Server Data Storage",
  "dev_storage_cap_hr_spec_num_value" = "Storage (hrs)",
  "dev_storage_cap_mb_spec_num_value" = "Storage (MB)",
  "usability_n_of_studies" = "Usability Studies (n)",
  "validity_and_reliability_n_of_studies" = "Validity & Reliability Studies (n)",
  "usability_evidence_level" = "Usability Evidence Level",
  "validity_and_reliability_evidence_level" = "Validity & Reliability Evidence Level"
)

# * * 10.1 Submit data ---------------------------

rename_subm<- names(rename_map)

rename_subm <- rename_subm[!rename_subm %in% c("sia_es_long", "sia_es_short")]

rename_subm <- c("name", "email", "telephone", "institution", rename_subm, "additional_information")

#  * 11 Time-out message -----------------------------------------------
disconnected <- tagList(
  p(strong("Time Out!", style = "color: #1c75bc; font-size:30px")),
  p(tags$img(src = "favicon.ico", height = 100, width = 100)),
  p("You haven't been active for over 1 hour", br(),
    "or your system went into sleep mode.", br(),
    "To help", strong("Un-Stress", style = "color: #f15a29; font-size:18px"), "the server", br(),
    "your session has ended.", style = "font-size:16px"),
  p(reload_button("Refresh")),
  p("Just hit refresh to continue", br(),
    "where you left off!", style = "font-size:16px")
)



