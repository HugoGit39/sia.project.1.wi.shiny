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

# * * 11 Glossery ----

GLOS <- list(

  A = div(style = "font-size:16px", align = "justify",
          p(strong("Accelerometer"), br(),
            "—"),
          p(strong("Additional software"), br(),
            "Additional software available (e.g., for analysis purposes).")
  ),

  B = div(style = "font-size:16px", align = "justify",
          p(strong("Battery life"), br(),
            "Maximum battery life as specified by the manufacturer (in hours). Note: this is the maximum; enabling detailed recording (e.g., continuous EDA or raw data) can significantly reduce battery life."),
          p(strong("Bio-cueing"), br(),
            "Options to cue users based on their physiology, as described by the manufacturers (e.g., vibrations, stress notifications, sound alerts)."),
          p(strong("Bio-feedback"), br(),
            "Access users get into their physiology (e.g., via device display)."),
          p(strong("BP — Blood Pressure"), br(),
            "Blood pressure.")
  ),

  C = div(style = "font-size:16px", align = "justify",
          p(strong("CE approval/label"), br(),
            "Device has been assessed to meet EU market regulations. Devices officially sold to European clients need CE marking."),
          p(strong("Charging duration"), br(),
            "Time needed (in minutes) to fully recharge the battery."),
          p(strong("Charging method"), br(),
            "The method by which the device's battery is replenished."),
          p(strong("Compatibility"), br(),
            "System compatibility of the device (both mobile and computer).")
  ),

  D = div(style = "font-size:16px", align = "justify",
          p(strong("Data transfer method"), br(),
            "All methods of transferring the data from the device to a computer or a mobile phone."),
          p(strong("Device name"), br(),
            "Name of the device."),
          p(strong("Device costs"), br(),
            "One-time purchase price and additional costs (e.g., subscription, software needed to operate the device, discount options, etc.)."),
          p(strong("Device storage capacity"), br(),
            "Hours and/or size of data that can be recorded and stored internally, as specified by the manufacturer. If a range is given, the maximum values are specified. Storage hours may vary with changes in sampling frequencies."),
          p(strong("Dividers/Notes"), br(),
            "—")
  ),

  E = div(style = "font-size:16px", align = "justify",
          p(strong("ECG — Electrocardiogram"), br(),
            "Electrocardiogram."),
          p(strong("EDA — Electrodermal Activity"), br(),
            "Also known as Galvanic Skin Response."),
          p(strong("EMG — Electromyography"), br(),
            "Electromyography.")
  ),

  F = div(style = "font-size:16px", align = "justify",
          p(strong("FDA approval/clearance"), br(),
            "Device (or its components) has/have been FDA-approved or cleared.")
  ),

  G = div(style = "font-size:16px", align = "justify",
          p(strong("GDPR compliance"), br(),
            "Compliance with the General Data Protection Regulation. Beware of uncertainties (e.g., hosting in the USA claiming GDPR compliance). Researchers must determine the final verdict."),
          p(strong("General usability synthesis"), br(),
            "Short synthesis of the usability results of reviewed studies (includes user experience/friendliness and adherence)."),
          p(strong("General validity and reliability synthesis"), br(),
            "Short synthesis of validity/reliability results across all parameters studied."),
          p(strong("GPS — Global Positioning System"), br(),
            "Global Positioning System."),
          p(strong("Gyroscope"), br(),
            "Note: accelerometers and gyroscopes are often combined in an IMU (Inertial Measurement Unit).")
  ),

  H = div(style = "font-size:16px", align = "justify",
          p(strong("Highest level of validation evidence"), br(),
            "External is regarded as the highest level, followed by internal, and lastly no validation."),
          p(strong("Hyperlink to the device RVU page"), br(),
            "Link to the device’s RVU sheet (use the device name as the display text).")
  ),

  I = div(style = "font-size:16px", align = "justify",
          p(strong("ICG — Impedance Cardiography"), br(),
            "Impedance cardiography."),
          p(strong("Internal storage method"), br(),
            "Availability and method of internal data storage.")
  ),

  J = div(style = "font-size:16px", align = "justify", p("")),
  K = div(style = "font-size:16px", align = "justify", p("")),

  L = div(style = "font-size:16px", align = "justify",
          p(strong("Last updated"), br(),
            "Date at which information on a given device was last updated."),
          p(strong("Location"), br(),
            "Where the device is worn (e.g., wrist, chest). May differ from the recording location.")
  ),

  M = div(style = "font-size:16px", align = "justify",
          p(strong("Main use"), br(),
            "Primary intended setting the device was developed for (usually listed on website/manual)."),
          p(strong("Manufacturer"), br(),
            "Name of the manufacturer."),
          p(strong("Market status"), br(),
            "Current — available for purchase; Discontinued — no longer sold/supported; Upcoming — soon to be available."),
          p(strong("Most recent date of RVU search"), br(),
            "Date of the most recent RVU search for a device.")
  ),

  N = div(style = "font-size:16px", align = "justify",
          p(strong("Number of usability studies reviewed"), br(),
            "Count of usability studies included (use 0 if none)."),
          p(strong("Number of validity and reliability studies reviewed"), br(),
            "Total number of validity, reliability and usability studies included for the device.")
  ),

  O = div(style = "font-size:16px", align = "justify",
          p(strong("On other signals"), br(),
            "—"),
          p(strong("Other signals"), br(),
            "All other signals the device can record which are not listed above. Separate using ‘;’.")
  ),

  P = div(style = "font-size:16px", align = "justify",
          p(strong("Parameter sampling window"), br(),
            "The window over which each parameter is calculated (e.g., 1 min to 1 day)."),
          p(strong("PPG — Photoplethysmography"), br(),
            "Photoplethysmography."),
          p(strong("Provided parameters"), br(),
            "Processed data that can be exported. Systems may filter/average over large time windows—check computation details before use.")
  ),

  Q = div(style = "font-size:16px", align = "justify", p("")),

  R = div(style = "font-size:16px", align = "justify",
          p(strong("Raw data available"), br(),
            "Whether signal-level data can be recorded and exported for analysis."),
          p(strong("Release date"), br(),
            "Official market release date of the device."),
          p(strong("Respiration"), br(),
            "Respiration and how it is derived (e.g., via ICG or PPG; specify under method)."),
          p(strong("Required software"), br(),
            "Software required to record and/or extract the data."),
          p(strong("RVU — Related fields"), br(),
            "—")
  ),

  S = div(style = "font-size:16px", align = "justify",
          p(strong("Server data storage"), br(),
            "Data stored on external servers (and their location). Often unclear—may require direct inquiry."),
          p(strong("SiA long-term usefulness score"), br(),
            "Average perceived usefulness for future long-term SiA studies (assessed by three authors)."),
          p(strong("SiA short-term usefulness score"), br(),
            "Average perceived usefulness for future short-term SiA studies (assessed by three authors)."),
          p(strong("Size"), br(),
            "Device dimensions (mm): Length × Width × Height, or Diameter × Height for round objects. Rings use US ring sizes (e.g., 4–10)."),
          p(strong("Skin Temp — Skin Temperature"), br(),
            "Skin temperature."),
          p(strong("Studied parameters"), br(),
            "List of parameters included across all reviewed studies."),
          p(strong("Support notes"), br(),
            "—")
  ),

  T = div(style = "font-size:16px", align = "justify",
          p(strong("Data transfer method"), br(),
            "All methods of transferring data from device to computer or phone."),
          p(strong("Water resistance (see also ‘Testing standards’)"), br(),
            "Depth (m) and time (min). Different naming standards exist (ATM, ISO, IEC). We standardize to meters/minutes. ATM-to-meters is based on static pressure; real-world dynamic pressure can exceed ratings (e.g., 3 ATM = 30 m = splash only).")
  ),

  U = div(style = "font-size:16px", align = "justify",
          p(strong("General usability synthesis"), br(),
            "Short synthesis of usability results, including adherence.")
  ),

  V = div(style = "font-size:16px", align = "justify",
          p(strong("Validation — Highest level of validation evidence"), br(),
            "External > Internal > No validation.")
  ),

  W = div(style = "font-size:16px", align = "justify",
          p(strong("Wearable type"), br(),
            "Type of device (e.g., watch, CPU + electrodes). If a type is missing, add to the options."),
          p(strong("Website"), br(),
            "Link to the device webpage."),
          p(strong("Weight"), br(),
            "Device weight (grams), whole numbers (decimals rounded)."),
          p(strong("Water resistance"), br(),
            "See ‘T — Water resistance’.")
  ),

  X = div(style = "font-size:16px", align = "justify", p("")),
  Y = div(style = "font-size:16px", align = "justify", p("")),
  Z = div(style = "font-size:16px", align = "justify", p(""))
)

#  * 12 Time-out message -----------------------------------------------
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



