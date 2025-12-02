############################################################################################
#
# Function for email
#
# Stress in Action 2025
#
#############################################################################################

# Function to send email
send_email <- function(body, subject, attachment = NULL) {

  # SMTP settings (use environment variables for security)
  smtp <- server(
    host <- Sys.getenv("SMTP_HOST"),
    port <- as.integer(Sys.getenv("SMTP_PORT")),
    username <- Sys.getenv("MAIL_NAME_ID"),
    password <- Sys.getenv("MAIL_KEY_ID")
  )

  # Create email message
  msg <- envelope(
    to = Sys.getenv("MAIL_NAME_ID"),
    from = Sys.getenv("MAIL_NAME_ID")
  ) %>%
    subject(subject) %>%
    text(body)

  # Add attachment only if provided
  if (!is.null(attachment) && file.exists(attachment)) {
    msg <- msg %>% attachment(attachment)
  }

  # Send email
  smtp(msg, verbose = FALSE)
}

