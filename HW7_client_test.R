## Write your client test code here
# HW7_client_test.R
# Client script to test predict_prob and predict_class API endpoints

library(httr)
library(jsonlite)

# -------------------------------------------------------------
# API LOCATION
# -------------------------------------------------------------
api_url <- "http://127.0.0.1:8000"

# -------------------------------------------------------------
# Sample test data (MUST match HW4 variable names)
# You may add more rows—this tests vector length!
# -------------------------------------------------------------
test_data <- data.frame(
  provider_id = c(10, 20),
  age = c(35, 44),
  specialty = c(3, 1),
  appt_time = c("2025-01-10 15:00:00", "2025-02-05 09:30:00"),
  appt_made = c("2025-01-01", "2025-01-20"),
  lead_time_days = c(9, 16),
  appt_hour = c(15, 9),
  appt_weekday = c(6, 4),
  no_show = c(0, 0)   # HW4 expects this column; it's ignored by prediction
)

# Convert to JSON for POST
json_body <- toJSON(test_data, dataframe = "rows")

# -------------------------------------------------------------
# FUNCTION TO CALL ENDPOINTS
# -------------------------------------------------------------

call_endpoint <- function(endpoint) {
  url <- paste0(api_url, "/", endpoint)
  response <- POST(
    url,
    body = json_body,
    encode = "json",
    content_type_json()
  )
  stop_for_status(response)
  content(response, as = "parsed", simplifyVector = TRUE)
}

# -------------------------------------------------------------
# TEST predict_prob
# -------------------------------------------------------------
cat("\n---- Testing /predict_prob ----\n")
prob_result <- call_endpoint("predict_prob")
print(prob_result)
cat("Length:", length(prob_result), "\n")

# -------------------------------------------------------------
# TEST predict_class
# -------------------------------------------------------------
cat("\n---- Testing /predict_class ----\n")
class_result <- call_endpoint("predict_class")
print(class_result)
cat("Length:", length(class_result), "\n")

# -------------------------------------------------------------
# BASIC CHECKS
# -------------------------------------------------------------
cat("\n---- Verification ----\n")

if (length(prob_result) == nrow(test_data)) {
  cat("predict_prob length ✓ correct\n")
} else {
  cat("predict_prob length ✗ incorrect\n")
}

if (length(class_result) == nrow(test_data)) {
  cat("predict_class length ✓ correct\n")
} else {
  cat("predict_class length ✗ incorrect\n")
}

if (all(class_result %in% c(0, 1))) {
  cat("predict_class output ✓ only 0/1\n")
} else {
  cat("predict_class output ✗ contains invalid values\n")
}
