## HW7_api_server.R
## Complete API server for patient no-show prediction

library(plumber)
library(tidyverse)
library(lubridate)
library(caret)
library(pROC)

set.seed(2025)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------
train <- read_csv("train_dataset.csv.gz")
test  <- read_csv("test_dataset.csv.gz")

# ------------------------------------------------------------
# 2. Feature Engineering (now works for BOTH training + prediction)
# ------------------------------------------------------------
featurize <- function(df) {

  df <- df %>%
    mutate(
      appt_time      = ymd_hms(appt_time),
      appt_made      = as.Date(appt_made),
      lead_time_days = as.numeric(as_date(appt_time) - as_date(appt_made)),
      appt_hour      = hour(appt_time),
      appt_weekday   = as.factor(wday(appt_time, label = TRUE))
    ) %>%
    select(-any_of(c("id", "address")))

  # Only convert no_show for training data — skip during prediction
  if ("no_show" %in% names(df)) {
    df$no_show <- factor(df$no_show, levels = c(0,1), labels = c("Show","NoShow"))
  }

  return(df)
}

# Apply features to training data
train <- featurize(train)

# ------------------------------------------------------------
# 3. Train Logistic Model (same approach as HW4)
# ------------------------------------------------------------
trainIndex <- createDataPartition(train$no_show, p = 0.8, list = FALSE)
train_data <- train[trainIndex, ]
valid_data <- train[-trainIndex, ]

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

log_model <- train(
  no_show ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

# ------------------------------------------------------------
# 4. API ENDPOINTS
# ------------------------------------------------------------

#* @apiTitle No-Show Prediction API

# ---------- PREDICT PROBABILITY ----------
#* @post /predict_prob
function(data) {
  df <- as.data.frame(data)
  df_feat <- featurize(df)
  probs <- predict(log_model, newdata = df_feat, type = "prob")$NoShow
  return(probs)
}

# ---------- PREDICT CLASS ----------
#* @post /predict_class
function(data) {
  df <- as.data.frame(data)
  df_feat <- featurize(df)
  pred <- predict(log_model, newdata = df_feat)
  return(ifelse(pred == "NoShow", 1, 0))
}
