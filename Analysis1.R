# Analysis1.R: This contains the analysis of the Single A vs. Frontier League data

# Load required libraries
library(tidyverse)
library(ggplot2)
library(caret)
library(nnet)
library(openxlsx)

# Load data
FL_2024 <- read.xlsx("Frontier League Hitter Data.xlsx", sheet = "2024 FL m1")
FL_2024 = FL_2024 %>%
  select(-c(p_2025_a_or_lower, p_2026_a_or_lower, p_2027_a_or_lower))
FL_2024 <- as.data.frame(FL_2024)
Statcast <- read.xlsx("Frontier League Hitter Data.xlsx", sheet = "2024 Statcast m1")

# Rename columns for consistency
names(FL_2024) <- tolower(names(FL_2024))
names(Statcast) <- tolower(names(Statcast))

# Find average xwoba for 2024 Statcast data
avg_xwoba <- mean(Statcast$xwoba, na.rm = TRUE)

# Calculate xwoba_plus
Statcast <- Statcast %>%
  mutate(xwoba_plus = xwoba / avg_xwoba)

# Combine levels into a single categorical variable
Statcast <- Statcast %>%
  mutate(
    target_level = case_when(
      `2022.level` == "A+" ~ "A+",
      `2022.level` == "AA" ~ "AA",
      `2022.level` == "AAA" ~ "AAA",
      `2022.level` == "MLB" ~ "MLB",
      TRUE ~ "Other"  # Include DNP, Foreign, Ind, etc., as "Other"
    )
  )

# Convert to factor
Statcast$target_level <- factor(Statcast$target_level, 
                                levels = c("A+", "AA", "AAA", "MLB", "Other"))

## Define a train multinomial model function

train_multinomial_model <- function(data, predictors, target) {
  # Create formula
  formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  
  # Train multinomial logistic regression model
  multinom_model <- multinom(formula, data = data, trace = FALSE)
  
  # Predict probabilities on the training set
  predicted_probs <- predict(multinom_model, data, type = "probs")
  
  # Convert target to one-hot encoded
  actuals <- model.matrix(~ 0 + data[[target]])
  colnames(actuals) <- levels(data[[target]])  # Ensure column names match levels
  
  # Align columns between actuals and predicted_probs
  actuals <- actuals[, colnames(predicted_probs), drop = FALSE]  
  predicted_probs <- predicted_probs[, colnames(actuals), drop = FALSE]
  
  # Calculate log loss
  logloss <- -mean(rowSums(actuals * log(predicted_probs + 1e-15)))  # Add epsilon for numerical stability
  
  # Return model and log loss
  list(model = multinom_model, logloss = logloss)
}

# Predictor sets
predictor_sets <- list(
  Traditional = c("obp", "slg", "k_percentage", "bb_percentage"),
  HitQuality = c("hard_hit_percentage", "avg_angle"),
  Expected = c("xwoba_plus")
)

# Train models
multinom_results <- list()
for (set_name in names(predictor_sets)) {
  predictors <- predictor_sets[[set_name]]
  cat("\nTraining multinomial model with predictor set:", set_name, "\n")
  multinom_results[[set_name]] <- train_multinomial_model(Statcast, predictors, "target_level")
}

# Create a data frame to summarize log loss results
log_loss_results <- data.frame(
  Predictor_Set = names(multinom_results),
  LogLoss = sapply(multinom_results, function(x) x$logloss)
)

# Print log loss results
print(log_loss_results)

# Plot log loss
ggplot(log_loss_results, aes(x = Predictor_Set, y = LogLoss, fill = Predictor_Set)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Log Loss for Predictor Sets in 2025 Multinomial Model",
    x = "Predictor Set",
    y = "Log Loss"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Use the Traditional model for prediction
FL_2024_probs <- predict(multinom_results$Traditional$model, 
                         newdata = FL_2024 %>% select(all_of(predictor_sets$Traditional)), 
                         type = "probs")

# Add probabilities as new columns to FL_2024
FL_2024 <- FL_2024 %>%
  mutate(
    p_2025_a_plus = FL_2024_probs[, "A+"],
    p_2025_aa = FL_2024_probs[, "AA"],
    p_2025_other = FL_2024_probs[, "Other"]
  )


# Load Statcast data for training
Statcast <- read.xlsx("Frontier League Hitter Data.xlsx", sheet = "2024 Statcast m1")
names(Statcast) <- tolower(names(Statcast))

# --- Preprocessing ---
# Calculate average xwOBA and add xwOBA+ to Statcast
avg_xwoba <- mean(Statcast$xwoba, na.rm = TRUE)
Statcast <- Statcast %>%
  mutate(xwoba_plus = xwoba / avg_xwoba)

# Combine 2023 levels into a single categorical variable for 2026 predictions
Statcast <- Statcast %>%
  mutate(
    target_level = case_when(
      `2023.level` == "A+" ~ "A+",
      `2023.level` == "AA" ~ "AA",
      `2023.level` == "AAA" ~ "AAA",
      `2023.level` == "MLB" ~ "MLB",
      TRUE ~ "Other"  # All other categories
    )
  )

# Convert target_level to a factor
Statcast$target_level <- factor(Statcast$target_level, 
                                levels = c("A+", "AA", "AAA", "MLB", "Other"))

# --- Define Train Function for Multinomial Logistic Regression ---
train_multinomial_model <- function(data, predictors, target) {
  # Create formula
  formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  
  # Train multinomial logistic regression model
  multinom_model <- multinom(formula, data = data, trace = FALSE)
  
  # Predict probabilities on training data
  predicted_probs <- predict(multinom_model, data, type = "probs")
  
  # One-hot encode the target variable
  actuals <- model.matrix(~ 0 + data[[target]])
  colnames(actuals) <- levels(data[[target]])
  
  # Align predicted probabilities with actuals
  actuals <- actuals[, colnames(predicted_probs), drop = FALSE]
  predicted_probs <- predicted_probs[, colnames(actuals), drop = FALSE]
  
  # Calculate log loss
  logloss <- -mean(rowSums(actuals * log(predicted_probs + 1e-15)))  # Epsilon for numerical stability
  
  list(model = multinom_model, logloss = logloss)
}

# --- Train Models ---
# Predictor sets
predictor_sets <- list(
  Traditional = c("obp", "slg", "k_percentage", "bb_percentage"),
  HitQuality = c("hard_hit_percentage", "avg_angle"),
  Expected = c("xwoba_plus")
)

# Train models for each predictor set and store results
multinom_results <- list()
for (set_name in names(predictor_sets)) {
  predictors <- predictor_sets[[set_name]]
  cat("\nTraining multinomial model with predictor set:", set_name, "\n")
  multinom_results[[set_name]] <- train_multinomial_model(Statcast, predictors, "target_level")
}

# Summarize log loss results
log_loss_results <- data.frame(
  Predictor_Set = names(multinom_results),
  LogLoss = sapply(multinom_results, function(x) x$logloss)
)

# Print and plot log loss results
print(log_loss_results)
ggplot(log_loss_results, aes(x = Predictor_Set, y = LogLoss, fill = Predictor_Set)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Log Loss for Predictor Sets in 2026 Multinomial Model",
    x = "Predictor Set",
    y = "Log Loss"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 2026 Predictions for Frontier League Players ---
# Use the Traditional model (example: lowest log loss)
FL_2026_probs <- predict(multinom_results$Traditional$model, 
                         newdata = FL_2024 %>% select(all_of(predictor_sets$Traditional)), 
                         type = "probs")

# Add predicted probabilities as new columns to FL_2024
FL_2024 <- FL_2024 %>%
  mutate(
    p_2026_a_plus = FL_2026_probs[, "A+"],
    p_2026_aa = FL_2026_probs[, "AA"],
    p_2026_aaa = FL_2026_probs[, "AAA"],
    p_2026_mlb = FL_2026_probs[, "MLB"],
    p_2026_other = FL_2026_probs[, "Other"]
  )

# Print updated FL_2024 with 2026 probabilities
print(head(FL_2024))

# Combine 2024 levels into a single categorical variable for 2027 predictions
Statcast <- Statcast %>%
  mutate(
    target_level = case_when(
      `2024.level` == "A+" ~ "A+",
      `2024.level` == "AA" ~ "AA",
      `2024.level` == "AAA" ~ "AAA",
      `2024.level` == "MLB" ~ "MLB",
      TRUE ~ "Other"  # All other categories
    )
  )

# Convert target_level to a factor
Statcast$target_level <- factor(Statcast$target_level, 
                                levels = c("A+", "AA", "AAA", "MLB", "Other"))

# --- Define Train Function for Multinomial Logistic Regression ---
train_multinomial_model <- function(data, predictors, target) {
  # Create formula
  formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  
  # Train multinomial logistic regression model
  multinom_model <- multinom(formula, data = data, trace = FALSE)
  
  # Predict probabilities on training data
  predicted_probs <- predict(multinom_model, data, type = "probs")
  
  # One-hot encode the target variable
  actuals <- model.matrix(~ 0 + data[[target]])
  colnames(actuals) <- levels(data[[target]])
  
  # Align predicted probabilities with actuals
  actuals <- actuals[, colnames(predicted_probs), drop = FALSE]
  predicted_probs <- predicted_probs[, colnames(actuals), drop = FALSE]
  
  # Calculate log loss
  logloss <- -mean(rowSums(actuals * log(predicted_probs + 1e-15)))  # Epsilon for numerical stability
  
  list(model = multinom_model, logloss = logloss)
}

# --- Train Models ---
# Predictor sets
predictor_sets <- list(
  Traditional = c("obp", "slg", "k_percentage", "bb_percentage"),
  HitQuality = c("hard_hit_percentage", "avg_angle"),
  Expected = c("xwoba_plus")
)

# Train models for each predictor set and store results
multinom_results <- list()
for (set_name in names(predictor_sets)) {
  predictors <- predictor_sets[[set_name]]
  cat("\nTraining multinomial model with predictor set:", set_name, "\n")
  multinom_results[[set_name]] <- train_multinomial_model(Statcast, predictors, "target_level")
}

# Summarize log loss results
log_loss_results <- data.frame(
  Predictor_Set = names(multinom_results),
  LogLoss = sapply(multinom_results, function(x) x$logloss)
)

# Print and plot log loss results
print(log_loss_results)
ggplot(log_loss_results, aes(x = Predictor_Set, y = LogLoss, fill = Predictor_Set)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Log Loss for Predictor Sets in Multinomial Model",
    x = "Predictor Set",
    y = "Log Loss"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 2027 Predictions for Frontier League Players ---
# Use the Traditional model (example: lowest log loss)
FL_2027_probs <- predict(multinom_results$Traditional$model, 
                         newdata = FL_2024 %>% select(all_of(predictor_sets$Traditional)), 
                         type = "probs")

# Add predicted probabilities as new columns to FL_2024
FL_2024 <- FL_2024 %>%
  mutate(
    p_2027_a_plus = FL_2027_probs[, "A+"],
    p_2027_aa = FL_2027_probs[, "AA"],
    p_2027_aaa = FL_2027_probs[, "AAA"],
    p_2027_mlb = FL_2027_probs[, "MLB"],
    p_2027_other = FL_2027_probs[, "Other"]
  )

FL_2024 = FL_2024 %>%
  select(-c(p_2025_aaa, p_2025_mlb))

# Write the final data frame to a CSV file
write.csv(FL_2024, "Frontier_League_Player_Projections.csv", row.names = FALSE)

