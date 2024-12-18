# Analysis2.R: This contains the analysis of the Frontier League 2023 vs. 2024 data

library(tidyverse)
library(ggplot2)
library(nnet)
library(caret)

FL_2024 <- read.xlsx("Frontier League Hitter Data.xlsx", sheet = "2024 FL m2")
FL_2023 <- read.xlsx("Frontier League Hitter Data.xlsx", sheet = "2023 FL m2")

# Take out all empty columns of FL_2024
FL_2024 <- FL_2024[, colSums(is.na(FL_2024)) != nrow(FL_2024)]

# Add Age to all FL_2024 players manually
age_vector = c(27, 27, 22, 29, 27, 27, 25, 27, 24, 26, 25, 28, 26, 24, 27, 
                25, 27, 23, 26, 26, 24, 27, 29, 25, 26, 28, 31, 26, 27, 29, 29, 
                27, 29, 25, 26, 25, 24, 24, 27, 25, 29, 24, 27, 28, 26, 37, 28, 
                28, 25, 28, 28, 26, 28, 25, 23, 24, 27, 31, 22, 26, 25, 24, 24, 
                23, 27, 28, 24, 27, 24, 29, 25, 25, 28, 24, 28, 24, 27, 27, 25,
                29, 22, 26, 24, 25, 27, 27, 24, 27, 25, 35, 25)

FL_2024$Age = age_vector


# Let's first check out a table of the Levels 
table(FL_2023$"2024.Level")


## Because each of A, A+, and AA each have only one player, we will combine them into one level
FL_2023$`2024.Level` <- as.character(FL_2023$`2024.Level`)
# Combine A, A+, and AA into a single category called "LowMinors"
FL_2023$`2024.Level`[FL_2023$`2024.Level` %in% c("A", "A+", "AA")] <- "Affiliated"

# Now check the distribution again
table(FL_2023$`2024.Level`)

# Graph of talent levels 
ggplot(FL_2023, aes(x = `2024.Level`, y = xWOBA_plus)) + 
  geom_boxplot() + 
  labs(title = "xWOBA+ and 2024 Level for 2023 FL Players", x = "Level", y = "xWOBA+") + 
  theme_minimal()

# Adding average exit velocity and launch angle to both dataframes
# Calculate launch speed, and launch angle for each player
pitches_data_2023 = read.csv("pitches_data.csv")
pitches_data_2024 = read.csv("pitches_data_2024_1.csv")

# Calculate hit qualities for each player
# Make Avg_ExitSpeed_50 the average of the top 50% of their Exit Speeds
hit_quality_2023 <- pitches_data_2023 %>%
  filter(Batter %in% FL_2023$Batter) %>%
  filter(!is.na(ExitSpeed)) %>%
  group_by(Batter) %>%
  summarize(
    Avg_ExitSpeed_50 = mean(ExitSpeed[ExitSpeed >= quantile(ExitSpeed, 0.5, na.rm = TRUE)], na.rm = TRUE),
    Avg_Angle = mean(Angle, na.rm = TRUE)
  ) %>%
  arrange(desc("ExitSpeed"))

hit_quality_2024 <- pitches_data_2024 %>%
  filter(Batter %in% FL_2024$Batter) %>%
  filter(!is.na(ExitSpeed)) %>%
  group_by(Batter) %>%
  summarize(
    Avg_ExitSpeed = mean(ExitSpeed, na.rm = TRUE),
    Avg_Angle = mean(Angle, na.rm = TRUE)
  ) %>%
  arrange(desc("ExitSpeed"))

# Merge the hit quality data with the FL data
FL_2023 <- left_join(FL_2023, hit_quality_2023, by = c("Batter" = "Batter"))
FL_2024 <- left_join(FL_2024, hit_quality_2024, by = c("Batter" = "Batter"))

# Analysis
# Ensure the outcome variable is a factor
FL_2023$`2024.Level` <- as.factor(FL_2023$`2024.Level`)

## Graph histogram of 2024.Level
ggplot(FL_2023, aes(x = `2024.Level`)) + 
  geom_bar() + 
  labs(title = "Distribution of 2024 Levels for 2023 FL Players", x = "Level", y = "Count") + 
  theme_minimal()

# Set a seed for reproducibility
set.seed(425)

# Split FL_2023 into training (80%) and testing (20%)
train_index <- createDataPartition(FL_2023$`2024.Level`, p = 0.8, list = FALSE)
train_data <- FL_2023[train_index, ]
test_data <- FL_2023[-train_index, ]

# Define predictor groups
traditional_vars <- c("BA", "OPS")
                      
rate_vars = c("K_percentage", "BB_percentage")

expected_vars <- c("xWOBA_plus")

age_var <- "Age"

hit_quality_vars <- c("Avg_ExitSpeed_50", "Avg_Angle") 

############################################
# Model 1: Age
############################################

model1_predictors <- c(age_var)

# Subset training and testing data
train_model1 <- train_data %>%
  select(all_of(model1_predictors), `2024.Level`)
test_model1 <- test_data %>%
  select(all_of(model1_predictors), `2024.Level`)

# Fit multinom model
model1 <- multinom(`2024.Level` ~ ., data = train_model1)

# Summary of model 1
summary(model1)

# Predict on test data
test_preds_model1 <- predict(model1, newdata = test_model1)

# Evaluate performance
conf_matrix_model1 <- confusionMatrix(test_preds_model1, test_model1$`2024.Level`)
conf_matrix_model1

############################################
# Model 2: Expected Stats
############################################

model2_predictors <- c(expected_vars)

# Subset data
train_model2 <- train_data %>%
  select(all_of(model2_predictors), `2024.Level`)
test_model2 <- test_data %>%
  select(all_of(model2_predictors), `2024.Level`)

# Fit multinom model
model2 <- multinom(`2024.Level` ~ ., data = train_model2)

# Summary of model 2
summary(model2)

# Predict on test data
test_preds_model2 <- predict(model2, newdata = test_model2)

# Evaluate performance
conf_matrix_model2 <- confusionMatrix(test_preds_model2, test_model2$`2024.Level`)
conf_matrix_model2

############################################
# Model 3: Traditional Stats
############################################

model3_predictors <- c(traditional_vars)

# Subset data
train_model3 <- train_data %>%
  select(all_of(model3_predictors), `2024.Level`)
test_model3 <- test_data %>%
  select(all_of(model3_predictors), `2024.Level`)

# Fit multinom model
model3 <- multinom(`2024.Level` ~ ., data = train_model3)

# Summary of model 3
summary(model3)

# Predict on test data
test_preds_model3 <- predict(model3, newdata = test_model3)

# Evaluate performance
conf_matrix_model3 <- confusionMatrix(test_preds_model3, test_model3$`2024.Level`)
conf_matrix_model3

############################################
# Model 4: Hit Quality
############################################

model4_predictors <- c(hit_quality_vars)

# Subset data
train_model4 <- train_data %>%
  select(all_of(model4_predictors), `2024.Level`)
test_model4 <- test_data %>%
  select(all_of(model4_predictors), `2024.Level`)

# Fit multinom model
model4 <- multinom(`2024.Level` ~ ., data = train_model4)

# Summary of model 4
summary(model4)

# Predict on test data
test_preds_model4 <- predict(model4, newdata = test_model4)

# Evaluate performance
conf_matrix_model4 <- confusionMatrix(test_preds_model4, test_model4$`2024.Level`)
conf_matrix_model4

############################################
# Model 5: Rate Stats
############################################

model5_predictors <- c(rate_vars)

# Subset data
train_model5 <- train_data %>%
  select(all_of(model5_predictors), `2024.Level`)
test_model5 <- test_data %>%
  select(all_of(model5_predictors), `2024.Level`)

# Fit multinom model
model5 <- multinom(`2024.Level` ~ ., data = train_model5)

# Summary of model 5
summary(model5)

# Predict on test data
test_preds_model5 <- predict(model5, newdata = test_model5)

# Evaluate performance
conf_matrix_model5 <- confusionMatrix(test_preds_model5, test_model5$`2024.Level`)
conf_matrix_model5

############################################
# Model 6: Traditional + Rate Stats
############################################

model6_predictors <- c(traditional_vars, rate_vars)

# Subset data
train_model6 <- train_data %>%
  select(all_of(model6_predictors), `2024.Level`)
test_model6 <- test_data %>%
  select(all_of(model6_predictors), `2024.Level`)

# Fit multinom model
model6 <- multinom(`2024.Level` ~ ., data = train_model6)

# Summary of model 6
summary(model6)

# Predict on test data
test_preds_model6 <- predict(model6, newdata = test_model6)

# Evaluate performance
conf_matrix_model6 <- confusionMatrix(test_preds_model6, test_model6$`2024.Level`)
conf_matrix_model6

############################################
# Consolidating Outcomes
############################################

# Combine "Amer", "Atl", "PL", and "FL" into "Independent"
# Combine "Mex" and "Foreign" to "Foreign"

FL_2023_new = FL_2023
FL_2024_new = FL_2024

# Combine classes first
FL_2023_new$`2024.Level` <- as.character(FL_2023_new$`2024.Level`)
FL_2023_new$`2024.Level`[FL_2023_new$`2024.Level` %in% c("A", "A+", "AA")] <- "Affiliated"
FL_2023_new$`2024.Level`[FL_2023_new$`2024.Level` %in% c("Amer", "Atl", "PL", "FL", "Mex", "Foreign")] <- "Independent"
FL_2023_new$`2024.Level` <- as.factor(FL_2023_new$`2024.Level`)

## New graph of 2024.Level
ggplot(FL_2023_new, aes(x = `2024.Level`)) + 
  geom_bar() + 
  labs(title = "Distribution of 2024 Levels for 2023 FL Players", x = "Level", y = "Count") + 
  theme_minimal()

## Graph of talent levels
ggplot(FL_2023_new, aes(x = `2024.Level`, y = xWOBA_plus)) + 
  geom_boxplot() + 
  labs(title = "xWOBA+ and 2024 Level for 2023 FL Players", x = "Level", y = "xWOBA+") + 
  theme_minimal()


set.seed(425)
train_index <- createDataPartition(FL_2023_new$`2024.Level`, p = 0.8, list = FALSE)
train_data <- FL_2023[train_index, ]
test_data <- FL_2023[-train_index, ]

############################################
# Model 7: Traditional Stats + Rate Stats
############################################
model7_predictors <- c(traditional_vars, rate_vars)

# Subset data
train_model7 <- train_data %>%
  select(all_of(model7_predictors), `2024.Level`)
test_model7 <- test_data %>%
  select(all_of(model7_predictors), `2024.Level`)

# Fit multinom model
model7 <- multinom(`2024.Level` ~ ., data = train_model7)

# Summary of model 6
summary(model7)

# Predict on test data
test_preds_model7 <- predict(model7, newdata = test_model7)

# Evaluate performance
conf_matrix_model7 <- confusionMatrix(test_preds_model7, test_model7$`2024.Level`)
conf_matrix_model7

############################################
# Compare Model Performance
############################################

## Model 6 performed the best by accuracy. Let's use it to predict 2025 levels for FL_202

############################################
# Predicting 2025 Levels for FL_2024
############################################

############################################
# Predicting 2025 Levels for FL_2024
############################################

# Ensure FL_2024 has the same columns (predictors) as the model chosen (model6, in this example)
FL_2024_predictors <- FL_2024 %>%
  select(all_of(model6_predictors))

# Instead of predicting a single class, predict probabilities for each class
FL_2024_probs <- predict(model6, newdata = FL_2024_predictors, type = "probs")

# Examine the first few rows of the probability matrix
head(FL_2024_probs)

############################################
# After predicting probabilities with type = "probs"
############################################

# Attach the probability columns to FL_2024
FL_2024_with_probs <- cbind(FL_2024, FL_2024_probs)

# Assuming your model's outcome classes include "Affiliated" and "DNP" as columns in FL_2024_probs
# Show top 5 highest chances of going to Affiliated
top_5_affiliated <- FL_2024_with_probs %>%
  arrange(desc(Affiliated)) %>%
  head(5) %>%
  select(Batter, Affiliated)

top_5_affiliated

# Show top 5 highest chances of going DNP
top_5_dnp <- FL_2024_with_probs %>%
  arrange(desc(DNP)) %>%
  head(5) %>%
  select(Batter, DNP)

top_5_dnp

## Make a graph of the Affiliated probability and xWOBA+
ggplot(FL_2024_with_probs, aes(x = xWOBA_plus, y = Affiliated)) + 
  geom_point() + 
  labs(title = "Affiliated Probability and xWOBA+ for 2024 FL Players", x = "Affiliated Probability", y = "xWOBA+") + 
  theme_minimal()




