# PrepData.R: Data extraction and cleaning

## Let's first load in the libraries
library(tidyverse)

## Section 1: Data Cleaning

## Two 2023 Datasets
pitches_data = read.csv("pitches_data.csv")
pitches_data_2 = read.csv("pitches_data2.csv")

## Two 2024 Datasets
pitches_data_2024_1 = read.csv("pitches_data_2024_1.csv")
pitches_data_2024_2 = read.csv("pitches_data_2024_2.csv")

# 2023 Data Cleaning and Exploration
## Take out repeat rows
# Because each game results in two entries (one for each team), we will filter out the duplicates and add a unique identifier for each pitch.

pitches_data <- pitches_data %>%
  distinct() %>%
  mutate(pitch_id = row_number())

pitches_data_2 <- pitches_data_2 %>%
  distinct() %>%
  mutate(pitch_id = row_number())

# In order to get expected statistics, we want to start with batted balls. We 
# will filter out pitches that did not result in a batted ball, and then filter 
# out any missing values for ExitSpeed and Angle. These are pitches that were 
# not hit, and therefore do not have a batted ball result. We will also create 
# binary columns for each type of hit (single, double, triple, home run) to use 
# as our outcomes.

# Filter for batted balls
batted_balls <- pitches_data %>%
  filter(
    !is.na(PlayResult),
    !PlayResult %in% c("Walk", "StrikeoutSwinging", "StrikeoutLooking")
  ) %>%
  filter(!is.na(ExitSpeed), !is.na(Angle)) %>%
  mutate(
    is_single = ifelse(PlayResult == "Single", 1, 0),
    is_double = ifelse(PlayResult == "Double", 1, 0),
    is_triple = ifelse(PlayResult == "Triple", 1, 0),
    is_home_run = ifelse(PlayResult == "HomeRun", 1, 0)
  )

## Section 2: Is Direction a Valid Predictor of Hits?
# Now that we have all batted balls, let's try to create models to predict both 
# whether a particular batted ball will be a hit and what type of hit. While 
# some research includes Direction (otherwise known as spray angle) as a 
# feature, we will first use Leave-one-out cross validation to see if it is a 
# good predictor of getting a single, double, triple, or home run in our model.

# 5-Fold Cross-Validation
set.seed(42)
n <- nrow(batted_balls)
folds <- sample(rep(1:5, length.out = n))  # Randomly assign each row to one of 5 folds

# Initialize results data frame
results <- data.frame(
  model_set = character(),
  outcome = character(),
  fold = integer(),
  log_loss = numeric(),
  stringsAsFactors = FALSE
)

## Perform the Cross Validation
for (fold in 1:5) {
  # Split data into training and testing sets
  train_data <- batted_balls[folds != fold, ]
  test_data <- batted_balls[folds == fold, ]
  
  # Fit models without Direction
  model_single <- glm(is_single ~ ExitSpeed + Angle, data = train_data, family = binomial)
  model_double <- glm(is_double ~ ExitSpeed + Angle, data = train_data, family = binomial)
  model_triple <- glm(is_triple ~ ExitSpeed + Angle, data = train_data, family = binomial)
  model_home_run <- glm(is_home_run ~ ExitSpeed + Angle, data = train_data, family = binomial)
  
  # Fit models with Direction
  model_single2 <- glm(is_single ~ ExitSpeed + Angle + Direction, data = train_data, family = binomial)
  model_double2 <- glm(is_double ~ ExitSpeed + Angle + Direction, data = train_data, family = binomial)
  model_triple2 <- glm(is_triple ~ ExitSpeed + Angle + Direction, data = train_data, family = binomial)
  model_home_run2 <- glm(is_home_run ~ ExitSpeed + Angle + Direction, data = train_data, family = binomial)
  
  # Predict probabilities for the test set
  preds_no_dir <- data.frame(
    is_single = predict(model_single, test_data, type = "response"),
    is_double = predict(model_double, test_data, type = "response"),
    is_triple = predict(model_triple, test_data, type = "response"),
    is_home_run = predict(model_home_run, test_data, type = "response")
  )
  
  preds_with_dir <- data.frame(
    is_single = predict(model_single2, test_data, type = "response"),
    is_double = predict(model_double2, test_data, type = "response"),
    is_triple = predict(model_triple2, test_data, type = "response"),
    is_home_run = predict(model_home_run2, test_data, type = "response")
  )
  
  # Calculate log loss for each model
  log_loss_no_dir <- colMeans(
    -test_data[, c("is_single", "is_double", "is_triple", "is_home_run")] * 
      log(preds_no_dir) -
      (1 - test_data[, c("is_single", "is_double", "is_triple", "is_home_run")]) * 
      log(1 - preds_no_dir),
    na.rm = TRUE
  )
  
  log_loss_with_dir <- colMeans(
    -test_data[, c("is_single", "is_double", "is_triple", "is_home_run")] * 
      log(preds_with_dir) -
      (1 - test_data[, c("is_single", "is_double", "is_triple", "is_home_run")]) * 
      log(1 - preds_with_dir),
    na.rm = TRUE
  )
  
  # Store results
  results <- rbind(
    results,
    data.frame(
      model_set = "NoDirection",
      outcome = names(log_loss_no_dir),
      fold = fold,
      log_loss = log_loss_no_dir,
      stringsAsFactors = FALSE
    ),
    data.frame(
      model_set = "WithDirection",
      outcome = names(log_loss_with_dir),
      fold = fold,
      log_loss = log_loss_with_dir,
      stringsAsFactors = FALSE
    )
  )
}

# Summarize results
results_summary <- aggregate(log_loss ~ model_set + outcome, data = results, 
                             FUN = mean)
print(results_summary)

# There is virtually no difference in log loss between models with and without 
# Direction. This suggests that Direction is not a good predictor of hits. 
# We will proceed with the models without Direction.


# Because each logistic regression is operating separately, this could lead to a 
# batted ball with probabilities of being a single, double, triple, and home run 
# that sum to more than 1. We will fit a new multinomial logistic regression model 
# to predict the type of hit for each batted ball.

# Combine outcomes into a single categorical variable
batted_balls <- batted_balls %>%
  mutate(outcome = case_when(
    is_single == 1 ~ "Single",
    is_double == 1 ~ "Double",
    is_triple == 1 ~ "Triple",
    is_home_run == 1 ~ "HomeRun",
    TRUE ~ "Not a Hit"
  ))

# Fit a multinomial logistic regression model
model_multinom <- multinom(outcome ~ ExitSpeed + Angle, data = batted_balls)

## Let's now use our multinomiual model for prediction
# Predict probabilities for each outcome
batted_balls <- batted_balls %>%
  bind_cols(predict(model_multinom, newdata = batted_balls, type = "probs"))

## Change column names
colnames(batted_balls)[colnames(batted_balls) == "Single"] <- "x1B"
colnames(batted_balls)[colnames(batted_balls) == "Double"] <- "x2B"
colnames(batted_balls)[colnames(batted_balls) == "Triple"] <- "x3B"
colnames(batted_balls)[colnames(batted_balls) == "HomeRun"] <- "xHR"

## Make xBA and xSLG columns
batted_balls <- batted_balls %>%
  mutate(
    xBA = x1B + x2B + x3B + xHR,
    xSLG = x1B + 2 * x2B + 3 * x3B + 4 * xHR
  )


# Merge results back into the original pitches dataset
pitches_data <- pitches_data %>%
  left_join(
    batted_balls %>%
      select(pitch_id, x1B, x2B, x3B, xHR, xBA, xSLG),
    by = "pitch_id"
  )

# Ok, now our `pitches_data` data frame contains the expected batting average 
# and slugging percentage for each batted ball. This will come in handy soon.

## Section 3: Checking our Model

# Let's graph this as well twice. For all pitches in 2023 that have an ExitSpeed 
# and Angle, let's graph ExitSpeed vs. xBA and Angle vs. xBA. 

# Graph ExitSpeed vs. xBA
ggplot(batted_balls, aes(x = ExitSpeed, y = xBA)) +
  geom_point(alpha = 0.5) +
  ## Add smooth non-linear trend line
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Exit Speed vs. Expected Batting Average",
       x = "Exit Speed (mph)",
       y = "Expected Batting Average") +
  theme_minimal()

# Graph Angle vs. xBA
ggplot(batted_balls, aes(x = Angle, y = xBA)) +
  geom_point(alpha = 0.5) +
  ## Add smooth non-linear trend line
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Angle vs. Expected Batting Average",
       x = "Angle (degrees)",
       y = "Expected Batting Average") +
  theme_minimal()

# These graphs make sense. In general, batting average increases as Exit Speed 
# increases, with lots of variation below 90 MPH, which is when the ball is hit 
# hard enough for it to matter, as explained in the Introduction. However, 
# because we are talking about all kinds of hits, there is signficant variation 
# below 90mph. As for Angle, the expected batting averages are most variable 
# between 0 and 50 degrees, which makes sense as those will generate the most 
# diverse outcomes. All other angles  have a more consistent expected batting 
# average because on the low end of Angle, they will likely just be ground balls 
# (single or not), and on the high end, they are almost all pop ups. 

# Let's graph ExitSpeed and Angle vs. xSLG, because this will be an even better 
# check on if our model is working.
## Graph ExitSpeed vs. xSLG
ggplot(batted_balls, aes(x = ExitSpeed, y = xSLG)) +
  geom_point(alpha = 0.5) +
  ## Add smooth non-linear trend line
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Exit Speed vs. Expected Slugging Percentage",
       x = "Exit Speed (mph)",
       y = "Expected Slugging Percentage") +
  theme_minimal()

# Graph Angle vs. xSLG
ggplot(batted_balls, aes(x = Angle, y = xSLG)) +
  geom_point(alpha = 0.5) +
  ## Add smooth non-linear trend line
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Angle vs. Expected Slugging Percentage",
       x = "Angle (degrees)",
       y = "Expected Slugging Percentage") +
  theme_minimal()

# Both graphs are easier to understand than the xBA graphs. For Exit Speed, 
# the expected slugging percentage increases exponentially with Exit Speed, 
# which makes sense because higher Exit Speed in baseball is generally associated 
# with harder hit balls. The reason there is less variation is because in this 
# graph, all of the weakly hit balls that had a chance to be singles in our xBA 
# graphs are now just singles and not super significant in improving xSLG 
# (Extra base hits are weighed more).

# For Angle, the expected slugging percentage is highest between 20 and 40 degrees, 
# which makes sense because those are the angles that are most likely to result 
# in line drives, and thus, extra base hits. We say that a Launch angle is in the 
# sweet spot between 8 and 32 degrees in the MLB, which aligns with this graph.

## Section 4: Getting Qualified Hitters

# There are multiple ways to qualify hitters based on the number of plate 
# appearances they have, including the standard 3.1 plate appearances per game. 
# However, due to the nature of the minor/independent leagues, where players are 
# moved around a lot and often don't do a full season in one place, we will use a
# more flexible approach. We will define a qualified hitter as someone who has at 
# least 130 plate appearances in a season, which is the definition used for rookie 
# status in the MLB.

# Let's calculate the number of plate appearances for each batter in the 2023 season.

# Calculate the number of plate appearances for each batter
library(dplyr)

# Filter for 2023 season data (if needed)
pitches_2023 <- pitches_data  # Assuming pitches_data is already filtered for 2023

# Identify Plate Appearances (PAs)
# Option 1: Using PlayResult to detect end of a plate appearance
pitches_2023 <- pitches_2023 %>%
  mutate(is_PA_end = !is.na(PlayResult))  # End of PA if PlayResult is not NA

# Option 2: Using PAofInning to group plate appearances
# Uncomment the line below if PAofInning is preferred:
# pitches_2023 <- pitches_2023 %>% mutate(is_PA_end = PAofInning != lag(PAofInning, default = first(PAofInning)))

# Count PAs per batter
plate_appearances <- pitches_2023 %>%
  filter(is_PA_end) %>%  # Only rows marking the end of a plate appearance
  group_by(Batter) %>%
  summarize(
    total_PA = n()
  ) %>%
  arrange(desc(total_PA))

# Filter for qualified hitters (at least 130 PAs)
qualified_hitters <- plate_appearances %>%
  filter(total_PA >= 130)

# Display leaderboard
print(qualified_hitters)

# Based on looking at the data, it seems that using PlayResult was the better 
# option. It is not exact, but with 130 PAs of all of these players, it should be 
# enough PAs to get a good idea of their expected statistics.

# We now have a leaderboard of 147 players with at least 130 plate appearances 
# in the 2023 season. We will use this list to filter out the data for our 
# qualified hitters.

# Filter for qualified hitters
qualified_hitters <- qualified_hitters$Batter

qualified_hitters_2023 <- pitches_2023 %>%
  filter(Batter %in% qualified_hitters)

## Section 5: Calculating Expected Statistics

# Now that we have a dataframe of pitches only with qualified hitters, 
# let's find each of those qualified hitter's xBA and xSLG. First, we only 
# want to keep rows where there is a PlayResult. Then, for each batter, we 
# can use a combination of their xBA's, their strikeouts, walks, errors, and 
# sacrifices to get their xBA. Remember that a sacrifice is not considered an 
# at-bat, so we will need to filter those out.

# Filter the dataframe so it doesn't include non-PlayResult rows
qualified_pitches <- qualified_hitters_2023 %>%
  filter(!is.na(PlayResult))

## Check PlayResult column for unique values
unique(qualified_pitches$PlayResult)

# Calculate xBA and xSLG for each qualified hitter
qualified_stats <- qualified_pitches %>%
  mutate(
    is_batted_ball = !is.na(xBA),  # A batted ball has a valid xBA
    is_at_bat = !PlayResult %in% c("Walk", "Sacrifice")
  ) %>%
  ## Filter out rows where play result is a Single, Double, Triple, HomeRun, Error, Sacrifice, or FieldersChoice and doesn't have a batted ball
  filter(
    !(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Error", "Sacrifice", "FieldersChoice") & 
        (is.na(ExitSpeed) | is.na(Angle)))
  ) %>%
  group_by(Batter) %>%
  summarize(
    Total_PA = n(),  # Total plate appearances
    Total_AB = sum(is_at_bat, na.rm = TRUE),  # At-bats exclude walks and sacrifices
    Walks = sum(PlayResult == "Walk", na.rm = TRUE),
    xOBP = (Walks + sum(xBA[is_batted_ball], na.rm = TRUE)) / Total_PA,
    xBA = sum(xBA[is_batted_ball], na.rm = TRUE) / Total_AB,  # Combine batted balls and strikeouts
    xSLG = sum(
      x1B[is_batted_ball] + 2 * x2B[is_batted_ball] + 3 * x3B[is_batted_ball] + 4 * xHR[is_batted_ball],
      na.rm = TRUE
    ) / Total_AB,  # Weighted slugging contributions divided by at-bats
    K_percentage = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking"), na.rm = TRUE) / Total_PA,
    BB_percentage = sum(PlayResult == "Walk", na.rm = TRUE) / Total_PA,
    Strikeouts = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking"), na.rm = TRUE),
    Hits = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE),
    Singles = sum(PlayResult == "Single", na.rm = TRUE),
    Doubles = sum(PlayResult == "Double", na.rm = TRUE),
    Triples = sum(PlayResult == "Triple", na.rm = TRUE),
    HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
    xHomeRuns = sum(xHR[is_batted_ball], na.rm = TRUE),
    x1B = sum(x1B[is_batted_ball], na.rm = TRUE),
    x2B = sum(x2B[is_batted_ball], na.rm = TRUE),
    x3B = sum(x3B[is_batted_ball], na.rm = TRUE),
    Errors = sum(PlayResult == "Error", na.rm = TRUE),
    OBP = (Hits + Walks) / Total_PA,
    BA = Hits / Total_AB,
    SLG = (Singles + 2 * Doubles + 3 * Triples + 4 * HomeRuns) / Total_AB,
    OPS = OBP + SLG,
    xOPS = xOBP + xSLG,
  ) %>%
  arrange(desc(xHomeRuns))  # Sort by xOPS

# Display leaderboard
print(qualified_stats)

## Section 6: Run Expectancy Matrix
# In order to get one of our most important offensive statistics, wOBA, 
# which is a statistic that the Statcast data has for their minor league players, 
# we need to calculate the run value of each play. To do that, we will go back to 
# our pitch by pitch data. While usually Run Expectancy Matrices are based on 
# base-out states, we do not have runner data for the Frontier League, and thus 
# need to try to calculate run value in different ways.

### Run Value Method #1: Outs
# In Method #1, instead of base-out states, we will treat each play as just 
# having an out state. We will use the expected number of runs scored until the 
# end of the inning from each out state to see how each play (Single, Double, 
# Triple, Home Run, Walk, Strikeout) changes the expected number of runs scored. 
# We will then use this to calculate the average run value for each type of play.

# Add cumulative runs scored and runs to the end of the inning
df <- pitches_data_2 %>%
  ## Only keep rows with a PlayResult
  filter(!is.na(PlayResult)) %>%
  group_by(GameID, Inning, Top.Bottom) %>%
  mutate(
    RunsScored = ifelse(is.na(RunsScored), 0, RunsScored),  # Replace NA with 0
    CumulativeRuns = cumsum(RunsScored),  # Cumulative sum of runs
    RunsToEndOfInning = max(CumulativeRuns) - CumulativeRuns  # Runs remaining to end of inning
  )

# Calculate run expectancy for each number of outs
run_expectancy <- df %>%
  group_by(Outs) %>%
  summarize(
    AvgRunsToEnd = mean(RunsToEndOfInning, na.rm = TRUE)
  )

# Print run expectancy for validation
print(run_expectancy)

# Get outs on play for each row
df = df %>%
  mutate(postOuts = Outs + OutsOnPlay)

# Merge run expectancy into the dataframe
df <- df %>%
  # Add pre-event run expectancy (based on `Outs`)
  left_join(run_expectancy, by = c("Outs" = "Outs")) %>%
  rename(PreRunExpectancy = AvgRunsToEnd) %>%
  
  # Add post-event run expectancy (based on `postOuts`)
  left_join(run_expectancy, by = c("postOuts" = "Outs")) %>%
  rename(PostRunExpectancy = AvgRunsToEnd) %>%
  
  # Set post run expectancy to 0 if postOuts is 3
  mutate(
    PostRunExpectancy = ifelse(postOuts == 3, 0, PostRunExpectancy)
  )

# Calculate run values for each play
df <- df %>%
  mutate(
    RunValue = PostRunExpectancy - PreRunExpectancy + RunsScored)
    
# Our run value is just the difference in run values for the out states before and 
# after the play plus the number of runs scored on that play. Now that we have the 
# run value for each play, we can calculate the average run value for each type 
# of play (Single, Double, Triple, HomeRun, Walk).

# Filter for valid PlayResults and calculate average run values
df_run_values <- df %>%
  group_by(PlayResult) %>%
  summarize(
    AvgRunValue = mean(RunValue, na.rm = TRUE)
  )

# Print linear weights
print(df_run_values)

# A good test that our avergae run values make sense is that a Home Run has the 
# highest run value, followed by a Triple, Double, and Single. Also, 
# StrikeoutLooking and StrikeoutSwinging should have the same run value, as they 
# are both strikeouts. However, in this scenario walks are given almost no weight. 
# This is because batters rarely walk in a run (only happens with bases loaded), 
# so even though there is an extra man on base with potential for more runs, we 
# cannot account for that in this model.

# Let's try a revised approach where we calculate the average number of runs 
# scored from that point until the end of the inning, grouped by Outs and the 
# PlayResult.

### Method 2: Out-Play Result
# In this method, we will group all plays by Outs and PlayResult, and see how 
# many runs are scored, on average, until the end of the inning, plus the number 
# of runs scored on that play. This should allow us to give walks some positive 
# run value, as they will generate runs by the end of the inning. We will then 
# use this to calculate the average run value for each type of play.

df2 = df %>%
  select(-RunValue)
# Calculate run expectancy after each play
run_value_by_play <- df2 %>%
  group_by(Outs, PlayResult) %>%  # Group by outs and play type
  summarize(
    AvgRunsToEnd = mean(RunsToEndOfInning + RunsScored, na.rm = TRUE),  # Average runs scored until the end of the inning
    .groups = "drop"
  )

# Print run expectancy by play type
print(run_value_by_play)

# Let's put those average runs to end values back into our original dataframe.
# Merge run expectancy into the main dataframe
df2 <- df2 %>%
  left_join(run_value_by_play, by = c("Outs", "PlayResult"))


# Aggregate run values for wOBA weights
df2_run_values <- df2 %>%
  group_by(PlayResult) %>%
  
  summarize(
    AvgRunValue = mean(AvgRunsToEnd, na.rm = TRUE),  # Average run value for each play type
    .groups = "drop"
  )

# Print linear weights
print(df2_run_values)

# We now have, for each playresult, a number that encapsulates both how many 
# runs are scored on that play and how many runs are scored until the end of the 
# inning. This is a much better way to calculate run value, as it gives walks a 
# positive value, and also gives more weight to plays that score more runs. 

# The only issue with this method is that Strikeouts still have a positive run 
# value, as even after strikeouts, runs can be scored.

### Method #3: Outs-PlayResult with Run Expectancy

# We will now say that the run value of a play is how many runs are scored on that 
# play, plus the difference in run expectancy by our base-out state before and 
# after the play, plus the number of runs scored until the end of the inning. 
# This should give us the most accurate run value for each play.

df3 = df %>%
  select(-RunValue)

df3_run_values  = df3 %>%
  mutate(RunsValueByPlay = RunsToEndOfInning+RunsScored + (PostRunExpectancy - PreRunExpectancy)) %>%
  group_by(PlayResult) %>%
  summarize(AvgRunsValueByPlay = mean(RunsValueByPlay, na.rm = TRUE))

print(df3_run_values)

# This makes the most sense, and Strikeouts now have zero run value, 
# so we will move forward using these run values.

## Calculating wOBA
# Now that we have the run value for each play, we can calculate the weights 
# for wOBA. The first step to getting linear weights is to make all weights 
# so that the run value of an out/StrikeoutLooking/StrikeoutSwinging is 0.
# 
# These linear weights are contained within the `df2_run_values` and 
# `df3_run_values` dataframes. We will now make sure our Out value is 0.

# Adjust run values to make outs have a value of 0
df2_run_values =
  df2_run_values %>%
  mutate(AvgRunValue = AvgRunValue - df2_run_values$AvgRunValue[df2_run_values$PlayResult == "Out"])

df3_run_values =
  df3_run_values %>%
  mutate(AvgRunsValueByPlay = AvgRunsValueByPlay - df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Out"])

# In order to get the actual weights for our wOBA equation, we will want 
# average wOBA to be equal to average OBP. We will need to go back to our 
# Pitches dataframe to find out the average OBP for all Frontier League 
# batters, not just qualified batters in 2023.

# Calculate average OBP for all batters in 2023
avg_OBP <- pitches_data %>%
  filter(!is.na(PlayResult)) %>%
  summarize(
    Total_PA = n(),
    Walks = sum(PlayResult == "Walk", na.rm = TRUE),
    OBP = (sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE) + Walks) / Total_PA,
    .groups = "drop"
  )

# The average OBP for all Frontier League batters in 2023 is 0.3079113. 
# Let's use our Pitch by Pitch data to get wOBA weights according to our two 
# methods. We will need to get the total number of Walks, Singles, Doubles, 
# Triples, and Home Runs, and divide by the total number of plate appearances 
# to get the linear weight for each play type.

# Calculate wOBA weights for each play type
avg_wOBA_before_adjustment = pitches_data %>%
  filter(!is.na(PlayResult)) %>%
  summarize(
    Total_PA = n(),
    Walks = sum(PlayResult == "Walk", na.rm = TRUE),
    Singles = sum(PlayResult == "Single", na.rm = TRUE),
    Doubles = sum(PlayResult == "Double", na.rm = TRUE),
    Triples = sum(PlayResult == "Triple", na.rm = TRUE),
    HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
  )

wOBA_method_2 = (df2_run_values$AvgRunValue[df2_run_values$PlayResult == "Walk"] * avg_wOBA_before_adjustment$Walks + 
                   df2_run_values$AvgRunValue[df2_run_values$PlayResult == "Single"] * avg_wOBA_before_adjustment$Singles + 
                   df2_run_values$AvgRunValue[df2_run_values$PlayResult == "Double"] * avg_wOBA_before_adjustment$Doubles + 
                   df2_run_values$AvgRunValue[df2_run_values$PlayResult == "Triple"] * avg_wOBA_before_adjustment$Triples + 
                   df2_run_values$AvgRunValue[df2_run_values$PlayResult == "HomeRun"] * avg_wOBA_before_adjustment$HomeRuns) / avg_wOBA_before_adjustment$Total_PA

wOBA_method_3 = (df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Walk"] * avg_wOBA_before_adjustment$Walks +
                   df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Single"] * avg_wOBA_before_adjustment$Singles + 
                   df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Double"] * avg_wOBA_before_adjustment$Doubles + 
                   df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Triple"] * avg_wOBA_before_adjustment$Triples + 
                   df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "HomeRun"] * avg_wOBA_before_adjustment$HomeRuns) / avg_wOBA_before_adjustment$Total_PA
print(wOBA_method_2)
print(wOBA_method_3)

# Using Method #2, we get an average wOBA using unadjusted linear weights of 
# 0.2814665 and using Method #3, we get an average wOBA of 0.3501425.

# We will now take our run value dataframes and scale so that wOBA is equal to 
# OBP. We will then use these weights to calculate wOBA for each qualified 
# hitter in 2023.

# Scale wOBA weights to match average OBP
wOBA_method_2_weights = df2_run_values %>%
  mutate(AvgRunValue = AvgRunValue * (avg_OBP$OBP / wOBA_method_2))
print(wOBA_method_2_weights)

wOBA_method_3_weights = df3_run_values %>%
  mutate(AvgRunsValueByPlay = AvgRunsValueByPlay * (avg_OBP$OBP / wOBA_method_3))
print(wOBA_method_3_weights)

# Sanity Check: Our wOBA weights are about the same for the two methods. 
# Because method #3 incorporates more information, we will use those weights to 
# calculate wOBA and xWOBA for each qualified hitter in 2023.


# Calculate wOBA for each qualified hitter
qualified_stats_2023 <- qualified_stats %>%
  group_by(Batter) %>%
  mutate(
    wOBA = (Singles * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Single"] +
              Doubles * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Double"] +
              Triples * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Triple"] +
              HomeRuns * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "HomeRun"] + 
              Walks * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Walk"]) / Total_PA,
    xWOBA = (x1B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Single"] +
               x2B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Double"] +
               x3B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Triple"] +
               xHomeRuns * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "HomeRun"] + 
               Walks * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Walk"]) / Total_PA
  ) %>%
  arrange(desc(xWOBA))

# Yay! We now have xwOBA and wOBA for every qualified hitter in our Frontier 
# League 2023 dataset. We can now use this data to analyze the best hitters 
# in the league.

## Plus Statistics
# In order for our statistics to be more comparable across different leagues and 
# years of the Frontier League, we will want to calculate plus statistics for 
# each qualified hitter. We will calculate xwOBA+ and wOBA+ for each hitter, 
# which will be the ratio of their wOBA or xwOBA to the league average wOBA or 
# xwOBA. We will do the same for xBA, xOBP, xSLG, xOPS, K% and BB%. We will make 
# a couple of assumptions here that should not hurt us too badly and employ two 
# methods, which we will use for different models.

# The first method: We will disregard the fact that Plus Statistics are compared 
# against the league-average hitter and just use qualified hitters to get our 
# "league average." We do this because we do not have Pitch by Pitch data for our
# Single A Statcast data, so we cannot calculate league average statistics.

# The second method: We will use the average statistics of all qualified hitters 
# in 2023 to calculate league average statistics. This will be used in our 
# comparison to 2024 Statcast hitters.

# In both methods, we do not take into account Park Factor because it is rather 
# hard to calculate for this particular pitch by pitch data. Most Plus Statistics 
# take Park Factor into account on a game-by-game basis. 

### Method #1: Using Qualified Hitters

# First, let's find the average xBA, xOBP, xOPS, K%, BB%, xWOBA, and xWOBA 
# for all qualified hitters in 2023.
# Calculate average statistics for all qualified hitters in 2023
avg_stats_2023 <- qualified_stats_2023 %>%
  ungroup() %>%
  select(xBA, xOBP, xSLG, xOPS, K_percentage, BB_percentage, wOBA, xWOBA) %>%
  summarize(
    Avg_xBA = mean(xBA, na.rm = TRUE),
    Avg_xOBP = mean(xOBP, na.rm = TRUE),
    Avg_xSLG = mean(xSLG, na.rm = TRUE),
    Avg_xOPS = mean(xOPS, na.rm = TRUE),
    Avg_K_percentage = mean(K_percentage, na.rm = TRUE),
    Avg_BB_percentage = mean(BB_percentage, na.rm = TRUE),
    Avg_wOBA = mean(wOBA, na.rm = TRUE),
    Avg_xWOBA = mean(xWOBA, na.rm = TRUE)
  )

# Now, lets add the ratio of each qualified hitter's statistics to the 
# league average statistics to the qualified hitters dataframe.
# Calculate plus statistics for each qualified hitter
qualified_stats_2023_m1 <- qualified_stats_2023 %>%
  mutate(
    xBA_plus = xBA / avg_stats_2023$Avg_xBA,
    xOBP_plus = xOBP / avg_stats_2023$Avg_xOBP,
    xSLG_plus = xSLG / avg_stats_2023$Avg_xSLG,
    xOPS_plus = xOPS / avg_stats_2023$Avg_xOPS,
    K_percentage_plus = K_percentage / avg_stats_2023$Avg_K_percentage,
    BB_percentage_plus = BB_percentage / avg_stats_2023$Avg_BB_percentage,
    wOBA_plus = wOBA / avg_stats_2023$Avg_wOBA,
    xWOBA_plus = xWOBA / avg_stats_2023$Avg_xWOBA
  ) %>%
  ## Multiply each of these columns by 100
  mutate(across(ends_with("plus"), ~ . * 100)) %>%
  arrange(desc(xWOBA_plus))

### Method #2: Using All Qualified Hitters in 2023
## First, let's find the average xBA, xOBP, xSLG, xOPS, K%, BB%, xWOBA, and 
## xWOBA for all batters in 2023.

# Calculate average statistics for all batters in 2023
avg_stats_all_2023 <- pitches_data %>%
  filter(!is.na(PlayResult)) %>%
  mutate(
    is_batted_ball = !is.na(xBA),  # A batted ball has a valid xBA
    is_at_bat = !PlayResult %in% c("Walk", "Sacrifice")
  ) %>%
  ungroup() %>%
  summarize(
    Total_PA = n(),
    Total_AB = sum(!PlayResult %in% c("Walk", "Sacrifice"), na.rm = TRUE),
    Walks = sum(PlayResult == "Walk", na.rm = TRUE),
    xOBP = (Walks + sum(xBA[is_batted_ball], na.rm = TRUE)) / Total_PA,
    Singles = sum(PlayResult == "Single", na.rm = TRUE),
    Doubles = sum(PlayResult == "Double", na.rm = TRUE),
    Triples = sum(PlayResult == "Triple", na.rm = TRUE),
    HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
    K_percentage = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking"), na.rm = TRUE) / Total_PA,
    BB_percentage = sum(PlayResult == "Walk", na.rm = TRUE) / Total_PA,
    xBA = sum(xBA, na.rm = TRUE) / Total_AB,
    xSLG = sum(
      xSLG,
      na.rm = TRUE
    ) / Total_AB,
    xOPS = xOBP + xSLG,
    x1B = sum(x1B, na.rm = TRUE),
    x2B = sum(x2B, na.rm = TRUE),
    x3B = sum(x3B, na.rm = TRUE),
    xHR = sum(xHR, na.rm = TRUE),
  ) %>%
  mutate(wOBA = ((Singles * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Single"] +
                    Doubles * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Double"] +
                    Triples * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Triple"] +
                    HomeRuns * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "HomeRun"] + 
                    Walks * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Walk"]) / Total_PA),
         xwOBA = ((x1B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Single"] +
                     x2B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Double"] +
                     x3B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Triple"] +
                     xHR * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "HomeRun"] + 
                     Walks * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Walk"]) / Total_PA))

# Now, let's add the ratio of each qualified hitter's statistics to the 
# league average statistics to the qualified hitters dataframe.
# Calculate plus statistics for each qualified hitter
qualified_stats_2023_m2 <- qualified_stats_2023 %>%
  mutate(
    xBA_plus = xBA / avg_stats_all_2023$xBA,
    xOBP_plus = xOBP / avg_stats_all_2023$xOBP,
    xSLG_plus = xSLG / avg_stats_all_2023$xSLG,
    xOPS_plus = xOPS / avg_stats_all_2023$xOPS,
    K_percentage_plus = K_percentage / avg_stats_all_2023$K_percentage,
    BB_percentage_plus = BB_percentage / avg_stats_all_2023$BB_percentage,
    wOBA_plus = wOBA / avg_stats_all_2023$wOBA,
    xWOBA_plus = xWOBA / avg_stats_all_2023$xwOBA
  ) %>%
  ## Multiply each of these columns by 100
  mutate(across(ends_with("plus"), ~ . * 100)) %>%
  arrange(desc(xWOBA_plus))

# Methods 1 and 2 here get the same order of hitters, but the magnitude to 
# which they are above average is different. This is because the league average 
# statistics are different in each method. We will use Method 2 for our comparison 
# to 2024 Statcast data, as it is more accurate.
# 
# Now that we have all of our statistics, we can move on to our comparison to 
# 2024 Statcast data. First, we will save all relevant dataframes as CSVs to 
# use in our projects. The rest of this will not have comments, as it mirrors what
# I did in the first part.

pitches_data_2024_1 = pitches_data_2024_1 %>%
  distinct() %>%
  mutate(pitch_id = row_number())

pithces_data_2024_2 = pitches_data_2024_2 %>%
  distinct() %>%
  mutate(pitch_id = row_number())

# Filter for batted balls
batted_balls <- pitches_data_2024_1 %>%
  filter(
    !is.na(PlayResult),
    !PlayResult %in% c("Walk", "StrikeoutSwinging", "StrikeoutLooking")
  ) %>%
  filter(!is.na(ExitSpeed), !is.na(Angle)) %>%
  mutate(
    is_single = ifelse(PlayResult == "Single", 1, 0),
    is_double = ifelse(PlayResult == "Double", 1, 0),
    is_triple = ifelse(PlayResult == "Triple", 1, 0),
    is_home_run = ifelse(PlayResult == "HomeRun", 1, 0)
  )

# Combine outcomes into a single categorical variable
batted_balls <- batted_balls %>%
  mutate(outcome = case_when(
    is_single == 1 ~ "Single",
    is_double == 1 ~ "Double",
    is_triple == 1 ~ "Triple",
    is_home_run == 1 ~ "HomeRun",
    TRUE ~ "Not a Hit"
  ))

# Fit a multinomial logistic regression model
model_multinom <- multinom(outcome ~ ExitSpeed + Angle, data = batted_balls)

# Predict probabilities for each outcome
batted_balls <- batted_balls %>%
  bind_cols(predict(model_multinom, newdata = batted_balls, type = "probs"))

## Change column names
colnames(batted_balls)[colnames(batted_balls) == "Single"] <- "x1B"
colnames(batted_balls)[colnames(batted_balls) == "Double"] <- "x2B"
colnames(batted_balls)[colnames(batted_balls) == "Triple"] <- "x3B"
colnames(batted_balls)[colnames(batted_balls) == "HomeRun"] <- "xHR"

## Make xBA and xSLG columns
batted_balls <- batted_balls %>%
  mutate(
    xBA = x1B + x2B + x3B + xHR,
    xSLG = x1B + 2 * x2B + 3 * x3B + 4 * xHR
  )


# Merge results back into the original pitches dataset
pitches_data_2024_1 <- pitches_data_2024_1 %>%
  left_join(
    batted_balls %>%
      select(pitch_id, x1B, x2B, x3B, xHR, xBA, xSLG),
    by = "pitch_id"
  )

# Graph ExitSpeed vs. xBA
ggplot(batted_balls, aes(x = ExitSpeed, y = xBA)) +
  geom_point(alpha = 0.5) +
  ## Add smooth non-linear trend line
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Exit Speed vs. Expected Batting Average",
       x = "Exit Speed (mph)",
       y = "Expected Batting Average") +
  theme_minimal()

# Graph Angle vs. xBA
ggplot(batted_balls, aes(x = Angle, y = xBA)) +
  geom_point(alpha = 0.5) +
  ## Add smooth non-linear trend line
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Angle vs. Expected Batting Average",
       x = "Angle (degrees)",
       y = "Expected Batting Average") +
  theme_minimal()

## Graph ExitSpeed vs. xSLG
ggplot(batted_balls, aes(x = ExitSpeed, y = xSLG)) +
  geom_point(alpha = 0.5) +
  ## Add smooth non-linear trend line
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Exit Speed vs. Expected Slugging Percentage",
       x = "Exit Speed (mph)",
       y = "Expected Slugging Percentage") +
  theme_minimal()

# Graph Angle vs. xSLG
ggplot(batted_balls, aes(x = Angle, y = xSLG)) +
  geom_point(alpha = 0.5) +
  ## Add smooth non-linear trend line
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Angle vs. Expected Slugging Percentage",
       x = "Angle (degrees)",
       y = "Expected Slugging Percentage") +
  theme_minimal()


# Calculate the number of plate appearances for each batter
library(dplyr)

pitches_2024 <- pitches_data_2024_1

# Using PlayResult to detect end of a plate appearance
pitches_2024 <- pitches_2024 %>%
  mutate(is_PA_end = !is.na(PlayResult))  

# Count PAs per batter
plate_appearances <- pitches_2024 %>%
  filter(is_PA_end) %>% 
  group_by(Batter) %>%
  summarize(
    total_PA = n()
  ) %>%
  arrange(desc(total_PA))

# Filter for qualified hitters (at least 130 PAs)
qualified_hitters_2024 <- plate_appearances %>%
  filter(total_PA >= 130)

# Display leaderboard
print(qualified_hitters_2024)

# Filter for qualified hitters
qualified_hitters_2024 <- qualified_hitters_2024$Batter

qualified_hitters_2024 <- pitches_2024 %>%
  filter(Batter %in% qualified_hitters_2024)

# Filter the dataframe so it doesn't include non-PlayResult rows
qualified_pitches <- qualified_hitters_2024 %>%
  filter(!is.na(PlayResult))

## Check PlayResult column for unique values
unique(qualified_pitches$PlayResult)

# Calculate xBA and xSLG for each qualified hitter
qualified_stats <- qualified_pitches %>%
  mutate(
    is_batted_ball = !is.na(xBA),  # A batted ball has a valid xBA
    is_at_bat = !PlayResult %in% c("Walk", "Sacrifice")
  ) %>%
  ## Filter out rows where play result is a Single, Double, Triple, HomeRun, Error, Sacrifice, or FieldersChoice and doesn't have a batted ball
  filter(
    !(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Error", "Sacrifice", "FieldersChoice") & 
        (is.na(ExitSpeed) | is.na(Angle)))
  ) %>%
  group_by(Batter) %>%
  summarize(
    Total_PA = n(),  # Total plate appearances
    Total_AB = sum(is_at_bat, na.rm = TRUE),  # At-bats exclude walks and sacrifices
    Walks = sum(PlayResult == "Walk", na.rm = TRUE),
    xOBP = (Walks + sum(xBA[is_batted_ball], na.rm = TRUE)) / Total_PA,
    xBA = sum(xBA[is_batted_ball], na.rm = TRUE) / Total_AB,  # Combine batted balls and strikeouts
    xSLG = sum(
      x1B[is_batted_ball] + 2 * x2B[is_batted_ball] + 3 * x3B[is_batted_ball] + 4 * xHR[is_batted_ball],
      na.rm = TRUE
    ) / Total_AB,  # Weighted slugging contributions divided by at-bats
    K_percentage = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking"), na.rm = TRUE) / Total_PA,
    BB_percentage = sum(PlayResult == "Walk", na.rm = TRUE) / Total_PA,
    Strikeouts = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking"), na.rm = TRUE),
    Hits = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE),
    Singles = sum(PlayResult == "Single", na.rm = TRUE),
    Doubles = sum(PlayResult == "Double", na.rm = TRUE),
    Triples = sum(PlayResult == "Triple", na.rm = TRUE),
    HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
    xHomeRuns = sum(xHR[is_batted_ball], na.rm = TRUE),
    x1B = sum(x1B[is_batted_ball], na.rm = TRUE),
    x2B = sum(x2B[is_batted_ball], na.rm = TRUE),
    x3B = sum(x3B[is_batted_ball], na.rm = TRUE),
    Errors = sum(PlayResult == "Error", na.rm = TRUE),
    OBP = (Hits + Walks) / Total_PA,
    BA = Hits / Total_AB,
    SLG = (Singles + 2 * Doubles + 3 * Triples + 4 * HomeRuns) / Total_AB,
    OPS = OBP + SLG,
    xOPS = xOBP + xSLG,
  ) %>%
  arrange(desc(xOPS))  # Sort by xOPS

# Display leaderboard
print(qualified_stats)

df <- pitches_data_2024_2 %>%
  ## Only keep rows with a PlayResult
  filter(!is.na(PlayResult)) %>%
  group_by(GameID, Inning, Top.Bottom) %>%
  mutate(
    RunsScored = ifelse(is.na(RunsScored), 0, RunsScored),  # Replace NA with 0
    CumulativeRuns = cumsum(RunsScored),  # Cumulative sum of runs
    RunsToEndOfInning = max(CumulativeRuns) - CumulativeRuns  # Runs remaining to end of inning
  )

# Calculate run expectancy for each number of outs
run_expectancy <- df %>%
  group_by(Outs) %>%
  summarize(
    AvgRunsToEnd = mean(RunsToEndOfInning, na.rm = TRUE)
  )

# Print run expectancy for validation
print(run_expectancy)

# Get outs on play for each row
df = df %>%
  mutate(postOuts = Outs + OutsOnPlay)

# Merge run expectancy into the dataframe
df <- df %>%
  # Add pre-event run expectancy (based on `Outs`)
  left_join(run_expectancy, by = c("Outs" = "Outs")) %>%
  rename(PreRunExpectancy = AvgRunsToEnd) %>%
  
  # Add post-event run expectancy (based on `postOuts`)
  left_join(run_expectancy, by = c("postOuts" = "Outs")) %>%
  rename(PostRunExpectancy = AvgRunsToEnd) %>%
  
  # Set post run expectancy to 0 if postOuts is 3
  mutate(
    PostRunExpectancy = ifelse(postOuts == 3, 0, PostRunExpectancy)
  )

df3_run_values  = df3 %>%
  mutate(RunsValueByPlay = RunsToEndOfInning+RunsScored + (PostRunExpectancy - PreRunExpectancy)) %>%
  group_by(PlayResult) %>%
  summarize(AvgRunsValueByPlay = mean(RunsValueByPlay, na.rm = TRUE))

df3_run_values =
  df3_run_values %>%
  mutate(AvgRunsValueByPlay = AvgRunsValueByPlay - df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Out"])

# Calculate average OBP for all batters in 2023
avg_OBP_2024 <- pitches_data_2024_1 %>%
  filter(!is.na(PlayResult)) %>%
  summarize(
    Total_PA = n(),
    Walks = sum(PlayResult == "Walk", na.rm = TRUE),
    OBP = (sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE) + Walks) / Total_PA,
    .groups = "drop"
  )

# Calculate wOBA weights for each play type
avg_wOBA_before_adjustment = pitches_data_2024_1 %>%
  filter(!is.na(PlayResult)) %>%
  summarize(
    Total_PA = n(),
    Walks = sum(PlayResult == "Walk", na.rm = TRUE),
    Singles = sum(PlayResult == "Single", na.rm = TRUE),
    Doubles = sum(PlayResult == "Double", na.rm = TRUE),
    Triples = sum(PlayResult == "Triple", na.rm = TRUE),
    HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
  )

wOBA_method_3 = (df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Walk"] * avg_wOBA_before_adjustment$Walks +
                   df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Single"] * avg_wOBA_before_adjustment$Singles + 
                   df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Double"] * avg_wOBA_before_adjustment$Doubles + 
                   df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "Triple"] * avg_wOBA_before_adjustment$Triples + 
                   df3_run_values$AvgRunsValueByPlay[df3_run_values$PlayResult == "HomeRun"] * avg_wOBA_before_adjustment$HomeRuns) / avg_wOBA_before_adjustment$Total_PA
print(wOBA_method_3)


# Scale wOBA weights
wOBA_method_3_weights = df3_run_values %>%
  mutate(AvgRunsValueByPlay = AvgRunsValueByPlay * (avg_OBP_2024$OBP / wOBA_method_3))
print(wOBA_method_3_weights)

# Calculate wOBA and xWOBA for each qualified hitter
qualified_stats_2024 <- qualified_stats %>%
  group_by(Batter) %>%
  mutate(
    wOBA = (Singles * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Single"] +
              Doubles * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Double"] +
              Triples * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Triple"] +
              HomeRuns * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "HomeRun"] + 
              Walks * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Walk"]) / Total_PA,
    xWOBA = (x1B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Single"] +
               x2B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Double"] +
               x3B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Triple"] +
               xHomeRuns * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "HomeRun"] + 
               Walks * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Walk"]) / Total_PA
  ) %>%
  arrange(desc(xWOBA))

## Plus Statistics

# Calculate average statistics for all qualified hitters in 2023
avg_stats_2024 <- qualified_stats_2024 %>%
  ungroup() %>%
  select(xBA, xOBP, xSLG, xOPS, K_percentage, BB_percentage, wOBA, xWOBA) %>%
  summarize(
    Avg_xBA = mean(xBA, na.rm = TRUE),
    Avg_xOBP = mean(xOBP, na.rm = TRUE),
    Avg_xSLG = mean(xSLG, na.rm = TRUE),
    Avg_xOPS = mean(xOPS, na.rm = TRUE),
    Avg_K_percentage = mean(K_percentage, na.rm = TRUE),
    Avg_BB_percentage = mean(BB_percentage, na.rm = TRUE),
    Avg_wOBA = mean(wOBA, na.rm = TRUE),
    Avg_xWOBA = mean(xWOBA, na.rm = TRUE)
  )

# Calculate plus statistics for each qualified hitter
qualified_stats_2024_m1 <- qualified_stats_2024 %>%
  mutate(
    xBA_plus = xBA / avg_stats_2023$Avg_xBA,
    xOBP_plus = xOBP / avg_stats_2023$Avg_xOBP,
    xSLG_plus = xSLG / avg_stats_2023$Avg_xSLG,
    xOPS_plus = xOPS / avg_stats_2023$Avg_xOPS,
    K_percentage_plus = K_percentage / avg_stats_2023$Avg_K_percentage,
    BB_percentage_plus = BB_percentage / avg_stats_2023$Avg_BB_percentage,
    wOBA_plus = wOBA / avg_stats_2023$Avg_wOBA,
    xWOBA_plus = xWOBA / avg_stats_2023$Avg_xWOBA
  ) %>%
  ## Multiply each of these columns by 100
  mutate(across(ends_with("plus"), ~ . * 100)) %>%
  arrange(desc(xWOBA_plus))

# Calculate average statistics for all batters in 2024
avg_stats_all_2024 <- pitches_data_2024_1 %>%
  filter(!is.na(PlayResult)) %>%
  mutate(
    is_batted_ball = !is.na(xBA),  # A batted ball has a valid xBA
    is_at_bat = !PlayResult %in% c("Walk", "Sacrifice")
  ) %>%
  ungroup() %>%
  summarize(
    Total_PA = n(),
    Total_AB = sum(!PlayResult %in% c("Walk", "Sacrifice"), na.rm = TRUE),
    Walks = sum(PlayResult == "Walk", na.rm = TRUE),
    xOBP = (Walks + sum(xBA[is_batted_ball], na.rm = TRUE)) / Total_PA,
    Singles = sum(PlayResult == "Single", na.rm = TRUE),
    Doubles = sum(PlayResult == "Double", na.rm = TRUE),
    Triples = sum(PlayResult == "Triple", na.rm = TRUE),
    HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
    K_percentage = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking"), na.rm = TRUE) / Total_PA,
    BB_percentage = sum(PlayResult == "Walk", na.rm = TRUE) / Total_PA,
    xBA = sum(xBA, na.rm = TRUE) / Total_AB,
    xSLG = sum(
      xSLG,
      na.rm = TRUE
    ) / Total_AB,
    xOPS = xOBP + xSLG,
    x1B = sum(x1B, na.rm = TRUE),
    x2B = sum(x2B, na.rm = TRUE),
    x3B = sum(x3B, na.rm = TRUE),
    xHR = sum(xHR, na.rm = TRUE),
  ) %>%
  mutate(wOBA = ((Singles * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Single"] +
                    Doubles * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Double"] +
                    Triples * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Triple"] +
                    HomeRuns * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "HomeRun"] + 
                    Walks * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Walk"]) / Total_PA),
         xwOBA = ((x1B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Single"] +
                     x2B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Double"] +
                     x3B * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Triple"] +
                     xHR * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "HomeRun"] + 
                     Walks * wOBA_method_3_weights$AvgRunsValueByPlay[wOBA_method_3_weights$PlayResult == "Walk"]) / Total_PA))

# Calculate plus statistics for each qualified hitter
qualified_stats_2024_m2 <- qualified_stats_2024 %>%
  mutate(
    xBA_plus = xBA / avg_stats_all_2024$xBA,
    xOBP_plus = xOBP / avg_stats_all_2024$xOBP,
    xSLG_plus = xSLG / avg_stats_all_2024$xSLG,
    xOPS_plus = xOPS / avg_stats_all_2024$xOPS,
    K_percentage_plus = K_percentage / avg_stats_all_2024$K_percentage,
    BB_percentage_plus = BB_percentage / avg_stats_all_2024$BB_percentage,
    wOBA_plus = wOBA / avg_stats_all_2024$wOBA,
    xWOBA_plus = xWOBA / avg_stats_all_2024$xwOBA
  ) %>%
  ## Multiply each of these columns by 100
  mutate(across(ends_with("plus"), ~ . * 100)) %>%
  arrange(desc(xWOBA_plus))

## Include some other statistics
# Calculate hard hit percentage, launch speed, and launch angle for each player
hard_hit <- pitches_data_2024_1 %>%
  filter(Batter %in% qualified_hitters_2024$Batter) %>%
  filter(!is.na(ExitSpeed)) %>%
  group_by(Batter) %>%
  summarize(
    Total_Hard_Hits = sum(ExitSpeed >= 95, na.rm = TRUE),
    Total_Batted_Balls = n(),
    Hard_Hit_Percentage = Total_Hard_Hits / Total_Batted_Balls,
    Avg_ExitSpeed = mean(ExitSpeed, na.rm = TRUE),
    Avg_Angle = mean(Angle, na.rm = TRUE)
  ) %>%
  arrange(desc(Hard_Hit_Percentage))

# Add hard hit percentage, launch speed, and launch angle to qualified hitters dataframe
qualified_stats_2024_m1 <- qualified_stats_2024_m1 %>%
  left_join(
    hard_hit %>%
      select(Batter, Hard_Hit_Percentage, Avg_ExitSpeed, Avg_Angle),
    by = "Batter"
  )

# Save dataframes as CSV files
write.csv(qualified_stats_2023_m1, "qualified_stats_2023_m1.csv", row.names = FALSE)
write.csv(qualified_stats_2023_m2, "qualified_stats_2023_m2.csv", row.names = FALSE)
write.csv(qualified_stats_2024_m1, "qualified_stats_2024_m1.csv", row.names = FALSE)
write.csv(qualified_stats_2024_m2, "qualified_stats_2024_m2.csv", row.names = FALSE)

































