## In this script, we will obtain the data from a MySQL database. 
# We will use the RMySQL package to connect to the database and extract the data 
# we need for our analysis. We will then save the data as a CSV file for further 
# analysis.

# NOTE: This document cannot be run by anyone else because I needed it for 
# processing the raw data from the OnePoint Shared Folder I had access to 
# as an intern. I am unable to share that link, so this is just showing the code
# I used to get every pitch from both the 2023 and 2024 seasons.

library(RMySQL)
library(DBI)
library(tidyverse)
conn <- dbConnect(MySQL(), dbname = "425-capstone-database", user = "root", password = "lance123a")

## Define the seasons and loop through them
seasons <- c("2023", "2024")

## The following is a function to process the data from my local computer
# Function to process a single season
process_season <- function(season) {
  # Define folder path for the season
  folder_path <- paste0("yakkertech_", season)
  
  # Get all CSV files in the folder (recursive)
  csv_files <- list.files(
    path = folder_path,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Ensure there are files to process
  if (length(csv_files) == 0) {
    message("No CSV files found for season: ", season)
    return(NULL)
  }
  
  # Define reference schema using the first file
  reference_file <- csv_files[1]
  reference_data <- read_csv(reference_file, show_col_types = TRUE)
  col_classes <- sapply(reference_data, function(x) class(x)[1])
  
  # Function to coerce columns to match reference schema
  coerce_to_reference <- function(data, ref_classes) {
    for (col in names(ref_classes)) {
      if (col %in% names(data)) {
        target_class <- ref_classes[[col]]
        if (target_class == "character") {
          data[[col]] <- as.character(data[[col]])
        } else if (target_class == "numeric") {
          data[[col]] <- as.numeric(data[[col]])
        } else if (target_class == "integer") {
          data[[col]] <- as.integer(data[[col]])
        } else if (target_class == "logical") {
          data[[col]] <- as.logical(data[[col]])
        } else if (target_class == "Date") {
          data[[col]] <- as.Date(data[[col]], format = "%Y-%m-%d")
        }
      } else {
        # Add missing columns with NA
        data[[col]] <- NA
      }
    }
    return(data)
  }
  
  # Process all files for the season
  data_list <- lapply(csv_files, function(file) {
    tryCatch({
      temp_data <- read_csv(file, show_col_types = FALSE)
      temp_data <- coerce_to_reference(temp_data, col_classes)
      
      # Ensure columns match reference schema
      missing_cols <- setdiff(names(reference_data), names(temp_data))
      for (col in missing_cols) {
        temp_data[[col]] <- NA
      }
      temp_data <- temp_data[names(reference_data)]
      temp_data
    }, error = function(e) {
      message("Error processing file: ", file, " - ", e$message)
      NULL
    })
  })
  
  # Filter out NULL entries (failed files)
  data_list <- Filter(Negate(is.null), data_list)
  
  # Combine all data frames for the season
  combined_data <- bind_rows(data_list)
  
  # Convert Date column if present
  if ("Date" %in% names(combined_data)) {
    combined_data$Date <- as.Date(combined_data$Date, format = "%Y-%m-%d")
  }
  
  # Connect to the database
  con <- dbConnect(
    MySQL(),
    user = 'root',
    password = 'lance123a',
    dbname = '425-capstone-database',
    host = 'localhost',
    port = 3306,
    client.flag = CLIENT_LOCAL_FILES  # Enable local file loading
  )
  
  # Write the combined data to the database
  dbWriteTable(
    conn = con,
    name = paste0("pitches_", season),  # Table name includes the season
    value = combined_data,
    overwrite = TRUE,  # Overwrites the table if it already exists
    row.names = FALSE
  )
  
  # Disconnect from the database
  dbDisconnect(con)
  
  cat("Successfully processed season:", season, "\n")
}

# Process the seasons
# process_season("2023")
# process_season("2024")

# Connect to our SQL database:
con <- dbConnect(
  MySQL(),
  user = 'root',
  password = 'lance123a',
  dbname = '425-capstone-database',
  host = 'localhost',
  port = 3306
)

## We will obtain the 2023 data and write it to csv's:
# Load the first set of pitch-by-pitch data for 2023
query_2023_1 <- "
    SELECT 
        Date,
        Time,
        PAofInning,
        Pitcher,
        PitcherTeam,
        PitcherThrows,
        Batter,
        BatterSide,
        BatterTeam,
        PlayResult, 
        OutsOnPlay,
        ExitSpeed, 
        Angle, 
        Direction
    FROM pitches_2023;
"
pitches_data <- dbGetQuery(con, query_2023_1)
write.csv(pitches_data, "pitches_data.csv", row.names = FALSE)

## And do pitches_data_2 that gets everything from pitches_2023
query_2023_2 <- "
    SELECT 
      Date,
      Time,
      PlayResult,       
      Outs,           
      OutsOnPlay,     
      RunsScored,        
      Inning,            
      `Top/Bottom`, 
      GameID 
    FROM pitches_2023;
    "

pitches_data_2 <- dbGetQuery(con, query_2023_2)
write.csv(pitches_data_2, "pitches_data2.csv", row.names = FALSE)

## Then we can get the 2024 data:
# Load the pitch-by-pitch data for 2024
query_2024_1 <- "
    SELECT 
        Date,
        Time,
        PAofInning,
        Pitcher,
        PitcherTeam,
        PitcherThrows,
        Batter,
        BatterSide,
        BatterTeam,
        PlayResult, 
        OutsOnPlay,
        ExitSpeed, 
        Angle, 
        Direction
    FROM pitches_2024;
"
pitches_data_2024_1 <- dbGetQuery(con, query_2024_1)
write.csv(pitches_data_2024_1, "pitches_data_2024_1.csv", row.names = FALSE)

## And do pitches_data_2 that gets everything from pitches_2023
query2024_2 <- "
    SELECT 
      Date,
      Time,
      PlayResult,       
      Outs,           
      OutsOnPlay,     
      RunsScored,        
      Inning,            
      `Top/Bottom`, 
      GameID 
    FROM pitches_2024;
    "

pitches_data_2024_2 <- dbGetQuery(con, query2024_2)
write.csv(pitches_data_2024_2, "pitches_data_2024_2.csv", row.names = FALSE)








