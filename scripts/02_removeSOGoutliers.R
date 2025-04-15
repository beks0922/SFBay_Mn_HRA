library(tidyverse)
library(readr)

# List of your data file names
data_files <- c("outputs/ais/ais_reclass_2016.rds", 
                "outputs/ais/ais_reclass_2017.rds", 
                "outputs/ais/ais_reclass_2018.rds", 
                "outputs/ais/ais_reclass_2019.rds")

# Function to perform outlier detection and filtering
perform_outlier_detection <- function(file_name) {
  # Load the data
  data <- readRDS(file_name)
  
  # Separate data by vessel type and perform outlier detection for each group
  filtered_data <- data %>%
    group_by(ReclassifiedVesselType) %>%
    mutate(
      q1 = quantile(SOG, 0.25, na.rm = TRUE),
      q3 = quantile(SOG, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      lower_bound = q1 - 1.5 * iqr,
      upper_bound = q3 + 1.5 * iqr,
      outlier = SOG < lower_bound | SOG > upper_bound
    ) %>%
    filter(!outlier) %>%
    ungroup()
  
  # Extract file name from file path
  file_name_only <- basename(file_name)
  
  # Construct the output file path
  output_file <- file.path("outputs/ais", paste0("filtered_", file_name_only))
  
  # Save filtered data to a new file
  saveRDS(filtered_data, file = output_file)
}

# Loop over data files and perform outlier detection and filtering
for (file in data_files) {
  perform_outlier_detection(file)
}

# Define function to create AIS seasons
create_ais_seasons <- function(year){
  ais <- dir("outputs/ais", 
             pattern = str_glue("filtered_ais_reclass_{year}.rds"), 
             full.names = TRUE) %>% 
    lapply(readRDS) %>% 
    lapply(function(df) {
      df$Status <- as.character(df$Status)  # Convert Status column to character
      return(df) 
    }) %>% 
    bind_rows() %>% 
    mutate(season = case_when(
      month(BaseDateTime) <= 3 ~ "Winter",
      month(BaseDateTime) <= 6 ~ "Spring",
      month(BaseDateTime) <= 9 ~ "Summer",
      month(BaseDateTime) <= 12 ~ "Fall"
    )) %>% 
    mutate(year = year)
}

#create ais_seasons dataframe and save to RDS file
ais_seasons <- map_df(2016:2019, create_ais_seasons)
saveRDS(ais_seasons, "outputs/ais/ais_seasons.rds")