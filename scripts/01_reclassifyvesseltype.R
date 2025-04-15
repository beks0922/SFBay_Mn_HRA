library(googlesheets4)
library(here)
library(sf)
library(tidyverse)

# Download reclassification table
gs4_deauth() # Just so Google doesn't try to authenticate you
#Reclassification for all vessel types
vessel_type_reclass <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yixJkzDKr65gLkf_78YL0DBsRWEssa9q6SXRaM-0bmQ/edit?usp=sharing",
  sheet = "VesselType"
)

# Reclassification for passenger vessel types 
passenger_reclass <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yixJkzDKr65gLkf_78YL0DBsRWEssa9q6SXRaM-0bmQ/edit?usp=sharing",
  sheet = "PassengerType"
) %>% 
  mutate(VesselName = str_to_lower(VesselName))

#write function to reclassify all vessel types for each ais_*year* file
ais_reclass <- function(year) {
  #read in ais files
  ais_files <- list.files("data/ais", 
                          full.names=TRUE, 
                          pattern = str_glue("{year}\\.csv$"))
  ais_df <- map_df(ais_files, read_csv, show_col_types = FALSE)
  
  #reclassify vessel types  
  ais_reclass <- ais_df %>% 
    left_join(vessel_type_reclass, by = "VesselType") %>% 
    mutate(VesselName = str_to_lower(VesselName)) %>% 
    left_join(passenger_reclass, by = "VesselName") %>% 
    replace_na(list(ReclassifiedVesselType = "Other", 
                    PassengerType = "OtherPassenger")) %>% 
    mutate(ReclassifiedVesselType = ifelse(ReclassifiedVesselType == "Passenger", 
                                           PassengerType,
                                           ReclassifiedVesselType)) 
  #save output
  saveRDS(ais_reclass, str_glue("outputs/ais/ais_reclass_{year}.rds"))
}

#run function for each file by year
walk(2016:2019, ais_reclass)


