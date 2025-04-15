library(sf)
library(terra)
library(tmap)
library(tidyverse)
library(raster)
sf_use_s2(FALSE)

# Create raster template
study_area <- st_read(here::here("data", "studyarea")) %>% 
  st_crop(xmin = -122.6501, ymin = 37.5, xmax = -121, ymax = 38) %>% 
  st_transform("EPSG:3488")
study_area_template <- rast(ext(study_area), 
                            resolution = 100,
                            crs = "EPSG:3488")

# function that creates speed raster
create_speed_raster <- function(vessel_df, keys, template) {
  # Project lat lon to albers
  vessel_albers <- vessel_df %>% 
    st_as_sf(coords = c("LON", "LAT"),
             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
    st_transform("EPSG:3488") 
  # Rasterize points
  speed_raster <- vessel_albers %>% 
    rasterize(template, 
              field = "SOG", 
              fun = \(spd, ...) weighted.mean(spd, spd/60)) %>% 
    crop(study_area, mask = TRUE) 
  keys %>% 
    mutate(ais_raster = list(speed_raster))
}
  
#read in each RDS file, mutate, and group by season/vessel type/year
create_ais_seasons <- function(year, template){
  ais <- dir("outputs/ais", 
             pattern = str_glue("filtered_ais_reclass_{year}.rds"), 
             full.names = TRUE) %>% 
    readRDS() %>% 
    mutate(season = case_when(
      month(BaseDateTime) <= 3 ~ "Winter",
      month(BaseDateTime) <= 6 ~ "Spring",
      month(BaseDateTime) <= 9 ~ "Summer",
      month(BaseDateTime) <= 12 ~ "Fall"
    )) %>% 
    mutate(year = year)
  ais %>%
    group_by(year, season, ReclassifiedVesselType) %>% 
    group_map(create_speed_raster, template = template) %>% 
    list_rbind()
}

# map create_ais_seasons function across years
ais_seasons <- map_df(2016:2019, create_ais_seasons, template = study_area_template)

# combine years into season files
combine_years <- function(data_list){
  nr_elements <- length(data_list)
  comb <- data_list[[1]]
  if(nr_elements>1){
    for(i in 2:(nr_elements)){
      comb <- merge(comb, data_list[[i]])
    }
  }
  return(comb)
}

# group speed rasters by season and vessel type
speed_rasters <- ais_seasons %>%
  group_by(season, ReclassifiedVesselType) %>%
  summarize(ais_raster = list(combine_years(ais_raster)))

#create function to save season/vessel type rasters
save_rasters <- function(raster_list) {
  for (i in 1:nrow(raster_list)) {
    season <- raster_list$season[i]
    vessel_type <- raster_list$ReclassifiedVesselType[i]
    current_raster <- raster_list$ais_raster[[i]]
    writeRaster(current_raster, filename = file.path("outputs/speed/", str_glue("speed_{season}_{vessel_type}.tif")), 
                overwrite = TRUE)
  }
}

# Call the function to save the rasters using the existing vessel_density list
save_rasters(speed_rasters)


# Write a function that mutates data into quantiles, adds Rating col, and saves polygons
create_classified_polys <- function(season_types, vessel_types) {
  for (season_type in season_types) {
    for (vessel_type in vessel_types) {
      # Construct the file path using str_glue
      raster_file <- str_glue("outputs/speed/speed_{season_type}_{vessel_type}.tif")
      
      # Read the saved raster using terra's rast function
      speed_raster <- rast(raster_file)
      
      speed_ratings <- classify(speed_raster, c(-Inf, 10, 14, Inf))
      speed_levels <- levels(speed_ratings)[[1]] %>%
        transmute(ID, Rating = factor(1:3))
      levels(speed_ratings) <- speed_levels
      names(speed_ratings) <- "Rating"  # This renames the column to "Rating"
      
      # Convert to polygons
      polygons <- as.polygons(speed_ratings)
      
      # Convert to an sf object
      sf_polygons <- st_as_sf(polygons)
      
      # Save each polygon separately
      output_filename <- str_glue("outputs/speed/speed_{season_type}_{vessel_type}.shp")
      st_write(sf_polygons, output_filename, delete_dsn = TRUE)
    }
  }
}

# List season and vessel types
season_types <- c("Spring", "Summer", "Fall")
vessel_types <- c("Cargo", "Tanker", "TugTow", "HSF", "Cruise", "OtherPassenger", "Pleasure", "Other")

# Call the function to process and save the quantile polygons individually
create_classified_polys(season_types, vessel_types)


