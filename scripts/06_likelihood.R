library(sf)
library(terra)
library(tidyverse)
sf_use_s2(FALSE)

#Read vessel density file
vessel_density <- readRDS("outputs/vessel_density.rds")
# Cannot read vessel density file directly
# Recreate it from the tifs
# So replace SpatRaster with stored TIFs (but aren't actually using these raster files, just using this to get our seasons and vessel types)
x <- (rast("outputs/ais/ais_Summer_Pleasure.tif"))
for(i in 1:nrow(vessel_density)){
  vessel_density$ais_raster[[i]] <- rast(paste0("outputs/ais/ais_", 
                                              vessel_density$season[i], "_", 
                                              vessel_density$ReclassifiedVesselType[i], 
                                              ".tif"))
}

# Intersects whale shape and vessel shapes
# Creates new Rating field for sum of ratings
lik_of_inter <- function(season, vessel_type) {
  vessel_path <- file.path("outputs/ais", str_glue("ais_{season}_{vessel_type}.shp"))
  whale_path <- file.path("outputs/whales", str_glue("whales_{season}.shp"))
  st_intersection(
    st_read(vessel_path),
    st_read(whale_path)
  ) %>% 
    st_collection_extract("POLYGON") %>% 
    mutate(Rating = case_match(as.numeric(Rating) + as.numeric(`Rating.1`),
                               c(2, 3) ~ 1,
                               4       ~ 2,
                               c(5, 6) ~ 3)) %>% 
    dplyr::select(-`Rating.1`)
}

# naming and identifying file paths, running lik_of_inter on each combo 
whale_vessel_loi <- map(str_split(paste0(vessel_density$season, "_", vessel_density$ReclassifiedVesselType), "_"),
                        \(s_v) lik_of_inter(s_v[1], s_v[2]))

loi_paths <- map_chr(str_split(paste0(vessel_density$season, "_", vessel_density$ReclassifiedVesselType), "_"),
                     \(s_v) file.path("outputs/likelihood", 
                                      str_glue("loi_{s_v[1]}_{s_v[2]}.shp")))

# walk whale_vessel_loi and loi_paths functions for all files
walk2(whale_vessel_loi, 
      loi_paths, 
      \(v, p) write_sf(v, p))
