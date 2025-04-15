library(spatstat)
library(terra)
library(tidyverse)
library(tmap)
library(sf)
sf_use_s2(FALSE)

# Create raster template
study_area <- st_read(here::here("data", "studyarea")) %>% 
  st_crop(xmin = -122.6501, ymin = 37.5, xmax = -121, ymax = 38) %>% 
  st_transform("EPSG:3488")
study_area_template <- rast(ext(study_area), 
                            resolution = 100,
                            crs = "EPSG:3488")

# Load whale data
read_whale <- function(path) {
  read_csv(path) %>%
    select(Year, Mo, Day, Lat_DD, Long_DD) %>% 
    mutate(Long_DD = ifelse(Long_DD > 0, -Long_DD, Long_DD))
}
whales_all <- dir("data/whales", pattern = "csv$", full.names=TRUE) %>% 
  map(read_whale) %>% 
  list_rbind()

# Assign season based on month 
whales_season <- whales_all %>% 
  mutate(season = case_when(
    Mo <= 3 ~ "Winter",
    Mo <= 6 ~ "Spring",
    Mo <= 9 ~ "Summer",
    Mo <= 12 ~ "Fall"
  ))

## Create whale raster function 
create_whale_raster <- function(whale_df, season, template, area) {
  # Estimate kernel density
  study_area_owin <- as.owin(area)
  pts <- st_coordinates(whale_df)
  whale_ppp <- ppp(pts[, 1], pts[, 2], window = study_area_owin)
  whale_kde <- resample(rast(density(whale_ppp, 5e3)),
                        template,
                        method = "sum")
  names(whale_kde) <- "density"
  # Save rasters of "abundance" (density * total observed)
  whale_abu <- whale_kde * nrow(pts)
  writeRaster(whale_abu, 
              str_glue("outputs/whales/whales_{season}_abu.tif"),
              overwrite = TRUE)
  names(whale_abu) <- season$season
  whale_abu
}

#create function to convert raster to polygon
raster_to_polygon <- function(season_abu, all_abus) {
  # Classify density by quantiles across all seasons
  whale_quantiles <- quantile(values(all_abus), 
                              c(0.33, 0.67, 1), 
                              na.rm = TRUE)
  whale_ratings <- classify(season_abu, 
                            c(0, whale_quantiles),
                            include.lowest = TRUE)

    # Rename levels to avoid brackets
  whale_levels <- levels(whale_ratings)[[1]] %>% 
    transmute(ID,
              Rating = factor(1:3))
  levels(whale_ratings) <- whale_levels
  
  # Return a vector
  result <- as.polygons(whale_ratings)
  names(result) <- names(season_abu)
  result
}

# map function onto group 
whale_density  <- whales_season %>%
  filter(season != "Winter") %>% 
  drop_na() %>%
  st_as_sf(coords = c("Long_DD", "Lat_DD"),
           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  st_transform("EPSG:3488")  %>% 
  st_intersection(study_area) %>% 
  group_by(season) %>% 
  group_map(create_whale_raster, 
            template = study_area_template,
            area = study_area) %>% 
  map(\(x) raster_to_polygon(x, rast(.))) %>% 
  set_names(map_chr(., names))
  
for (i in seq(length(whale_density))) {
  names(whale_density[[i]]) <- "Rating"
}

#save whale_density RDS file
saveRDS(whale_density, "outputs/whale_density.rds")

#save each whale kde shapefile 
walk2(
  whale_density,
  names(whale_density),
  function(r, n) {
    output_path <- file.path("outputs/whales", str_glue("whales_{n}.shp"))
    writeVector(r, output_path, overwrite = TRUE)
  }
)
