# Get summary stats on adjective results

# Second in a series. 
# First script turns the original prebake raster into fuel adjective rasters
# This script will extract statistics - pixel counts of EACH category, 
#  mean of adjective value. 
# This script is geotiff-wide (complete bounding box around extent of CA)
# and the third will graph/visualize. 


### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra)

### User settings ----------------------------------

#FL or ROS
adj_to_use <- "FL" 

#adj folder
folder_adj <- file.path("nonforest_prebakes",
                        paste0("adjectivized_", adj_to_use))

### Data ---------------------------------------------

#Note: baseline will be in here as well (13)
adj_files <- list.files(folder_adj,
                         full.names = TRUE,
                         pattern = "tif$")


### Calc for dist rasters ----------------------------

get_adj_zonal <- function(x){

  #this adjective fuel raster 
  this_raster <- terra::rast(x) 
  
  #frequency table 
  this_freq <- terra::freq(this_raster)
  
  #average of fuel adj across whole landscape 
  this_stats <- terra::global(this_raster, fun=c("mean", "sd"), na.rm=TRUE)

  #package up results
  this_freq_sum <- this_freq %>% 
    dplyr::select(-layer) %>% 
    rename(adjective = value,
           pixel_count = count) %>% 
    mutate(total_pixels = sum(this_freq$count),
           total_mean = this_stats$mean,
           total_sd = this_stats$sd) %>% 
    mutate(fml_layer = tools::file_path_sans_ext(basename(x))) %>% 
    separate(fml_layer, into=c(NA, NA, "DIST", "adj"), sep="_") %>% 
    dplyr::select(DIST, adj, everything())
  
  #lapply returns list, so list of data table
  this_results <- list(this_freq_sum)
}

#gather all 12 DIST file stats + baseline
all_stats <- sapply(adj_files, get_adj_zonal)

#combine
all_tbl <- do.call(rbind, all_stats) %>% 
  as_tibble()

#save
saveRDS(all_tbl, 
        file=file.path("data_qa", 
                       paste0("fuel_adj_stats_", adj_to_use, ".RDS")))

