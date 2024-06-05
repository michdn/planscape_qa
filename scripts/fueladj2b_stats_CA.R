# Get summary stats on adjective results, California state boundary

# Alternative second script in a series. 
# First script turns the original prebake raster into fuel adjective rasters
# This script will extract statistics - pixel counts of EACH category, 
#  mean of adjective value. 
# This script is extracting the state of CA (not geotiff-wide).
# and the third will graph/visualize (based on geotiff-wide). 

# NOTE: This script was not ultimately used. 


### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra,
  sf,
  exactextractr)

### User settings ----------------------------------

#FL or ROS
adj_to_use <- "ROS" 

#adj folder
folder_adj <- file.path("nonforest_prebakes",
                        paste0("adjectivized_", adj_to_use))

### Data ---------------------------------------------

#Note: baseline will be in here as well (13)
adj_files <- list.files(folder_adj,
                        full.names = TRUE,
                        pattern = "tif$")

# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data_qa/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA") %>% st_transform(crs="epsg:5070")

### Calc for dist rasters ----------------------------

get_adj_zonal <- function(x){
  
  #this adjective fuel raster 
  this_raster <- terra::rast(x) 
  
  #frequency table 
  #this_freq <- terra::freq(this_raster)
  
  #exact extract for california boundary
  this_freq <- exact_extract(this_raster, 
                             ca, 
                             function(value, coverage_fraction ) {table(value)})
  #add column name
  colnames(this_freq) <- c("pixel_count")

  
  #average of fuel adj across whole landscape 
  #this_stats <- terra::global(this_raster, fun=c("mean", "sd"), na.rm=TRUE)
  this_stats <- exact_extract(this_raster, 
                              ca,
                              fun = c("mean", "stdev"))
  
  #package up results
  this_freq_sum <- this_freq %>% 
    #adj counts
    as_tibble() %>% 
    mutate(adjective = dimnames(this_freq)[[1]]) %>% 
    mutate(total_pixels = sum(this_freq)) %>% 
    #CA mean and stdev
    bind_cols(this_stats) %>% 
    rename(total_mean = mean,
           total_stdev = stdev) %>% 
    #identifiers
    mutate(fml_layer = tools::file_path_sans_ext(basename(x))) %>% 
    separate(fml_layer, into=c(NA, NA, "DIST", "adj"), sep="_") %>% 
    dplyr::select(DIST, adj, everything())
  
  #list of data table
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
                       paste0("fuel_adj_stats_CA_", adj_to_use, ".RDS")))

