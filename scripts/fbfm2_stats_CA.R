# Get summary stats on baseline-to-DIST fuel model rasters

# Second in a series. 
# First script created a combined baseline-DIST{} raster so 
#  we can get PER PIXEL fuel changes. 
# This script extract summaries - pixel counts of each combination of change
#  within California state boundaries. 
# Third script will graph. 


### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra,
  sf,
  exactextractr) 
#future.apply)

options(future.globals.onReference = "warning")


### User settings ----------------------------------

#adj folder
folder_in <- file.path("nonforest_prebakes",
                       "fbfm_change")

### Data ---------------------------------------------

raster_files <- list.files(folder_in,
                           full.names = TRUE,
                           pattern = "tif$")


# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data_qa/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA") %>% st_transform(crs="epsg:5070")

### Calc for dist rasters ----------------------------

get_zonal <- function(x){
  
  #this adjective fuel raster 
  this_raster <- terra::rast(x) 
  
  #exact extract for california boundary
  this_freq <- exact_extract(this_raster, 
                             ca, 
                             function(value, coverage_fraction ) {table(value)})
  #add column name
  colnames(this_freq) <- c("pixel_count")
  
  
  #package up results
  this_freq_sum <- this_freq %>% 
    #adj counts
    as_tibble() %>% 
    mutate(fmchanges = dimnames(this_freq)[[1]]) %>% 
    mutate(total_pixels = sum(this_freq)) %>% 
    #identifiers
    mutate(fml_layer = tools::file_path_sans_ext(basename(x))) %>% 
    separate(fml_layer, into=c(NA, NA, "DIST", NA), sep="_") %>% 
    dplyr::select(DIST, fmchanges, everything())
  
  #list of data table
  this_results <- list(this_freq_sum)
}

#gather all 
(start_time <- Sys.time())
all_stats <- sapply(raster_files, get_zonal)
(end_time <- Sys.time())
(elapsed_time <- end_time - start_time)

# plan(multisession, workers = 2) #availableCores(omit=1)
# all_stats <- future.apply::future_sapply(raster_files, get_zonal)

#combine
all_tbl <- do.call(rbind, all_stats) %>% 
  as_tibble()

#save
saveRDS(all_tbl, 
        file=file.path("data_qa", 
                       "fbfm_pixelchanges_CA.RDS"))
# write_csv(all_tbl, 
#         file=file.path("data_qa", 
#                        "fbfm_pixelchanges_CA.csv"))

