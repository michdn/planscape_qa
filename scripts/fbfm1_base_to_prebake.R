# overlay each DIST on baseline and get per pixel fbfm40 changes

# First in a series. 
# This script creates a combined baseline-DIST{} raster so 
#  we can get PER PIXEL fuel changes. 
# Second script will extract summaries/stats, and third will graph. 

### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra, 
  future.apply)

options(future.globals.onReference = "warning")


### User settings --------------------------------------

#raster locations
folder_prebake <- file.path("nonforest_prebakes", 
                            "prebakes")

#raster output
folder_out <- file.path("nonforest_prebakes", 
                        "fbfm_change")
dir.create(folder_out, recursive = TRUE)


### Data --------------------------------------------------

raster_files <- list.files(folder_prebake,
                           full.names = TRUE,
                           pattern = "tif$")

# ## create cropped baseline 
# orig <- rast("data_input_fm40/fbfm40_ffv3_2023_11_nbflip_mosiac.tif")
# #use one prebake to match up, as was done in fuel adjs
# orig_crop <- project(orig, rast(raster_files[1]), method="near")
# terra::writeRaster(orig_crop, 
#                    file.path("data_input_fm40", 
#                              "fbfm40_ffv3_2023_11_nbflip_mosiac_clipped.tif"))


baseline_file <- file.path("data_input_fm40",
                           "fbfm40_ffv3_2023_11_nbflip_mosiac_clipped.tif")

### setup -----------------------------------------------

#create tibble
input_tbl <- tibble(raster_file = raster_files) %>% 
  mutate(file_name = basename(raster_file),
         out_file = file.path(folder_out, 
                              paste0(tools::file_path_sans_ext(file_name),
                                     "_transition.tif")))


collapse_rasters <- function(r){
  
  this_row <- input_tbl[r,]
  
  #this DIST raster
  this_raster <- terra::rast(this_row[["raster_file"]])
  
  #same baseline
  base_raster <- terra::rast(baseline_file)
  
  #stack them, with baseline value times 1000 (~bit moved)
  this_stack <- c(base_raster*1000, 
                  this_raster)
  
  #Add them together, so baseline value (B) & DIST value (D) look like: BBBDDD
  this_change <- terra::app(this_stack, fun = "sum")
  
  #save
  terra::writeRaster(this_change,
                     this_row[["out_file"]]) 
  
}

### set up for parallel runs

#remove everything not needed to pass along
rm(folder_out, folder_prebake, raster_files)

### RUN ----------------------------------------------------

#UPDATE WORKER NUMBER
plan(multisession, workers = 4) #availableCores(omit=1)

(start_time <- Sys.time())

future_lapply(1:nrow(input_tbl), collapse_rasters)

(end_time <- Sys.time())
(elapsed_time <- end_time - start_time)
