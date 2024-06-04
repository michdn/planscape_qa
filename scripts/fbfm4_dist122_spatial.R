# DIST 122 example
# highlight spatial WHERE NBtB changes are happening

### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra)
#future.apply)

#options(future.globals.onReference = "warning")


### User settings --------------------------------------

#raster locations
folder_prebake <- file.path("nonforest_prebakes", 
                            "prebakes")

#raster output
folder_out <- file.path("nonforest_prebakes", 
                        "fbfm_change_burnbinary")
dir.create(folder_out, recursive = TRUE)


### Data --------------------------------------------------

raster_files <- list.files(folder_prebake,
                           full.names = TRUE,
                           pattern = "tif$")


baseline_file <- file.path("data_input_fm40",
                           "fbfm40_ffv3_2023_11_nbflip_mosiac_clipped.tif")

nb_xwalk_raw <- read_csv("data_qa/fbfm_nonburn_crosswalk.csv")


#need to duplicate/adjust for special WUI codes
low100_list <- c(101,102,103,104,105,106,107,108)
#add 100
rest_list <- c(121,122,123,124,
               141,142,143,144,145,146,147,148,149,
               161,162,163,165,
               181,182,183,184,185,186,187,188,189,
               201,202,203)

low100 <- nb_xwalk_raw %>%
  filter(FBFM %in% low100_list) %>%
  rename(old_fbfm = FBFM) %>%
  mutate(FBFM = old_fbfm + 110) %>%
  dplyr::select(-old_fbfm)

rest <- nb_xwalk_raw %>%
  filter(FBFM %in% rest_list) %>%
  rename(old_fbfm = FBFM) %>%
  mutate(FBFM = old_fbfm + 100) %>%
  dplyr::select(-old_fbfm)


nb_xwalk <- bind_rows(nb_xwalk_raw, low100, rest) %>%
  arrange(FBFM) %>% 
  select(FBFM, Burn) %>% 
  #matrix for terra::classify()
  as.matrix


### setup -----------------------------------------------

# ONLY DIST122
raster_files <- grep(raster_files, pattern="_DIST122", value=TRUE)




#create tibble
input_tbl <- tibble(raster_file = raster_files) %>% 
  mutate(file_name = basename(raster_file),
         out_file = file.path(folder_out, 
                              paste0(tools::file_path_sans_ext(file_name),
                                     "_burnflag.tif"))) 


collapse_nbflag_rasters <- function(r){
  
  this_row <- input_tbl[r,]
  
  #this DIST raster
  this_raster <- terra::rast(this_row[["raster_file"]])
  
  #get rid of 0s
  this_raster_z <- terra::subst(this_raster,
                                from=0,
                                to=NA)
  
  
  #recode for burnable
  this_raster_b <- terra::classify(this_raster_z,
                                   nb_xwalk)
  
  
  #same baseline
  base_raster <- terra::rast(baseline_file)
  
  #get rid of 0s
  base_z <- terra::subst(base_raster,
                         from=0,
                         to=NA)
  
  #recode for burnable
  base_b <- terra::classify(base_z,
                            nb_xwalk)
  
  
  
  #stack them, with baseline value times 10 (~bit moved)
  this_stack <- c(base_b*10, 
                  this_raster_b)
  
  #Add them together, so baseline value (B) & DIST value (D) look like: BD
  this_change <- terra::app(this_stack, fun = "sum")
  
  #save
  terra::writeRaster(this_change,
                     this_row[["out_file"]]) 
  
}


lapply(1:nrow(input_tbl), collapse_rasters)
