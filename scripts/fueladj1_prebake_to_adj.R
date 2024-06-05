# Turn all nonforest prebakes into fuel adjective values

# First in a series. 
# This script turns the original prebake raster into fuel adjective rasters
# Second script will extract statistics, and third will graph/visualize. 

### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra)

### User settings ----------------------------------

#FL or ROS
adj_to_use <- "FL" 

#prebake folder
folder_prebake <- file.path("nonforest_prebakes", 
                            "prebakes")


#outpath
folder_out <- file.path("nonforest_prebakes",
                        paste0("adjectivized_", adj_to_use))
dir.create(folder_out, recursive = TRUE)


### Adj Data -------------------------------------------

#adjectives 
adj_raw <- read_csv("data_qa/fbfm_adjective_original_crosswalk.csv")

#non-burn codes to add in as well
nonburn_codes <- tibble(FBFM = c(91, 92, 93, 98, 99),
                        adj_code = rep(1, 5))

#need to duplicate/adjust for special WUI codes
low100_list <- c(101,102,103,104,105,106,107,108)
#add 100
rest_list <- c(121,122,123,124,
               141,142,143,144,145,146,147,148,149,
               161,162,163,165,
               181,182,183,184,185,186,187,188,189,
               201,202,203)

low100 <- adj_raw %>%
  filter(FBFM %in% low100_list) %>%
  rename(old_fbfm = FBFM) %>%
  mutate(FBFM = old_fbfm + 110) %>%
  dplyr::select(-old_fbfm)

rest <- adj_raw %>%
  filter(FBFM %in% rest_list) %>%
  rename(old_fbfm = FBFM) %>%
  mutate(FBFM = old_fbfm + 100) %>%
  dplyr::select(-old_fbfm)


adj <- bind_rows(adj_raw, low100, rest) %>%
  arrange(FBFM)




if (adj_to_use == "FL"){
  
  #nb VL L M H VH X
  # 1 2  3 4 5  6 7
  adj_codes <- tibble(FL = c("VL", "L", "M", "H", "VH", "X"),
                      adj_code = 2:7)
  
  
  adj_xwalk <- adj %>% 
    select(FBFM, FL) %>% 
    #matrices must be of same type, raster is numeric
    #turn adjectives into factor, basically
    mutate(FBFM = as.numeric(FBFM)) %>% 
    left_join(adj_codes, by = join_by("FL")) %>% 
    dplyr::select(-FL) %>% 
    #add nonburnables as a 0 category
    bind_rows(nonburn_codes) %>%
    #matrix for terra::classify()
    as.matrix
  
} else if (adj_to_use == "ROS"){
  
  #nb VL L M H VH X
  # 1 2  3 4 5  6 7
  adj_codes <- tibble(ROS = c("VL", "L", "M", "H", "VH", "X"),
                      adj_code = 2:7)
  
  
  adj_xwalk <- adj %>% 
    select(FBFM, ROS) %>% 
    #matrices must be of same type, raster is numeric
    #turn adjectives into factor, basically
    mutate(FBFM = as.numeric(FBFM)) %>% 
    left_join(adj_codes, by = join_by("ROS")) %>% 
    dplyr::select(-ROS) %>% 
    #add nonburnables as a 0 category
    bind_rows(nonburn_codes) %>%
    #matrix for terra::classify()
    as.matrix
}

# # special, both FL and ROS final crosswalk
# #  could use this instead in the future
# fl_adj_codes <- tibble(FL = c("VL", "L", "M", "H", "VH", "X"),
#                     FL_adj_code = 2:7)
# ros_adj_codes <- tibble(ROS = c("VL", "L", "M", "H", "VH", "X"),
#                     ROS_adj_code = 2:7)
# #ROS then FL
# adj_full <- adj %>% 
#   left_join(ros_adj_codes, by = join_by("ROS")) %>% 
#   left_join(fl_adj_codes, by = join_by("FL")) %>% 
#   bind_rows(tibble(FBFM = c(91, 92, 93, 98, 99),
#                    ROS = rep("NB", 5),
#                    FL = rep("NB", 5),
#                    ROS_adj_code = rep(1, 5),
#                    FL_adj_code = rep(1, 5))) %>% 
#   arrange(FBFM)
# write_csv(adj_full, "data_qa/fbfm_adjective_full_crosswalk.csv")

### Reclass setup --------------------------------

#id fuels
fuel_files <- list.files(folder_prebake,
                         full.names = TRUE,
                         pattern = "tif$")


#create tibble
fuel_tbl <- tibble(fuel_file = fuel_files) %>% 
  mutate(file_name = basename(fuel_file),
         out_file = file.path(folder_out, 
                              paste0(tools::file_path_sans_ext(file_name),
                                     "_", 
                                     adj_to_use, 
                                     ".tif")))

#create function for lapply
#function to reclass
reclass_fuel <- function(r){
  
  this_row <- fuel_tbl[r,]
  
  this_fml <- terra::rast(this_row[["fuel_file"]])
  
  # NO. Keep 0 as NoData, looking at total pixel counts later (even No Data areas)
  # #change existing 0s to NA first
  # #maybe could be done at the same time as reclass, but better safe
  # this_fml <- terra::subst(this_fml, 
  #                          from=0,
  #                          to=NA)
  
  #reclass
  this_fml_adj <- terra::classify(this_fml,
                                  adj_xwalk)
  
  #save
  terra::writeRaster(this_fml_adj,
                     this_row[["out_file"]])
  
}

### Run -------------------------------------

(start_time <- Sys.time())

lapply(1:nrow(fuel_tbl), reclass_fuel)


### Also baseline ----------------------------------------

#baseline
orig <- rast("data_input_fm40/fbfm40_ffv3_2023_11_nbflip_mosiac.tif")
#throw small baseline into same folder as prebakes
orig_crop <- project(orig, rast(fuel_files[1]), method="near")
orig_adj <- terra::classify(orig_crop, adj_xwalk)
terra::writeRaster(orig_adj, file.path(folder_out, 
                                      paste0("FM40_planscapeNFPB_Baseline_", 
                                             adj_to_use, 
                                             ".tif")))

(end_time <- Sys.time())
(elapsed_time <- end_time - start_time)

