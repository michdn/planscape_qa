

### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra,
  sf,
  ggalluvial)

options(scipen = 999)

### Data ----------------------------------------------

#Original version of FM40 FFv3 
# (Pre special WUI codes, pre nonburn flip)
orig <- terra::rast("data_input_fm40/conus_fuels_Fuels_FM40_202311v1.tif")

#"Operational" version of FM40 FFv3
op <- terra::rast("data_input_fm40/fbfm40_ffv3_2023_11_nbflip_mosiac.tif")

# Treemap for coverage
tm <- terra::rast("data_qa/treemap/CONUS_treemap.tif")

# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- terra::vect("data_qa/tl_2023_us_state/tl_2023_us_state.shp")
ca <- subset(states, states$STUSPS == "CA")


### Plotting set up --------------------------------------------

#official FBFM colors
colors_raw <- read_csv("data_qa/fbfm_colors.csv") %>%
  #as character and 3 digits across
  mutate(fbfm_value = as.character(fbfm_value),
         fbfm_value = str_pad(fbfm_value, width=3, side="left", pad="0")) %>%
  #remove -9999
  dplyr::filter(!fbfm_value == "-9999")

#add extra colors (special WUI codes)
#add 110
low100_list <- c(101,102,103,104,105,106,107,108)
#add 100
rest_list <- c(121,122,123,124,
          141,142,143,144,145,146,147,148,149,
          161,162,163,165,
          181,182,183,184,185,186,187,188,189,
          201,202,203)

low100 <- colors_raw %>%
  filter(fbfm_value %in% low100_list) %>%
  rename(old_fbfm = fbfm_value) %>%
  mutate(fbfm_value = (as.numeric(old_fbfm) + 110) %>%
           as.character(),
         fbfm_label = paste0(fbfm_label, "-WUI")) %>%
  dplyr::select(-old_fbfm)

rest <- colors_raw %>%
  filter(fbfm_value %in% rest_list) %>%
  rename(old_fbfm = fbfm_value) %>%
  mutate(fbfm_value = (as.numeric(old_fbfm) + 100) %>%
           as.character(),
         fbfm_label = paste0(fbfm_label, "-WUI")) %>%
  dplyr::select(-old_fbfm)


colors <- bind_rows(colors_raw, low100, rest) %>%
  arrange(fbfm_value)

#without adj
colors_fbfm <- setNames(as.character(colors$color_hex),
                        colors$fbfm_value)




### Crop to CA first to reduce computation -----------------

orig_ca <- crop(orig, project(ca, crs(orig)))
op_ca <- crop(op, project(ca, crs(op)))
tm_ca <- crop(tm, project(ca, crs(tm)))

#op and tm in NAD83 Albers. orig in 5070
orig_ca_proj <- project(orig_ca, op_ca, method="near")

### compare orig and op

#stack fm40
orop_stack <- c(orig_ca_proj * 1000,
                op_ca)
#add together original(OOO) and operational (PPP): OOOPPP
orop <- terra::app(orop_stack, fun = "sum")

#freq_test <- freq(orop) #Yes, this looks right

#mask just california boundaries in treemap (not extent from crop)
tm_ca_masked <- mask(tm_ca, project(ca, crs(tm)))
#mask only where there is data
tm_ca_binary <- ifel(!is.na(tm_ca_masked), 1, NA)

#now, just orop where there are treemap values in CA
orop_masked <- mask(orop, tm_ca_binary)


### histogram and sankey --------------------------------

orop_freq <- freq(orop_masked) 

of <- orop_freq %>%
  as_tibble() %>%
  dplyr::select(-layer) %>%
  #prep for split
  mutate(value = str_pad(value, width=6, side="left", pad="0")) %>%
  #split
  #calling base 'original' here
  separate_wider_position(cols=value, widths=c(original=3,
                                                  operational=3))

nonburns <- c("091", "092", "093", "098", "099")

#total FOREST pixels in CA
total_pixels <- sum(of$count)

#nonburn % in original (irrespective of operational) [forested areas]
of %>%
  filter(original %in% nonburns) %>%
  summarize(pixels = sum(count)) %>%
  mutate(ca_perc = pixels/total_pixels*100)
# pixels ca_perc
# 2582227    2.17

#nonburn % in operational (irrespective of origin) [forested areas]
of %>%
  filter(operational %in% nonburns) %>%
  summarize(pixels = sum(count)) %>%
  mutate(ca_perc = pixels/total_pixels*100)
# pixels ca_perc
# 415689   0.349


#pixels in nonburn in BOTH [forested areas]
of %>%
  filter(original %in% nonburns,
         operational %in% nonburns) %>%
  summarize(pixels = sum(count)) %>%
  mutate(ca_perc = pixels/total_pixels*100)
# pixels ca_perc
# 415689   0.349


#pixels nonburn in original NOT in operational [forested areas]
#i.e. reverted
of %>%
  filter(original %in% nonburns,
         !operational %in% nonburns) %>%
  summarize(pixels = sum(count)) %>%
  mutate(ca_perc = pixels/total_pixels*100)
# pixels ca_perc
# 2166538    1.82

#pixels nonburn NOT in original but in operational [forested areas]
of %>%
  filter(!original %in% nonburns,
         operational %in% nonburns) %>%
  summarize(pixels = sum(count)) %>%
  mutate(ca_perc = pixels/total_pixels*100)
# pixels ca_perc
# 0       0


#burnable changes
# these shouldn't really exist except for special WUI codes???
of %>%
  filter(!original %in% nonburns,
         !operational %in% nonburns,
         !original == operational) %>%
  arrange(desc(count))
#0 rows

# explore reverted
of %>%
  filter(original %in% nonburns,
         !operational %in% nonburns) %>%
  arrange(desc(count))

of %>%
  filter(original %in% nonburns,
         !operational %in% nonburns) %>%
  group_by(original) %>%
  summarize(pixels = sum(count)) %>%
  arrange(desc(pixels))

of %>%
  filter(original %in% nonburns,
         !operational %in% nonburns) %>%
  group_by(operational) %>%
  summarize(pixels = sum(count)) %>%
  arrange(desc(pixels))

### Sankey -----------------------

of_lodes <- of %>%
  to_lodes_form(axes=c("original", "operational")) %>%
  rename(FM40 = stratum)

all_plot <- ggplot(data=of_lodes,
                    aes(x=x, y=count, stratum=FM40, alluvium=alluvium,
                        fill=FM40)) +
  geom_flow() +
  geom_stratum(width=1/2) +
  scale_fill_manual("FBFM40", values = colors_fbfm) +
  labs(title="Original and operational FBFM40",
       subtitle="Within FORESTED area in CA",
       x="",
       y="Pixel count") +
  theme_bw()

all_plot

ggsave(all_plot,
       filename = file.path("plots_qa",
                            "original_operational",
                            "original_operational_sankey.jpg"),
       height=6, width=5, units=c("in"))

# only different
of_diff_lodes <- of %>%
  filter(!original == operational) %>%
  to_lodes_form(axes=c("original", "operational")) %>%
  rename(FM40 = stratum)

diff_plot <- ggplot(data=of_diff_lodes,
                    aes(x=x, y=count, stratum=FM40, alluvium=alluvium,
                        fill=FM40)) +
  geom_flow() +
  geom_stratum(width=1/2) +
  scale_fill_manual("FBFM40", values = colors_fbfm) +
  labs(title="ONLY DIFFERENCES between\noriginal and operational FBFM40",
       subtitle="Within FORESTED area in CA",
       x="",
       y="Pixel count") +
  theme_bw()

diff_plot

ggsave(diff_plot,
       filename = file.path("plots_qa",
                            "original_operational",
                            "original_operational_differencesonly_sankey.jpg"),
       height=6, width=5, units=c("in"))










# ## ORIGINAL. ALIGNMENT ISSUES. I THINK. 
# 
# ### Data -----------------------------------------------
# 
# # #"Operational" version of FM40 FFv3
# # op <- terra::rast("data_input_fm40/fbfm40_ffv3_2023_11_nbflip_mosiac.tif")
# # 
# # #"Base" version of FM40 FFv3 (called 'original' later)
# # # (Pre special WUI codes, pre nonburn flip)
# # ba <- terra::rast("data_input_fm40/conus_fuels_Fuels_FM40_202311v1.tif")
# 
# baop <- terra::rast("data_input_fm40/base_operational_combinechange.tif")
# 
# 
# # Treemap for coverage
# tm <- terra::rast("data_qa/treemap/CONUS_treemap.tif")
# 
# #If CA state boundary wanted, download from 
# # https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
# states <- terra::vect("data_qa/tl_2023_us_state/tl_2023_us_state.shp")
# ca <- subset(states, states$STUSPS == "CA")
# 
# ca_5070 <- project(ca, crs(baop))
# 
# 
# #official FBFM colors
# colors_raw <- read_csv("data_qa/fbfm_colors.csv") %>% 
#   #as character and 3 digits across
#   mutate(fbfm_value = as.character(fbfm_value),
#          fbfm_value = str_pad(fbfm_value, width=3, side="left", pad="0")) %>% 
#   #remove -9999
#   dplyr::filter(!fbfm_value == "-9999")
# 
# #add extra colors (special WUI codes)
# #add 110
# low100_list <- c(101,102,103,104,105,106,107,108) 
# #add 100
# rest_list <- c(121,122,123,124,                             
#           141,142,143,144,145,146,147,148,149,
#           161,162,163,165,
#           181,182,183,184,185,186,187,188,189,
#           201,202,203)
# 
# low100 <- colors_raw %>% 
#   filter(fbfm_value %in% low100_list) %>% 
#   rename(old_fbfm = fbfm_value) %>% 
#   mutate(fbfm_value = (as.numeric(old_fbfm) + 110) %>% 
#            as.character(),
#          fbfm_label = paste0(fbfm_label, "-WUI")) %>% 
#   dplyr::select(-old_fbfm)
# 
# rest <- colors_raw %>% 
#   filter(fbfm_value %in% rest_list) %>% 
#   rename(old_fbfm = fbfm_value) %>% 
#   mutate(fbfm_value = (as.numeric(old_fbfm) + 100) %>% 
#            as.character(),
#          fbfm_label = paste0(fbfm_label, "-WUI")) %>% 
#   dplyr::select(-old_fbfm)
# 
# 
# colors <- bind_rows(colors_raw, low100, rest) %>% 
#   arrange(fbfm_value)
# 
# #without adj
# colors_fbfm <- setNames(as.character(colors$color_hex),
#                         colors$fbfm_value) 
# 
# 
# ### Create base-to-op fuel layer ----------------------
# 
# # #project op to ba (same extent (and resolution) as well)
# # # nearest neighbor since categorical
# # op_proj <- project(op, ba, method = "near")
# # 
# # #stack fm40
# # baop_stack <- c(ba * 1000,
# #                 op_proj)
# # #add together BBB(base) and OOO(operational: BBBOOO
# # baop <- terra::app(baop_stack, fun = "sum")
# # #above steps took 40 minutes (project, stack, sum)
# # 
# # #save out to just read in this later
# # terra::writeRaster(baop, "data_input_fm40/base_operational_combinechange.tif")
# 
# ### treemap coverage ---------------------------------
# 
# #slow, clip all to CA to work with easier
# # bug in crop with mask=TRUE. crop then mask.
# 
# baop_ca <- crop(baop, ca_5070)
# baop_ca_mask <- mask(baop_ca, ca_5070)
# 
# #SOMETHING WEIRD, values shooting up way high after project
# #tm_ca <- project(tm, baop_ca, method = "near") # near?
# #do step by step
# tm_crop <- crop(tm, ca_5070)
# tm_crop_5070 <- project(tm_crop, baop_ca, method="near")
# tm_ca_mask <- mask(tm_crop_5070, ca_5070)
# 
# #just baop_ca_mask where there is treemap data (not no data)
# tm_ca_binary <- ifel(!is.na(tm_ca_mask), 1, NA)
# 
# #just baop where there is treemap values (i.e. forest)
# baop_tm <- mask(baop_ca_mask, tm_ca_binary)
# 
# #save out baop_tm since above was so finicky
# terra::writeRaster(baop_tm, "data_input_fm40/base_op_ca_treemap_only.tif")
# 
# #baop_tm <- readRaster("data_input_fm40/base_op_ca_treemap_only.tif")
# 
# 
# ### Frequency of baop values, split and analyse ------------------
# bt_freq <- terra::freq(baop_tm)
# 
# bt <- bt_freq %>% 
#   as_tibble() %>% 
#   dplyr::select(-layer) %>% 
#   #prep for split
#   mutate(value = str_pad(value, width=6, side="left", pad="0")) %>% 
#   #split
#   #calling base 'original' here
#   separate_wider_position(cols=value, widths=c(original=3, 
#                                                   operational=3))
# 
# bt  
# 
# nonburns <- c("091", "092", "093", "098", "099")
# 
# #total FOREST pixels in CA
# total_pixels <- sum(bt$count)
# 
# #nonburn % in original (irrespective of operational) [forested areas]
# bt %>% 
#   filter(original %in% nonburns) %>% 
#   summarize(pixels = sum(count)) %>% 
#   mutate(ca_perc = pixels/total_pixels*100)
# # pixels ca_perc
# # 2515408    2.11
# 
# #nonburn % in operational (irrespective of origin) [forested areas]
# bt %>% 
#   filter(operational %in% nonburns) %>% 
#   summarize(pixels = sum(count)) %>% 
#   mutate(ca_perc = pixels/total_pixels*100)
# # pixels ca_perc
# # 415714   0.349
# 
# 
# #pixels in nonburn in BOTH [forested areas]
# bt %>% 
#   filter(original %in% nonburns,
#          operational %in% nonburns) %>% 
#   summarize(pixels = sum(count)) %>% 
#   mutate(ca_perc = pixels/total_pixels*100)
# # pixels ca_perc
# # 77369  0.0649
# # SO LOW??
# 
# #pixels nonburn in original NOT in operational [forested areas]
# #i.e. reverted
# bt %>% 
#   filter(original %in% nonburns,
#          !operational %in% nonburns) %>%
#   summarize(pixels = sum(count)) %>% 
#   mutate(ca_perc = pixels/total_pixels*100)
# # pixels ca_perc
# # 2438039    2.05
# 
# #pixels nonburn NOT in original but in operational [forested areas]
# #???
# bt %>% 
#   filter(!original %in% nonburns,
#          operational %in% nonburns) %>%
#   summarize(pixels = sum(count)) %>% 
#   mutate(ca_perc = pixels/total_pixels*100)
# # pixels ca_perc
# # 338345   0.284
# # ???? projection alignment??? what else could it be? 
# 
# 
# #i.e. reverted
# 
# bt %>% 
#   filter(original %in% nonburns,
#          !operational %in% nonburns) %>% 
#   arrange(desc(count))
# 
# bt %>% 
#   filter(original %in% nonburns,
#          !operational %in% nonburns) %>% 
#   group_by(original) %>% 
#   summarize(pixels = sum(count)) %>% 
#   arrange(desc(pixels))
# 
# bt %>% 
#   filter(original %in% nonburns,
#          !operational %in% nonburns) %>% 
#   group_by(operational) %>% 
#   summarize(pixels = sum(count)) %>% 
#   arrange(desc(pixels))
# 
# #burnable changes
# # these shouldn't really exist except for special WUI codes???
# bt %>% 
#   filter(!original %in% nonburns,
#          !operational %in% nonburns,
#          !original == operational) %>% 
#   arrange(desc(count))
# # oooh. roughly equal flips.... sounds like an alignment issue
# 
# 
