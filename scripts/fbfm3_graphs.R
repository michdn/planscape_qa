# analyzing and graphing sankey of fuel changes
# from base to postDIST

# Third in a series. 
# First script created a combined baseline-DIST{} raster so 
#  we can get PER PIXEL fuel changes. 
# Second script extract summaries - pixel counts of each combination of change
#  within California state boundaries. 
# This script will graph and create visualizations. 


### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  viridis,
  ggalluvial,
  ggfittext,
  gridExtra)

#stop scientific notation (numbers are fuel model numbers)
options(scipen = 999)


### Data -------------------------------------------------

fmc <- readRDS(file.path("data_qa",
                         "fbfm_pixelchanges_CA.RDS"))

folder_plots <- file.path("plots",
                          "fm_change_plots")
dir.create(folder_plots)


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


### Process ----------------------------------------------

nonburns <- c("091", "092", "093", "098", "099")

#split out
# NOTE! baseline=000 means baseline was zero
# Due to multiplication and addition, this will NOT capture pixels
#  that came from or to NA.
fmc_spl <- fmc %>% 
  #prep for split
  mutate(fmchanges = str_pad(fmchanges, width=6, side="left", pad="0")) %>%
  #split
  separate_wider_position(cols=fmchanges, widths=c(baseline=3,
                                                   postdist=3)) %>% 
  #recode burn/non burn for overall view
  mutate(base_cat = case_when(
    baseline %in% nonburns ~ "NonBurn",
    baseline == "000" ~ "0 (NA)",
    .default = "Burnable")) %>% 
  mutate(post_cat = case_when(
    postdist %in% nonburns ~ "NonBurn",
    postdist == "000" ~ "0 (NA)",
    .default = "Burnable")) 

# table of burn category changes
fmc_cat <- fmc_spl %>% 
  group_by(DIST, base_cat, post_cat) %>% 
  summarize(pixels = sum(pixel_count),
            total_pixels = first(total_pixels),
            .groups="drop") %>% 
  mutate(perc = pixels/total_pixels*100) %>% 
  select(DIST, base_cat, post_cat, perc) %>% 
  arrange(DIST, desc(perc))

# See if any had large abnormal changes
# No. 

#table to show 
fmc_cat %>% 
  filter(post_cat == "Burnable", 
         base_cat == "NonBurn") %>% 
  dplyr::select(DIST, perc) %>% 
  rename(`Disturbance Code`=DIST, 
         `Percent of CA` = perc) %>% 
  write_csv(file="data_qa/nonburn_to_burn_bydist.csv")
  

### Nonburn to burnable changes -----------------------

fmc_nbb <- fmc_spl %>% 
  filter(post_cat == "Burnable",
         base_cat == "NonBurn")

#what nb from?
fmc_nbb_from <- fmc_nbb %>% 
  group_by(DIST, baseline) %>% 
  summarize(pixels_nbb = sum(pixel_count),
            .groups="drop") %>% 
  dplyr::select(DIST, baseline, pixels_nbb) %>% 
  arrange(DIST, desc(pixels_nbb)) %>% 
  #totals of nonburn in baseline
  left_join(fmc_spl %>% 
              filter(baseline %in% nonburns) %>% 
              group_by(DIST, baseline) %>% 
              summarize(ca_fm_pixels = sum(pixel_count),
                        .groups = "drop"),
            by = join_by(DIST, baseline)) %>% 
  #percentage of nonburn pixels fm value that turns burnable
  mutate(perc_nb_nbb = pixels_nbb/ca_fm_pixels*100)

fmc_nbb_from %>% 
  group_by(baseline) %>% 
  summarize(mean_pixels = mean(pixels_nbb),
            mean_ca_fm_pixels = mean(ca_fm_pixels)) %>% 
  mutate(perc = mean_pixels/mean_ca_fm_pixels*100) %>% 
  arrange(desc(mean_pixels))

fmc_nbb_from

p_perc_nbtb <- ggplot() + 
  geom_col(data=fmc_nbb_from,
           mapping = aes(x=baseline,
                         y=perc_nb_nbb,
                         fill=baseline)) + 
  scale_fill_viridis(option = "plasma", discrete = TRUE) + 
  facet_wrap(~DIST) + 
  labs(title = "Percentage of Non-burnable that Changed to Burnable",
       y="Percentage of each FM40",
       x="Baseline FBFM40") + 
  theme_bw()

p_perc_nbtb

ggsave(plot=p_perc_nbtb, 
       filename=file.path("plots",
                          "nbtb_perc_by_fm40_dist.jpg"),
       height = 5, width = 7, units = c("in"))

#what turning into? 
fmc_nbb %>% 
  group_by(DIST, postdist) %>% 
  summarize(pixels_nbb = sum(pixel_count),
            .groups="drop") %>% 
  arrange(DIST, desc(pixels_nbb))


fmc_nbb %>% 
  filter(DIST == "DIST122") %>% 
  group_by(DIST, postdist) %>% 
  summarize(pixels_nbb = sum(pixel_count),
            .groups="drop") %>% 
  arrange(DIST, desc(pixels_nbb))

### Sankey NB fuel changes ---------------------------

fm_nb_lodes <- fmc_nbb %>% 
  dplyr::select(DIST, baseline, postdist, pixel_count) %>%  
  rename(post_disturbance = postdist) %>% 
  to_lodes_form(axes = c("baseline", "post_disturbance")) %>% 
  rename(FBFM = stratum)

dists <- fmc_nbb %>% pull(DIST) %>% unique() %>% sort()

for (i in seq_along(dists)){
  this_dist <- dists[i]
  
  nb_plot <- ggplot(data=fm_nb_lodes %>% 
                      filter(DIST==this_dist),
                    aes(x=x, y=pixel_count,
                        stratum=FBFM, alluvium=alluvium,
                        fill=FBFM, label=FBFM)) +
    geom_flow() +
    geom_stratum(width=1/2) +
    scale_fill_manual("FBFM40", values = colors_fbfm) +
    ggfittext::geom_fit_text(stat="stratum", min.size=5, width=1/2,
                             color="white", show.legend=FALSE) + 
    labs(title=paste0(this_dist, 
                      ": Non-Burn Changes from Baseline"),
         subtitle="Within California, ONLY Non-burn that changed",
         x="",
         y="Pixel count") +
    theme_bw()
  nb_plot
  
  ggsave(plot=nb_plot,
         filename=file.path("plots",
                            "NB_fm_change_plots",
                            paste0(this_dist, "_CA_NB_changes.jpg")),
         height = 6, width = 6, units=c("in"))
  
}
#overview
nb_all_plot <- ggplot(data=fm_nb_lodes,
                      aes(x=x, y=pixel_count,
                          stratum=FBFM, alluvium=alluvium,
                          fill=FBFM, label=FBFM)) +
  geom_flow() +
  geom_stratum(width=1/2) +
  scale_fill_manual("FBFM40", values = colors_fbfm) +
  ggfittext::geom_fit_text(stat="stratum", min.size=5, width=1/2,
                           color="white", show.legend=FALSE) + 
  labs(title="Non-Burn Changes from Baseline",
       subtitle="Within California, ONLY Non-burn that changed",
       x="",
       y="Pixel count") +
  theme_bw() + 
  facet_wrap(~DIST)
nb_all_plot


ggsave(plot=nb_all_plot,
       filename=file.path("plots",
                          "NB_fm_change_plots",
                          "CA_NB_changes.jpg"),
       height = 8, width = 10, units=c("in"))




### Sankey fuel changes -----------------------------

# OVERALL

#facet will not work. 
# loop and save individual

fm_lodes <- fmc_spl %>% 
  dplyr::select(DIST, baseline, postdist, pixel_count) %>%  
  rename(post_disturbance = postdist) %>% 
  to_lodes_form(axes = c("baseline", "post_disturbance")) %>% 
  rename(FBFM = stratum)

dists <- fmc_spl %>% pull(DIST) %>% unique() %>% sort()

for (i in seq_along(dists)){
  this_dist <- dists[i]
  
  fm_plot <- ggplot(data=fm_lodes %>% 
                      filter(DIST==this_dist),
                    aes(x=x, y=pixel_count,
                        stratum=FBFM, alluvium=alluvium,
                        fill=FBFM, label=FBFM)) +
    geom_flow() +
    geom_stratum(width=1/2) +
    scale_fill_manual("FBFM40", values = colors_fbfm) +
    ggfittext::geom_fit_text(stat="stratum", min.size=5, width=1/2,
                             color="white", show.legend=FALSE) + 
    labs(title=paste0(this_dist, 
                      ": FBFM40 Changes from Baseline"),
         subtitle="Within California",
         x="",
         y="Pixel count") +
    theme_bw()
  #fm_plot
  
  ggsave(plot=fm_plot,
         filename=file.path(folder_plots,
                            paste0(this_dist, "_CA_FBFM_changes.jpg")),
         height = 6, width = 6, units=c("in"))
  
}

#overview
fm_all_plot <- ggplot(data=fm_lodes,
                  aes(x=x, y=pixel_count,
                      stratum=FBFM, alluvium=alluvium,
                      fill=FBFM, label=FBFM)) +
  geom_flow() +
  geom_stratum(width=1/2) +
  scale_fill_manual("FBFM40", values = colors_fbfm) +
  ggfittext::geom_fit_text(stat="stratum", min.size=5, width=1/2,
                           color="white", show.legend=FALSE) + 
  labs(title="FBFM40 Changes from Baseline",
       subtitle="Within California",
       x="",
       y="Pixel count") +
  theme_bw() + 
  facet_wrap(~DIST)
fm_all_plot


ggsave(plot=fm_all_plot,
       filename=file.path(folder_plots,
                          "CA_FBFM_changes_allfacet.jpg"),
       height = 8, width = 10, units=c("in"))






# ### Sankey categories ---------------------
# Abandoned - Very boring, mostly burnable, nonburn. Include tables as appendix
# 
# cat_colors <- ("")
# 
# fm_cat_lodes <- fmc_cat %>%
#   to_lodes_form(axes=c("base_cat", "post_cat")) %>%
#   rename(FM_category = stratum,
#          percent_CA = perc)
# 
# cat_plot <- ggplot(data=fm_cat_lodes,
#                    aes(x=x, y=percent_CA, 
#                        stratum=FM_category, alluvium=alluvium,
#                        fill=FM_category)) +
#   geom_flow() +
#   geom_stratum(width=1/2) +
#   #scale_fill_manual("FBFM40", values = colors_fbfm) +
#   labs(title="",
#        subtitle="CA",
#        x="",
#        y="") +
#   theme_bw() + 
#   facet_wrap(~DIST)
# cat_plot
