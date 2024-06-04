#fuel adjective graphs

# 1. means, all 

# 2. diff from baseline, individual

### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  viridis,
  gridExtra)

#stop scientific notation
options(scipen = 999)


### Data -------------------------------------------------

ros <- readRDS(file.path("data_qa",
                         "fuel_adj_stats_ROS.RDS"))

fl <- readRDS(file.path("data_qa",
                         "fuel_adj_stats_FL.RDS"))



### 1. Means comparison, ROS and FL ---------------------

# do DIFFERENCE 
# DIST-baseline, so negative number are a reduction in fire behavior after disturbance

# separate for ROS vs FL 

# colors for DIST

ros_means <- ros %>% 
  dplyr::select(DIST, adj, total_mean) %>% 
  #total rows all same for each adj value
  distinct()

ros_diff <- ros_means %>% 
  filter(!DIST == "Baseline") %>% 
  #add baseline to each
  cross_join(ros_means %>% 
            filter(DIST == "Baseline") %>% 
              dplyr::select(-DIST, -adj) %>% 
              rename(base_mean = total_mean)) %>% 
  #calc diff
  mutate(mean_difference = total_mean - base_mean)


p_ros <- ggplot() + 
  geom_col(data=ros_diff,
           mapping=aes(x=DIST, y=mean_difference,
                       fill=DIST)) + 
  scale_fill_viridis("Disturbance", discrete = TRUE) +
  labs(title="Difference in Rate of Spread (ROS) Fuel Adjective\nfrom Baseline to Disturbed",
       subtitle="Across entire landscape",
       x="Disturbance code",
       y="Post Treatment - Baseline ROS adjective mean",
       caption = paste0("We expect a decrease in fuel adjective values after treatment, 
                  so we expect negative numbers here.
                  Baseline mean was ", 
       ros_means %>% filter(DIST=="Baseline") %>% pull(total_mean) %>% round(2))) + 
  geom_hline(yintercept=0) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=-45, vjust=0.5, hjust=0.1))
p_ros

ggsave(plot=p_ros, 
       filename="plots/fueladj_ROS_meandiff.jpg",
       height=5, width=5, units=c("in"))


# flame length
fl_means <- fl %>% 
  dplyr::select(DIST, adj, total_mean) %>% 
  #total rows all same for each adj value
  distinct()

fl_diff <- fl_means %>% 
  filter(!DIST == "Baseline") %>% 
  #add baseline to each
  cross_join(fl_means %>% 
               filter(DIST == "Baseline") %>% 
               dplyr::select(-DIST, -adj) %>% 
               rename(base_mean = total_mean)) %>% 
  #calc diff
  mutate(mean_difference = total_mean - base_mean)


p_fl <- ggplot() + 
  geom_col(data=fl_diff,
           mapping=aes(x=DIST, y=mean_difference,
                       fill=DIST)) + 
  scale_fill_viridis("Disturbance", discrete = TRUE) +
  labs(title="Difference in Flame Length (FL) Fuel Adjective\nfrom Baseline to Disturbed",
       subtitle="Across entire landscape",
       x="Disturbance code",
       y="Post Treatment - Baseline FL adjective mean",
       caption = paste0("We expect a decrease in fuel adjective values after treatment, 
                  so we expect negative numbers here.
                  Baseline mean was ", 
                        fl_means %>% filter(DIST=="Baseline") %>% pull(total_mean) %>% round(2))) + 
  geom_hline(yintercept=0) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=-45, vjust=0.5, hjust=0.1))
p_fl

ggsave(plot=p_fl, 
       filename="plots/fueladj_FL_meandiff.jpg",
       height=5, width=5, units=c("in"))


# 2. adjective category changes from baseline ------------------------------

#as all are the same area, can leave as pixels instead of % total area.
# separate for ROS and FL

#2a. per DIST (facet), pixel diff from baseline per adj category

ros_bl <- ros %>%
  filter(DIST == "Baseline") %>% 
  dplyr::select(adjective, pixel_count) %>%
  rename(baseline_count = pixel_count)
  
ros_adjdiff <- ros %>% 
  #add baseline to each DIST 
  filter(!DIST == "Baseline") %>% 
  dplyr::select(DIST, adjective, pixel_count) %>% 
  left_join(ros_bl, by = join_by(adjective)) %>% 
  #calc diff per category. negative is decrease post DIST
  mutate(pixel_diff = pixel_count - baseline_count) %>% 
  #relabel adjectives
  mutate(adj_fx = factor(adjective, 
                         levels=c(0, 1, 2, 3, 4, 5, 6, 7),
                         labels=c("NA", "NB", "VL", "L", "M", "H", "VH", "X")))

# ggplot() + 
#   geom_col(data = ros_adjdiff,
#            mapping = aes(x = adj_fx, y = pixel_diff, 
#                          fill = adj_fx)) + 
#            #position = position_dodge()) + 
#   geom_hline(yintercept = 0, color = "black") + 
#   facet_wrap(~DIST, ncol=4, nrow=3) + 
#   theme_bw() + 
#   scale_fill_viridis("Adjective", discrete = TRUE, option = "inferno") + 
#   labs(title = "Change in ROS fuel adjectives compared to baseline",
#        x = "ROS adjective",
#        y = "Difference in pixels (DIST - baseline)")

# # WEIRD. Different numbers of value=0 pixels. Should be No data. Should be same??
# ros_adjdiff %>% 
#   filter(adj_fx == "NA") %>% 
#   dplyr::select(DIST, pixel_diff) %>% 
#   arrange(desc(pixel_diff))
# # It's okay. NV/ID area in a LF zone that doesn't touch CA, plus minor edge effects.

p_adjfx_ros <- ggplot() + 
  geom_col(data = ros_adjdiff %>% 
             filter(!adj_fx == "NA"),
           mapping = aes(x = adj_fx, y = pixel_diff, 
                         fill = adj_fx)) + 
  #position = position_dodge()) + 
  geom_hline(yintercept = 0, color = "black") + 
  facet_wrap(~DIST, ncol=4, nrow=3) + 
  theme_bw() + 
  scale_fill_viridis("Adjective", discrete = TRUE, option = "magma", direction =-1) + 
  labs(title = "Change in ROS fuel adjectives compared to baseline",
       x = "ROS adjective",
       y = "Difference in pixels (DIST - baseline)")

p_adjfx_ros
ggsave(plot=p_adjfx_ros, 
       filename="plots/fueladj_ROS_adj_change.jpg",
       height=5, width=6, units=c("in"))


#FL
fl_bl <- fl %>%
  filter(DIST == "Baseline") %>% 
  dplyr::select(adjective, pixel_count) %>%
  rename(baseline_count = pixel_count)

fl_adjdiff <- fl %>% 
  #add baseline to each DIST 
  filter(!DIST == "Baseline") %>% 
  dplyr::select(DIST, adjective, pixel_count) %>% 
  left_join(fl_bl, by = join_by(adjective)) %>% 
  #calc diff per category. negative is decrease post DIST
  mutate(pixel_diff = pixel_count - baseline_count) %>% 
  #relabel adjectives
  mutate(adj_fx = factor(adjective, 
                         levels=c(0, 1, 2, 3, 4, 5, 6, 7),
                         labels=c("NA", "NB", "VL", "L", "M", "H", "VH", "X")))
p_adjfx_fl <- ggplot() + 
  geom_col(data = fl_adjdiff %>% 
             filter(!adj_fx == "NA"),
           mapping = aes(x = adj_fx, y = pixel_diff, 
                         fill = adj_fx)) + 
  #position = position_dodge()) + 
  geom_hline(yintercept = 0, color = "black") + 
  facet_wrap(~DIST, ncol=4, nrow=3) + 
  theme_bw() + 
  scale_fill_viridis("Adjective", discrete = TRUE, option = "magma", direction =-1) + 
  labs(title = "Change in FL fuel adjectives compared to baseline",
       x = "FL adjective",
       y = "Difference in pixels (DIST - baseline)")

p_adjfx_fl
ggsave(plot=p_adjfx_fl, 
       filename="plots/fueladj_FL_adj_change.jpg",
       height=5, width=6, units=c("in"))



#2b. per adjective category, pixel count by base and DISTS

# on second thought, not really needed. Could be done later, if wanted. 
