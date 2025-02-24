# analysis script for bird manuscript grid points, management history, and habitat categories


# load packages -----------------------------------------------------------
library(dplyr)

# load data ---------------------------------------------------------------
load("processed_data/tabular/bird_points_hab1.RData")# long table with mgmt history, some hab info, KS scheme info

load("processed_data/tabular/gp_mgmt_summary.RData")# wide table with one gp per row, summary hab and restor history

load("processed_data/tabular/gp_mgmt_sf.RData")# spatial feature grid points and summary hab + resto history

load("processed_data/spatial/MPG_bound.RData")# spatial feature MPG boundary

#indicator species analysis-do communities change over time-does management intensity explain it? significance test
# communitiy similarity analysis-NMDS?
load("processed_data/tabular/foliar_all.RData")

foliar_all %>% 
  left_join(gp_mgmt_sf, by= "grid_point") %>% 
  filter(survey_sequence == "2011-12") %>% 
  group_by(origin_status, hab_2010) %>% 
  summarise(origin_avg_cover=mean(intercepts_pct))
  

