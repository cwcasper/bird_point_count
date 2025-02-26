# analysis script for bird manuscript grid points, management history, and habitat categories
# load packages -----------------------------------------------------------
library(dplyr)
library(tidyr)

# load data ---------------------------------------------------------------
load("processed_data/tabular/bird_points_hab1.RData")# long table with mgmt history, some hab info, KS scheme info
load("processed_data/tabular/gp_mgmt_summary.RData")# wide table with one gp per row, summary hab and restor history
load("processed_data/tabular/gp_mgmt_sf.RData")# spatial feature grid points and summary hab + resto history
load("processed_data/spatial/MPG_bound.RData")# spatial feature MPG boundary
load("processed_data/tabular/foliar_all.RData") # long table, any hit veg
load("processed_data/tabular/foliar_top.RData") #long table, top hit veg

# exploratory data analysis ----
## how does veg data line up with habitat categories
## planted vs. native vs. weeds by habitat types ----
foliar_sum_2010<-foliar_all %>% 
  left_join(gp_mgmt_summary, by= "grid_point") %>% 
  filter(survey_sequence == "2011-12") %>% 
  group_by(grid_point, hab_2010, origin_status) %>% 
  summarise(sum_cover=sum(intercepts_pct)) %>%
  ungroup() %>% 
  group_by(hab_2010, origin_status) %>% 
  summarise(avg_cover= mean(sum_cover)) %>% 
  pivot_wider(names_from = origin_status, values_from = avg_cover, values_fill = list(avg_cover = NA))

foliar_sum_2025<-foliar_all %>% 
  left_join(gp_mgmt_summary, by= "grid_point") %>% 
  filter(survey_sequence == "2024") %>% 
  group_by(grid_point, hab_2025, origin_status) %>% 
  summarise(sum_cover=sum(intercepts_pct)) %>%
  ungroup() %>% 
  group_by(hab_2025, origin_status) %>% 
  summarise(avg_cover= mean(sum_cover)) %>% 
  pivot_wider(names_from = origin_status, values_from = avg_cover, values_fill = list(avg_cover = NA))

## bare ground by habitat types and time ----
no_veg<-foliar_top %>% 
  left_join(gp_mgmt_summary, by = "grid_point") %>% 
  filter(key_plant_code=="NV", type1_biome == "rangeland") %>%
  group_by(hab_2025, survey_sequence) %>% 
  summarise(mean_nv=mean(top_intercepts_pct, na.rm = TRUE),
            n       = n(),
            sd_nv   = sd(top_intercepts_pct, na.rm = TRUE),
            se_nv   = sd_nv / sqrt(n),
            # 95% CI using a t-distribution
            ci_lower = mean_nv - qt(0.975, df = n - 1) * se_nv,
            ci_upper = mean_nv + qt(0.975, df = n - 1) * se_nv,
            .groups = "drop")

#indicator species analysis-do communities change over time-does management intensity explain it? significance test
# communitiy similarity analysis-NMDS?