# load packages -----------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)

# import data -------------------------------------------------------------
load("processed_data/tabular/gp_mgmt_sf.RData")# grid points with Kate's Schema
load("processed_data/spatial/MPG_bound.RData")# ranch boundary

# data vis -------------------------------------------------------
## maps ----
# visualize 1) points that change categorical habitat types
ggplot() +
  geom_sf(data = MPG_bound, fill = "grey90", color = "black") +
  geom_sf(data = gp_mgmt_sf %>% filter(!is.na(bird_MS_pt)),
          aes(fill = hab_2010),
          shape = 21,
          size = 2,
          color = "black")+
  scale_fill_brewer(palette = "Set3")+
  labs(fill= "2010 Habitats") +
  theme_minimal()

ggplot() +
  geom_sf(data = MPG_bound, fill = "grey90", color = "black") +
  geom_sf(data = gp_mgmt_sf %>% filter(!is.na(bird_MS_pt)),
          aes(fill = hab_2025),
          shape = 21,
          size = 2,
          color = "black")+
  scale_fill_brewer(palette = "Set3")+
  labs(fill= "2025 Habitats") +
  theme_minimal()

# 2) managment intesity gradient
ggplot() +
  geom_sf(data = MPG_bound, fill = "grey90", color = "black") +
  geom_sf(data = gp_mgmt_sf, 
          aes(fill = if_else(action_count == 0, NA_real_, action_count)), 
          shape = 21, 
          size = 2, 
          color = "black") +
  scale_fill_viridis_c(option = "cividis", na.value = NA) +
  labs(fill = "Management Intensity") +
  theme_minimal()

## bare ground ----
ggplot(no_veg, aes(x=survey_sequence, y=mean_nv, group = hab_2025, color = hab_2025))+
  geom_line(size=1) +
  geom_point(size=2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = hab_2025),
              alpha = 0.2, color = NA) +
  labs(
    x= "year",
    y= "mean non-veg cover (%)",
    color = "habitat class",
    fill = "habitat class")+
  theme_minimal()+
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80"),  # Darker grid lines
    panel.grid.minor = element_line(color = "grey90")
  )
ggsave("output/figures/noveg1.png", width = 8, height = 6, dpi = 300)
