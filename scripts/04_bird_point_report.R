# load packages -----------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(sf)

# import data -------------------------------------------------------------
load("processed_data/tabular/gp_mgmt_sf.RData")# grid points with Kate's Schema
load("processed_data/spatial/MPG_bound.RData")# ranch boundary

# data vis -------------------------------------------------------
# visualize 1) points that change categorical habitat types,
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
