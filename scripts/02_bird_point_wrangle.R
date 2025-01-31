# wrangle, clean, join, data for bird point count analysis
# produce few succinct .Rdata files to use from hereafter

# load packages ----------------------------------------------------------
library(dplyr)
library(sf)


# Wrangle objects from 01 script ---------------------------------------------
# 1 select habitat types from gp_meta
names(gp_meta)
grid_point_info<-gp_meta %>% 
  select(grid_point, lat, long, type1_biome, type2_vegetation_community, type3_vegetation_indicators, type4_indicators_history)

# 2 join restoration history shapefile to grid points
