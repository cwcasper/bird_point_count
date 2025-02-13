# wrangle, clean, join, transform data for bird point count analysis
# produce few succinct .Rdata files to use from hereafter

# load packages ----------------------------------------------------------
library(dplyr)
library(sf)

# Wrangle objects loaded from 01 script ---------------------------------------------
# 1 select habitat types from gp_meta, get rid of stuff I don't want
names(gp_meta)
grid_point_info<-gp_meta %>% 
  select(grid_point, lat, long, type1_biome, type2_vegetation_community, type3_vegetation_indicators, type4_indicators_history)

# 2 join restoration history spatial features to grid points
## gp_point_info to spatial features ----
wgs84_epsg<-4326
zone11_epsg<- 26911

gp_info_utm<- st_as_sf(grid_point_info,
                           coords = c ('long','lat'),
                           crs= wgs84_epsg)

gp_info_utm<-st_transform(gp_info_utm,
                               crs = zone11_epsg)

# Perform spatial join: Keep all grid points, allow one-to-many matches with resto Hx polys
gp_resto <- st_join(gp_info_utm, all_resto, join = st_intersects, left = TRUE)

# 3 clean and simplify grid_point_info
dput(names(gp_resto))
gp_resto_simple<-gp_resto %>% 
  select(c("grid_point", "Year", "Objective", "type1_biome", "type2_vegetation_community", 
             "type3_vegetation_indicators", "type4_indicators_history", "OBJECTID",
             "Action", "Method", "Type","layer"))

## add grid points of interest for bird manuscript and make super simple----
gp_resto_simple <- gp_resto_simple %>%
  mutate(bird_MS_pt = if_else(grid_point %in% bird_gp$Name, "bird_MS_pt", NA_character_))

# attach grid point veg data to confirm habitat type codes


# save as Rdata objects and output shapefiles ---------------------------------------------------
gp_resto_df<-gp_resto_simple %>% st_drop_geometry()
save(gp_resto_df, file ="processed_data/tabular/bird_point_hab1.RData")
write.csv(gp_resto_df, file="processed_data/tabular/bird_points_hab1.csv", row.names = FALSE)

shapefile_path<-"processed_data/spatial/gp_resto_simple.shp"
st_write(gp_resto_simple, shapefile_path, delete_layer = TRUE)
