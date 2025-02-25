# wrangle, clean, join, transform data for bird point count analysis
# produce few succinct .Rdata files to use from hereafter

# load packages ----------------------------------------------------------
library(dplyr)
library(sf)

# Wrangle objects loaded from 01 script ---------------------------------------------
# 1 select habitat types from gp_meta, get rid of stuff I don't want
grid_point_info<-gp_meta %>% 
  select(grid_point, lat, long, type1_biome, type2_vegetation_community, type3_vegetation_indicators, type4_indicators_history)

# 2 join restoration history spatial features to grid points
## grid points to spatial features ----
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

## veg data wrangle ----
# Define planted species list
planted_species <- c("AGRCRI","THIINT", "MEDSAT", "ZEAMAY", "TRIAES","TRIINC", "PSAJUN", "PHLPRA",
                     "ONOVIC","MEDFAL", "GLYMAX")

# Add new column based on native status and species name
veg_meta <- veg_meta %>%
  mutate(origin_status = case_when(
    plant_native_status == "native" ~ "native",  # Keep native species as NA or exclude from classification
    key_plant_code %in% planted_species ~ "planted",  # Mark known planted species
    TRUE ~ "weed"  # Default to unintentional for other nonnatives
  ))

str(foliar_all)
foliar_all<-foliar_all %>% 
  select(survey_ID, year, survey_sequence, grid_point, key_plant_code, intercepts_pct) %>% 
  left_join(veg_meta, by= "key_plant_code")

## ground cover wrangle ----


## grid point wrangle----
## add grid points of interest for bird manuscript and make super simple
gp_resto_simple <- gp_resto_simple %>%
  mutate(bird_MS_pt = if_else(grid_point %in% bird_gp$Name, "bird_MS_pt", NA_character_))

# add cultivation history to grid points re-code to schema provided by Kate Stone
gp_resto_1 <- gp_resto_simple %>%
  mutate(
    # Recode hab_2010 from type4_indicators_history
    hab_2010 = recode(
      type4_indicators_history,
      
      # mappings:
      "closed canopy conifer" = "upland conifer",
      "mixed canopy conifer" = "upland conifer",
      "open canopy conifer" = "upland conifer",
      "forage grass diversification" = "grassland planted forage",
      "forage grass diversification (natural)" = "grassland planted forage",
      "forage grass restoration" = "grassland planted forage",
      "irrigated agriculture restoration" = "agriculture",
      "uncultivated grassland native or degraded" = "grassland native",
      "mixed sage and bitterbrush" = "persistent shrubland",
      "sagebrush" = "persistent shrubland",
      "bitterbrush" = "persistent shrubland",
      "other shrub deciduous" = "deciduous shrubs",
      "wooded draw non-forest" = "shrubby draw",
      
      # By default, keep the original value
      .default = type3_vegetation_indicators
    ),
    
    # Recode hab_2025 from type4_indicators_history
    hab_2025 = recode(
      type4_indicators_history,
      
      # mappings:
      "closed canopy conifer" = "upland conifer",
      "mixed canopy conifer" = "upland conifer",
      "open canopy conifer" = "upland conifer",
      "forage grass diversification" = "restoration grassland",
      "forage grass diversification (natural)" = "restoration grassland",
      "forage grass restoration" = "restoration grassland",
      "irrigated agriculture restoration" = "restoration grassland irrigated",
      "uncultivated grassland native or degraded" = "grassland native",
      "mixed sage and bitterbrush" = "persistent shrubland",
      "sagebrush" = "persistent shrubland",
      "bitterbrush" = "persistent shrubland",
      "other shrub deciduous" = "deciduous shrubs",
      "wooded draw non-forest" = "shrubby draw",
      
      # By default, keep the original value
      .default = type4_indicators_history
    )
  ) %>% st_drop_geometry() # convert from sf object to dataframe-join to gp_meta later if needed

str(gp_resto_1)

# count management actions, summarize, make new column for earliest actions
gp_resto_2 <- gp_resto_1 %>%
  mutate(Year = as.numeric(Year)) %>% 
  group_by(grid_point) %>%
  arrange(Year, .by_group = TRUE) %>% 
  summarise(
    first_management_year = if (any(!is.na(Year))) first(Year[!is.na(Year)]) else NA_real_,
    first_management_action = if (any(!is.na(Year))) first(layer[!is.na(Year)]) else NA_character_,
    action_count=sum(!is.na(layer)),
    mgmt_summary=paste(unique(na.omit(layer)),collapse = ",")
  ) %>%
  ungroup()

gp_mgmt_summary<-gp_resto_2 %>% 
  left_join(gp_resto_1 %>% distinct(grid_point, bird_MS_pt, hab_2010, hab_2025, type1_biome),
            by= "grid_point")

# add geometry to make grid point management summary a SF object we can map
gp_mgmt_sf<-gp_info_utm %>% 
  select(grid_point) %>% 
  left_join(gp_mgmt_summary, by = "grid_point")

# save as Rdata objects and output shapefiles ---------------------------------------------------
## bird grid point simplified habitat codes----
gp_resto_df<-gp_resto_simple %>% st_drop_geometry()
save(gp_resto_df, file ="processed_data/tabular/bird_points_hab1.RData")
write.csv(gp_resto_df, file="processed_data/tabular/bird_points_hab1.csv", row.names = FALSE)

shapefile_path1<-"processed_data/spatial/gp_resto_simple.shp"
st_write(gp_resto_simple, shapefile_path1, delete_layer = TRUE)

## veg data with new 'origin_status' planting codes ----
save(veg_meta, file ="processed_data/tabular/veg_meta_CCmod.RData")
write.csv(veg_meta, file="processed_data/tabular/veg_meta_CCmod.csv", row.names = FALSE)

save(foliar_all, file ="processed_data/tabular/foliar_all.RData")
save(foliar_top, file = "processed_data/tabular/foliar_top.RData")

## gp history and management type summary ----
save(gp_mgmt_sf, file ="processed_data/tabular/gp_mgmt_sf.RData" )
save(gp_mgmt_summary, file = "processed_data/tabular/gp_mgmt_summary.RData") #as DF
write.csv(gp_mgmt_summary, file = "processed_data/tabular/gp_mgmt_summary.csv", row.names = FALSE)
st_write(gp_mgmt_sf, "processed_data/spatial/gp_mgmt_sf.gpkg", delete_dsn = TRUE)

## spatial features
save(MPG_bound, file="processed_data/spatial/MPG_bound.RData") # ranch boundary SF object
