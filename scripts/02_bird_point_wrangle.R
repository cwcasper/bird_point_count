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

## add grid points of interest for bird manuscript and make super simple----
gp_resto_simple <- gp_resto_simple %>%
  mutate(bird_MS_pt = if_else(grid_point %in% bird_gp$Name, "bird_MS_pt", NA_character_))

# attach grid point veg data to confirm habitat type codes
## recode Veg Meta for planted non-natives which indicate cultivation history ----
# Define planted species list
planted_species <- c("AGRCRI","THIINT", "MEDSAT", "ZEAMAY", "TRIAES","TRIINC", "PSAJUN", "PHLPRA",
                     "ONOVIC","MEDFAL", "GLYMAX")

# Add new column based on native status and species name
veg_meta <- veg_meta %>%
  mutate(origin_status = case_when(
    plant_native_status == "native" ~ NA_character_,  # Keep native species as NA or exclude from classification
    key_plant_code %in% planted_species ~ "planted",  # Mark known planted species
    TRUE ~ "unintentional"  # Default to unintentional for other nonnatives
  ))

# add cultivation history to grid points re-code to schema provided by Kate Stone ----
gp_resto_1 <- gp_resto_simple %>%
  # 1. Create hab_2010: recode "former cultivated" to "grassland planted forage"
  mutate(
    hab_2010 = if_else(type3_vegetation_indicators == "former cultivated",
                       "grassland planted forage", 
                       type3_vegetation_indicators),
    # 2. Set hab_2025 from the original type4_indicators_history column
    hab_2025 = type4_indicators_history,
    # 3. Update hab_2010: if hab_2025 is "irrigated agriculture restoration", set hab_2010 to "agriculture"
    hab_2010 = if_else(hab_2025 == "irrigated agriculture restoration",
                       "agriculture", 
                       hab_2010),
    # 4. Recode hab_2025 values for restoration
    hab_2025 = recode(hab_2025,
                      "forage grass restoration" = "restoration grassland",
                      "irrigated agriculture restoration" = "restoration grassland irrigated"),
    # 5. Recode "uncultivated grassland native or degraded" to "grassland native" in hab_2010
    hab_2010 = recode(hab_2010,
                      "uncultivated grassland native or degraded" = "grassland native"),
    # 6. Also recode that value in hab_2025
    hab_2025 = recode(hab_2025,
                      "uncultivated grassland native or degraded" = "grassland native"),
    # 7. Ensure that if hab_2010 is "grassland planted forage", then hab_2025 becomes "restoration grassland"
    hab_2025 = if_else(hab_2010 == "grassland planted forage",
                       "restoration grassland", 
                       hab_2025)
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
  left_join(gp_resto_1 %>% distinct(grid_point, bird_MS_pt, hab_2010, hab_2025),
            by= "grid_point")

_# save as Rdata objects and output shapefiles ---------------------------------------------------
## bird grid point simplified habitat codes----
gp_resto_df<-gp_resto_simple %>% st_drop_geometry()
save(gp_resto_df, file ="processed_data/tabular/bird_points_hab1.RData")
write.csv(gp_resto_df, file="processed_data/tabular/bird_points_hab1.csv", row.names = FALSE)

shapefile_path<-"processed_data/spatial/gp_resto_simple.shp"
st_write(gp_resto_simple, shapefile_path, delete_layer = TRUE)

## veg metadata with new 'origin_status' planting codes ----
save(veg_meta, file ="processed_data/tabular/veg_meta_CCmod.RData")
write.csv(veg_meta, file="processed_data/tabular/veg_meta_CCmod.csv", row.names = FALSE)

## gp history and management type summary
save(gp_mgmt_summary, file = "processed_data/tabular/gp_mgmt_summary.RData")
write.csv(gp_mgmt_summary, file = "processed_data/tabular/gp_mgmt_summary.csv", row.names = FALSE)
