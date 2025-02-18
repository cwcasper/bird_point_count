# get data for bird point count analysis
# 1) grid point list from Big Query
# 2) restoration history spatial data from egnyte

# load packages -----------------------------------------------------------
library(bigrquery)
library(dplyr)
library(sf)
library(here)
library(kableExtra)

# functions to direct to BQ warehouse and list of dataset-------------------------------------------------
## Pull from Big Query
bq_auth(cache=".bq_oauth")
billing <- "mpg-data-warehouse"
bq_pull <- function(select="*", from) {
  sql <- paste("SELECT", select, "FROM", from)
  return(
    bq_project_query(billing, sql) %>%
      bq_table_download() %>%
      as.data.frame()
  )
}

## MPG BQ Datasets easier pull
### Function to load metadata and pull table data
pull_table_by_name <- function(table_name, metadata_file = "raw_data/tabular/MPG_BQ_datasets.csv", select = "*") {
  # Read the metadata CSV
  metadata <- read.csv(metadata_file, stringsAsFactors = FALSE)
  
  # Look for the table entry
  table_info <- metadata[metadata$table_name == table_name, ]
  
  if (nrow(table_info) == 0) {
    stop("Table name not found in metadata!")
  }
  
  # Get the full BigQuery path
  full_table_path <- table_info$bq_warehouse_path
  
  # Pull the data using bq_pull
  message("Pulling data for table: ", table_name, " from ", full_table_path)
  data <- bq_pull(select = select, from = full_table_path)
  return(data)
}

metadata<-read.csv("raw_data/tabular/MPG_BQ_datasets.csv", stringsAsFactors = FALSE )

# View and pull tables available in MPG Data Warehouse (annotated) from Chuck's list -----
metadata %>% # tables listed in chuck's selection from warehouse
  select(table_name, data_description) %>% 
  kable()

gp_meta <- pull_table_by_name("grid_points") # grid point numbers and info
veg_meta<-pull_table_by_name("veg_meta") # plant list and info

# grid_point location, sample year, other info
gp_meta <- bq_pull(
  select= "*",
  from = "`mpg-data-warehouse.grid_point_summaries.location_position_classification`" # grid points
)

## Pull spatial data from egnyte
get_mpg_data <- function(cloud_dir, shapefile_name) {
  os_base <- '/Users/ccasper/Library/CloudStorage/Egnyte-mpgcloud/'
  file.path(os_base, 'Shared/Workspace/MPG Ranch/GIS Maps', shapefile_name)
}

# Load MPG boundary shapefile
mpg_boundary_path <- get_mpg_data('mpgcloud', 'shapefiles/MPG_boundary_Dec2018.shp')
MPG_bound <- st_read(mpg_boundary_path)
st_crs(MPG_bound)

# Transform CRS
MPG_bound <- st_transform(MPG_bound, 26911)
st_crs(MPG_bound)

# Load all_resto shapefile
all_resto_path <- get_mpg_data('mpgcloud', 'Restoration/MPG_Geo/MPG_Restoration/All_Restoration.shp')
all_resto <- st_read(all_resto_path)
st_crs(all_resto)

# Transform all_resto if needed
all_resto <- st_transform(all_resto, 26911)
st_crs(all_resto)

#load C.Casper's habitat types
hab_type_path<-get_mpg_data('mpgcloud', 'projects/veg_class_archive/nested_class/base_CC_habitat_types.shp')
hab_types<-st_read(hab_type_path)
st_crs(hab_types)
hab_types<-st_transform(hab_types, 26911)

## pull from local working directory
## points sent by Kate that have associated bird data used for bird manuscript January 2025
bird_gp<-read.csv("raw_data/tabular/bird_grid_points.csv")
