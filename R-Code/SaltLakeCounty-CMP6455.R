####################################################
#' Salt Lake County Transit Analysis
#' 
#' Daniel Ritter, Corey Bishop, Muwaffaq Usman Adam
####################################################

# ==================================================
# 1. Setting up environment
# ==================================================

# install.packages("sf)
# install.packages("raster")
# install.packages("tmap")
# install.packages("mapsf")
# install.packages("leaflet")
# install.packages("tidycensus")
# install.packages("tigris")
# remotes::install_github("GIScience/openrouteservice-r")
# install.packages("tidyverse")

require(sf)
require(raster)
require(tmap)
require(mapsf)
require(leaflet)
require(tidycensus)
require(tigris)
require(openrouteservice)
require(tidyverse)

# Set Census API key
# census_api_key("INSERT-API-KEY", install = TRUE)


# ==================================================
# 2. Prepare data for suitability analysis
# ==================================================

# Set folder path
path = "path" # Set your file path here

# View variables
# v21 = load_variables(year = 2021, dataset = "acs5", cache = TRUE)
# s21 = load_variables(year = 2021, dataset = "acs5/subject", cache = TRUE)
# d21 = load_variables(year = 2021, dataset = "acs5/profile", cache = TRUE)

## Population Density and Minority Percentage ------

# Query total and white populations and clean output
vars = c(
  total_pop = "B02001_001", 
  white_pop = "B02001_002")
acs_pop = get_acs(
  year = 2021, survey = "acs5", 
  geography = "block group", 
  state = "UT", county = "Salt Lake", 
  variables = vars, 
  output = "wide", geometry = TRUE) %>% 
  select(-NAME, -total_popM, -white_popM) %>%
  rename(total_pop = total_popE, 
         white_pop = white_popE) %>%
  print(n = 10)

# Transform to EPSG 3566 and calculate population density
acs_pop_den = acs_pop %>% 
  st_as_sf(acs_pop) %>% 
  st_transform(3566) %>% 
  mutate(area = st_area(acs_pop), 
         pop_density = total_pop / area, .before = geometry) %>% 
  print(n = 10)

# Calculate non-white population percentage and replace NaN values with 0
acs_pop_mod = acs_pop_den %>% 
  mutate(perc_min = 1 - white_pop/total_pop, .before = area) %>%
  mutate_if(is.double, ~replace(., is.nan(.), 0)) %>% 
  print(n = 10)


## Senior Population Percentage --------------------

# Query senior population and clean output
vars = c(
  m65to66 = "B01001_020", 
  m67to69 = "B01001_021", 
  m70to74 = "B01001_022", 
  m75to79 = "B01001_023", 
  m80to84 = "B01001_024", 
  m85plus = "B01001_025", 
  f65to66 = "B01001_044", 
  f67to69 = "B01001_045", 
  f70to74 = "B01001_046", 
  f75to79 = "B01001_047", 
  f80to84 = "B01001_048", 
  f85plus = "B01001_049")
acs_sen = get_acs(
  year = 2021, survey = "acs5", 
  geography = "block group", 
  state = "UT", county = "Salt Lake", 
  variables = vars, 
  geometry = FALSE) %>% 
  select(-NAME, -moe) %>% 
  print(n = 10)

# Find total number of seniors per block group
acs_sen_tot = acs_sen %>% 
  group_by(GEOID) %>% 
  summarize(sen_pop = sum(estimate)) %>% 
  print(n = 10)

# Join total population and senior population data
acs_pop_join = left_join(acs_pop_mod, acs_sen_tot, by = "GEOID")

# Calculate senior population percentage and replace NaN values with 0
acs_pop_summary = acs_pop_join %>% 
  mutate(perc_sen = sen_pop/total_pop, .before = area) %>%
  mutate_if(is.double, ~replace(., is.nan(.), 0)) %>% 
  print(n = 10)


## Poverty Percentage ------------------------------

# Query population below poverty level and clean output
vars = c(
  tot_pop = "S1701_C01_001", 
  below_pov = "S1701_C02_001", 
  perc_pov = "S1701_C03_001")
acs_pov = get_acs(
  year = 2021, survey = "acs5", 
  geography = "tract", 
  state = "UT", county = "Salt Lake", 
  variables = vars, 
  output = "wide", geometry = TRUE) %>% 
  rename(total_pop = tot_popE, below_pov = below_povE) %>% 
  mutate(perc_pov = perc_povE / 100) %>% # Clean % of pop below poverty level
  mutate_if(is.double, ~replace(., is.na(.), 0)) %>% # Replace NA values with 0
  select(-NAME, -tot_popM, -below_povM, -perc_povM, -perc_povE) %>%
  print(n = 10)


## Vehicle Percentage ------------------------------

# Query households without vehicle and clean output
vars = c(
  tot_hh = "DP04_0057", 
  no_veh = "DP04_0058")
acs_veh = get_acs(
  year = 2021, survey = "acs5", 
  geography = "tract", 
  state = "UT", county = "Salt Lake", 
  variables = vars, 
  output = "wide", geometry = TRUE) %>% 
  rename(tot_hh = tot_hhE, no_veh = no_vehE) %>% 
  mutate(perc_no_veh = no_veh / tot_hh) %>% # Calculate % of no-vehicle households
  mutate_if(is.double, ~replace(., is.nan(.), 0)) %>% # Replace NaN values with 0
  select(-NAME, -tot_hhM, -no_vehM) %>% 
  print(n = 10)


## Employment --------------------------------------

# Download employment data from LEHD:
# Parameters: Work Area Profile, 2019 All Jobs
# https://onthemap.ces.census.gov/

# # Read employment data .shp
# jobs_temp = st_read(file.path(path, "LEHD/points_2019.shp"))
# 
# # Select GEOID and total number of jobs
# # Drop geometry since LEHD reports counts by block centroid
# jobs = jobs_temp %>% 
#   select(id, c000) %>% 
#   rename(GEOID = id, tot_jobs = c000) %>% 
#   st_drop_geometry()
# 
# # Query Census block boundaries
# blocks_temp = blocks("UT", "Salt Lake", year = 2019)
# 
# # Select GEOID and geometry
# blocks = blocks_temp %>%
#   select(GEOID10, geometry) %>%
#   rename(GEOID = GEOID10)
# 
# # Join employment data with block geometry
# jobs_geom = left_join(blocks, jobs, by = "GEOID")
# 
# # Replace NA values with 0
# lehd_jobs = jobs_geom %>% 
#   mutate(tot_jobs = replace_na(tot_jobs, 0))
# 
# # Calculate job density
# lehd_jobs = lehd_jobs %>% 
#   mutate(area = st_area(lehd_jobs), 
#          job_density = tot_jobs / area, .before = geometry)
# 
# # Save as RDS
# setwd("wd") # Set your file path here
# saveRDS(lehd_jobs, file = "jobs_block.RData")

# Read spatial employment data as spatial object
setwd("wd") # Set your file path here
lehd_jobs = st_as_sf(readRDS("jobs_block.RData"))


# ==================================================
# 3. Complete accessibility analysis
# ==================================================

## Get transit stop coordinates --------------------

# Download transit stop data from UTA:
# https://gis.utah.gov/data/transportation/transit/

# # Read stop .shp
# stop_temp = st_read(file.path(path, "UTA/UTA_Stops_and_Most_Recent_Ridership.shp"))
# 
# # Filter for Salt Lake County
# stop_slco = stop_temp %>% 
#   filter(County == "Salt Lake") %>% 
#   print(n = 10)
# 
# # Separate by bus and rail
# bus_stops_temp = stop_slco %>% 
#   filter(Mode == "Bus")
# rail_stops_temp = stop_slco %>% 
#   filter(Mode == "Rail")
# 
# # Plot stops to check accuracy
# bus_stops_temp %>% st_geometry() %>% plot()
# rail_stops_temp %>% st_geometry() %>% plot()
# 
# # Drop geometry and unnecessary columns
# bus_stops_temp = bus_stops_temp %>% 
#   st_drop_geometry() %>% 
#   select(-FID, -ZipCode, -AVGBoard, -AVGAlight, -Route, -Mode)
# rail_stops_temp = rail_stops_temp %>% 
#   st_drop_geometry() %>% 
#   select(-FID, -ZipCode, -AVGBoard, -AVGAlight, -Route, -Mode)
# 
# # Remove duplicate stops
# bus_stops = bus_stops_temp[!duplicated(bus_stops_temp$StopName), ]
# rail_stops = rail_stops_temp[!duplicated(rail_stops_temp$StopName), ]
# 
# # Set stop coordinates
# bus_coords = data_frame(X = bus_stops$Longitude, Y = bus_stops$Latitude)
# rail_coords = data_frame(X = rail_stops$Longitude, Y = rail_stops$Latitude)
# 
# # Check number of stops
# bus_coords %>% nrow()
# rail_coords %>% nrow()
# 
# # Save stop coordinates as RDS
# setwd("wd") # Set your file path here
# saveRDS(bus_coords, file = "bus_coords.RData")
# saveRDS(rail_coords, file = "rail_coords.RData")
# 
# # Read saved coordinates
# setwd("wd") # Set your file path here
# bus_coords = readRDS("bus_coords.RData")
# rail_coords = readRDS("rail_coords.RData")

## Query transit stop walksheds ----------------

# NOTE: Need to run bus stop loop on multiple days and combine daily dataframes 
#       because of OpenRouteService's 500 call daily limit.

# # Set working directory
# setwd("wd") # Set your file path here
# 
# # Create dataframe for loop output
# bus_shed1 = data_frame()
# bus_shed2 = data_frame()
# bus_shed3 = data_frame()
# bus_shed4 = data_frame()
# bus_shed5 = data_frame()
# bus_shed6 = data_frame()
# bus_shed7 = data_frame()
# rail_shed = data_frame()
# 
# # Loop through rail stops and save as RDS
# for(i in 1:61){
#   cat(paste("Calculating isochrone for rail stops:", i,"\n"))
#   res = ors_isochrones(
#     rail_coords[i, ],
#     range = 15*60,
#     interval = 5*60,
#     profile = "foot-walking",
#     output = "sf")
#   Sys.sleep(4)
#   rail_shed = rbind(rail_shed, res)
#   }
# saveRDS(rail_shed, file = "rail_shed.RData")
# 
# Loop through bus stops (1-450) and save as RDS
# for(i in 1:450){
#   cat(paste("Calculating isochrone for bus stops:", i,"\n"))
#   res = ors_isochrones(
#     bus_coords[i, ], 
#     range = 15*60, 
#     interval = 5*60, 
#     profile = "foot-walking",
#     output = "sf")
#   Sys.sleep(4)
#   bus_shed1 = rbind(bus_shed1, res)
#   }
# saveRDS(bus_shed1, file = "bus_shed1_partial.RData")
# 
# Loop through bus stops (451-900) and save as RDS
# for(i in 451:900){
#   cat(paste("Calculating isochrone for bus stops:", i,"\n"))
#   res = ors_isochrones(
#     bus_coords[i, ], 
#     range = 15*60, 
#     interval = 5*60, 
#     profile = "foot-walking",
#     output = "sf")
#   Sys.sleep(4)
#   bus_shed2 = rbind(bus_shed2, res)
#   }
# saveRDS(bus_shed2, file = "bus_shed2.RData")
# 
# Loop through bus stops (901-1350) and save as RDS
# for(i in 901:1350){
#   cat(paste("Calculating isochrone for bus stops:", i,"\n"))
#   res = ors_isochrones(
#     bus_coords[i, ], 
#     range = 15*60, 
#     interval = 5*60, 
#     profile = "foot-walking",
#     output = "sf")
#   Sys.sleep(4)
#   bus_shed3 = rbind(bus_shed3, res)
#   }
# saveRDS(bus_shed3, file = "bus_shed3.RData")
# 
# Loop through bus stops (1350-1800) and save as RDS
# for(i in 1351:1800){
#   cat(paste("Calculating isochrone for bus stops:", i,"\n"))
#   res = ors_isochrones(
#     bus_coords[i, ], 
#     range = 15*60, 
#     interval = 5*60, 
#     profile = "foot-walking",
#     output = "sf")
#   Sys.sleep(4)
#   bus_shed4 = rbind(bus_shed4, res)
#   }
# saveRDS(bus_shed4, file = "bus_shed4.RData")
# 
# Loop through bus stops (1801-2250) and save as RDS
# for(i in 1801:2250){
#   cat(paste("Calculating isochrone for bus stops:", i,"\n"))
#   res = ors_isochrones(
#     bus_coords[i, ], 
#     range = 15*60, 
#     interval = 5*60, 
#     profile = "foot-walking",
#     output = "sf")
#   Sys.sleep(4)
#   bus_shed5 = rbind(bus_shed5, res)
#   }
# saveRDS(bus_shed5, file = "bus_shed5.RData")
# 
# Loop through bus stops (2251-2700) and save as RDS
# for(i in 2251:2700){
#   cat(paste("Calculating isochrone for bus stops:", i,"\n"))
#   res = ors_isochrones(
#     bus_coords[i, ], 
#     range = 15*60, 
#     interval = 5*60, 
#     profile = "foot-walking",
#     output = "sf")
#   Sys.sleep(4)
#   bus_shed6 = rbind(bus_shed6, res)
#   }
# saveRDS(bus_shed6, file = "bus_shed6.RData")
# 
# Loop through bus stops (2701-3037) and save as RDS
# for(i in 2701:3037){
#   cat(paste("Calculating isochrone for bus stops:", i,"\n"))
#   res = ors_isochrones(
#     bus_coords[i, ], 
#     range = 15*60, 
#     interval = 5*60, 
#     profile = "foot-walking",
#     output = "sf")
#   Sys.sleep(4)
#   bus_shed7 = rbind(bus_shed7, res)
#   }
# saveRDS(bus_shed7, file = "bus_shed7.RData")


## Process walksheds -------------------------------

# # Read saved sheds as spatial objects
# setwd("wd") # Set your file path here
# rail_shed = st_as_sf(readRDS("rail_shed.RData"))
# bus_shed1 = st_as_sf(readRDS("bus_shed1.RData"))
# bus_shed2 = st_as_sf(readRDS("bus_shed2.RData"))
# bus_shed3 = st_as_sf(readRDS("bus_shed3.RData"))
# bus_shed4 = st_as_sf(readRDS("bus_shed4.RData"))
# bus_shed5 = st_as_sf(readRDS("bus_shed5.RData"))
# bus_shed6 = st_as_sf(readRDS("bus_shed6.RData"))
# bus_shed7 = st_as_sf(readRDS("bus_shed7.RData"))
# 
# # Combine individual data and save as RDS
# bus_shed = do.call("rbind", list(bus_shed1, bus_shed2, bus_shed3, bus_shed4, bus_shed5, bus_shed6, bus_shed7))
# saveRDS(bus_shed, file = "bus_shed_total.RData")

# Read saved sheds
setwd("wd") # Set your file path here
rail_shed = readRDS("rail_shed.RData")
bus_shed = readRDS("bus_shed_total.RData")

# Transform transit stop walksheds
rail_shed = rail_shed %>% st_transform(3566)
bus_shed = bus_shed %>% st_transform(3566)

# Plot transit stop walksheds
rail_shed %>% st_geometry() %>% plot()
bus_shed %>% st_geometry() %>% plot()

# Turn off spherical geometry to address error message for st_union
sf_use_s2(FALSE)

# Dissolve rail sheds (by distance)
rail_300 = rail_shed %>% filter(value == "300") %>% st_union()
rail_600 = rail_shed %>% filter(value == "600") %>% st_union()
rail_900 = rail_shed %>% filter(value == "900") %>% st_union()

# View rail sheds
rail_300 %>% plot()
rail_600 %>% plot()
rail_900 %>% plot()

# Convert from sfc to sf and add dummy field for raster creation
rail_300 = rail_300 %>% st_sf() %>% mutate(shed = 1)
rail_600 = rail_600 %>% st_sf() %>% mutate(shed = 1)
rail_900 = rail_900 %>% st_sf() %>% mutate(shed = 1)

# Filter bus sheds by distance
bus_300 = bus_shed %>% filter(value == "300") %>% st_union()
bus_600 = bus_shed %>% filter(value == "600") %>% st_union()
bus_900 = bus_shed %>% filter(value == "900") %>% st_union()

# View bus sheds
bus_300 %>% plot()
bus_600 %>% plot()
bus_900 %>% plot()

# Convert from sfc to sf and add dummy field for raster creation
bus_300 = bus_300 %>% st_sf() %>% mutate(shed = 1)
bus_600 = bus_600 %>% st_sf() %>% mutate(shed = 1)
bus_900 = bus_900 %>% st_sf() %>% mutate(shed = 1)

# Turn on spherical geometry
sf_use_s2(TRUE)


# ==================================================
# 4. Complete suitability analysis (rasterize)
# ==================================================

# # Check CRS
# acs_pop_summary %>% st_crs()
# acs_pov %>% st_crs()
# acs_veh %>% st_crs()
# lehd_jobs %>% st_crs()

# Transform CRS
acs_pov = acs_pov %>% st_transform(3566)
acs_veh = acs_veh %>% st_transform(3566)
lehd_jobs = lehd_jobs %>% st_transform(3566)

# Get Salt Lake County boundary
us_counties = counties(cb=TRUE, year=2021)
SLCo_3566 = us_counties %>% filter(STATEFP == "49" & COUNTYFP == "035") %>% st_transform(3566)


## Create demographic rasters ----------------------------------

# Create a raster template for Salt Lake County
raster_template = raster(extent(SLCo_3566), resolution = 300, crs = 3566)

# Check empty raster
qtm(raster_template)

# Create rasters for demographic characteristics and job density
popdensity_r = rasterize(x = acs_pop_summary, y = raster_template, field = "pop_density")
minority_r = rasterize(x = acs_pop_summary, y = raster_template, field = "perc_min")
senior_r = rasterize(x = acs_pop_summary, y = raster_template, field = "perc_sen")
poverty_r = rasterize(x = acs_pov, y = raster_template, field = "perc_pov")
vehicle_r = rasterize(x = acs_veh, y = raster_template, field = "perc_no_veh")
jobdensity_r = rasterize(x = lehd_jobs, y = raster_template, field = "job_density")

# Plot rasters
r1 = tm_shape(popdensity_r) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "Population Density", main.title.size = 1)
r2 = tm_shape(minority_r) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "% Non-White", main.title.size = 1)
r3 = tm_shape(senior_r) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "% Seniors", main.title.size = 1)
r4 = tm_shape(poverty_r) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "% Below Poverty Line", main.title.size = 1)
r5 = tm_shape(vehicle_r) +  tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "% Without Vehicle", main.title.size = 1)
r6 = tm_shape(jobdensity_r) +  tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "Job Density", main.title.size = 1)

tmap_arrange(r1, r2, r3, r4, r5, r6, nrow = 2)


## Create transit rasters ----------------------------------

# Create rasters for transit accessibility
rail_300_r = rasterize(x = rail_300, y = raster_template, field = "shed")
rail_600_r = rasterize(x = rail_600, y = raster_template, field = "shed")
rail_900_r = rasterize(x = rail_900, y = raster_template, field = "shed")
bus_300_r = rasterize(x = bus_300, y = raster_template, field = "shed")
bus_600_r = rasterize(x = bus_600, y = raster_template, field = "shed")
bus_900_r = rasterize(x = bus_900, y = raster_template, field = "shed")

# Replace NA values with 0
rail_300_r[is.na(rail_300_r[])] <-- 0
rail_600_r[is.na(rail_600_r[])] <-- 0
rail_900_r[is.na(rail_900_r[])] <-- 0
bus_300_r[is.na(bus_300_r[])] <-- 0
bus_600_r[is.na(bus_600_r[])] <-- 0
bus_900_r[is.na(bus_900_r[])] <-- 0

# Check rasters
rail_300_r %>% plot()
rail_600_r %>% plot()
rail_900_r %>% plot()
bus_300_r %>% plot()
bus_600_r %>% plot()
bus_900_r %>% plot()


# ==================================================
# 5. Complete suitability analysis (reclassify)
# ==================================================

# Check distribution of raster data
popdensity_r %>% hist()
minority_r %>% hist()
senior_r %>% hist()
poverty_r %>% hist()
vehicle_r %>% hist()
jobdensity_r %>% hist()


## Reclassify population density -------------------

# Reclassify by Fisher method
popden_f = mf_get_breaks(x = popdensity_r, nbreaks = 7, breaks = "fisher")
hist(popden_f)
abline(v = popden_f, col = "red")

# Create reclassification table
m = c(
  popden_f[1], popden_f[2], 1, 
  popden_f[2], popden_f[3], 2, 
  popden_f[3], popden_f[4], 3, 
  popden_f[4], popden_f[5], 4,
  popden_f[5], popden_f[6], 5,
  popden_f[6], popden_f[7], 6,
  popden_f[7], popden_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
popdensity_r2 = reclassify(popdensity_r, rcl = reclass_table, right = TRUE)

# Check reclassification
popdensity_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
popdensity_r %>% plot(main = "Original", axes = FALSE)
popdensity_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Reclassify percent minority ---------------------

# Reclassify by Fisher method
minority_f = mf_get_breaks(x = minority_r, nbreaks = 7, breaks = "fisher")
hist(minority_f)
abline(v = minority_f, col = "red")

# Create reclassification table
m = c(
  minority_f[1], minority_f[2], 1, 
  minority_f[2], minority_f[3], 2, 
  minority_f[3], minority_f[4], 3, 
  minority_f[4], minority_f[5], 4,
  minority_f[5], minority_f[6], 5,
  minority_f[6], minority_f[7], 6,
  minority_f[7], minority_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
minority_r2 = reclassify(minority_r, rcl = reclass_table, right = TRUE)

# Check reclassification
minority_r2 %>% plot()

# Compare the original and reclassified tracts
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
minority_r %>% plot(main = "Original", axes = FALSE)
minority_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Reclassify percent senior -----------------------

# Reclassify by Fisher method
senior_f = mf_get_breaks(x = senior_r, nbreaks = 7, breaks = "fisher")
hist(senior_f)
abline(v = senior_f, col = "red")

# Create reclassification table
m = c(
  senior_f[1], senior_f[2], 1, 
  senior_f[2], senior_f[3], 2, 
  senior_f[3], senior_f[4], 3, 
  senior_f[4], senior_f[5], 4,
  senior_f[5], senior_f[6], 5,
  senior_f[6], senior_f[7], 6,
  senior_f[7], senior_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
senior_r2 = reclassify(senior_r, rcl = reclass_table, right = TRUE)

# Check reclassification
senior_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
senior_r %>% plot(main = "Original", axes = FALSE)
senior_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Reclassify percent poverty ----------------------

# Reclassify by Fisher method
poverty_f = mf_get_breaks(x = poverty_r, nbreaks = 7, breaks = "fisher")
hist(poverty_f)
abline(v = poverty_f, col = "red")

# Create reclassification table
m = c(
  poverty_f[1], poverty_f[2], 1, 
  poverty_f[2], poverty_f[3], 2, 
  poverty_f[3], poverty_f[4], 3, 
  poverty_f[4], poverty_f[5], 4,
  poverty_f[5], poverty_f[6], 5,
  poverty_f[6], poverty_f[7], 6,
  poverty_f[7], poverty_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
poverty_r2 = reclassify(poverty_r, rcl = reclass_table, right = TRUE)

# Check reclassification
poverty_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
poverty_r %>% plot(main = "Original", axes = FALSE)
poverty_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Reclassify no vehicle households ----------------

# Reclassify by Fisher method
vehicle_f = mf_get_breaks(x = vehicle_r, nbreaks = 7, breaks = "fisher")
hist(vehicle_f)
abline(v = vehicle_f, col = "red")

# Create reclassification table
m = c(
  vehicle_f[1], vehicle_f[2], 1, 
  vehicle_f[2], vehicle_f[3], 2, 
  vehicle_f[3], vehicle_f[4], 3, 
  vehicle_f[4], vehicle_f[5], 4,
  vehicle_f[5], vehicle_f[6], 5,
  vehicle_f[6], vehicle_f[7], 6,
  vehicle_f[7], vehicle_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
vehicle_r2 = reclassify(vehicle_r, rcl = reclass_table, right = TRUE)

# Check reclassification
vehicle_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
vehicle_r %>% plot(main = "Original", axes = FALSE)
vehicle_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Reclassify job density --------------------------

# Reclassify by Fisher method
job_f = mf_get_breaks(x = jobdensity_r, nbreaks = 7, breaks = "fisher")
hist(job_f)
abline(v = job_f, col = "red")

# Create reclassification table
m = c(
  job_f[1], job_f[2], 1, 
  job_f[2], job_f[3], 2, 
  job_f[3], job_f[4], 3, 
  job_f[4], job_f[5], 4,
  job_f[5], job_f[6], 5,
  job_f[6], job_f[7], 6,
  job_f[7], job_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
jobdensity_r2 = reclassify(jobdensity_r, rcl = reclass_table, right = TRUE)

# Check reclassification
jobdensity_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
jobdensity_r %>% plot(main = "Original", axes = FALSE)
jobdensity_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Rescale rasters ---------------------------------

# Rescale data to 0-1
popdensity_rescale = (popdensity_r2 - minValue(popdensity_r2))/(maxValue(popdensity_r2) - minValue(popdensity_r2))
minority_rescale = (minority_r2 - minValue(minority_r2))/(maxValue(minority_r2) - minValue(minority_r2))
senior_rescale = (senior_r2 - minValue(senior_r2))/(maxValue(senior_r2) - minValue(senior_r2))
poverty_rescale = (poverty_r2 - minValue(poverty_r2))/(maxValue(poverty_r2) - minValue(poverty_r2))
vehicle_rescale = (vehicle_r2 - minValue(vehicle_r2))/(maxValue(vehicle_r2) - minValue(vehicle_r2))
jobdensity_rescale = (jobdensity_r2 - minValue(jobdensity_r2))/(maxValue(jobdensity_r2) - minValue(jobdensity_r2))

# Check rescale
rs1 = tm_shape(popdensity_rescale) + tm_raster(style = "cont", alpha = 0.75, palette = "Reds", title = "Value") + tm_layout(main.title = "Population Density", main.title.size = 1)
rs2 = tm_shape(minority_rescale) + tm_raster(style = "cont", alpha = 0.75, palette = "Reds", title = "Value") + tm_layout(main.title = "% Non-White", main.title.size = 1)
rs3 = tm_shape(senior_rescale) + tm_raster(style = "cont", alpha = 0.75, palette = "Reds", title = "Value") + tm_layout(main.title = "% Seniors", main.title.size = 1)
rs4 = tm_shape(poverty_rescale) + tm_raster(style = "cont", alpha = 0.75, palette = "Reds", title = "Value") + tm_layout(main.title = "% Below Poverty Line", main.title.size = 1)
rs5 = tm_shape(vehicle_rescale) +  tm_raster(style = "cont", alpha = 0.75, palette = "Reds", title = "Value") + tm_layout(main.title = "% Without Vehicle", main.title.size = 1)
rs6 = tm_shape(jobdensity_rescale) +  tm_raster(style = "cont", alpha = 0.75, palette = "Reds", title = "Value") + tm_layout(main.title = "Job Density", main.title.size = 1)

tmap_arrange(rs1, rs2, rs3, rs4, rs5, rs6, nrow = 2)

# Create interactive map
rescale_layered = tm_shape(popdensity_rescale, name = "Population Density") + tm_raster(style = "cont", alpha = 0.75, palette = "Reds", legend.show = FALSE) + 
  tm_shape(minority_rescale, name = "Percent Minority") + tm_raster(style = "cont", alpha = 0.75, palette = "Reds", legend.show = FALSE) + 
  tm_shape(senior_rescale, name = "Percent Senior") + tm_raster(style = "cont", alpha = 0.75, palette = "Reds", legend.show = FALSE) + 
  tm_shape(poverty_rescale, name = "Percent Below Poverty Line") + tm_raster(style = "cont", alpha = 0.75, palette = "Reds", legend.show = FALSE) + 
  tm_shape(vehicle_rescale, name = "Percent Without Vehicle") +  tm_raster(style = "cont", alpha = 0.75, palette = "Reds", legend.show = FALSE) + 
  tm_shape(jobdensity_rescale, name = "Job Density") +  tm_raster(style = "cont", alpha = 0.75, palette = "Reds", legend.show = FALSE)

rescale_layered %>% tmap_leaflet() %>% hideGroup(c("Percent Minority", "Percent Senior", "Percent Below Poverty Line", "Percent Without Vehicle", "Job Density"))

# Save rescaled rasters
setwd("wd") # Set your file path here
saveRDS(popdensity_rescale, file = "popdensity_rescale.RData")
saveRDS(minority_rescale, file = "minority_rescale.RData")
saveRDS(senior_rescale, file = "senior_rescale.RData")
saveRDS(poverty_rescale, file = "poverty_rescale.RData")
saveRDS(vehicle_rescale, file = "vehicle_rescale.RData")
saveRDS(jobdensity_rescale, file = "jobdensity_rescale.RData")

# Read saved rasters
setwd("wd") # Set your file path here
popdensity_rescale = readRDS("popdensity_rescale.RData")
minority_rescale = readRDS("minority_rescale.RData")
senior_rescale = readRDS("senior_rescale.RData")
poverty_rescale = readRDS("poverty_rescale.RData")
vehicle_rescale = readRDS("vehicle_rescale.RData")
jobdensity_rescale = readRDS("jobdensity_rescale.RData")


# ==================================================
# 6. Complete suitability analysis (overlay)
# ==================================================

# Compute transit shed overlay
transit_overlay = (bus_300_r * 0.25) + (bus_600_r * 0.15) + (bus_900_r * 0.10) + 
                  (rail_300_r * 0.25) + (rail_600_r * 0.15) + (rail_900_r * 0.10)

# Map transit shed overlay
tm_shape(transit_overlay) + tm_raster(style = "cont", palette = "Blues", title = "Weight") + 
  tm_layout(title = "Transit Accessibility Overlay", title.position = c('center', 'top'))


## Scenario 1 ----------------------------------------

# Compute weighted overlay (ridership based on MetroNext study)
suitability_ridership = (popdensity_rescale * 0.3) + (minority_rescale * 0.1) + (senior_rescale * 0.1) + 
                        (poverty_rescale * 0.1) + (vehicle_rescale * 0.1) + (jobdensity_rescale * 0.3)
suitability_ridership_t = (popdensity_rescale * 0.3) + (minority_rescale * 0.1) + (senior_rescale * 0.1) + 
                          (poverty_rescale * 0.1) + (vehicle_rescale * 0.1) + (jobdensity_rescale * 0.3) - 
                          (transit_overlay * 0.50)

# Reclassify ridership overlay
suit1_breaks = mf_get_breaks(x = suitability_ridership, nbreaks = 20, breaks = "quantile")
suit1t_breaks = mf_get_breaks(x = suitability_ridership_t, nbreaks = 20, breaks = "quantile")

# Create reclassification table to highlight top 5% of values
m1 = c(
  -1, suit1_breaks[19], NA,
  suit1_breaks[19], suit1_breaks[20], 1,
  suit1_breaks[20], suit1_breaks[21], 2)
m1_t = c(
  -1, suit1t_breaks[19], NA, 
  suit1t_breaks[19], suit1t_breaks[20], 1,
  suit1t_breaks[20], suit1t_breaks[21], 2)

# Create matrix from vector
reclass_table = matrix(m1, ncol = 3, byrow = TRUE)
reclass_table_t = matrix(m1_t, ncol = 3, byrow = TRUE)

# Reclassify rasters
suitability_ridership_reclass = reclassify(suitability_ridership, rcl = reclass_table, right = TRUE)
suitability_ridership_t_reclass = reclassify(suitability_ridership_t, rcl = reclass_table_t, right = TRUE)

# Check reclassification
suitability_ridership_reclass %>% plot()
suitability_ridership_t_reclass %>% plot()


## Scenario 2 ----------------------------------------

# Compute weighted overlay (jobs based on Andy's feedback)
suitability_jobs = (popdensity_rescale * 0.2) + (minority_rescale * 0.05) + (senior_rescale * 0.05) + 
  (poverty_rescale * 0.05) + (vehicle_rescale * 0.05) + (jobdensity_rescale * 0.6)
suitability_jobs_t = (popdensity_rescale * 0.2) + (minority_rescale * 0.05) + (senior_rescale * 0.05) + 
  (poverty_rescale * 0.05) + (vehicle_rescale * 0.05) + (jobdensity_rescale * 0.6) - 
  (transit_overlay * 0.50)

# Reclassify ridership overlay
suit2_breaks = mf_get_breaks(x = suitability_jobs, nbreaks = 20, breaks = "quantile")
suit2t_breaks = mf_get_breaks(x = suitability_jobs_t, nbreaks = 20, breaks = "quantile")

# Create reclassification table to highlight top 5% of values
m2 = c(
  -1, suit2_breaks[19], NA, 
  suit2_breaks[19], suit2_breaks[20], 1,
  suit2_breaks[20], suit2_breaks[21], 2)
m2_t = c(
  -1, suit2t_breaks[19], NA, 
  suit2t_breaks[19], suit2t_breaks [20], 1,
  suit2t_breaks[20], suit2t_breaks[21], 2)

# Create matrix from vector
reclass_table = matrix(m2, ncol = 3, byrow = TRUE)
reclass_table_t = matrix(m2_t, ncol = 3, byrow = TRUE)

# Reclassify rasters
suitability_jobs_reclass = reclassify(suitability_jobs, rcl = reclass_table, right = TRUE)
suitability_jobs_t_reclass = reclassify(suitability_jobs_t, rcl = reclass_table_t, right = TRUE)

# Check reclassification
suitability_jobs_reclass %>% plot()
suitability_jobs_t_reclass %>% plot()


# ==================================================
# 7. Create interactive map with system overlay
# ==================================================

## Get transit stop coordinates () --------------------

# Download transit route data from UTA:
# https://gis.utah.gov/data/transportation/transit/

# Read route .shp
path = "wd" # Set your file path here
routes_temp = st_read(file.path(path, "UTA/UTA_Routes_and_Most_Recent_Ridership.shp"))

# Separate by bus and rail
bus_routes_temp = routes_temp %>%
  filter(RouteType %in% c("Local", "Fast Bus", "Commuter", "Express", "Shuttle", "Ski", "BRT", "Flex"))
rail_routes_temp = routes_temp %>%
  filter(RouteType %in% c("Trolley", "Light Rail", "Streetcar", "Heavy Rail"))

# Drop unnecessary columns and transform to EPSG 3566
bus_routes = bus_routes_temp %>% 
  select(geometry) %>% 
  st_transform(3566)
rail_routes = rail_routes_temp %>% 
  select(geometry) %>% 
  st_transform(3566)
SLCo_clip = SLCo_3566 %>% 
  select(geometry)

# Plot full routes
bus_routes %>% plot()
rail_routes %>% plot()

# Clip using Salt Lake County boundary
bus_routes_SLCo = st_intersection(bus_routes, SLCo_clip)
rail_routes_SLCo = st_intersection(rail_routes, SLCo_clip)

# Plot clipped routes
bus_routes_SLCo %>% plot()
rail_routes_SLCo %>% plot()


## Map Scenario 1 ----------------------------------

# Create interactive map of full overlay
scen1_full = tm_shape(suitability_ridership_t, name = "Suitability (accounting for transit)") + 
    tm_raster(style = "cont", midpoint = 0, title = "Values") +
  tm_shape(suitability_ridership, name = "Suitability (not accounting for transit)") + 
    tm_raster(style = "cont", midpoint = 0, title = "Values") + 
  tm_shape(bus_routes_SLCo, name = "Existing Bus Network") + 
    tm_lines(col = "black", lwd = 0.8) + 
  tm_shape(rail_routes_SLCo, name = "Existing Rail Network") + 
    tm_lines(col = "purple", lwd = 2)

scen1_full %>% tmap_leaflet() %>% hideGroup("Suitability (not accounting for transit)")

# Create interactive map of most suitable areas
scen1_top = tm_shape(suitability_ridership_t_reclass, name = "Suitability (accounting for transit)") + 
    tm_raster(style = "cat", labels = c("Suitable", "Most Suitable"), palette = c("#9ECAE1", "#2171B5"), title = "Suitability") +
  tm_shape(suitability_ridership_reclass, name = "Suitability (not accounting for transit)") + 
    tm_raster(style = "cat", labels = c("Suitable", "Most Suitable"), palette = c("#9ECAE1", "#2171B5"), title = "Suitability") +
  tm_shape(bus_routes_SLCo, name = "Existing Bus Network") + 
    tm_lines(col = "black", lwd = 0.8) + 
  tm_shape(rail_routes_SLCo, name = "Existing Rail Network") + 
    tm_lines(col = "#D6001C", lwd = 2)

scen1_top %>% tmap_leaflet() %>% hideGroup("Suitability (not accounting for transit)")


## Map Scenario 2 ----------------------------------

# Create interactive map of full overlay
scen2_full = tm_shape(suitability_jobs_t, name = "Suitability (accounting for transit)") + 
    tm_raster(style = "cont", midpoint = 0, title = "Values") +
  tm_shape(suitability_jobs, name = "Suitability (not accounting for transit)") + 
    tm_raster(style = "cont", midpoint = 0, title = "Values") + 
  tm_shape(bus_routes_SLCo, name = "Existing Bus Network") + 
    tm_lines(col = "black", lwd = 0.8) + 
  tm_shape(rail_routes_SLCo, name = "Existing Rail Network") + 
    tm_lines(col = "purple", lwd = 2)

scen2_full %>% tmap_leaflet() %>% hideGroup("Suitability (not accounting for transit)")

# Create interactive map of most suitable areas
scen2_top = tm_shape(suitability_jobs_t_reclass, name = "Suitability (accounting for transit)") + 
  tm_raster(style = "cat", labels = c("Suitable", "Most Suitable"), palette = c("#9ECAE1", "#2171B5"), title = "Suitability") +
  tm_shape(suitability_jobs_reclass, name = "Suitability (not accounting for transit)") + 
  tm_raster(style = "cat", labels = c("Suitable", "Most Suitable"), palette = c("#9ECAE1", "#2171B5"), title = "Suitability") +
  tm_shape(bus_routes_SLCo, name = "Existing Bus Network") + 
  tm_lines(col = "black", lwd = 0.8) + 
  tm_shape(rail_routes_SLCo, name = "Existing Rail Network") + 
  tm_lines(col = "#D6001C", lwd = 2)

scen2_top %>% tmap_leaflet() %>% hideGroup("Suitability (not accounting for transit)")


## Outputs -----------------------------------------

# https://dritter4.github.io/CMP-6455/transit_analysis/rescaled-variables.html
# https://dritter4.github.io/CMP-6455/transit_analysis/ridership-suitability-top.html
# https://dritter4.github.io/CMP-6455/transit_analysis/ridership-suitability-full.html
# https://dritter4.github.io/CMP-6455/transit_analysis/jobs-suitability-top.html
# https://dritter4.github.io/CMP-6455/transit_analysis/jobs-suitability-top.html

