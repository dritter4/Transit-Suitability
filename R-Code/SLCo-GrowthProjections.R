###############################################################################
## Script: SLCo-GrowthProjections.R
##
## Purpose: Incorporate growth projections into Salt Lake County 
## transit suitability analysis from CMP 6455 project
##
## Author: Daniel Ritter
## Date: 2023/05/15
##
## Notes: Script uses outputs from SaltLakeCounty-CMP6455.R script 
## which are included in the project folder.
###############################################################################

# ==================================================
# 1. Set up environment
# ==================================================

# install.packages("sf)
# install.packages("raster")
# install.packages("tmap")
# install.packages("mapsf")
# install.packages("leaflet")
# install.packages("tigris")
# install.packages("tidyverse")

require(sf)
require(raster)
require(tmap)
require(mapsf)
require(leaflet)
require(tigris)
require(tidyverse)


# ==================================================
# 2. Wrangle growth projection data
# ==================================================

# Load growth projection shapefiles
popgrowth_temp = st_read("PopulationProjTAZ/PopulationTAZProjections.shp")
jobgrowth_temp = st_read("JobProjTAZ/AllJobsTAZProjections.shp")

# View data
popgrowth_temp %>% glimpse()
jobgrowth_temp %>% glimpse()

# Filter for Salt Lake County and reproject to EPSG 3566
popgrowth_temp = popgrowth_temp %>% 
  filter(CO_NAME == "SALT LAKE") %>% 
  st_transform(3566)
jobgrowth_temp = jobgrowth_temp %>% 
  filter(CO_NAME == "SALT LAKE") %>% 
  st_transform(3566)

# Select necessary columns and rename to identify growth type
popgrowth = popgrowth_temp %>% 
  mutate(pop_YEAR2019 = YEAR2019, 
         pop_YEAR2050 = YEAR2050, 
         pop_YEAR2019D = round(YEAR2019D, digits = 2), 
         pop_YEAR2050D = round(YEAR2050D, digits = 2), 
         pop_diff19to50 = round(YEAR2050 - YEAR2019, digits = 2), 
         pop_diff19to50d = round(YEAR2050D - YEAR2019D, digits = 2)) %>% 
  select(CO_TAZID, CityArea, pop_YEAR2019, pop_YEAR2050, pop_YEAR2019D, pop_YEAR2050D, pop_diff19to50, pop_diff19to50d)
jobgrowth = jobgrowth_temp %>% 
  mutate(job_YEAR2019 = YEAR2019, 
         job_YEAR2050 = YEAR2050, 
         job_YEAR2019D = round(YEAR2019D, digits = 2), 
         job_YEAR2050D = round(YEAR2050D, digits = 2), 
         job_diff19to50 = round(YEAR2050 - YEAR2019, digits = 2), 
         job_diff19to50d = round(YEAR2050D - YEAR2019D, digits = 2)) %>% 
  select(CO_TAZID, CityArea, job_YEAR2019, job_YEAR2050, job_YEAR2019D, job_YEAR2050D, job_diff19to50, job_diff19to50d)

# Join objects
# growth = st_join(popgrowth, jobgrowth, by = CO_TAZID)

# Check join
# growth %>% glimpse()


# ==================================================
# 3. Visualize projections
# ==================================================

## Create interactive map --------------------------

# Define diverging palette
div1 = c("#5aae61", "#a6dba0", "#e7d4e8", "#c2a5cf", "#9970ab", "#762a83")
div2 = c("#5aae61", "#a6dba0", "#d9f0d3", "#e7d4e8", "#9970ab", "#762a83")

# Define variables for popups
# vars = c("Area" = "CityArea", "Change in population" = "pop_diff19to50", "Change in jobs" = "job_diff19to50", "Change in population density" = "pop_diff19to50d", "Change in job density" = "job_diff19to50d")

# Create map
growthsummary = 
  tm_shape(popgrowth, name = "Change in population (2019 to 2050)") + 
  tm_polygons("pop_diff19to50", style = "fixed", breaks = c(-650, -175, -50, 0, 300, 1500, 20000), palette = div2, midpoint = NA, title = "Pop Change", alpha = 0.75, border.alpha = 0.25, popup.vars = c("City area" = "CityArea", "2019 population" = "pop_YEAR2019", "2050 population" = "pop_YEAR2050", "Change in population" = "pop_diff19to50")) +  
  tm_shape(jobgrowth, name = "Change in jobs (2019 to 2050)") + 
  tm_polygons("job_diff19to50", style = "fixed", breaks = c(-1500, -25, 0, 50, 275, 1500, 15000), palette = div1, midpoint = NA, title = "Job Change", alpha = 0.75, border.alpha = 0.25, popup.vars = c("City area" = "CityArea", "2019 jobs" = "job_YEAR2019", "2050 jobs" = "job_YEAR2050", "Change in jobs" = "job_diff19to50")) +  
  tm_shape(popgrowth, name = "Change in population density (2019 to 2050)") + 
  tm_polygons("pop_diff19to50d", style = "fixed", breaks = c(-4.0, -1.0, -0.3, 0.0, 2.0, 20.0, 140.0), palette = div2, midpoint = NA, title = "Pop Density Change", alpha = 0.75, border.alpha = 0.25, popup.vars = c("City area" = "CityArea", "2019 population density" = "pop_YEAR2019D", "2050 population density" = "pop_YEAR2050D", "Change in population density" = "pop_diff19to50d")) +  
  tm_shape(jobgrowth, name = "Change in job density (2019 to 2050)") + 
  tm_polygons("job_diff19to50d", style = "fixed", breaks = c(-50.0, -0.5, 0.0, 0.5, 2.0, 10.0, 125.0), palette = div1, midpoint = NA, title = "Job Density Change", alpha = 0.75, border.alpha = 0.25, popup.vars = c("City area" = "CityArea", "2019 job density" = "job_YEAR2019D", "2050 population density" = "job_YEAR2050D", "Change in job density" = "job_diff19to50d"))

growthsummary %>% tmap_leaflet() %>% 
  hideGroup(c("Change in jobs (2019 to 2050)", "Change in population density (2019 to 2050)", "Change in job density (2019 to 2050)"))


## Create static maps ------------------------------

c1 = tm_shape(popgrowth) + tm_polygons("pop_YEAR2019D", style = "fixed", breaks = c(0, 0, 1.26, 6.28, 10.01, 20.33, 150), palette = "Blues", legend.show = FALSE, border.alpha = 0) + tm_layout(main.title = "2019 Population Density", main.title.size = 1)
c2 = tm_shape(popgrowth) + tm_polygons("pop_YEAR2050D", style = "fixed", breaks = c(0, 0, 1.26, 6.28, 10.01, 20.33, 150), palette = "Blues", legend.show = FALSE, border.alpha = 0) + tm_layout(main.title = "2050 Population Density", main.title.size = 1)
c3 = tm_shape(jobgrowth) + tm_polygons("job_YEAR2019D", style = "fixed", breaks = c(0, 0.01, 0.71, 2.35, 8.34, 40.02, 480), palette = "Blues", legend.show = FALSE, border.alpha = 0) + tm_layout(main.title = "2019 Job Density", main.title.size = 1)
c4 = tm_shape(jobgrowth) + tm_polygons("job_YEAR2050D", style = "fixed", breaks = c(0, 0.01, 0.71, 2.35, 8.34, 40.02, 480), palette = "Blues", legend.show = FALSE, border.alpha = 0) + tm_layout(main.title = "2050 Job Density", main.title.size = 1)

tmap_arrange(c1, c2, c3, c4, nrow = 2)


## Create static maps for GIF ----------------------
for(i in 2026:2050){
  file = paste0("/Users/danie/OneDrive/Documents/Projects/TransitSuitability/SaltLakeCounty/SLCo-GrowthProjections/Maps/PopChange/count", i, ".png")
  pop = (tm_shape(popgrowth_temp) + 
           tm_polygons((paste0("YEAR", (i))), style = "fixed", breaks = c(0, 50, 500, 1000, 1500, 3000, 20000), palette = "Blues", legend.show = FALSE, border.alpha = 0) + 
           tm_layout(main.title = "Population (2019-2050)", main.title.size = 1))
  job = (tm_shape(jobgrowth_temp) + 
           tm_polygons((paste0("YEAR", (i))), style = "fixed", breaks = c(0, 10, 100, 500, 1000, 3000, 20000), palette = "Blues", legend.show = FALSE, border.alpha = 0) + 
           tm_layout(main.title = "Jobs (2019-2050)", main.title.size = 1)) 
  image = tmap_arrange(pop, job, nrow = 1)
  tmap_save(tm = image, filename = file, width = 1600, height = 900)
}


# ==================================================
# 4. Rasterize and reclassify
# ==================================================

# Get Salt Lake County boundary
us_counties = counties(cb=TRUE, year=2021)
SLCo_3566 = us_counties %>% filter(STATEFP == "49" & COUNTYFP == "035") %>% st_transform(3566)

# Create a raster template for Salt Lake County
raster_template = raster(extent(SLCo_3566), resolution = 300, crs = 3566)

# Check empty raster
qtm(raster_template)

# Create rasters for change and projected population and job density
projpopdensity_r = rasterize(x = popgrowth, y = raster_template, field = "pop_YEAR2050D")
projjobdensity_r = rasterize(x = jobgrowth, y = raster_template, field = "job_YEAR2050D")
popdensitychange_r = rasterize(x = popgrowth, y = raster_template, field = "pop_diff19to50d")
jobdensitychange_r = rasterize(x = jobgrowth, y = raster_template, field = "job_diff19to50d")

# Plot rasters
r1 = tm_shape(projpopdensity_r) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "2050 Population Density", main.title.size = 1)
r2 = tm_shape(projjobdensity_r) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "2050 Job Density", main.title.size = 1)
r3 = tm_shape(popdensitychange_r) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "Change in Population Density (2019-2050)", main.title.size = 1)
r4 = tm_shape(jobdensitychange_r) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "Change in Job Density (2019-2050)", main.title.size = 1)

tmap_arrange(r1, r2)
tmap_arrange(r3, r4)

# Check distribution of raster data
projpopdensity_r %>% hist()
projjobdensity_r %>% hist()
popdensitychange_r %>% hist()
jobdensitychange_r %>% hist()


## Reclassify projected population density -------------------

# Reclassify by Fisher method
projpop_f = mf_get_breaks(x = projpopdensity_r, nbreaks = 7, breaks = "fisher")
hist(projpop_f)
abline(v = projpop_f, col = "red")

# Create reclassification table
m = c(
  projpop_f[1], projpop_f[2], 1, 
  projpop_f[2], projpop_f[3], 2, 
  projpop_f[3], projpop_f[4], 3, 
  projpop_f[4], projpop_f[5], 4,
  projpop_f[5], projpop_f[6], 5,
  projpop_f[6], projpop_f[7], 6,
  projpop_f[7], projpop_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
projpopdensity_r2 = reclassify(projpopdensity_r, rcl = reclass_table, right = TRUE)

# Check reclassification
projpopdensity_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
projpopdensity_r %>% plot(main = "Original", axes = FALSE)
projpopdensity_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Reclassify projected job density -------------------

# Reclassify by Fisher method
projjob_f = mf_get_breaks(x = projjobdensity_r, nbreaks = 7, breaks = "fisher")
hist(projjob_f)
abline(v = projjob_f, col = "red")

# Create reclassification table
m = c(
  projjob_f[1], projjob_f[2], 1, 
  projjob_f[2], projjob_f[3], 2, 
  projjob_f[3], projjob_f[4], 3, 
  projjob_f[4], projjob_f[5], 4,
  projjob_f[5], projjob_f[6], 5,
  projjob_f[6], projjob_f[7], 6,
  projjob_f[7], projjob_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
projjobdensity_r2 = reclassify(projjobdensity_r, rcl = reclass_table, right = TRUE)

# Check reclassification
projjobdensity_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
projjobdensity_r %>% plot(main = "Original", axes = FALSE)
projjobdensity_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Reclassify change in population density -------------------

# Reclassify by Fisher method
popchange_f = mf_get_breaks(x = popdensitychange_r, nbreaks = 7, breaks = "fisher")
hist(popchange_f)
abline(v = popchange_f, col = "red")

# Create reclassification table
m = c(
  popchange_f[1], popchange_f[2], 1, 
  popchange_f[2], popchange_f[3], 2, 
  popchange_f[3], popchange_f[4], 3, 
  popchange_f[4], popchange_f[5], 4,
  popchange_f[5], popchange_f[6], 5,
  popchange_f[6], popchange_f[7], 6,
  popchange_f[7], popchange_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
popdensitychange_r2 = reclassify(popdensitychange_r, rcl = reclass_table, right = TRUE)

# Check reclassification
popdensitychange_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
popdensitychange_r %>% plot(main = "Original", axes = FALSE)
popdensitychange_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Reclassify projected job density -------------------

# Reclassify by Fisher method
jobchange_f = mf_get_breaks(x = jobdensitychange_r, nbreaks = 7, breaks = "fisher")
hist(jobchange_f)
abline(v = jobchange_f, col = "red")

# Create reclassification table
# Note: Replaced jobchange_f[1] with -50 because this table doesn't reclassify 
#  jobchange_f[1] (-47.77) values as 1 for some reason
m = c(
  -50, jobchange_f[2], 1, 
  jobchange_f[2], jobchange_f[3], 2, 
  jobchange_f[3], jobchange_f[4], 3, 
  jobchange_f[4], jobchange_f[5], 4,
  jobchange_f[5], jobchange_f[6], 5,
  jobchange_f[6], jobchange_f[7], 6,
  jobchange_f[7], jobchange_f[8], 7)

# Create matrix from vector
reclass_table = matrix(m, ncol = 3, byrow = TRUE)

# Reclassify raster
jobdensitychange_r2 = reclassify(jobdensitychange_r, rcl = reclass_table, right = TRUE)

# Check reclassification
jobdensitychange_r2 %>% plot()

# Compare the original and reclassified rasters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
jobdensitychange_r %>% plot(main = "Original", axes = FALSE)
jobdensitychange_r2 %>% plot(main = "Reclassified", axes = FALSE)
dev.off()


## Rescale and plot rasters -------------------

# Rescale data to 0-1
projpopdensity_rescale = (projpopdensity_r2 - minValue(projpopdensity_r2))/(maxValue(projpopdensity_r2) - minValue(projpopdensity_r2))
projjobdensity_rescale = (projjobdensity_r2 - minValue(projjobdensity_r2))/(maxValue(projjobdensity_r2) - minValue(projjobdensity_r2))
popdensitychange_rescale = (popdensitychange_r2 - minValue(popdensitychange_r2))/(maxValue(popdensitychange_r2) - minValue(popdensitychange_r2))
jobdensitychange_rescale = (jobdensitychange_r2 - minValue(jobdensitychange_r2))/(maxValue(jobdensitychange_r2) - minValue(jobdensitychange_r2))

# Check rescale
rs1 = tm_shape(projpopdensity_rescale) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "Projected Population Density", main.title.size = 1)
rs2 = tm_shape(projjobdensity_rescale) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "Projected Job Density", main.title.size = 1)
rs3 = tm_shape(popdensitychange_rescale) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "Change in Population Density (2019-2050)", main.title.size = 1)
rs4 = tm_shape(jobdensitychange_rescale) + tm_raster(style = "cont", palette = "Reds", legend.show = FALSE) + tm_layout(main.title = "Change in Job Density (2019-2050)", main.title.size = 1)

tmap_arrange(rs1, rs2)
tmap_arrange(rs3, rs4)

# Save rescaled rasters
saveRDS(projpopdensity_rescale, file = "RescaledRasters/projpopdensity_rescale.RData")
saveRDS(projjobdensity_rescale, file = "RescaledRasters/projjobdensity_rescale.RData")
saveRDS(popdensitychange_rescale, file = "RescaledRasters/popdensitychange_rescale.RData")
saveRDS(jobdensitychange_rescale, file = "RescaledRasters/jobdensitychange_rescale.RData")


# ==================================================
# 5. Calculate weighted overlays
# ==================================================

# Read saved rasters
popD = readRDS("RescaledRasters/popdensity_rescale.RData")
minority = readRDS("RescaledRasters/minority_rescale.RData")
senior = readRDS("RescaledRasters/senior_rescale.RData")
poverty = readRDS("RescaledRasters/poverty_rescale.RData")
vehicle = readRDS("RescaledRasters/vehicle_rescale.RData")
jobD = readRDS("RescaledRasters/jobdensity_rescale.RData")
transit = readRDS("RescaledRasters/transit_overlay.RData")
projpopD = readRDS("RescaledRasters/projpopdensity_rescale.RData")
projjobD = readRDS("RescaledRasters/projjobdensity_rescale.RData")
popDchange = readRDS("RescaledRasters/popdensitychange_rescale.RData")
jobDchange = readRDS("RescaledRasters/jobdensitychange_rescale.RData")


## Create projection overlay -----------------------

# Compute overlay
suitability1 = (projpopD * 0.2) + (projjobD * 0.2) + (popD * 0.15) + (jobD * 0.15) + 
  (minority * 0.075) + (senior * 0.075) + (poverty * 0.075) + (vehicle * 0.075)
suitability1_t = (projpopD * 0.2) + (projjobD * 0.2) + (popD * 0.15) + (jobD * 0.15) + 
  (minority * 0.075) + (senior * 0.075) + (poverty * 0.075) + (vehicle * 0.075) - 
  (transit * 0.50)

# Reclassify overlay
suit1_breaks = mf_get_breaks(x = suitability1, nbreaks = 20, breaks = "quantile")
suit1t_breaks = mf_get_breaks(x = suitability1_t, nbreaks = 20, breaks = "quantile")

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
reclass_table1 = matrix(m1, ncol = 3, byrow = TRUE)
reclass_table1_t = matrix(m1_t, ncol = 3, byrow = TRUE)

# Reclassify rasters
suitability1_reclass = reclassify(suitability1, rcl = reclass_table1, right = TRUE)
suitability1_t_reclass = reclassify(suitability1_t, rcl = reclass_table1_t, right = TRUE)

# Check reclassification
suitability1_reclass %>% plot()
suitability1_t_reclass %>% plot()


## Create change overlay -----------------------

# Compute overlay
suitability2 = (popDchange * 0.2) + (projpopD * 0.1) + (popD * 0.1) + 
  (jobDchange * 0.15) + (projjobD * 0.075) + (jobD * 0.075) + 
  (minority * 0.075) + (senior * 0.075) + (poverty * 0.075) + (vehicle * 0.075)
suitability2_t = (popDchange * 0.2) + (projpopD * 0.1) + (popD * 0.1) + 
  (jobDchange * 0.15) + (projjobD * 0.075) + (jobD * 0.075) + 
  (minority * 0.075) + (senior * 0.075) + (poverty * 0.075) + (vehicle * 0.075) - 
  (transit * 1.0)

# Reclassify overlay
suit2_breaks = mf_get_breaks(x = suitability2, nbreaks = 20, breaks = "quantile")
suit2t_breaks = mf_get_breaks(x = suitability2_t, nbreaks = 20, breaks = "quantile")

# Create reclassification table to highlight top 5% of values
m2 = c(
  -1, suit2_breaks[19], NA,
  suit2_breaks[19], suit2_breaks[20], 1,
  suit2_breaks[20], suit2_breaks[21], 2)
m2_t = c(
  -1, suit2t_breaks[19], NA, 
  suit2t_breaks[19], suit2t_breaks[20], 1,
  suit2t_breaks[20], suit2t_breaks[21], 2)

# Create matrix from vector
reclass_table2 = matrix(m2, ncol = 3, byrow = TRUE)
reclass_table2_t = matrix(m2_t, ncol = 3, byrow = TRUE)

# Reclassify rasters
suitability2_reclass = reclassify(suitability2, rcl = reclass_table2, right = TRUE)
suitability2_t_reclass = reclassify(suitability2_t, rcl = reclass_table2_t, right = TRUE)

# Check reclassification
suitability2_reclass %>% plot()
suitability2_t_reclass %>% plot()


# ==================================================
# 6. Create suitability map with system overlay
# ==================================================

## Get transit stop coordinates () --------------------

# Download transit route data from UTA:
# https://gis.utah.gov/data/transportation/transit/

# Read route shapefile
routes_temp = st_read("UTARoutes/UTA_Routes_and_Most_Recent_Ridership.shp")

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


## Map overlays ----------------------------------

# Create projection overlay map
suit1_full = tm_shape(suitability1_t_reclass, name = "Most suitable (accounting for transit)") + 
  tm_raster(style = "cat", labels = c("Suitable", "Most Suitable"), palette = c("#9ECAE1", "#2171B5"), title = "Suitability") +
  tm_shape(suitability1_reclass, name = "Most suitable (not accounting for transit)") + 
  tm_raster(style = "cat", labels = c("Suitable", "Most Suitable"), palette = c("#9ECAE1", "#2171B5"), title = "Suitability") + 
  tm_shape(suitability1_t, name = "Full suitability (accounting for transit)") + 
  tm_raster(style = "cont", midpoint = 0, title = "Values") +
  tm_shape(suitability1, name = "Full suitability (not accounting for transit)") + 
  tm_raster(style = "cont", midpoint = 0, title = "Values") + 
  tm_shape(bus_routes_SLCo, name = "Existing Bus Network") + 
  tm_lines(col = "black", lwd = 0.8) + 
  tm_shape(rail_routes_SLCo, name = "Existing Rail Network") + 
  tm_lines(col = "purple", lwd = 2)

suit1_full %>% tmap_leaflet() %>% hideGroup(c("Most suitable (not accounting for transit)", "Full suitability (accounting for transit)", "Full suitability (not accounting for transit)"))


# Create change overlay map
suit2_full = tm_shape(suitability2_t_reclass, name = "Most suitable (accounting for transit)") + 
  tm_raster(style = "cat", labels = c("Suitable", "Most Suitable"), palette = c("#9ECAE1", "#2171B5"), title = "Suitability") +
  tm_shape(suitability2_reclass, name = "Most suitable (not accounting for transit)") + 
  tm_raster(style = "cat", labels = c("Suitable", "Most Suitable"), palette = c("#9ECAE1", "#2171B5"), title = "Suitability") + 
  tm_shape(suitability2_t, name = "Full suitability (accounting for transit)") + 
  tm_raster(style = "cont", midpoint = 0, title = "Values") +
  tm_shape(suitability2, name = "Full suitability (not accounting for transit)") + 
  tm_raster(style = "cont", midpoint = 0, title = "Values") + 
  tm_shape(bus_routes_SLCo, name = "Existing Bus Network") + 
  tm_lines(col = "black", lwd = 0.8) + 
  tm_shape(rail_routes_SLCo, name = "Existing Rail Network") + 
  tm_lines(col = "purple", lwd = 2)

suit2_full %>% tmap_leaflet() %>% hideGroup(c("Most suitable (not accounting for transit)", "Full suitability (accounting for transit)", "Full suitability (not accounting for transit)"))
  
