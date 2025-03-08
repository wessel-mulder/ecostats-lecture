rm(list = ls())
library(dplyr)
library(sf)
library(sp)
library(dismo)

setwd("/Users/fcd538/Library/CloudStorage/Box-Box/PhD/Teaching")

abs <- read.csv("golden_eagle_gbif_absence_2015_2025.csv", 
                header = T,
                sep = "\t")
pres <- read.csv("golden_eagle_gbif_presence_2015_2025.csv", 
                     header = T,
                     sep = "\t")

occ <- rbind(pres, abs)

eur <- st_read("/Users/fcd538/Library/CloudStorage/Box-Box/PhD/Teaching/Europe/Europe_merged.shp")
st_crs(eur)
#### filtering ####
# Remove exact duplicates (same coordinates and occurrence status)
occ <- occ %>% distinct(decimalLatitude, decimalLongitude, occurrenceStatus, .keep_all = TRUE)

# Remove records with missing coordinates
occ <- occ %>% filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))


# Remove erroneous coordinates
occ <- occ %>%
  filter(decimalLatitude != 0 & decimalLongitude != 0) %>%
  filter(decimalLatitude > -90 & decimalLatitude < 90) %>%
  filter(decimalLongitude > -180 & decimalLongitude < 180)

# Keep only records from 2015
occ <- occ[occ$year == 2015,]

table(occ$occurrenceStatus) # Unbalanced, need to make some pseudoabsences

pres_points <- st_as_sf(occ %>% filter(occurrenceStatus == "PRESENT"), 
                        coords = c("decimalLongitude", "decimalLatitude"), 
                        crs = 4326)

#st_write(pres_points, "pres_points.shp")
# Number of pseudo-absences to generate
num_pseudo_abs <- nrow(occ %>% filter(occurrenceStatus == "PRESENT"))

# Get bounding box of presence points
bbox <- st_bbox(pres_points)

# Define study area extent (adjust if necessary)
xmin <- bbox["xmin"]
xmax <- bbox["xmax"]
ymin <- bbox["ymin"]
ymax <- bbox["ymax"]

res_value <- 0.1  # 0.1° resolution (~11 km at the equator)

# Create an empty raster with the same extent as the Europe shapefile
r <- raster(ext = extent(eur), res = res_value, crs = st_crs(eur)$proj4string)

# Rasterize the Europe shapefile (set land areas to 1, others to NA)
r <- rasterize(as_Spatial(eur), r, field = 1, fun = "first")

# Generate random points within the study area
pseudo_abs_points <- randomPoints(ext = extent(xmin, xmax, ymin, ymax), 
                                  mask = r,
                                  n = num_pseudo_abs)

# Convert to a dataframe and create the occurrenceStatus column
pseudo_abs <- data.frame(decimalLongitude = pseudo_abs_points[, 1], 
                         decimalLatitude = pseudo_abs_points[, 2], 
                         occurrenceStatus = "ABSENT")


# Save pseudoabsences as a shapefile
#st_write(pseudo_abs_points, "pseudo_abs_points.shp")

pres_points <- occ %>% filter(occurrenceStatus == "PRESENT")
pres_points <- pres_points[,c("decimalLongitude", "decimalLatitude", "occurrenceStatus")]
pres_points$presence_absence <- 1

pseudo_abs$presence_absence <- 0
df <- rbind(pseudo_abs,pres_points)
df$presence_absence <- as.factor(df$presence_absence)

df <- st_as_sf(df,coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

st_write(df, "occurrence_data.shp")
