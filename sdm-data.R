# GETTING STARTED ---------------------------------------------------------
library('sdm')
library('terra')
library('dplyr')
library('sf')

# SESSION 1 --------------------------------------------------------------------
### EUROPE SHAPEFILE
europe <- vect('data/Europe/Europe_merged.shp')
europe_extent <- ext(europe)

### TEMPERATURE 
temp_2015 <- rast('data/current/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif')
temp_2015 <- crop(temp_2015,europe)
prec_2015 <- rast('data/current/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio12_1981-2010_V.2.1.tif')
prec_2015 <- crop(prec_2015,europe)

temp_2015_test <- aggregate(temp_2015,fact = 30)
prec_2015_test <- aggregate(prec_2015,fact = 30)

stack <- c(temp_2015_test,prec_2015_test)
names(stack) <- c('temperature_annual','precipitation_annual')
writeRaster(stack, "session1_stack.tif", overwrite = TRUE)

test <- rast('session1_stack.tif')

plot(test)

# SESSION 2 - ALL BIO VARIABLES  ---------------------------------------------------------------
environment <- list()

environment[['2015']] <- list(
  # landuse
  landuse_path = 'data/current/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc',
  year = 2015,
  #climate
  climate_path = 'data/current/chelsav2/GLOBAL/climatologies/1981-2010/bio/',
  
  #output file 
  stack_name = 'session2_2015_stack_all.tif'
)

environment[['2100_ssp1']] <- list(
  # landuse
  landuse_path = 'data/ssp126/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc',
  year = 2100,
  #climate
  climate_path = 'data/ssp126/chelsav2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp126/bio/',
  
  #output file 
  stack_name = 'session2_2100_ssp1_stack_all.tif'
)

environment[['2100_ssp5']] <- list(
  # landuse
  landuse_path = 'data/ssp585/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp534-2-1-f_gn_2015-2100.nc',
  year = 2100,
  #climate
  climate_path = 'data/ssp585/chelsav2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp585/bio/',
  
  #output file 
  stack_name = 'session2_2100_ssp5_stack_all.tif'
)

times <- c('2015','2100_ssp1','2100_ssp5')
for(i in times){
  print(i)
  data <- environment[[i]]

elev <- rast('data/ETOPO_2022_v1_60s_N90W180_bed.tif')
elev <- crop(elev,europe)
#plot(elev)

# landuse 
landuse_ssp1 <- rast(data$landuse_path)
time_info <- time(landuse_ssp1)
index <- which(time_info == data$year)
landuse_2015 <- subset(landuse_ssp1, index)
landuse_2015 <- crop(landuse_2015,europe)
landuse_2015 <- landuse_2015[[1:12]]
#plot(landuse_2015)
dominant_landuse <- which.max(landuse_2015)
dominant_landuse <- as.factor(dominant_landuse)

reclass_matrix <- matrix(c(
  # Forests (primary)
  1, 1,
  
  # Forests (secondary)
  3, 2,
  
  # Vegetated lands (primary)
  2, 3,
  
  # Vegetated lands (secondary)
  4, 4,
  
  # managed pastures / rangelands
  5, 5,
  6, 5,
  
  # urban land
  7, 7,

  # crop lands 
  8, 6,
  9, 6,
  10, 6,
  11, 6, 
  12, 6
  

  
), ncol = 2, byrow = TRUE)
landuse_processed <- classify(dominant_landuse,reclass_matrix)
levels(landuse_processed) <- data.frame(ID = seq_along(1:7), Class = c('Forests (primary)','Forests (secondary)',
                                                          'Vegetated lands (primary)','Vegetated lands (secondary)',
                                                          'Pastures / Rangelands','Croplands','Urban'))
colors <- c("#11573d",  # 1 forest primary (green)
            "#329470",  # 1 forest secondary (green)
            "#7DD181",  # 2 vegetated lands primary (lightgreen)
            "#7de88f",  # 2 vegetated lands secondary (lightgreen)
            "#DD7230",  # 3 pasture/rangeland (orange)
            "#FCE762",  # 4 cropland (yellow)
            "#D00000")  # 5 urban (red)
#plot(landuse_processed, col = colors, legend = 'top')

### TEMPERATURE 
# List all raster files in the folder
folder_path <- data$climate_path
raster_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

# Load all raster files into a SpatRaster stack
raster_stack <- rast(raster_files)
raster_stack <- crop(raster_stack,europe)

#aggregates
elev <- aggregate(elev,fact = 15)
raster_stack <- aggregate(raster_stack,fact = 30)

elev <- project(elev,raster_stack)
landuse_processed <- project(landuse_processed,raster_stack)

stack <- c(elev,landuse_processed,raster_stack)
names(stack) <- c('elevation','landuse',
                  'bio1','bio10','bio11','bio12',
                  'bio13','bio14','bio15','bio16','bio17',
                  'bio18','bio19','bio2','bio3','bio4',
                  'bio5','bio6','bio7','bio8','bio9')
writeRaster(stack,data$stack_name,overwrite=T)

}







# SESSION 2 - SUBSETS BIO VARIABLES  ---------------------------------------------------------------
environment <- list()

environment[['2015']] <- list(
  # landuse
  landuse_path = 'data/current/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc',
  year = 2015,
  #climate
  climate_path = 'data/current/chelsav2/GLOBAL/climatologies/1981-2010/bio/',
  
  #output file 
  stack_name = 'session2_2015_stack_subset.tif'
)

environment[['2100_ssp1']] <- list(
  # landuse
  landuse_path = 'data/ssp126/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc',
  year = 2100,
  #climate
  climate_path = 'data/ssp126/chelsav2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp126/bio/',
  
  #output file 
  stack_name = 'session2_2100_ssp1_stack_subset.tif'
)

environment[['2100_ssp5']] <- list(
  # landuse
  landuse_path = 'data/ssp585/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp534-2-1-f_gn_2015-2100.nc',
  year = 2100,
  #climate
  climate_path = 'data/ssp585/chelsav2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp585/bio/',
  
  #output file 
  stack_name = 'session2_2100_ssp5_stack_subset.tif'
)

times <- c('2015','2100_ssp1','2100_ssp5')
for(i in times){
  print(i)
  data <- environment[[i]]
  
  elev <- rast('data/ETOPO_2022_v1_60s_N90W180_bed.tif')
  elev <- crop(elev,europe)
  #plot(elev)
  
  # landuse 
  landuse_ssp1 <- rast(data$landuse_path)
  time_info <- time(landuse_ssp1)
  index <- which(time_info == data$year)
  landuse_2015 <- subset(landuse_ssp1, index)
  landuse_2015 <- crop(landuse_2015,europe)
  landuse_2015 <- landuse_2015[[1:12]]
  #plot(landuse_2015)
  dominant_landuse <- which.max(landuse_2015)
  dominant_landuse <- as.factor(dominant_landuse)
  
  reclass_matrix <- matrix(c(
    # Forests (primary)
    1, 1,
    
    # Forests (secondary)
    3, 2,
    
    # Vegetated lands (primary)
    2, 3,
    
    # Vegetated lands (secondary)
    4, 4,
    
    # managed pastures / rangelands
    5, 5,
    6, 5,
    
    # urban land
    7, 7,
    
    # crop lands 
    8, 6,
    9, 6,
    10, 6,
    11, 6, 
    12, 6
  ), ncol = 2, byrow = TRUE)
  landuse_processed <- classify(dominant_landuse,reclass_matrix)
  levels(landuse_processed) <- data.frame(ID = seq_along(1:7),class = c('Forests (primary)','Forests (secondary)',
                                                                         'Vegetated lands (primary)','Vegetated lands (secondary)',
                                                                         'Pastures / Rangelands','Croplands','Urban'))
  colors <- c("#11573d",  # 1 forest primary (green)
              "#329470",  # 1 forest secondary (green)
              "#7DD181",  # 2 vegetated lands primary (lightgreen)
              "#7de88f",  # 2 vegetated lands secondary (lightgreen)
              "#DD7230",  # 3 pasture/rangeland (orange)
              "#FCE762",  # 4 cropland (yellow)
              "#D00000")  # 5 urban (red)
  #plot(landuse_processed, col = colors, legend = 'top')
  
  ### TEMPERATURE 
  # List all raster files in the folder
  folder_path <- data$climate_path
  raster_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  raster_files <- raster_files[c(1,15,16,4,5,6)]
  
  # Load all raster files into a SpatRaster stack
  raster_stack <- rast(raster_files)
  raster_stack <- crop(raster_stack,europe)
  
  #aggregates
  elev <- aggregate(elev,fact = 15)
  raster_stack <- aggregate(raster_stack,fact = 30)
  
  elev <- project(elev,raster_stack)
  landuse_processed <- project(landuse_processed,raster_stack)
  
  stack <- c(elev,landuse_processed,raster_stack)
  names(stack) <- c('elevation','landuse',
                    'temperature_annual','temperature_warmest_month','temperature_coldest_month',
                    'precipitation_annual','precipitation_wettest_month','precipitation_driest_month')
  
  writeRaster(stack,data$stack_name,overwrite=T)
  
}

# ALTERNATIVE LANDUSE -----------------------------------------------------

test <- rast('data/alternative landuse/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2015.nc')
test <- rast('data/alternative landuse/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2100.nc')

# Get the layer index with the highest value for each cell
dominant_landuse <- which.max(test)
plot(dominant_landuse)

test <- t(dominant_landuse)
test <- crop(test,europe)

test_cropped <- test
plot(test_cropped)
# Create a reclassification matrix: [from, to, new_class]
reclass_matrix <- matrix(c(
  # Crops
  1, 26, 1, 
  28, 29, 1, 
  
  # Pasture & Rangeland
  30, 31, 2,  

  # Forests
  33, 35, 3,
  
  # Semi-Natural & Open Land
  27, 27, 4,
  36, 38, 4,
  
  # Urban & Developed Land
  32, 32, 5,  
  
  # Non-Vegetated (Bare, Ice, Desert)
  39, 39, 6   
  
), ncol = 3, byrow = TRUE)

# Apply classification
r_classified <- subst(test_cropped, 1:26,1)
r_classified <- subst(r_classified, 28:29,1)


plot(r_classified)
barplot(test_cropped)
plot(r_classified)
# Save the reclassified raster
writeRaster(r_classified, "reclassified_landuse.tif", overwrite = TRUE)

# Plot result
plot(r_classified, main = "Reclassified Land Use")


# ELEVATION ---------------------------------------------------------------

test <-rast('session2_2015_stack_subset.tif')
plot(test$landuse)




# SIMPLIFY EUROPE ---------------------------------------------------------
library('sf')
library('terra')
europe <- st_read('data/Europe/Europe_merged.shp')
europe_simple <- st_simplify(europe,dTolerance = 1000)
europe_vect <- vect(europe_simple)
plot(europe_vect)

st_write(europe_simple,'data/europe_simple/europe.shp')


# CROP PRESENCE / ABSENCE POINTS ------------------------------------------
library('dplyr')
occ <- st_read('occurrence_data/occurrence_data.shp')
plot(occ)
occ_c <- st_intersection(occ,europe_simple)
occ_c <- occ_c %>%
  select(occrrnS,prsnc_b,geometry)

plot(occ_c)
st_write(occ_c,
         'occurrence_data/occurrence_data.shp',
         append = F)

test <- st_read('occurrence_data/occurrence_data.shp')
plot(test)


# MAKE IMAGES FOR SLIDE ---------------------------------------------------------
library('terra')
temp_2015 <- rast('data/current/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif')
plot(temp_2015)

temp_agg <- aggregate(temp_2015,fact = 600)
plot(temp_agg)

# plot elevation 
library('terra')
library('sf')
stack <- rast('data_sdmglm_session2/session2_2015_stack.tif')
europe <- vect('europe_simple/europe.shp')
elevation_masked <- mask(stack$elevation,europe)
landuse_masked <- mask(stack$landuse,europe)

# plot elevation
plot(main = 'Elevation',
     elevation_masked,
     col = terrain.colors(100))

# plot landuse 
colors <- c("#11573d",  # 1 forest primary (green)
            "#329470",  # 2 forest secondary (green)
            "#7DD181",  # 3 vegetated lands primary (lightgreen)
            "#7de88f",  # 4 vegetated lands secondary (lightgreen)
            "#DD7230",  # 5 pasture/rangeland (orange)
            "#FCE762",  # 6 cropland (yellow)
            "#D00000",  # 7 urban (red)
            "cornflowerblue") # 8 water (blue)

plot(main = 'Landuse',
     landuse_masked, 
     col = colors,
     legend = 'top')

# plot temperature ssps 
cur <- rast('session1_stack.tif')
cur <- cur$temperature_annual
cur <- mask(cur,europe)

ssp1 <- rast('session2_2100_ssp1_stack_subset.tif')
ssp1 <- ssp1$temperature_annual
ssp1 <- mask(ssp1,europe)


ssp5 <- rast('session2_2100_ssp5_stack_subset.tif')
ssp5 <- ssp5$temperature_annual
ssp5 <- mask(ssp5,europe)

# Define color palette
temp_colors <- colorRampPalette(c("blue", "lightblue", "ivory", "yellow", "red"))(100)

# Set up a layout: 3 maps + 1 color legend
layout(matrix(c(1,2,3,4), ncol = 2), widths = c(4,1))  # 3 maps + 1 narrow legend

# Set common limits
zlim <- c(-10, 30)  

## Set up a 1-row, 3-column layout for the maps
par(mfrow = c(1, 3), mar = c(3, 3, 3, 3))  # Adjust margins for better spacing

# Plot the three maps
plot(cur, main = "2015", col = temp_colors, range = zlim, legend = FALSE)
plot(ssp1, main = "2100 (SSP1)", col = temp_colors, range = zlim, legend = FALSE)
plot(ssp5, main = "2100 (SSP5)", col = temp_colors, range = zlim, legend = FALSE)

# Reset `par()` to a single plot for the legend
par(mfrow = c(1, 1), mar = c(3, 3, 2, 3))  # Adjust margins for legend spacing

# Add shared color legend centered below plots
image.plot(legend.only = TRUE, col = temp_colors, zlim = zlim, 
           horizontal = TRUE, legend.lab = "Annual average (°C)")
 

test <- rast('session2_2015_stack_subset.tif')
test <- test[[3:nlyr(test)]]
plot(test)
