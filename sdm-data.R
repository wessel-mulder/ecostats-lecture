# GETTING STARTED ---------------------------------------------------------
library('sdm')
library('terra')
library('dplyr')
library('sf')

# GETTING DATA -----------------------------------------------------------
path <- system.file('external',package='sdm')
lst <- list.files(path=path,pattern='asc$',full.names = T) # list the name of files in the specified pa
preds <- c(rast(lst))
plot(preds)

preds[[1]]


# 2015 --------------------------------------------------------------------

elev <- rast('data/ETOPO_2022_v1_60s_N90W180_bed.tif')
elev <- crop(elev,europe)
plot(elev)

### EUROPE SHAPEFILE
europe <- vect('data/Europe/Europe_merged.shp')

### LANDUSE
landuse_ssp1 <- rast('data/current/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc')
time_info <- time(landuse_ssp1)

# 2015
index <- which(time_info == '2015')
landuse_2015 <- subset(landuse_ssp1, index)
landuse_2015 <- crop(landuse_2015,europe)
landuse_2015 <- landuse_2015[[1:12]]
dominant_landuse <- which.max(landuse_2015)
names(landuse_2015)
plot(dominant_landuse)

plot(landuse_2015)
# 2100 ssp1
index <- which(time_info == '2100')
landuse_2100_ssp1 <- subset(landuse_ssp1, index)
landuse_2100_ssp1 <- crop(landuse_2100_ssp1,europe)
landuse_2100_ssp1 <- landuse_2100_ssp1[[1:12]]
dominant_landuse <- which.max(landuse_2100_ssp1)
plot(dominant_landuse)

# s200 ssp5
landuse_ssp5 <- rast('data/ssp585/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp534-2-1-f_gn_2015-2100.nc')
time_info <- time(landuse_ssp5)
index <- which(time_info == '2100')
landuse_2100_ssp5 <- subset(landuse_ssp5, index)
landuse_2100_ssp5 <- crop(landuse_2100_ssp5,europe)
landuse_2100_ssp5 <- landuse_2100_ssp5[[1:12]]
dominant_landuse <- which.max(landuse_2100_ssp5)
plot(dominant_landuse)

plot(landuse_2015[[1]])
plot(landuse_2100_ssp1[[1]])
plot(landuse_2100_ssp5[[1]])

### TEMPERATURE 
temp_2015 <- rast('data/current/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif')
temp_2015 <- crop(temp_2015,europe)
prec_2015 <- rast('data/current/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio12_1981-2010_V.2.1.tif')
prec_2015 <- crop(prec_2015,europe)

temp_2100_ssp1 <- rast('data/ssp126/chelsav2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp126/bio/CHELSA_bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1.tif')
temp_2100_ssp1 <- crop(temp_2100_ssp1,europe)


temp_2100_ssp5 <- rast('data/ssp585/chelsav2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp585/bio/CHELSA_bio1_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1.tif')
temp_2100_ssp5 <- crop(temp_2100_ssp5,europe)

temp_2015
elev_test <- aggregate(elev,fact = 15)
temp_2015_test<- aggregate(temp_2015,fact = 30)
prec_2015_test<- aggregate(prec_2015,fact = 30)

max <- which.max(landuse_2015)
max <- project(max,elev_test)
max <- as.factor(max)
temp_2015_test <- project(temp_2015_test,elev_test)
prec_2015_test <- project(prec_2015_test,elev_test)

stack <- c(max,elev_test,temp_2015_test,prec_2015_test)
names(stack) <- c('landuse','elevation','temperature','precipitation')
writeRaster(stack, "session1_stack.tif", overwrite = TRUE)



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

