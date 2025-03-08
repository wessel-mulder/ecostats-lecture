# GETTING STARTED ---------------------------------------------------------
library('terra')
library('sf')
library('lme4')


# SESSION 1 ------------------------------------------------------------
predictors <- rast('session1_stack.tif')
europe <- st_read('Europe/Europe_merged.shp')
presence_points <- st_read('pres_points/pres_points.shp')
absence_points <- st_read('pseudo_ab_points/pseudo_abs_points.shp')

plot(predictors[[1]])
plot(presence_points$geometry,add=T)
plot(absence_points$geometry,add=T,
     col = 'red')

presence <- extract(predictors,presence_points,bind=T)
presence$presence_absence <- 1

absence <- extract(predictors,absence_points,bind=T)
absence$presence_absence <- 0

df <- rbind(presence,absence)
table(df$presence_absence)
plot(df$presence_absence)

df$presence_absence <- as.factor(df$presence_absence)
m1 <- glm(presence_absence~temperature + precipitation,
          family = binomial,
          data = df)

summary(m1)

p1 <- terra::predict(predictors,m1,type='response')
p1 <- mask(p1,europe)
plot(p1$lyr1)



# SESSION 2 ------------------------------------------------------------
library(terra)
library(caret)  # For findCorrelation()

remove_correlated_rasters <- function(raster_stack, cutoff = 0.8) {
  # Convert raster stack to a dataframe of pixel values
  raster_values <- as.data.frame(raster_stack, na.rm = TRUE)
  
  # Compute correlation matrix
  cor_matrix <- cor(raster_values, use = "pairwise.complete.obs")
  
  # Identify highly correlated layers to remove
  high_cor <- findCorrelation(cor_matrix, cutoff = cutoff, names = TRUE)
  
  # Keep only uncorrelated layers
  raster_filtered <- raster_stack[[!names(raster_stack) %in% high_cor]]
  
  return(raster_filtered)
}

# Example usage
r_filtered <- remove_correlated_rasters(predictors[[3:nlyr(predictors)]], cutoff = 0.8)

# Check the remaining layers
names(r_filtered)

predictors <- rast('session2_2015_stack.tif')
predictors_2100_ssp1 <- rast('session2_2100_ssp1_stack.tif')
predictors_2100_ssp5 <- rast('session2_2100_ssp5_stack.tif')

europe <- st_read('Europe/Europe_merged.shp')
presence_points <- vect('pres_points/pres_points.shp')
absence_points <- vect('pseudo_ab_points/pseudo_abs_points.shp')

plot(predictors[[1]])
plot(presence_points,add=T)
plot(absence_points,add=T,
     col = 'red')

presence <- extract(predictors,presence_points,bind=T)
presence$presence_absence <- 1

absence <- extract(predictors,absence_points,bind=T)
absence$presence_absence <- 0

df <- rbind(presence,absence)

plot(elevation~presence_absence,
     data = df)

df <- df[,49:ncol(df)]
df$presence_absence <- as.factor(df$presence_absence)
names(df)

# check correlation
remove_high_vif <- function(df, vif_threshold = 5) {
  # Fit an initial linear model
  model <- lm(1 ~ ., data = df)  # Dummy response variable to check VIF
  
  # Compute VIF
  vif_values <- vif(model)
  
  # Remove variables with high VIF
  while (max(vif_values) > vif_threshold) {
    high_vif_var <- names(which.max(vif_values))  # Most collinear variable
    df <- df[, !colnames(df) %in% high_vif_var]  # Remove it
    
    # Recompute VIF after removal
    model <- lm(1 ~ ., data = df)
    vif_values <- vif(model)
  }
  
  return(df)
}

df <-na.omit(df)
# Example usage
df_no_multicollinearity <- remove_high_vif(df, vif_threshold = 5)

# model 1
m1 <- glm(presence_absence~.,
          family = binomial,
          data = df)

summary(m1)

p1 <- terra::predict(predictors,m1,type='response')
p1 <- mask(p1,europe)
habitat_colors <- colorRampPalette(c("#A50026", "#D73027", "#FEE08B", "#66BD63", "#006837"))
plot(main = '2015',
     p1,
     col = habitat_colors(100))
#plot(presence_points,add=T,
 #    cex = 0.5)
p2 <- terra::predict(predictors_2100_ssp1,m1,type='response')
p2 <- mask(p2,europe)
habitat_colors <- colorRampPalette(c("#A50026", "#D73027", "#FEE08B", "#66BD63", "#006837"))
plot(main = '2100_ssp1',
     p2,
     col = habitat_colors(100))

p3 <- terra::predict(predictors_2100_ssp5,m1,type='response')
p3 <- mask(p3,europe)
habitat_colors <- colorRampPalette(c("#A50026", "#D73027", "#FEE08B", "#66BD63", "#006837"))
plot(main = '2100_ssp5',
     p3,
     col = habitat_colors(100))




# model 2
m2 <- glm(presence_absence~bio1+bio12,
          family = binomial,
          data = df)

summary(m2)

p2 <- terra::predict(predictors,m1,type='response')
p2 <- mask(p2,europe)
habitat_colors <- colorRampPalette(c("#A50026", "#D73027", "#FEE08B", "#66BD63", "#006837"))
plot(p2,
     col = habitat_colors(100))


