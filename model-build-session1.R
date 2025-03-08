# GETTING STARTED ---------------------------------------------------------
library('terra')
library('sf')
library('lme4')


# LOADING DATA ------------------------------------------------------------
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


