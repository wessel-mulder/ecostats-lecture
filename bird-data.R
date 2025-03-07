# GETTING STARTED ---------------------------------------------------------
library('terra')
library('sf')

# LOADING DATA ------------------------------------------------------------
chiff <- st_read("../data/distributions/atlas_shapefiles/CommonChiffchaff//")
denmark <- st_read('../data/other/shapefiles_europe/23686383/denmark/denmark.shp')
temp <- rast('../data/environmental/e_obs/cropped_temp_yearmean.nc')
prec <- rast('../data/environmental/e_obs/cropped_prec_yearsum.nc')

time_info <- time(temp)
# get year
index <- which(format(time_info, "%Y") %in% 2014)
# extract mean 
temp_2014 <- subset(temp,index)
prec_2014 <- subset(prec,index)


summary(chiff)
plot(chiff['pa_atlas1'])



p_lat <- chiff$center_lat[chiff$pa_atlas3 == 1]
a_lat <- chiff$center_lat[chiff$pa_atlas3 == 0]

p_lng <- chiff$center_lng[chiff$pa_atlas3 == 1]
a_lng <- chiff$center_lng[chiff$pa_atlas3 == 0]

p <- data.frame(pa = 1,
               lat = p_lat,
               lon = p_lng)
a <- data.frame(pa = 0,
                lat = a_lat,
                lon = a_lng)

pa <- rbind(p,a)
pa

# Convert to sf point object
points <- st_as_sf(pa, coords = c("lon", "lat"), crs = 4326)  # CRS 4326 = WGS84 (lon/lat)





plot(denmark$geometry)
plot(points, add = T)

stack <- c(temp_2014,prec_2014)
stack <- crop(stack,denmark$geometry)
plot(stack)


presence <- extract(stack,points[points$pa == 1,])
presence$pa <- 1
absence <- extract(stack,points[points$pa == 0,])
absence$pa <- 0

df <- rbind(presence,absence)

m1 <- glm(pa~tg_45 + rr_45,
          family = 'binomial',
          data = df)

summary(m1)

p <-terra::predict(stack,m1,type = 'response')

plot(p)

l <- nrow(points)
indices <- sample(c(1:1500), 200)
p <- points[indices,]

indices <- sample(c(1501:2165), 100)
a <- points[indices,]


plot(denmark$geometry)
plot(p, col = 'forestgreen',
     pch = 16,
     add = T)
plot(a, col = 'firebrick',
     pch = 16,
     add = T)

legend(x = 'topright',
       legend = c('presence','absence'),
       fill = c('forestgreen','firebrick'))




n <- 100         # Sample size
lambda <- 1       # Mean of Poisson distribution
p_zero <- 0.3     # Probability of excess zeros

# Generate standard Poisson data
poisson_data <- rpois(n, lambda)

# Introduce excess zeros
zero_inflated_data <- ifelse(runif(n) < p_zero, 0, poisson_data)

# View distribution
table(zero_inflated_data)

# Plot histogram
barplot(table(zero_inflated_data), main='Count data',
        col="blue")

indices <- sample(l,50)
plot(denmark$geometry)
#plot(points[indices,],
 #    col = 'red',
 #    pch = 4,
 #    cex = 1,
 #    add = T)
plot(points[indices,],
     col = 'blue',
     pch = 1,
     cex = 3,
     add = T)
plot(points[sample(indices,10),],
     col = 'blue',
     pch = 16,
     cex = 3,
     add = T)
legend()

# WOODPECKER --------------------------------------------------------------
green <- st_read("../data/distributions/atlas_shapefiles/EuropeanGreenWoodpecker/EuropeanGreenWoodpecker.shp")
denmark <- st_read('../data/other/shapefiles_europe/23686383/denmark/denmark.shp')
temp <- rast('../data/environmental/e_obs/cropped_temp_yearmean.nc')
prec <- rast('../data/environmental/e_obs/cropped_prec_yearsum.nc')
landuse <- rast('../data/environmental/hilda/hildap_vGLOB-1.0-f_netcdf_preprocessedWM/hildaplus_preprocessed.nc')

time_info <- time(temp)
# get year
index <- which(format(time_info, "%Y") %in% 2014)
# extract mean 
temp_2014 <- subset(temp,index)
prec_2014 <- subset(prec,index)
landuse_2014 <- subset(landuse,index)



summary(green)
plot(green['pa_atlas3'])

chiff <- green

p_lat <- chiff$center_lat[chiff$pa_atlas3 == 1]
a_lat <- chiff$center_lat[chiff$pa_atlas3 == 0]

p_lng <- chiff$center_lng[chiff$pa_atlas3 == 1]
a_lng <- chiff$center_lng[chiff$pa_atlas3 == 0]

p <- data.frame(pa = 1,
                lat = p_lat,
                lon = p_lng)
a <- data.frame(pa = 0,
                lat = a_lat,
                lon = a_lng)

pa <- rbind(p,a)
pa

# Convert to sf point object
points <- st_as_sf(pa, coords = c("lon", "lat"), crs = 4326)  # CRS 4326 = WGS84 (lon/lat)





plot(denmark$geometry)
plot(points, add = T)

ext(landuse_2014) <- ext(temp_2014)

stack <- c(temp_2014,prec_2014,landuse_2014)
stack <- crop(stack,denmark)
stack <- mask(stack,denmark)

temp_2014 <- disagg(temp_2014,10)
temp_2014 <- crop(temp_2014,denmark)
temp_2014 <- mask(temp_2014,denmark)
prec_2014 <- disagg(prec_2014,10)
prec_2014 <- crop(prec_2014,denmark)
prec_2014 <- mask(prec_2014,denmark)

stack <- c(temp_2014,prec_2014,landuse_2014)
plot(stack[[1]])
plot(points,add=T)

presence <- extract(stack,points)
presence$pa <- points$pa

presence$pa <- 1
absence <- extract(stack,points[points$pa == 0,])
absence$pa <- 0

df <- rbind(presence,absence)

m1 <- glm(pa~tg_45 + rr_45,
          family = 'binomial',
          data = df)

summary(m1)

m2 <- glm(pa~tg_45 + rr_45 + hildaplus_preprocessed_45,
          family = 'binomial',
          data = df)

summary(m2)



p1 <-terra::predict(stack,m1,type = 'response')
plot(p1)

p2 <-terra::predict(stack,m2,type = 'response')
plot(p2)
plot(points,add=T)
plot(stack$tg_45)
plot(stack$rr_45)

p <- 
