

setwd("C:/gceb52/RidwanFinal/Ridwan SHITTU")
globalmaps <- getMap()
#South Africa

soa <- subset(globalmaps, globalmaps$ADMIN == "South Africa")

#bio <-raster::getData("worldclim", var = "bio", res = 10)
#' Plot the first raster layer, i.e. annual mean temperature
#plot(raster(bio, 1))
# Add the outline of the study area
#plot(soa, add=TRUE)
#' Crop to study area extent (with a 5 degree buffer in each direction)
biocrop_soa <- crop(bio, extent(soa) + 10)
biocrop_soa <- raster::mask(biocrop_soa, soa)
#plot(raster(biocrop_soa1, 1))#' Plot the first raster layer of the cropped climate data

#ndvi<- raster("c:/gceb52/ndvi.tif")
#plot(ndvi)
#ndvi_soa<- crop(ndvi, extent(soa)+10)
#plot(ndvi_soa)
#ndvi_soa1 <- raster::mask(ndvi_soa, soa)
#plot(ndvi_soa1)
#writeRaster(ndvi_soa1, filename = "ndvi_S", format="GTiff", overwrite=T)


glwd_S <- raster("glwd_S.tif")


ndvi_S <- raster("ndvi_S.tif")

plot(raster(biocrop_soa, 1))





#####RESAMPLING and reprojection of ndvi

e_soa<-extent(10, 40, -40, -20)



s_soa<-raster(e_soa, nrows=500, ncols=580, crs=bio@crs)

# use this raster to reproject your original raster (since your using the same crs,
# resample should work fine


ndvi_S<-resample(ndvi_S, s_soa, method="bilinear")

biocrop_soa <-  resample(biocrop_soa, s_soa, method = "bilinear")

glwd_S <- resample(glwd_S, s_soa, method = "bilinear")

#plot(ndvi_S)



allenv_soa <- raster::stack(biocrop_soa, ndvi_S, glwd_S)



# Download species location data from gbif


#species0 <- gbif("Loxodonta", "soaicana") # Download species location data from gbif
#species <- subset(species0,select=c("lat","lon"))
#species <- na.omit(species)
#coordinates(species) <- c("lon", "lat")  # set spatial coordinates


# Add projection information
#proj4string(species) <- CRS("+proj=longlat +datum=WGS84")





#' Select species records for which environmental information is available
#' -------------------------------
species_soa <- species[complete.cases(extract(biocrop_soa, species)), ]



#' Collinearity
#' -----------------------------
#' ### Visual inspection of collinearity ###
cm_soa <- cor(getValues(allenv_soa), use = "complete.obs")
plotcorr(cm_soa, col=ifelse(abs(cm_soa) > 0.7, "red", "grey"), main= "Environmental Variable Colinearity for South Africa")



#' ### Select an uncorrelated subset of environmental variables ###
env_soa <- subset(allenv_soa, c("bio1", "bio9", "bio14", "bio17", "ndvi_S", "glwd_S"))


#' Select only one presence record in each cell of the environmental layer
presence_soa <- gridSample(species_soa, env_soa, n = 1)
#plot(presence_soa, col=2)

set.seed(2)
background_soa <- randomPoints(env_soa, 15000, species_soa)
#plot(background_soa, col=3)


#' 
#' Now we combine the presence and background points, adding a 
#' column "species" that contains the information about presence (1)
#' and background (0)
fulldata_soa <- SpatialPointsDataFrame(rbind(presence_soa, background_soa),
                                       data = data.frame("species_soa" = rep(c(1,0), 
                                                                             c(nrow(presence_soa), nrow(background_soa)))),
                                       match.ID = FALSE,
                                       proj4string = CRS(projection(env_soa)))

#' Add information of environmental conditions at point locations
fulldata_soa@data <- cbind(fulldata_soa@data, extract(env_soa, fulldata_soa))

fulldata_soa <- as(fulldata_soa, "data.frame")

fulldata_soa <- na.omit(fulldata_soa)

#' 
# Split data set into a training and test data set
set.seed(2)
fold <- kfold(fulldata_soa, k = 5)
traindata_soa <- fulldata_soa[fold != 1, ]
testdata_soa <- fulldata_soa[fold == 1, ]

#' We can now use a range of statistical methods to estimate the
#' probability of species occurrence.
#' Unfortunately, there are often subtle differences in how the models
#' are specified and in which data formats are useable

## Maxent
# The following code assumes that the column with the species information
# is in the first position


me_fulldata_soa <- maxent(fulldata_soa[, c("bio1", "bio9", "bio14", "bio17", "ndvi_S", "glwd_S")], fulldata_soa[, "species_soa"])

# Show variable importance
plot(me_fulldata_soa)

# Plot response functions
response(me_fulldata_soa, expand = 0)

# Model evaluation on test data
maxenttest_soa <- predict(me_fulldata_soa, testdata_soa)
val.prob(maxenttest_soa, testdata_soa[["species_soa"]])



# Prediction map
maxentmap_soa <- predict(me_fulldata_soa, env_soa)
plot(maxentmap_soa)
plot(soa, add=T)
```