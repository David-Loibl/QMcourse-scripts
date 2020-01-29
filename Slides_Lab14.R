## ----echo=FALSE, hide=TRUE, message=FALSE, warning=FALSE----------------------------------
library(ggplot2)
library(tidyverse)


## ---- warning=FALSE, message=FALSE--------------------------------------------------------
# managing and manipulating vector data
library(sp)

# read and write spatial data
library(rgdal)

# reading and manipulating raster data
library(raster)

# map visualization
library(mapview)


## ---- eval=F, warning=FALSE, message=FALSE------------------------------------------------
## # managing and manipulating vector data
## library(sf)


## ---- echo=FALSE--------------------------------------------------------------------------
library(png)
library(grid)
img <- readPNG("figs/fig_sp_classes.png")
grid.raster(img)


## ---- message=FALSE, warning=FALSE--------------------------------------------------------
abies <- read.csv("data/abiesalba.csv")
head(abies)

abies_sp <- SpatialPoints(abies, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


## ---- message=FALSE, warning=FALSE--------------------------------------------------------
mapview(abies_sp)


## ---- message=FALSE, warning=FALSE--------------------------------------------------------
soil <- stack("data/soil_europe_laea_10x10km.tif")
soil.layernames <- read.csv("data/soil_europe_laea_10x10km_layer.csv", stringsAsFactors = F)
names(soil) <- soil.layernames$code

soil_projection <- projection(soil)
soil_projection

abies_sp <- spTransform(abies_sp, CRS(soil_projection))


## ---- message=FALSE, warning=FALSE--------------------------------------------------------
soil.df <- raster::extract(soil, abies_sp)
head(soil.df)


## -----------------------------------------------------------------------------------------
library(ggplot2)
coords <- abies_sp@coords
coords <- as.data.frame(coords)

xmean <- mean(coords[, 1])
ymean <- mean(coords[, 2])

p <- ggplot(coords, aes(x = long, y = lat)) + 
  geom_point(alpha = 0.3) +
  geom_point(aes(x = xmean, y = ymean), col = "red", shape = 2)


## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=5, fig.width=5, fig.align='center'----
print(p)


## -----------------------------------------------------------------------------------------
xmedian <- median(coords[, 1])
ymedian <- median(coords[, 2])

p <- ggplot(coords, aes(x = long, y = lat)) + 
  geom_point(alpha = 0.3) +
  geom_point(aes(x = xmean, y = ymean), col = "red", shape = 2) +
  geom_point(aes(x = xmedian, y = ymedian), col = "blue", shape = 2)


## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=5, fig.width=5, fig.align='center'----
print(p)


## -----------------------------------------------------------------------------------------
dist <- raster::pointDistance(abies_sp, longlat = FALSE)
dist[1:3, 1:3]

dist <- dist[lower.tri(dist)]

c(min(dist), mean(dist), max(dist)) / 10000


## -----------------------------------------------------------------------------------------
ext <- extent(abies_sp)
ext

ras <- raster(ext)
ras


## -----------------------------------------------------------------------------------------
res(ras) <- c(5000, 5000)

projection(ras) <- projection(abies_sp)

ras


## ---- eval=FALSE--------------------------------------------------------------------------
## abies_ras <- rasterize(abies_sp, ras, fun = "count", background = 0)
## 
## mapview(abies_ras)


## ---- echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------
abies_ras <- rasterize(abies_sp, ras, fun = "count", background = 0)

mapview(abies_ras)


## ---- warning=FALSE, message=FALSE--------------------------------------------------------
library(spatialEco)
kernel_ras <- sp.kde(abies_sp, bw = 10000, newdata = abies_ras, standardize = T)


## -----------------------------------------------------------------------------------------
mapview(kernel_ras)


## ---- eval=FALSE--------------------------------------------------------------------------
## abies_ras[abies_ras == 0] <- NA
## 
## distance_abies <- distance(abies_ras)
## 
## mapview(distance_abies)


## ---- echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------
abies_ras[abies_ras == 0] <- NA

distance_abies <- distance(abies_ras)

mapview(distance_abies)


## ---- warning=FALSE, message=TRUE---------------------------------------------------------
library(maps)
map <- map_data("world")

p <- ggplot() + xlim (-20, 59) + ylim(35, 71) +
  geom_density2d(data = abies, aes(x = long, y = lat), size = 0.3) + 
  geom_polygon(data = map, aes(x=long, y = lat, group = group)) + 
  stat_density2d(data = abies, 
                 aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 50, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0.1, 0.5), guide = FALSE)


## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=5, fig.width=6, fig.align='center'----
print(p)

