##%######################################################%##
#                                                          #
####           Intro to spatial analysis in R           ####
####             Course by Our Coding Club              ####
####    Working with rasters and remote-sensing data    ####
####                     22.8.2024                      ####
#                                                          #
##%######################################################%##

# Satellite data available from https://scihub.copernicus.eu/

# Working directory ----
setwd("C:\\Users\\pavla\\OneDrive\\Documents\\GitHub\\R-course\\CC_course_stream2-master\\04_Spatial_analysis")

# Used libraries ----
library(sp)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)

# Custom functions ----
theme_raster <- function(){
  theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=20),		       	            # font size
          axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text
}

# Function to calculate vegetation indices
VI <- function(img, k, i){
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk-bi)/(bk+bi)  # NDVI equation
  return(vi)
}

# Work with raster file ----

# Load data
tay <- raster('taycrop.tif')

# Get properties of the tay raster
tay

# Create raster layers for each of the spectral band
b1 <- raster('taycrop.tif', band=1)
b2 <- raster('taycrop.tif', band=2)
b3 <- raster('taycrop.tif', band=3)
b4 <- raster('taycrop.tif', band=4)
b5 <- raster('taycrop.tif', band=5)
b6 <- raster('taycrop.tif', band=6)
b7 <- raster('taycrop.tif', band=7)
b8 <- raster('taycrop.tif', band=8)
b9 <- raster('taycrop.tif', band=9)
b10 <- raster('taycrop.tif', band=10)
b11 <- raster('taycrop.tif', band=11)
b12 <- raster('taycrop.tif', band=12)

compareRaster(b2,b3)

# Plot the band
plot(b8)  # only plots 100 000 px
image(b8) # stretches the image

zoom(b8)
# Crop and plot an extent
plot(tay)
e <- drawExtent()
cropped_tay <- crop(b7,e)
plot(cropped_tay)

# Visualize spectral bands

image(b8, col = viridis_pal(option = "D")(10), main = "Sentinel 2 image of Loch Tay")

# to also save
png('tayplot.png', width = 4, height = 4, units = "in", res = 300)                	
image(b8, col= viridis_pal(option="D")(10), main="Sentinel 2 image of Loch Tay")
dev.off()         									
# dev.off() is a function that "clears the slate" - it just means you are done using that specific plot

# Create and visualize a raster stack
tayRGB <- stack(list(b4,b3,b2))
plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "Sentinel RGB colour composite")

# Create a plot using rasterVis
gplot(b8) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("West of Loch tay, raster plot") +
  xlab("Longtitude") +
  ylab("Latitude") +
  theme_raster()

ggsave("ggtay.png", scale = 1.5, dpi = 300)

# Make facetted plots ----
# Stack all the bands
bands <- stack(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)

# Create a facetted plot
gplot(bands) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c() +
  facet_wrap(~variable) +
  coord_quickmap() +
  labs(title = "West of Loch tay, raster plots\n",
       x = "\nLongtitude", y = "Latitude\n") +
  theme_raster()

ggsave("allbands.png", scale = 4, dpi = 300)

# Load the original file as a raster brick and display it
brick_tay <- brick('taycrop.tif')
plot(brick_tay)

# Calculating vegetation indices ----
# NDVI - calculate using function VI
# For Sentinel 2, the relevant bands to use are:
# NIR = 8, red = 4

NDVI <- VI(brick_tay, 8, 4)

png('ndviplot.png', width = 4, height = 4, units = "in", res = 300)
plot(NDVI, col = rev(terrain.colors(10)), main = 'Sentinel 2, Loch Tay-NDVI')
dev.off()

# Create histogram of NDVI data
png('ndvihist.png', width = 4, height = 4, units = "in", res = 300)
hist(NDVI,
     main = 'Distribution of NDVI values',
     xlab = 'NDVI',
     ylab = 'Frequency',
     col = 'aquamarine3',
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5, 1, 0.05), labels = seq(-0.5, 1, 0.05))
dev.off()


# Mask pixels where NDVI is lower than 0.4
png('ndvimask.png', width = 4, height = 4, units = "in", res = 300)
veg <- reclassify(NDVI, cbind(-Inf, 0.4, NA))
plot(veg, main = "Vegetation cover")
dev.off()


# Saving a raster
writeRaster(x = NDVI,
            filename = "C:/Users/pavla/OneDrive/Documents/GitHub/R-course/CC_course_stream2-master/04_Spatial_analysis/tay_ndvi_2018.tif",
            format = "GTiff",
            datatype = 'INT2S')     # save as a INTEGER rather than a float

# Unsupervised classification ----
# Transform raster to array
pixels <- getValues(NDVI)
str(pixels)

# important to set the seed generator because `kmeans` initiates the centres in random locations
# the seed generator just generates random numbers

set.seed(99)

# create 10 clusters, allow 500 iterations, start with 5 random sets using 'Lloyd' method
kmncluster <- kmeans(na.omit(pixels), centers = 10, iter.max = 500,
                     nstart = 5, algorithm = 'Lloyd')

str(kmncluster)

# Convert the kmncluster$cluster array back to raster layer of the same dimension as the NDVI object
# Create copy of NDVI raster layer
copy <- NDVI

# Replace raster cell values with kmncluster$cluster
copy[] <- kmncluster$cluster
copy

# Plot the cluster side by side with the NDVI map to attribute a class to a land cover
png('ndvi_kmeans.png', width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(1,2))
plot(NDVI, col = rev(terrain.colors(10)), main = "NDVI")
plot(copy, main = "Kmeans", col = viridis_pal(option = "D")(10))
dev.off()
