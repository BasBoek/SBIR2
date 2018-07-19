library(raster)
library(tiff)

B <- raster("C:/Users/Cobra_rekenplek/Desktop/31UFT/6-30/B02.jp2")
G <- raster("C:/Users/Cobra_rekenplek/Desktop/31UFT/6-30/B03.jp2")
R <- raster("C:/Users/Cobra_rekenplek/Desktop/31UFT/6-30/B04.jp2")
RGB_6_30 <- brick(R,G,B)

NDVI_0630 <- ()

NDVI_0715 <- 

writeRaster(RGB_6_30, "C:/Users/Cobra_rekenplek/Desktop/31UFT/6-30/multispectral/RGB.tiff", format="GTiff")

B <- raster("C:/Users/Cobra_rekenplek/Desktop/31UFT/7-15/B02.jp2")
G <- raster("C:/Users/Cobra_rekenplek/Desktop/31UFT/7-15/B03.jp2")
R <- raster("C:/Users/Cobra_rekenplek/Desktop/31UFT/7-15/B04.jp2")
RGB_7_15 <- brick(R,G,B)

writeRaster(RGB_7_15, "C:/Users/Cobra_rekenplek/Desktop/31UFT/7-15/multispectral/RGB.tiff", format="GTiff")
