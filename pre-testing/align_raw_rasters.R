library("sp")
library("rJava")
library("raster")
library("dismo")
library("rgeos")
library("knitr")
library("rprojroot")

# you may also run into error: rgeos no longer available for this version of R
# if so, you can install the package from source (get the version 2023-7-18): https://cran.r-project.org/src/contrib/Archive/rgeos/
# library(devtools)
# install_local("~/Downloads/my_package")

#using the script below, we can easily
#an example with ssp580 EXT

r1 <-raster("/Users/liting/Downloads/to-send/EXT.tif") #raster("/Users/liting/Downloads/ensemble_8GCMs_ssp585_2071_2100/ensemble_8GCMs_ssp585_2071_2100_bioclim/ensemble_8GCMs_ssp585_2071_2100_AHM.tif")
r2 <- raster("/Users/liting/Documents/data/resample_mask/historical/AHM.tif")
# enable the follow one if you haven't reprojected using QGIS (like I did), the resulting raster layer will have very slight differences, but you can still use it.
#r1 <- projectRaster(r1, crs = crs(r2), method = 'bilinear')
r1 <- resample(r1,r2, "bilinear")
r1 <- mask(r1, r2)
file_save_path <-"/Users/liting/Desktop/EXT.tif"
writeRaster(r1, filename = file_save_path, format = "GTiff",overwrite=TRUE)

