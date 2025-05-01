library("sp")
library("rJava")
library("raster")
library("dismo")
# library("rgeos")
library("knitr")
library("rprojroot")

# todo
# 1. download all the files from adaptwest (https://adaptwest.databasin.org/pages/adaptwest-climatena/)
# 2. download all the files from land cover (https://zenodo.org/records/4584775)
# 3. locate a reference file for r2 (e.g. the AHM.tif in the example)
# 4. specify (and create) a folder to store the output rasters
# 5. write a for-loop for adaptwest rasters
# 6. write a for-loop for land cover rasters




# you may also run into error: rgeos no longer available for this version of R
# if so, you can install the package from source (get the version 2023-7-18): https://cran.r-project.org/src/contrib/Archive/rgeos/
# library(devtools)
# install_local("~/Downloads/my_package")

#using the script below, we can easily
#an example with ssp580 EXT
# we are reprojecting and aligning raster r1 with r2, so if you want to use a  for-loop, we are only changing r1 in every iteration.

r1 <-raster("/Users/liting/Downloads/to-send/EXT.tif") #raster("/Users/liting/Downloads/ensemble_8GCMs_ssp585_2071_2100/ensemble_8GCMs_ssp585_2071_2100_bioclim/ensemble_8GCMs_ssp585_2071_2100_AHM.tif")
r2 <- raster("/Users/liting/Documents/data/resample_mask/historical/AHM.tif")
# enable the follow one if you haven't reprojected using QGIS (like I did), the resulting raster layer will have very slight differences, but you can still use it.
#r1 <- projectRaster(r1, crs = crs(r2), method = 'bilinear') # reprojection
r1 <- resample(r1,r2, "bilinear") # solving the alignment issue of pixels from r1 and r2
r1 <- mask(r1, r2) # solving r1 and r2 having different geographical range
# where you want to save the reprojected and aligned r1 file
#file_save_path <-"/Users/liting/Desktop/EXT.tif"
# writeRaster(r1, filename = file_save_path, format = "GTiff",overwrite=TRUE)

