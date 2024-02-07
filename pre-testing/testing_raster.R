library("sp")
library('sf')
library("rJava")
library("raster")
library("dismo")
library("rgeos")
library("knitr")
library("rprojroot")
library("caret")
library("Metrics")

clim_dir <- "/Users/vivianhuang/Desktop/resample_mask/historical/"

this_clim_list <- list.files(clim_dir, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
this_clim <- raster::stack(this_clim_list)

this_clean_clim <- na.omit(raster::values(this_clim))

