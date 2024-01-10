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

occ_grid_path = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/cell/San.csv"
clim_grid_path = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/historical/5km.csv"

#read occurence data
occ_grid <- read.csv(occ_grid_path)
#read clim data
clim_grid <- read.csv(clim_grid_path)


# all occurrence (get rid of all the NA rows)
all_grid_historical_raw <- merge(x = occ_grid, y = clim_grid, by = "id")
occ_grid_historical_values <- subset(all_grid_historical_raw[complete.cases(all_grid_historical_raw), ],select = -c(left.y, top.y,right.y,bottom.y,X,left.x, top.x,right.x,bottom.x,forest_grassland))
# the whole area 
all_grid_historical_values <- subset(clim_grid[complete.cases(clim_grid), ],select = -c(left, top,right,bottom,X,forest_grassland))
# the whole area - occurrence cells = the area to select background points from
# Exampe 4: delete rows by name
diff_set <-  all_grid_historical_values[ ! all_grid_historical_values$id %in% occ_grid_historical_values$id, ]
all_p <- subset(occ_grid_historical_values,select = -c(count))
this_inspect_index_p <- unique(which(is.na(all_p), arr.ind=TRUE)[,1])
if(length(this_inspect_index_p) > 0){
  cat("\nNA values in occurrence points!")
  cat("\nRows with NA values are removed. Row indexs:",this_inspect_index_p)
  cat("\nRows with NA values are removed. Rows:")
  print(all_p[this_inspect_index_p,])
  all_p <- all_p[-this_inspect_index_p,]
  cat("\nPlease consider removing these rows from input datasets.")
} 
