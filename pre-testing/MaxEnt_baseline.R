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
source("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/pre-testing/process-data.R")

this_bug = 'San'
number_replicate = 2

########################################
# read bioclimatic and land cover data #
########################################

clim_list <- list.files("/Users/vivianhuang/Desktop/resample_mask/historical/", pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
#check if the list of rasters are what we want
print(clim_list)

clim <- raster::stack(clim_list)

########################################
# all the saving paths                 #
#c(maxent_result_dir, maxent_model_dir,maxent_model_path, save_raster_path, p_path,p_test_path,a_train_path,a_test_path)#
########################################

#read the occurence
occ_raw_path <- paste("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/data/",this_bug,'.csv',sep = '')
#/output folder saves all the output
all_path_stack <- all_saving_paths("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/",this_bug)

########################################
#       prepare the data.              
# three options: 
# (1) for one replicate with all prepare_input_data (occ_raw_path,clim,split_ratio=0.75)
# (2) prepare_input_data_kfold (occ_raw_path,clim,number_of_folds)
# (3) prepare_input_data_kfold_buffer (occ_raw_path,clim,number_of_folds,buff_width = 0.5)
########################################

input_data_stack <- prepare_input_data(occ_raw_path,clim)
input_data_stack_with_folds <- prepare_input_data_kfold(occ_raw_path,clim,number_replicate)
source("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/pre-testing/process-data.R")
shapefile_path <-"/Users/vivianhuang/desktop/R-modeling-scripts/r_chagasM/masked_raster/shapefile.shp"
input_data_stack_with_folds_buffer <- prepare_input_data_kfold_buffer(occ_raw_path,clim,number_replicate,shapefile_path)




########################################
# run MaxEnt                           #
########################################

knitr::opts_knit$set(root.dir = '/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM')
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.4.3/maxent.jar", 
                     destfile = paste0(system.file("java", package = "dismo"), 
                                       "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute

# train Maxent with tabular data
mod <- run_maxent_model(input_data_stack$x_train,input_data_stack$pa_train,all_path_stack[[1]],number_replicate)

mod_eval_train <- dismo::evaluate(p = input_data_stack$p_test, a = input_data_stack$a_test, model = mod[[1]])
# view the maxent model in a html brower mod@results

spatial_test <- rbind(input_data_stack$p_test, input_data_stack$a_test)
#input_data_stack$x_test
for (i in 1:number_replicate){
  mod_test <- predict(mod@models[[i]],input_data_stack$x_test)
}



#typeof(as.data.frame(input_data_stack$x_test))

########################################
# save model and results               #
########################################
saveRDS(mod, file = maxent_model_path)

#mod2 <- readRDS(maxent_model_path)
mod@results
mod <- readRDS(maxent_model_path)

