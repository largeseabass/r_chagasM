library("sp")
library("rJava")
library("raster")
library("dismo")
library("rgeos")

library("knitr")
library("rprojroot")

library("caret")
library("Metrics")
source("process-data.R")

this_bug = 'San'




########################################
# read bioclimatic and land cover data #
########################################

clim_list <- list.files("/Users/vivianhuang/Desktop/resample_mask/historical/", pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located

#check if the list of rasters are what we want
print(clim_list)

clim_raw <- raster::stack(clim_list)

########################################
# all the saving paths                 #
#c(maxent_result_dir, maxent_model_dir,maxent_model_path, save_raster_path, p_path,p_test_path,a_train_path,a_test_path)#
########################################

#read the occurence
occ_raw_path <- paste("/Users/vivianhuang/Desktop/R-modeling-scripts/data/",this_bug,'.csv',sep = '')
print(occ_raw_path)

all_path_stack <- all_saving_paths("/Users/vivianhuang/Desktop/R-modeling-scripts/output/",this_bug)

########################################
#       prepare the data.              #
#pa_train, pa_test, pder_train,pder_test, a_train, a_test, p_train, p_test#
########################################

input_data_stack <- prepare_input_data(occ_raw_path,clim)

########################################
# run MaxEnt                           #
########################################

knitr::opts_knit$set(root.dir = '/Users/vivianhuang/Desktop/R-modeling-scripts')
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.4.3/maxent.jar", 
                     destfile = paste0(system.file("java", package = "dismo"), 
                                       "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute


# train Maxent with tabular data
mod <- run_maxent_model(input_data_stack$x_train,input_data_stack$pa_train,all_path_stack[[1]])

# view the maxent model in a html brower
mod@results

#mod_test <- dismo::evaluate(p = input_data_stack$p_test, a = input_data_stack$a_test, model = this_mod)
mod_test <- predict(mod,input_data_stack$x_test)



########################################
# save model and results               #
########################################
saveRDS(mod, file = maxent_model_path)

#mod2 <- readRDS(maxent_model_path)
mod@results
mod <- readRDS(maxent_model_path)

