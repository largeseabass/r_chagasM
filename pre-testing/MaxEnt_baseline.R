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

#todo: update the clim_dir in all packed function


run_maxent_simple <- function(this_bug,number_replicate,top_file_dir){
  # create one train-test split, train the maxent model, generate evaluation metrics, and obtain the historical and future projected predictions.
  ########################################
  # read bioclimatic and land cover data #
  ########################################
  
  clim_list <- list.files("/Users/vivianhuang/Desktop/resample_mask/historical/", pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  
  ########################################
  # all the saving paths                 #
  ########################################
  
  #read the occurence
  occ_raw_path <- paste("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/data/",this_bug,'.csv',sep = '')
  #/output folder saves all the output
  all_path_stack <- all_saving_paths(top_file_dir=top_file_dir,this_bug=this_bug)
  
  ########################################
  #       prepare the data.              #
  ########################################
  
  input_data_stack <- prepare_input_data_basic(occ_raw_path=occ_raw_path,
                                               clim=clim,
                                               maxent_result_dir=all_path_stack$maxent_result_dir,
                                               split_ratio=0.90)
  
  ########################################
  # run MaxEnt                           #
  ########################################
  
  knitr::opts_knit$set(root.dir = '/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM')
  opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
  
  utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.4.3/maxent.jar", 
                       destfile = paste0(system.file("java", package = "dismo"), 
                                         "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute
  
  
  this_input_data_stack <- input_data_stack

  # train the model (for prediction)
  this_model <- run_maxent_model_training_basic(maxent_evaluate_dir=all_path_stack$maxent_evaluate_dir,
                                                final_x_train=this_input_data_stack$final_x_train,
                                                final_x_test=this_input_data_stack$final_x_test,
                                                final_pa_train=this_input_data_stack$final_pa_train,
                                                final_pa_test=this_input_data_stack$final_pa_test,
                                                final_p_train=this_input_data_stack$final_p_train,
                                                final_a_train=this_input_data_stack$final_a_train,
                                                final_p_test=this_input_data_stack$final_p_test,
                                                final_a_test=this_input_data_stack$final_a_test,
                                                maxent_model_dir=all_path_stack$maxent_model_dir,
                                                metric_saving=T,
                                                model_saving=T)
  
  ############################
  # predictions              #
  ############################
  
  dir_resample_mask <- "/Users/vivianhuang/desktop/resample_mask"
  run_maxent_model_prediction_basic(mod=this_model,
                                    clim=clim,
                                    maxent_raster_dir=all_path_stack$maxent_raster_dir,
                                    dir_resample_mask=dir_resample_mask)
}

run_maxent_kfold_cv <- function(this_bug,number_replicate,top_file_dir){
  ########################################
  # read bioclimatic and land cover data #
  ########################################
  
  clim_list <- list.files("/Users/vivianhuang/Desktop/resample_mask/historical/", pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  
  ########################################
  # all the saving paths                 #
  ########################################
  
  #read the occurence
  occ_raw_path <- paste("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/data/",this_bug,'.csv',sep = '')
  #/output folder saves all the output
  all_path_stack <- all_saving_paths(top_file_dir=top_file_dir,this_bug=this_bug)
  
  ########################################
  #       prepare the data.              #
  ########################################
  
  input_data_stack_with_folds <- prepare_input_data_kfold(occ_raw_path=occ_raw_path,
                                                          clim=clim,
                                                          number_of_folds=number_replicate,
                                                          maxent_result_dir=all_path_stack$maxent_result_dir)
  
  ########################################
  # run MaxEnt                           #
  ########################################
  
  knitr::opts_knit$set(root.dir = '/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM')
  opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
  
  utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.4.3/maxent.jar", 
                       destfile = paste0(system.file("java", package = "dismo"), 
                                         "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute
  
  # perform cross-validation
  this_input_data_stack <- input_data_stack_with_folds
  cv_result_list <- run_maxent_model_cv(list_x_train_full=this_input_data_stack$list_x_train_full,
                                        list_x_test_full=this_input_data_stack$list_x_test_full,
                                        list_pa_train=this_input_data_stack$list_pa_train,
                                        list_pa_test=this_input_data_stack$list_pa_test,
                                        list_p_train=this_input_data_stack$list_p_train,
                                        list_a_train=this_input_data_stack$list_a_train,
                                        list_p_test=this_input_data_stack$list_p_test,
                                        list_a_test=this_input_data_stack$list_a_test,
                                        maxent_evaluate_dir=all_path_stack$maxent_evaluate_dir,
                                        number_replicate=number_replicate,
                                        maxent_model_dir=all_path_stack$maxent_model_dir,
                                        metric_saving=T,
                                        model_saving=T)

  
  # train the model (for prediction)
  this_model <- run_maxent_model_training_all(maxent_evaluate_dir=all_path_stack$maxent_evaluate_dir,
                                              all_x_full=this_input_data_stack$all_x_full,
                                              all_pa=this_input_data_stack$all_pa,
                                              maxent_result_path=all_path_stack$maxent_result_path,
                                              dir_sub_name="kfold_all_input",
                                              maxent_model_dir=all_path_stack$maxent_result_path,
                                              model_saving=T)
  
  ########################################
  # predictions              #
  ########################################
  
  dir_resample_mask <- "/Users/vivianhuang/desktop/resample_mask"
  # perform predictions on all the cv models
  run_maxent_model_prediction_list(mod_list=cv_result_list$model_list,
                                   clim=clim,
                                   maxent_raster_dir=all_path_stack$maxent_raster_dir,
                                   dir_resample_mask=dir_resample_mask,
                                   dir_sub_name='cross_validation',
                                   number_replicate=number_replicate)
  # perform predictions on the model trained with all input data
  run_maxent_model_prediction_basic(mod=this_model,
                                    clim=clim,
                                    maxent_raster_dir=all_path_stack$maxent_raster_dir,
                                    dir_resample_mask=dir_resample_mask,
                                    dir_sub_name="kfold_all_input")
  
}


run_maxent_kfold_buffer_cv <- function(this_bug,number_replicate,top_file_dir){
  ########################################
  # read bioclimatic and land cover data #
  ########################################
  
  clim_list <- list.files("/Users/vivianhuang/Desktop/resample_mask/historical/", pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  
  ########################################
  # all the saving paths                 #
  ########################################
  
  #read the occurence
  occ_raw_path <- paste("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/data/",this_bug,'.csv',sep = '')
  #/output folder saves all the output
  all_path_stack <- all_saving_paths(top_file_dir=top_file_dir,this_bug=this_bug)
  
  ########################################
  #       prepare the data.              #
  ########################################
  # shapefile path for the vector having the shape of the reprojected and aligned input raster
  shapefile_path <-"/Users/vivianhuang/desktop/R-modeling-scripts/r_chagasM/masked_raster/shapefile.shp"

  input_data_stack_with_folds_buffer <- prepare_input_data_kfold_buffer(occ_raw_path=occ_raw_path,
                                                                        clim=clim,
                                                                        number_of_folds=number_replicate,
                                                                        shapefile_path=shapefile_path,
                                                                        maxent_result_dir=all_path_stack$maxent_result_dir,
                                                                        buff_width = 55555)

  ########################################
  # run MaxEnt                           #
  ########################################
  
  knitr::opts_knit$set(root.dir = '/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM')
  opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
  
  utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.4.3/maxent.jar", 
                       destfile = paste0(system.file("java", package = "dismo"), 
                                         "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute
  
  # perform cross-validation
  this_input_data_stack <- input_data_stack_with_folds_buffer
  cv_result_list <- run_maxent_model_cv(list_x_train_full=this_input_data_stack$list_x_train_full,
                                        list_x_test_full=this_input_data_stack$list_x_test_full,
                                        list_pa_train=this_input_data_stack$list_pa_train,
                                        list_pa_test=this_input_data_stack$list_pa_test,
                                        list_p_train=this_input_data_stack$list_p_train,
                                        list_a_train=this_input_data_stack$list_a_train,
                                        list_p_test=this_input_data_stack$list_p_test,
                                        list_a_test=this_input_data_stack$list_a_test,
                                        maxent_evaluate_dir=all_path_stack$maxent_evaluate_dir,
                                        number_replicate=number_replicate,
                                        maxent_model_dir=all_path_stack$maxent_model_dir,
                                        metric_saving=T,
                                        model_saving=T)
  
  # train the model (for prediction)
  this_model <- run_maxent_model_training_all(maxent_evaluate_dir=all_path_stack$maxent_evaluate_dir,
                                              all_x_full= this_input_data_stack$all_x_full,
                                              all_pa= this_input_data_stack$all_pa,
                                              maxent_result_path=all_path_stack$maxent_result_path,
                                              dir_sub_name="kfold_buffer_all_input",
                                              maxent_model_dir=all_path_stack$maxent_result_path,
                                              model_saving=T)

  ########################################
  # predictions              #
  ########################################
  
  dir_resample_mask <- "/Users/vivianhuang/desktop/resample_mask"
  
  # perform predictions on all the cv models
  run_maxent_model_prediction_list(mod_list=cv_result_list$model_list,
                                   clim=clim,
                                   maxent_raster_dir=all_path_stack$maxent_raster_dir,
                                   dir_resample_mask=dir_resample_mask,
                                   dir_sub_name='cross_validation',
                                   number_replicate=number_replicate)
  
  # perform predictions on the model trained with all input data
  run_maxent_model_prediction_basic(mod=this_model,
                                    clim=clim,
                                    maxent_raster_dir=all_path_stack$maxent_raster_dir,
                                    dir_resample_mask=dir_resample_mask,
                                    dir_sub_name="kfold_buffer_all_input")
  
}

this_bug = 'Rec'
number_replicate = 10
dir_sub_name = "all_input"#"kfold_buffer_all_input"
top_file_dir = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/kfold_buffer"


########################################
# read bioclimatic and land cover data #
########################################

# clim_list <- list.files("/Users/vivianhuang/Desktop/resample_mask/historical/", pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
# clim <- raster::stack(clim_list)

clim_dir <- "/Users/vivianhuang/Desktop/resample_mask/historical/"

########################################
# all the saving paths                 #
########################################

#read the occurence
occ_raw_path <- paste("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/data/",this_bug,'.csv',sep = '')
#/output folder saves all the output
all_path_stack <- all_saving_paths(top_file_dir=top_file_dir,this_bug=this_bug)

########################################
#       prepare the data.              #
########################################
# shapefile path for the vector having the shape of the reprojected and aligned input raster
shapefile_path <-"/Users/vivianhuang/desktop/R-modeling-scripts/r_chagasM/masked_raster/shapefile.shp"

this_input_data_stack <- prepare_input_data_kfold_buffer(occ_raw_path=occ_raw_path,
                                                         clim=clim_dir,
                                                         number_of_folds=number_replicate,
                                                         shapefile_path=shapefile_path,
                                                         maxent_result_dir=all_path_stack$maxent_result_dir,
                                                         buff_width = 55555)
########################################
# run MaxEnt                           #
########################################

knitr::opts_knit$set(root.dir = '/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM')
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.4.3/maxent.jar", 
                     destfile = paste0(system.file("java", package = "dismo"), 
                                       "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute

# perform cross-validation
cv_result_list <- run_maxent_model_cv(list_x_train_full=this_input_data_stack$list_x_train_full,
                                      list_x_test_full=this_input_data_stack$list_x_test_full,
                                      list_pa_train=this_input_data_stack$list_pa_train,
                                      list_pa_test=this_input_data_stack$list_pa_test,
                                      list_p_train=this_input_data_stack$list_p_train,
                                      list_a_train=this_input_data_stack$list_a_train,
                                      list_p_test=this_input_data_stack$list_p_test,
                                      list_a_test=this_input_data_stack$list_a_test,
                                      maxent_evaluate_dir=all_path_stack$maxent_evaluate_dir,
                                      number_replicate=number_replicate,
                                      maxent_model_dir=all_path_stack$maxent_model_dir,
                                      metric_saving=T,
                                      model_saving=T)

# train the model (for prediction)
this_model <- run_maxent_model_training_all(maxent_evaluate_dir=all_path_stack$maxent_evaluate_dir,
                                            all_x_full= this_input_data_stack$all_x_full,
                                            all_pa= this_input_data_stack$all_pa,
                                            maxent_result_path=all_path_stack$maxent_result_path,
                                            dir_sub_name=dir_sub_name,
                                            maxent_model_dir=all_path_stack$maxent_model_dir,
                                            model_saving=T)

########################################
# predictions              #
########################################


dir_resample_mask <- "/Users/vivianhuang/desktop/resample_mask"

cv_result_list_path <- paste(all_path_stack$maxent_evaluate_dir,'/cv_models.RDS',sep = '')
this_model_path <- paste(all_path_stack$maxent_evaluate_dir,'/',dir_sub_name,'_final_model_training_all.RDS',sep = '')
# perform predictions on all the cv models
rm(this_input_data_stack)

run_maxent_model_prediction_list(mod_list_path=cv_result_list_path,#cv_result_list$model_list,
                                 clim=clim_dir,
                                 maxent_raster_dir=all_path_stack$maxent_raster_dir,
                                 dir_resample_mask=dir_resample_mask,
                                 dir_sub_name='cross_validation',
                                 number_replicate=number_replicate)

# perform predictions on the model trained with all input data
run_maxent_model_prediction_basic(mod_path=this_model_path,
                                  clim=clim_dir,
                                  maxent_raster_dir=all_path_stack$maxent_raster_dir,
                                  dir_resample_mask=dir_resample_mask,
                                  dir_sub_name=dir_sub_name)

# run_maxent_kfold_buffer_cv(this_bug = this_bug, number_replicate = number_replicate,top_file_dir = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/kfold_buffer")
# 
# run_maxent_kfold_cv(this_bug = this_bug, number_replicate = number_replicate,top_file_dir = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/kfold")


# inspect_results <- readRDS("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/kfold_buffer/San/result/kfold_buffer_input_data_San.RDS")
# 
# 
# this_inspect_row <- as.data.frame(inspect_results$all_p)
# this_inspect_index <- unique(which(is.na(as.data.frame(inspect_results$all_p)), arr.ind=TRUE)[,1])
# 
# cat("\nRows with NA values are removed. Row indexs:",this_inspect_index)
# cat("\nRows with NA values are removed. Rows:")
# print(this_inspect_row[this_inspect_index,])
# after_delete <- this_inspect_row[-this_inspect_index,]