library("sp")
library("rJava")
library("raster")
library("dismo")
library('sf')
library("knitr")
library("rprojroot")
library("caret")
library("randomForest")
library("Metrics")


run_rf_model_prediction_single <- function(mod,this_item_name,clim_dir,ml_raster_dir_this){
  startTime <- Sys.time()
  print(this_item_name)
  print(clim_dir)
  clim_list <- list.files(clim_dir, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  print("make prediction")
  ped <- predict(clim, mod, type = "prob", index = 2)
  save_raster_path <- paste(ml_raster_dir_this,"/",this_item_name,"_",this_bug,'.tif',sep = '')
  writeRaster(ped, filename =save_raster_path, format = "GTiff",overwrite=TRUE)
  endTime <- Sys.time()
  print(endTime-startTime)
}

run_rf_model_prediction_basic <- function(mod_path,clim_dir,ml_raster_dir,dir_resample_mask,dir_sub_name){
  # take one trained maxent model
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  cat("\nbasic prediction")
  startTime <- Sys.time()
  mod <- readRDS(mod_path)
  dir_resample_mask_ssp1 <- paste(dir_resample_mask,'/ssp126_2071_2100/',sep = '')
  dir_resample_mask_ssp2 <- paste(dir_resample_mask,'/ssp245_2071_2100/',sep = '')
  dir_resample_mask_ssp3 <- paste(dir_resample_mask,'/ssp370_2071_2100/',sep = '')
  dir_resample_mask_ssp5 <- paste(dir_resample_mask,'/ssp585_2071_2100/',sep = '')
  
  ml_raster_dir_this <- paste(ml_raster_dir,'/',dir_sub_name,sep = '')
  print(ml_raster_dir_this)
  if (!dir.exists(ml_raster_dir_this)){
    dir.create(ml_raster_dir_this)
  }else{
    print("dir exists")
  }
  
  ########################################
  # historical prediction                #
  ########################################
  
  run_rf_model_prediction_single(mod=mod,
                                 this_item_name=paste("historical_predict","_allinput",sep = ''),
                                 clim_dir = clim_dir,
                                 ml_raster_dir_this=ml_raster_dir_this)
  
  ########################################
  # predictions for the future   SSP1    #
  ########################################
  run_rf_model_prediction_single(mod=mod,
                                 this_item_name=paste("ssp126_predict","_allinput",sep = ''),
                                 clim_dir = dir_resample_mask_ssp1,
                                 ml_raster_dir_this=ml_raster_dir_this)
  
  ########################################
  # predictions for the future   SSP2    #
  ########################################
  
  run_rf_model_prediction_single(mod=mod,
                                 this_item_name=paste("ssp245_predict","_allinput",sep = ''),
                                 clim_dir = dir_resample_mask_ssp2,
                                 ml_raster_dir_this=ml_raster_dir_this)
  
  ########################################
  # predictions for the future   SSP3    #
  ########################################
  
  run_rf_model_prediction_single(mod=mod,
                                 this_item_name=paste("ssp370_predict","_allinput",sep = ''),
                                 clim_dir = dir_resample_mask_ssp3,
                                 ml_raster_dir_this=ml_raster_dir_this)
  
  ########################################
  # predictions for the future   SSP5    #
  ########################################
  
  run_rf_model_prediction_single(mod=mod,
                                 this_item_name=paste("ssp585_predict","_allinput",sep = ''),
                                 clim_dir = dir_resample_mask_ssp5,
                                 ml_raster_dir_this=ml_raster_dir_this)
  
  endTime <- Sys.time()
  print(endTime-startTime)
}

run_rf_model_prediction_basic_ssp5only <- function(mod_path,clim_dir,ml_raster_dir,dir_resample_mask,dir_sub_name){
  # take one trained maxent model
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  cat("\nbasic prediction")
  startTime <- Sys.time()
  mod <- readRDS(mod_path)
  dir_resample_mask_ssp5 <- paste(dir_resample_mask,'/ssp585_2071_2100/',sep = '')
  
  ml_raster_dir_this <- paste(ml_raster_dir,'/',dir_sub_name,sep = '')
  print(ml_raster_dir_this)
  if (!dir.exists(ml_raster_dir_this)){
    dir.create(ml_raster_dir_this)
  }else{
    print("dir exists")
  }
  
  
  ########################################
  # predictions for the future   SSP5    #
  ########################################
  
  run_rf_model_prediction_single(mod=mod,
                                 this_item_name=paste("ssp585_predict","_allinput",sep = ''),
                                 clim_dir = dir_resample_mask_ssp5,
                                 ml_raster_dir_this=ml_raster_dir_this)
  
  endTime <- Sys.time()
  print(endTime-startTime)
}


########################################
# The Actual Run.                      #
########################################

bug_list <- list("San","Ger","Rec","Dim")
number_replicate <- 10
top_file_dir <- "/Users/liting/Documents/GitHub/r_chagasM/output/rf/pixel_buffer_off" # add an rf layer
species_input_dir <-"/Users/liting/Documents/GitHub/r_chagasM/output/pixel_buffer_off"
input_file_dir <-"/Users/liting/Documents/GitHub/r_chagasM"
clim_dir <- "/Users/liting/Documents/data/resample_mask/historical/"
shapefile_path <-paste(input_file_dir,"/masked_raster/shapefile.shp",sep = '')

for (this_bug in bug_list){
  # 1 create all saving path
  #all_path_stack <- all_ml_saving_paths(top_file_dir,this_bug)
  # 2 read all the input bug data
  # species_input_path <- paste(species_input_dir,"/",this_bug,"/result/kfold_buffer_input_data_",this_bug,".RDS",sep = '')
  # this_input_data_stack <- readRDS(species_input_path)
  # # 3 RF run_rf_model_cv
  # # perform cross-validation
  # cv_result_list <- run_rf_model_cv(list_x_train_full=this_input_data_stack$list_x_train_full,
  #                                       list_x_test_full=this_input_data_stack$list_x_test_full,
  #                                       list_pa_train=this_input_data_stack$list_pa_train,
  #                                       list_pa_test=this_input_data_stack$list_pa_test,
  #                                       list_p_train=this_input_data_stack$list_p_train,
  #                                       list_a_train=this_input_data_stack$list_a_train,
  #                                       list_p_test=this_input_data_stack$list_p_test,
  #                                       list_a_test=this_input_data_stack$list_a_test,
  #                                       ml_evaluate_dir=all_path_stack$ml_evaluate_dir,
  #                                       number_replicate=number_replicate,
  #                                       ml_model_dir=all_path_stack$ml_model_dir,
  #                                       metric_saving=T,
  #                                       model_saving=T)
  # 
  # # 4 RF run_rf_model_training_all
  # # train the model (for prediction)
  # this_model <- run_rf_model_training_all(ml_evaluate_dir=all_path_stack$ml_evaluate_dir,
  #                                             all_x_full= this_input_data_stack$all_x_full,
  #                                             all_pa= this_input_data_stack$all_pa,
  #                                             ml_result_path=all_path_stack$ml_result_path,
  #                                             dir_sub_name="all_input",
  #                                             ml_model_dir=all_path_stack$ml_model_dir,
  #                                             model_saving=T)
  # 
  # 
  # 5 run_rf_model_prediction_basic
  
  dir_resample_mask <- "/Users/liting/Documents/data/resample_mask"
  
  this_model_path <- paste(top_file_dir,'/',this_bug,'/result/model/all_input_final_model_training_all.RDS',sep = '')
  # perform predictions on the model trained with all input data
  run_rf_model_prediction_basic_ssp5only (mod_path=this_model_path,
                                clim=clim_dir,
                                ml_raster_dir=paste(top_file_dir,'/',this_bug,'/result/output_raster',sep = ''),
                                dir_resample_mask=dir_resample_mask,
                                dir_sub_name="all_input")
}