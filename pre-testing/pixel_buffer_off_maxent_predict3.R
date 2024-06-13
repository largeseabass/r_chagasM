library("sp")
library("rJava")
library("raster")
library("dismo")
library('sf')
library("knitr")
library("rprojroot")
library("caret")
library("Metrics")
run_maxent_model_prediction_single <- function(mod,this_item_name,clim_dir,maxent_raster_dir_this){
  startTime <- Sys.time()
  print(this_item_name)
  print(clim_dir)
  clim_list <- list.files(clim_dir, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  print("make prediction")
  ped <- predict(mod,clim)
  save_raster_path <- paste(maxent_raster_dir_this,"/",this_item_name,"_",this_bug,'.tif',sep = '')
  writeRaster(ped, filename =save_raster_path, format = "GTiff",overwrite=TRUE)
  endTime <- Sys.time()
  print(endTime-startTime)
}

run_maxent_model_prediction_basic <- function(mod_path,clim_dir,maxent_raster_dir,dir_resample_mask,dir_sub_name){
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
  
  maxent_raster_dir_this <- paste(maxent_raster_dir,'/',dir_sub_name,sep = '')
  print(maxent_raster_dir_this)
  if (!dir.exists(maxent_raster_dir_this)){
    dir.create(maxent_raster_dir_this)
  }else{
    print("dir exists")
  }
  
  ########################################
  # historical prediction                #
  ########################################
  
  run_maxent_model_prediction_single(mod=mod,
                                     this_item_name=paste("historical_predict","_allinput",sep = ''),
                                     clim_dir = clim_dir,
                                     maxent_raster_dir_this=maxent_raster_dir_this)
  
  ########################################
  # predictions for the future   SSP1    #
  ########################################
  run_maxent_model_prediction_single(mod=mod,
                                     this_item_name=paste("ssp126_predict","_allinput",sep = ''),
                                     clim_dir = dir_resample_mask_ssp1,
                                     maxent_raster_dir_this=maxent_raster_dir_this)
  
  ########################################
  # predictions for the future   SSP2    #
  ########################################
  
  run_maxent_model_prediction_single(mod=mod,
                                     this_item_name=paste("ssp245_predict","_allinput",sep = ''),
                                     clim_dir = dir_resample_mask_ssp2,
                                     maxent_raster_dir_this=maxent_raster_dir_this)
  
  ########################################
  # predictions for the future   SSP3    #
  ########################################
  
  run_maxent_model_prediction_single(mod=mod,
                                     this_item_name=paste("ssp370_predict","_allinput",sep = ''),
                                     clim_dir = dir_resample_mask_ssp3,
                                     maxent_raster_dir_this=maxent_raster_dir_this)
  
  ########################################
  # predictions for the future   SSP5    #
  ########################################
  
  run_maxent_model_prediction_single(mod=mod,
                                     this_item_name=paste("ssp585_predict","_allinput",sep = ''),
                                     clim_dir = dir_resample_mask_ssp5,
                                     maxent_raster_dir_this=maxent_raster_dir_this)
  
  endTime <- Sys.time()
  print(endTime-startTime)
}

########################################
# The Actual Run.                      #
########################################

bug_list <- list("Mex","Neo","Pro","Rub")
number_replicate <- 10
top_file_dir <- "/Users/liting/Documents/GitHub/r_chagasM/output/pixel_buffer_off"
input_file_dir <-"/Users/liting/Documents/GitHub/r_chagasM"
clim_dir <- "/Users/liting/Documents/data/resample_mask/historical/"
shapefile_path <-paste(input_file_dir,"/masked_raster/shapefile.shp",sep = '')

for (this_bug in bug_list){
  ########################################
  # all the saving paths                 #
  ########################################
  
  #read the occurence
  occ_raw_path <- paste(input_file_dir,"/data/",this_bug,'.csv',sep = '')
  
  ########################################
  # predictions              #
  # ########################################
  
  
  dir_resample_mask <- "/Users/liting/Documents/data/resample_mask"
  
  this_model_path <- paste(top_file_dir,'/',this_bug,'/result/model/all_input_final_model_training_all.RDS',sep = '')
  
  # perform predictions on the model trained with all input data
  run_maxent_model_prediction_basic(mod_path=this_model_path,
                                    clim=clim_dir,
                                    maxent_raster_dir=paste(top_file_dir,'/',this_bug,'/result/output_raster',sep = ''),
                                    dir_resample_mask=dir_resample_mask,
                                    dir_sub_name="all_input")
  
}