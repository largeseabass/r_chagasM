library("sp")
library('sf')
library("rJava")
library("raster")
library("dismo")
# library("rgeos")
library("knitr")
library("rprojroot")
library("caret")
library("Metrics")

###
# Author: Liting Huang
# Created Date: 2024.02.07.
# Raster PCA designed for large rasters made for Caret Package User.
# Usage: 
# all_saving_paths: generate saving paths under a given directory. You can skip this step.
# prepare_input_data_kfold_data: prepare input data (with PCA)
# This is designed for preparing input for kfold_cross_validation training and testing data. Read the descriptions in the front of the function for more details.
# 1) Use input data points to sample raster stack;
# 2) Perform PCA (here using the default 8 PCs) on input (tabular format) data (points with coordinates, then sampling raster stack using the points);
# 3) Save the PCA model.
# 4) Return input data and the PCA model.
# process_spatial_chunk: for small raster
# 1) Perform PCA on one chunk of raster stack by converting it to vector (data frame, each pixel in the raster gets converted to a row, with column showing the value of different variables)
# 2) Convert the dataframe with PCA values (here using the default 8 PCs) into 8 new raster layers (one for each PC), and return the raster layers. 
# process_raster_spatially: for large raster which gives java heap size error when processing with process_spatial_chunk
# 1) Determine the extent of the large raster (I use the input raster extent and modified the xmax to be -40 (search keywords: MODIFYEXTENT), you may want to change it)
# 2) crop the large raster into smaller chunks
# 3) feed each chunk into process_spatial_chunk and get a small PCA raster
# 4) merge these small PCA raster layers, and save the 8 PCA rasters.
###


all_saving_paths <- function(top_file_dir,this_bug){
  ###########
  # example all_saving_paths("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/","San")
  # top_file_dir: the directory to save all the output files
  # this_bug: specify which bug we are interested at, choose from: Dim, Ger, Ind, Lec, Lon, Max, Mex, Neo, Pro, Rec, Rub, San
  # return: a stack specify all the paths to save output files under top_file_dir
  ###########
  
  #subfolder for saving all the output (for all bugs), sanity check
  print(1)
  print(top_file_dir)
  if (!dir.exists(top_file_dir)){
    dir.create(top_file_dir)
  }else{
    print("dir exists")
  }
  
  #save all the output top_file_path/bug/
  save_all_output_dir <- paste(top_file_dir,"/",this_bug,sep = '')
  print(2)
  print(save_all_output_dir)
  if (!dir.exists(save_all_output_dir)){
    dir.create(save_all_output_dir)
  }else{
    print("dir exists")
  }
  
  
  
  #subfolder under save_all_output_path/result/ for maxent output
  maxent_result_dir <- paste(save_all_output_dir,'/result',sep = '')
  print(3)
  print(maxent_result_dir)
  if (!dir.exists(maxent_result_dir)){
    dir.create(maxent_result_dir)
  }else{
    print("dir exists")
  }
  
  #subfolder under save_all_output_path/result/ for maxent output
  maxent_evaluate_dir <- paste(save_all_output_dir,'/evaluate',sep = '')
  print(4)
  print(maxent_evaluate_dir)
  if (!dir.exists(maxent_evaluate_dir)){
    dir.create(maxent_evaluate_dir)
  }else{
    print("dir exists")
  }
  
  
  #subfolder under save_all_output_path/model/ for maxent model
  maxent_model_dir <- paste(maxent_result_dir,"/model",sep = '')
  print(5)
  print(maxent_model_dir)
  if (!dir.exists(maxent_model_dir)){
    dir.create(maxent_model_dir)
  }else{
    print("dir exists")
  }
  
  #subfolder under save_all_output_path/output_raster/ for maxent model
  maxent_raster_dir <- paste(maxent_result_dir,"/output_raster",sep = '')
  print(6)
  print(maxent_raster_dir)
  if (!dir.exists(maxent_raster_dir)){
    dir.create(maxent_raster_dir)
  }else{
    print("dir exists")
  }
  
  
  list_to_return <- list("maxent_result_dir"=maxent_result_dir, "maxent_evaluate_dir"=maxent_evaluate_dir,"maxent_model_dir"=maxent_model_dir,"maxent_raster_dir"=maxent_raster_dir )
  return(list_to_return)
}

prepare_input_data_kfold_pca <- function(occ_raw_path,clim_dir,number_of_folds,maxent_result_dir){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim_dir: the directory under which all input training rasters are stored (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  # maxent_result_dir: the directory to store the input data
  #
  # idea of this function:
  # conduct pca on the input data and create #(number_of_folds) dataframes and lists for cross validation
  # 
  # return:  
  # list of items (note: pa_train and pa_test has the same order of rows as x_train and x_test)
  # "list_pa_train" list of lists of 0 and 1, 0 indicates absence, 1 indicates presence, for training points
  # "list_pa_test"  list of lists of 0 and 1, 0 indicates absence, 1 indicates presence, for testing points
  # "list_x_train_full"  list of data frames with all the variable sampling for the training points (presence + absence) and coordinates
  # "list_x_test_full"  list of data frames with all the variable sampling for the testing points (presence + absence) and coordinates
  # "list_a_train" list of data frames with all the variable sampling for the absence training points and coordinates
  # "list_a_test" list of data frames with all the variable sampling for the absence testing points and coordinates
  # "list_p_train" list of data frames with all the variable sampling for the presence training points and coordinates
  # "list_p_test" list of data frames with all the variable sampling for the presence testing points and coordinates
  
  # "all_pa"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for all input points
  # "all_x_full"  data frame with all the variable sampling for the all input points (presence + absence) and coordinates
  # "all_a" data frame with all the variable sampling for all the absence points and coordinates
  # "all_p" data frame with all the variable sampling for all the presence points and coordinates
  # "pp_pca" the model used to conduct pca on input training data. we need it later to process the input data for trained model to give out predictions.
  
  
  cat("\nprepare input data...number of replicates: ",number_of_folds)
  startTime <- Sys.time()
  
  #### read clim
  clim_list <- list.files(clim_dir, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurrence data
  occ_raw <- read.csv(occ_raw_path)
  
  occ_only<- occ_raw[,c('DecimalLon',"DecimalLat")]
  colnames(occ_only) <- c('longitudes',"latitudes")
  coordinates(occ_only) <- ~longitudes + latitudes
  
  proj4string(occ_only)<- CRS("+proj=longlat +datum=WGS84 +no_defs")#CRS("+init=epsg:4326")
  
  
  ########################################
  # sample the background points         #
  ########################################
  
  occ_final <- occ_only
  
  studyArea <- clim
  
  set.seed(1) 
  cat('\nselect background points')
  bg <- sampleRandom(x=studyArea,
                     size=10000,
                     sp=T) # return spatial points
  
  set.seed(1)
  
  
  ########################################
  # all input data                     #
  ########################################
  
  # extracting env conditions for all occurrence points
  all_p <- as.data.frame(extract(clim, occ_only,sp = T))
  # extracting env conditions for all background points
  all_a <- as.data.frame(bg)
  names(all_a)[names(all_a) == "x"] <- "longitudes"
  names(all_a)[names(all_a) == "y"] <- "latitudes"
  # remove all the rows with NA value, print warning 
  this_inspect_index_p <- unique(which(is.na(all_p), arr.ind=TRUE)[,1])
  if(length(this_inspect_index_p) > 0){
    cat("\nNA values in occurrence points!")
    cat("\nRows with NA values are removed. Row indexs:",this_inspect_index_p)
    cat("\nRows with NA values are removed. Rows:")
    print(all_p[this_inspect_index_p,])
    all_p <- all_p[-this_inspect_index_p,]
    cat("\nPlease consider removing these rows from input datasets.")
  } 
  
  this_inspect_index_bg <- unique(which(is.na(as.data.frame(all_a)), arr.ind=TRUE)[,1])
  if(length(this_inspect_index_bg) > 0){
    cat("\nNA values in background points!")
    cat("\nRows with NA values are removed. Row indexs:",this_inspect_index_bg)
    cat("\nRows with NA values are removed. Rows:")
    print(all_a[this_inspect_index_bg,])
    all_a <- all_a[-this_inspect_index_bg,]
    cat("\nPlease double check input raster layers.")
  } 
  
  ########################################
  # process the input data in 3 steps    #
  # https://topepo.github.io/caret/pre-processing.html #
  ########################################
  combine_input_data_all <- as.data.frame(rbind(all_p, all_a))
  combine_input_data_lon <- combine_input_data_all[c("longitudes")]
  combine_input_data_lat <- combine_input_data_all[c("latitudes")]
  # zero or near-zero variance
  combine_input_data <- subset(combine_input_data_all,select = -c(longitudes, latitudes))
  nzv <- nearZeroVar(combine_input_data)
  combine_input_data_filtered <- combine_input_data[, -nzv]
  # linear dependencies
  comboInfo <- findLinearCombos(combine_input_data_filtered)
  if(length(comboInfo$remove)>0){
    combine_input_data_filtered <- combine_input_data_filtered[, -comboInfo$remove]
  }
  # principal component analysis (PCA)
  pp_pca <- preProcess(combine_input_data_filtered, method = c("pca"))
  pp_pca 
  combine_input_data_pca <- predict(pp_pca, newdata = combine_input_data_filtered)
  summary(pp_pca)
  # get the input data ready
  combine_input_data_pca$longitudes <- combine_input_data_lon
  combine_input_data_pca$latitudes <- combine_input_data_lat
  all_p <- head(combine_input_data_pca,nrow(all_p))
  all_a <- tail(combine_input_data_pca,nrow(all_a))
  ########################################
  # other input data                     #
  ########################################
  # prepare data for training
  all_pa <- c(rep(1, nrow(all_p)), rep(0, nrow(all_a)))
  all_x_full <- as.data.frame(rbind(all_p, all_a))
  ########################################
  # k-fold p and a.                      #
  ########################################
  # list[number_of_folds] of index to be hold out for each fold
  occ_only_dataframe <- as.data.frame(all_p)
  bg_dataframe <- as.data.frame(all_a)
  
  
  
  occ_kfold <- createFolds(occ_only_dataframe[[1]],k=number_of_folds) 
  bg_kfold <- createFolds(bg_dataframe[[1]],k=number_of_folds) 
  
  
  list_pa_train <- c()
  list_pa_test <- c()
  list_x_train_full <- c()
  list_x_test_full <- c()
  list_p_train <- c()
  list_a_train <- c()
  list_p_test <- c()
  list_a_test <- c()
  for (i in 1:number_of_folds){
    cat("\n",i)
    p_train <- occ_only_dataframe[-occ_kfold[[i]], ]  # this is the selection to be used for model training
    p_test <- occ_only_dataframe[occ_kfold[[i]], ]  # this is the opposite of the selection which will be used for model testing
    
    a_train <- bg_dataframe[-bg_kfold[[i]],]
    a_test <- bg_dataframe[bg_kfold[[i]],]
    
    pa_train <- c(rep(1, nrow(p_train)), rep(0, nrow(a_train)))
    pa_test <- c(rep(1, nrow(p_test)), rep(0, nrow(a_test)))
    x_train_full <- as.data.frame(rbind(p_train, a_train))
    x_test_full <- as.data.frame(rbind(p_test, a_test))
    
    list_pa_train <- c(list_pa_train,list(pa_train))
    list_pa_test <- c(list_pa_test,list(pa_test))
    list_x_train_full <- c(list_x_train_full,list(x_train_full))
    list_x_test_full <- c(list_x_test_full,list(x_test_full))
    list_p_train <- c(list_p_train,list(p_train))
    list_a_train <- c(list_a_train,list(a_train))
    list_p_test <- c(list_p_test,list(p_test))
    list_a_test <- c(list_a_test,list(a_test))
  }
  
  list_to_return <- list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"all_pa" =all_pa, "all_p"=all_p, "all_a"=all_a,"all_x_full"=all_x_full,"pp_pca"=pp_pca)
  saveRDS(list_to_return, file = paste(maxent_result_dir,"/kfold_input_data_",this_bug,".RDS",sep = ''))
  saveRDS(pp_pca, file = paste(maxent_result_dir,"/kfold_input_data_pca",this_bug,".RDS",sep = ''))
  endTime <- Sys.time()
  print(endTime-startTime)
  return(list_to_return)
}


# Called by process_raster_spatially to conduct pca on raster chunks
process_spatial_chunk <- function(chunk, pca_model) {
  # chunk: the raster layer to be processed
  # pca_model: the trained pca_model generated by the Caret package. For the training, please refer to prepare_input_data_kfold_pca
  # Assuming 'chunk' is a raster stack with all layers but a subset of the area
  pca_result <- predict(pca_model, newdata = na.omit(raster::values(chunk)))
  
  pca_raster_list <- list()
  for (i in 1:8){
    this_pcra <- chunk[[1]]
    this_pcra[!is.na(raster::values(chunk))] <- pca_result[, i]
    print(this_pcra)
    pca_raster_list[[i]] <- this_pcra
  }
  return(pca_raster_list)
}

# Main function to process the raster stack in spatial chunks. Divide raster into chunks, vectorize, PCA, rasterized, merge chunks together, and save to dir
process_raster_spatially <- function(raster_stack_path, pca_model, clim_type_name,num_chunks=40) {
  # raster_stack_path: the path to a directory under which all the files with .tif ending will be read as input raster and stack into a raster stack.
  # pca_model: the trained pca_model generated by the Caret package. For the training, please refer to prepare_input_data_kfold_pca
  # clim_type_name: the name of the directory to save the PCA raster layers under.
  # num_chunks=40: the number of small chunks to break the large raster layer into. As an example, for a raster layer covering the whole north america with 1km resolution, divide it into 40 chunks will ensure the PCA performs well.
  cat("start raster pca for all input data...")
  startTime <- Sys.time()
  processed_stack <- list()
  # process raster
  this_raster_stack_list <- list.files(raster_stack_path, pattern = '.tif$', full.names = T) 
  raster_stack <- raster::stack(this_raster_stack_list)
  this_raster_subset <- raster::subset(this_clim,names(pca_model$mean)) # not all the input variables are used to calculate PCA
  extent_raster <- extent(this_raster_subset)
  # crop the raster extent
  extent_raster@xmax <--40 #MODIFYEXTENT
  print(extent_raster)
  
  # Divide the extent into spatial chunks
  chunk_width <- (extent_raster@xmax - extent_raster@xmin) / num_chunks
  
  for (i in 1:40) {
    cat("\n",i)
    xmin_chunk <- extent_raster@xmin + (i - 1) * chunk_width
    xmax_chunk <- xmin_chunk + chunk_width
    chunk_extent <- extent(xmin_chunk, xmax_chunk, extent_raster@ymin, extent_raster@ymax)
    cat("\n crop")
    chunk <- crop(raster_stack, chunk_extent)
    this_raster_pca <- process_spatial_chunk(chunk, pca_model)
    print(this_raster_pca)
    processed_stack[[i]] <- this_raster_pca
  }
  
  # Combine processed chunks
  cat("\n merge")
  save_this_raster_pca_dir <- paste(all_path_stack$maxent_model_dir,"/",clim_type_name,sep = "")
  if (!dir.exists(save_this_raster_pca_dir)){
    dir.create(save_this_raster_pca_dir)
  }else{
    print("dir exists")
  }
  for (i in 1:8){
    raster_pci_list <-lapply(processed_stack, '[[', i)
    final_raster_pci <- do.call(merge,raster_pci_list)
    this_pca_save_path <- paste(save_this_raster_pca_dir,"/","PC",i,".tif",sep = "")
    cat(this_pca_save_path)
    raster::writeRaster(final_raster_pci, this_pca_save_path, format = "GTiff",overwrite=TRUE)
  }
  # end of testing
  endTime <- Sys.time()
  print(endTime-startTime)
  cat("finish pca")
  
}



# Example Usage:

this_bug = 'San'
number_replicate = 10 
top_file_dir = "/Users/liting/Documents/GitHub/r_chagasM/output/kfold_process" # where to save all the output
input_file_dir <-"/Users/liting/Documents/GitHub/r_chagasM" # directory to search for input files
clim_dir <- "/Users/liting/Documents/data/resample_mask/historical/" # directory to find historical climate data, all the rasters (ends with .tif) under this directory will be considered as inputs

########################################
# all the saving paths                 #
########################################

# read the occurrence data from input_file_dir/data/something.csv 
# you can change this to your own path
occ_raw_path <- paste(input_file_dir,"/data/",this_bug,'.csv',sep = '')
#/output folder saves all the output
all_path_stack <- all_saving_paths(top_file_dir=top_file_dir,this_bug=this_bug)

########################################
# prepare input data                   #
########################################

this_input_data_stack <- prepare_input_data_kfold_pca(occ_raw_path=occ_raw_path,
                                                      clim_dir=clim_dir,
                                                      number_of_folds=number_replicate,
                                                      maxent_result_dir=all_path_stack$maxent_result_dir)

##########################################
# perform pca on all input raster stacks #
##########################################
pca_model <- this_input_data_stack$pp_pca  # Assume pca_model is predefined
dir_resample_mask <- "/Users/liting/Documents/data/resample_mask"
# For historical data
process_raster_spatially(raster_stack_path=clim_dir, 
                         pca_model=pca_model, 
                         clim_type_name="historical")
# For SSP1

process_raster_spatially(raster_stack_path=paste(dir_resample_mask,'/ssp126_2071_2100/',sep = ''), 
                         pca_model=pca_model, 
                         clim_type_name="ssp126_2071_2100")
# For SSP2
process_raster_spatially(raster_stack_path=paste(dir_resample_mask,'/ssp245_2071_2100/',sep = ''), 
                         pca_model=pca_model, 
                         clim_type_name="ssp245_2071_2100")
# For SSP3
process_raster_spatially(raster_stack_path=paste(dir_resample_mask,'/ssp370_2071_2100/',sep = ''), 
                         pca_model=pca_model, 
                         clim_type_name="ssp370_2071_2100")
# For SSP5
process_raster_spatially(raster_stack_path=paste(dir_resample_mask,'/ssp585_2071_2100/',sep = ''), 
                         pca_model=pca_model, 
                         clim_type_name="ssp585_2071_2100")

