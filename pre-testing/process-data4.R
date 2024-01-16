# Run PCA for Grid Data

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


#source("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/pre-testing/process-data.R")


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

prepare_input_data_kfold_grid_pca <- function(occ_grid_path,clim_grid_path,number_of_folds,maxent_result_dir){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  #
  # idea of this function:
  # split the input data into training-testing sets
  # for the training set, further create #(number_of_folds) dataframes and lists for cross validation
  # 
  # return:  
  # list of items (note: pa_train and pa_test has the same order of rows as x_train and x_test)
  # "list_pa_train" list of lists of 0 and 1, 0 indicates absence, 1 indicates presence, for training points
  # "list_pa_test"  list of lists of 0 and 1, 0 indicates absence, 1 indicates presence, for testing points
  # "list_x_train_full"  list of data frames with all the variable sampling for the training points (presence + absence) and id
  # "list_x_test_full"  list of data frames with all the variable sampling for the testing points (presence + absence) and id
  # "list_a_train" list of data frames with all the variable sampling for the absence training points and id
  # "list_a_test" list of data frames with all the variable sampling for the absence testing points and id
  # "list_p_train" list of data frames with all the variable sampling for the presence training points and id
  # "list_p_test" list of data frames with all the variable sampling for the presence testing points and id
  
  # "all_pa"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for all input points
  # "all_x_full"  data frame with all the variable sampling for the all input points (presence + absence) and id
  # "all_a" data frame with all the variable sampling for all the absence points and id
  # "all_p" data frame with all the variable sampling for all the presence points and id
  
  
  cat("number of replicates: ",number_of_folds)
  cat("\ninput data preparation")
  startTime <- Sys.time()
  ########################################
  # read kissing bug related csv files   #
  ########################################
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
  # diff_set <- setdiff(subset(all_grid_historical[complete.cases(all_grid_historical), ],select = -c(left.y, top.y,right.y,bottom.y,X,left.x, top.x,right.x,bottom.x,count,forest_grassland)),
  #                     subset(clim_grid[complete.cases(clim_grid), ],select = -c(left, top,right,bottom,X,forest_grassland)))
  # 
  #background_area_values <- subset(diff_set,select = -c(id))
  ########################################
  # sample the background points         #
  ########################################
  
  background_grid_historical_values <- diff_set[sample(nrow(diff_set), 10000), ]
  
  ########################################
  # all input data                     #
  ########################################
  # extracting env conditions for all occurrence points
  all_p <- subset(occ_grid_historical_values,select = -c(count))
  # extracting env conditions for all background points
  all_a <- background_grid_historical_values
  
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
    cat("\nPlease double check input zonal statistic csv files.")
  } 
  
  
  ########################################
  # process the input data in 3 steps    #
  # https://topepo.github.io/caret/pre-processing.html #
  ########################################
  combine_input_data_all <- as.data.frame(rbind(all_p, all_a))
  combine_input_data_id <- combine_input_data_all$id

  # zero or near-zero variance
  combine_input_data <- subset(combine_input_data_all,select = -c(id))
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
  combine_input_data_pca$id <- combine_input_data_id
  all_p <- head(combine_input_data_pca,nrow(all_p))
  all_a <- tail(combine_input_data_pca,nrow(all_a))
  
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

prepare_input_data_kfold_buffer_grid_pca <- function(occ_grid_path,clim_grid_path,buffer_grid_path,number_of_folds,maxent_result_dir){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  #
  # idea of this function:
  # split the input data into training-testing sets
  # for the training set, further create #(number_of_folds) dataframes and lists for cross validation
  # 
  # return:  
  # list of items (note: pa_train and pa_test has the same order of rows as x_train and x_test)
  # "list_pa_train" list of lists of 0 and 1, 0 indicates absence, 1 indicates presence, for training points
  # "list_pa_test"  list of lists of 0 and 1, 0 indicates absence, 1 indicates presence, for testing points
  # "list_x_train_full"  list of data frames with all the variable sampling for the training points (presence + absence) and id
  # "list_x_test_full"  list of data frames with all the variable sampling for the testing points (presence + absence) and id
  # "list_a_train" list of data frames with all the variable sampling for the absence training points and id
  # "list_a_test" list of data frames with all the variable sampling for the absence testing points and id
  # "list_p_train" list of data frames with all the variable sampling for the presence training points and id
  # "list_p_test" list of data frames with all the variable sampling for the presence testing points and id
  
  # "all_pa"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for all input points
  # "all_x_full"  data frame with all the variable sampling for the all input points (presence + absence) and id
  # "all_a" data frame with all the variable sampling for all the absence points and id
  # "all_p" data frame with all the variable sampling for all the presence points and id
  
  
  cat("number of replicates: ",number_of_folds)
  cat("\ninput data preparation")
  startTime <- Sys.time()
  ########################################
  # read kissing bug related csv files   #
  ########################################
  #read occurence data
  occ_grid <- read.csv(occ_grid_path)
  #read clim data
  clim_grid <- read.csv(clim_grid_path)
  #read buffer data
  buffer_grid <- read.csv(buffer_grid_path)
  
  
  # all occurrence (get rid of all the NA rows)
  all_grid_historical_raw <- merge(x = occ_grid, y = clim_grid, by = "id")
  occ_grid_historical_values <- subset(all_grid_historical_raw[complete.cases(all_grid_historical_raw), ],select = -c(left.y, top.y,right.y,bottom.y,X,left.x, top.x,right.x,bottom.x,forest_grassland))
  # the whole area 
  all_grid_historical_values <- subset(clim_grid[complete.cases(clim_grid), ],select = -c(left, top,right,bottom,X,forest_grassland))
  # the area covered by buffer
  all_grid_buffer_values <- subset(buffer_grid[complete.cases(buffer_grid), ],select = -c(left, top,right,bottom))
  # the whole area - occurrence cells = the area to select background points from
  # delete rows by name
  diff_set_raw <-  all_grid_historical_values[ ! all_grid_historical_values$id %in% occ_grid_historical_values$id, ]
  diff_set <- diff_set_raw[ ! diff_set_raw$id %in% all_grid_buffer_values$id, ]
  ########################################
  # sample the background points         #
  ########################################
  
  background_grid_historical_values <- diff_set[sample(nrow(diff_set), 10000), ]
  
  ########################################
  # all input data                     #
  ########################################
  # extracting env conditions for all occurrence points
  all_p <- subset(occ_grid_historical_values,select = -c(count))
  # extracting env conditions for all background points
  all_a <- background_grid_historical_values
  
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
    cat("\nPlease double check input zonal statistic csv files.")
  } 
  
  ########################################
  # process the input data in 3 steps    #
  # https://topepo.github.io/caret/pre-processing.html #
  ########################################
  combine_input_data_all <- as.data.frame(rbind(all_p, all_a))
  combine_input_data_id <- combine_input_data_all$id
  
  # zero or near-zero variance
  combine_input_data <- subset(combine_input_data_all,select = -c(id))
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
  combine_input_data_pca$id <- combine_input_data_id
  all_p <- head(combine_input_data_pca,nrow(all_p))
  all_a <- tail(combine_input_data_pca,nrow(all_a))
  
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

run_maxent_model_cv_grid <- function(list_x_train_full,list_x_test_full,list_pa_train,list_pa_test,list_p_train,list_a_train,list_p_test,list_a_test,maxent_evaluate_dir,number_replicate,maxent_model_dir,metric_saving=T,model_saving=T){
  ### run maxent model with cross validations, calculate evaluation metrics, options to save evaluation metrics and all models
  # list_x_train_full,list_x_test_full,list_pa_train,list_pa_test: see prepare_input_data_kfold or prepare_input_data_kfold_buffer
  # maxent_evaluate_dir: the directory to save the output
  # number_replicate: the number of replicates created for k-fold cross validation, it needs to be the same as the value passed to prepare_input_data_kfold or prepare_input_data_kfold_buffer
  # metric_saving=T: save evaluation metrics to .RDS and .csv
  # model_saving=T: save list of models to .RDS 
  # return list of metric_result_list and model_list
  
  
  cat("\nstart to calculate cv")
  startTime <- Sys.time()
  
  replicate_string <- paste("replicates=",number_replicate,sep='')
  metric_result_list <- list()
  model_list <- list()
  for (i in 1:number_replicate){
    cat("\n",i)
    pder_train <- subset(list_x_train_full[[i]],select = -c(id))
    pa_train <- list_pa_train[[i]]
    #subfolder under save_all_output_path/result/ for maxent output
    maxent_evaluate_dir_this <- paste(maxent_evaluate_dir,'/',i,sep = '')
    print(maxent_evaluate_dir_this)
    if (!dir.exists(maxent_evaluate_dir_this)){
      dir.create(maxent_evaluate_dir_this)
    }else{
      print("dir exists")
    }
    
    mod <- maxent(x=pder_train, ## env conditions
                  p=pa_train,   ## 1:presence or 0:absence; occurence data + background points
                  path=paste0(maxent_evaluate_dir_this), ## folder for maxent output; 
                  # if we do not specify a folder R will put the results in a temp file, 
                  # and it gets messy to read those. . .
                  args=c("responsecurves","jackknife") ## parameter specification
    )
    actual_results <- list_pa_test[[i]]
    predicted_results <- predict(mod,subset(list_x_test_full[[i]],select = -c(id)))
    this_auc <- auc(actual_results, predicted_results)
    this_bias <- bias(actual_results, predicted_results)
    this_mae <- mae(actual_results, predicted_results)
    # calculate TSS
    # get threshold from training set and then use this threshold in testing set
    this_p_train <- subset(list_p_train[[i]],select = -c(id))
    this_a_train <- subset(list_a_train[[i]],select = -c(id))
    this_evaluate_train <- dismo::evaluate(p = this_p_train, a = this_a_train, model = mod)
    this_tss_original_list <-this_evaluate_train@TPR + this_evaluate_train@TNR - 1.0
    this_threshold_index <- which(this_tss_original_list == max(this_tss_original_list))[[1]]
    this_threshold <- this_evaluate_train@t[this_threshold_index]
    this_p_test <- subset(list_p_test[[i]],select = -c(id))
    this_a_test <- subset(list_a_test[[i]],select = -c(id))
    this_evaluate_test <- dismo::evaluate(p = this_p_test, a = this_a_test, model = mod,tr=this_threshold)
    this_test_tss <- max(this_evaluate_test@TPR + this_evaluate_test@TNR - 1.0)
    
    this_metric_list = list("mae"=this_mae,"tss"=this_test_tss,"bias"=this_bias,"auc"=this_auc,"tss_threshold"=this_threshold)
    metric_result_list[[i]] <- this_metric_list
    model_list[[i]] <- mod
    
  }
  
  if (metric_saving){
    csv_saving_path <- paste(maxent_evaluate_dir,'/cv_metric_results.csv',sep = '')
    write.csv(metric_result_list, file = csv_saving_path)
    rds_saving_path <- paste(maxent_evaluate_dir,'/cv_metric_results.RDS',sep = '')
    saveRDS(metric_result_list, file = rds_saving_path)
  }
  
  if (model_saving){
    model_saving_path <- paste(maxent_evaluate_dir,'/cv_models.RDS',sep = '')
    saveRDS(model_list, file = model_saving_path)
  }
  endTime <- Sys.time()
  print(endTime-startTime)
  return(list("metric_result_list"=metric_result_list,"model_list"=model_list))
}

run_maxent_model_training_basic_grid <- function(maxent_evaluate_dir,final_x_train,final_x_test,final_pa_train,final_pa_test,final_p_train,final_a_train,final_p_test,final_a_test,maxent_model_dir,metric_saving=T,model_saving=T){
  # run maxent model with given training set and calculate evaluation metric (with testing set)
  # maxent_evaluate_dir: the directory to save results
  # final_x_train,final_x_test,final_pa_train,final_pa_test,final_p_train,final_a_train,final_p_test,final_a_test,maxent_model_dir: : see prepare_input_data_kfold or prepare_input_data_kfold_buffer
  # metric_saving=T: if true, save the evaluation metrices to .RDS and .csv files
  # model_saving=T: if true, save the model to .RDS file
  # return the trained model
  cat("\nbasic training")
  startTime <- Sys.time()
  pder_train <- subset(final_x_train,select = -c(id))
  pa_train <-final_pa_train
  maxent_evaluate_dir_this <- paste(maxent_evaluate_dir,'/basic_final',sep = '')
  print(maxent_evaluate_dir_this)
  if (!dir.exists(maxent_evaluate_dir_this)){
    dir.create(maxent_evaluate_dir_this)
  }else{
    print("dir exists")
  }
  
  mod <- maxent(x=pder_train, ## env conditions
                p=pa_train,   ## 1:presence or 0:absence; occurence data + background points
                path=paste0(maxent_evaluate_dir_this), ## folder for maxent output; 
                # if we do not specify a folder R will put the results in a temp file, 
                # and it gets messy to read those. . .
                args=c("responsecurves","jackknife") ## parameter specification
  )
  actual_results <- final_pa_test
  predicted_results <- predict(mod,subset(final_x_test,select = -c(id)))
  this_auc <- auc(actual_results, predicted_results)
  this_bias <- bias(actual_results, predicted_results)
  this_mae <- mae(actual_results, predicted_results)
  # calculate TSS
  # get threshold from training set and then use this threshold in testing set
  this_p_train <- subset(final_p_train,select = -c(id))
  this_a_train <- subset(final_a_train,select = -c(id))
  this_evaluate_train <- dismo::evaluate(p = this_p_train, a = this_a_train, model = mod)
  this_tss_original_list <-this_evaluate_train@TPR + this_evaluate_train@TNR - 1.0
  this_threshold_index <- which(this_tss_original_list == max(this_tss_original_list))
  print(this_threshold_index)
  print(this_threshold_index[[1]])
  this_threshold <- this_evaluate_train@t[this_threshold_index]
  this_p_test <- subset(final_p_test,select = -c(id))
  this_a_test <- subset(final_a_test,select = -c(id))
  this_evaluate_test <- dismo::evaluate(p = this_p_test, a = this_a_test, model = mod,tr=this_threshold)
  this_test_tss <- max(this_evaluate_test@TPR + this_evaluate_test@TNR - 1.0)
  
  this_metric_list = list("mae"=this_mae,"tss"=this_test_tss,"bias"=this_bias,"auc"=this_auc,"tss_threshold"=this_threshold)
  
  if (metric_saving){
    csv_saving_path <- paste(maxent_evaluate_dir,'/basic_final_metric_results_training_basic.csv',sep = '')
    write.csv(this_metric_list, file = csv_saving_path)
    rds_saving_path <- paste(maxent_evaluate_dir,'/basic_final_metric_results_training_basic.RDS',sep = '')
    saveRDS(this_metric_list, file = rds_saving_path)
  }
  
  if (model_saving){
    # also save the model
    maxent_model_path <- paste(maxent_model_dir,'/basic_final_model_training_basic.RDS',sep = '')
    saveRDS(mod, file = maxent_model_path)
  }
  endTime <- Sys.time()
  print(endTime-startTime)
  return(mod)
}

run_maxent_model_training_all_grid <- function(maxent_evaluate_dir,all_x_full,all_pa,maxent_result_path,dir_sub_name,maxent_model_dir,model_saving=T){
  # run maxent model with all input data (hence no testing sets available for evaluation metric calculation)
  # maxent_evaluate_dir: the directory to save results
  # "all_pa"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for all input points
  # "all_x_full"  data frame with all the variable sampling for the all input points (presence + absence) and coordinates
  # model_saving=T: if true, save the model to .RDS file
  # return the trained model
  cat("\nall-data training")
  startTime <- Sys.time()
  pder_train <- subset(all_x_full,select = -c(id))
  pa_train <- all_pa
  maxent_evaluate_dir_this <- paste(maxent_evaluate_dir,'/',dir_sub_name,sep = '')
  print(maxent_evaluate_dir_this)
  if (!dir.exists(maxent_evaluate_dir_this)){
    dir.create(maxent_evaluate_dir_this)
  }else{
    print("dir exists")
  }
  
  mod <- maxent(x=pder_train, ## env conditions
                p=pa_train,   ## 1:presence or 0:absence; occurence data + background points
                path=paste0(maxent_evaluate_dir_this), ## folder for maxent output; 
                # if we do not specify a folder R will put the results in a temp file, 
                # and it gets messy to read those. . .
                args=c("responsecurves","jackknife") ## parameter specification
  )
  print("test")
  if (model_saving){
    # also save the model
    maxent_model_path <- paste(maxent_model_dir,'/',dir_sub_name,'_final_model_training_all.RDS',sep = '')
    print("test")
    print(maxent_model_path)
    saveRDS(mod, file = maxent_model_path)
  }
  endTime <- Sys.time()
  print(endTime-startTime)
  return(mod)
}

run_maxent_model_prediction_single_pca <- function(mod,this_item_name,grid_path_list_this,maxent_raster_dir_this,pp_pca,historical=F){
  startTime <- Sys.time()
  print(this_item_name)
  
  if(historical){
    this_grid <- read.csv(grid_path_list_this)
    this_all_area_grid <- subset(this_grid[complete.cases(this_grid), ],select = -c(left, top,right,bottom,X,forest_grassland))
  }else{
    clim_list1_1 <- subset(read.csv(grid_path_list_this[[1]]),select = -c(left, top,right,bottom))
    clim_list1_2 <- subset(read.csv(grid_path_list_this[[2]]),select = -c(left, top,right,bottom,MAR))
    this_all_area_grid <- merge(x = clim_list1_1, y = clim_list1_2, by = "id")
  }
  this_all_area_values <- this_all_area_grid[complete.cases(this_all_area_grid), ]
  this_cell_id <-  this_all_area_values$id
  this_all_area_grid_no_id <- subset(this_all_area_values,select = -c(id))
  print("make prediction")
  # add the PCA part in
  this_all_area_grid_no_id <- predict(pp_pca, newdata = this_all_area_grid_no_id)
  ped <- predict(mod,this_all_area_grid_no_id)
  # save csv file
  final_prediction <- as.data.frame(list(id = this_cell_id,
                                         prediction = ped))
  
  save_csv_path <- paste(maxent_raster_dir_this,"/",this_item_name,'_',this_bug,'.csv',sep = '')
  write.csv(final_prediction, file = save_csv_path)
  endTime <- Sys.time()
  print(endTime-startTime)
}

run_maxent_model_prediction_basic_grid_pca_old <- function(mod,grid_path_list,dir_sub_name,pp_pca,maxent_raster_dir){
  # take one trained maxent model
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  cat("\nbasic prediction")
  startTime <- Sys.time()
  
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
  #  
  print("historical_clim")
  historical_grid <- read.csv(grid_path_list$historical_all)
  historical_all_area_grid <- subset(historical_grid[complete.cases(historical_grid), ],select = -c(left, top,right,bottom,X,forest_grassland))
  historical_cell_id <-  historical_all_area_grid$id
  historical_all_area_grid_no_id <- subset(historical_all_area_grid,select = -c(id))
  historical_all_area_grid_no_id <- predict(pp_pca, newdata = historical_all_area_grid_no_id)
  ped <- predict(mod,historical_all_area_grid_no_id)
  # save csv file
  ped['id'] <- historical_cell_id
  save_csv_path <- paste(maxent_raster_dir_this,"/historical_predict_",this_bug,'.csv',sep = '')
  write.csv(ped, file = save_csv_path)
  
  endTime <- Sys.time()
  print(endTime-startTime)
  ########################################
  # predictions for the future   SSP1    #
  ########################################
  
  clim_list1_1 <- subset(read.csv(grid_path_list$ssp1_lc),select = -c(left, top,right,bottom))
  clim_list1_2 <- subset(read.csv(grid_path_list$ssp1_bc),select = -c(left, top,right,bottom))
  
  all_grid_clim1_raw <- merge(x = clim_list1_1, y = clim_list1_2, by = "id")
  occ_grid_clim1_values <- all_grid_clim1_raw[complete.cases(all_grid_clim1_raw), ]
  clim1_cell_id <- occ_grid_clim1_values$id
  clim1_all_area_grid_no_id <- subset(occ_grid_clim1_values,select = -c(id))
  clim1_all_area_grid_no_id <- predict(pp_pca, newdata = clim1_all_area_grid_no_id)
  ped1 <- predict(mod,clim1_all_area_grid_no_id)
  # save csv file
  ped1['id'] <- clim1_cell_id
  save_csv_path1 <- paste(maxent_raster_dir_this,"/ssp126_predict_",this_bug,'.csv',sep = '')
  write.csv(ped1, file = save_csv_path1)
  
  ########################################
  # predictions for the future   SSP2    #
  ########################################
  
  clim_list2_1 <- subset(read.csv(grid_path_list$ssp2_lc),select = -c(left, top,right,bottom))
  clim_list2_2 <- subset(read.csv(grid_path_list$ssp2_bc),select = -c(left, top,right,bottom))
  
  all_grid_clim2_raw <- merge(x = clim_list2_1, y = clim_list2_2, by = "id")
  occ_grid_clim2_values <- all_grid_clim2_raw[complete.cases(all_grid_clim2_raw), ]
  clim2_cell_id <- occ_grid_clim2_values$id
  clim2_all_area_grid_no_id <- subset(occ_grid_clim2_values,select = -c(id))
  clim2_all_area_grid_no_id <- predict(pp_pca, newdata = clim2_all_area_grid_no_id)
  ped2 <- predict(mod,clim2_all_area_grid_no_id)
  # save csv file
  ped2['id'] <- clim2_cell_id
  save_csv_path2 <- paste(maxent_raster_dir_this,"/ssp245_predict_",this_bug,'.csv',sep = '')
  write.csv(ped2, file = save_csv_path2)
  
  ########################################
  # predictions for the future   SSP3    #
  ########################################
  
  clim_list3_1 <- subset(read.csv(grid_path_list$ssp3_lc),select = -c(left, top,right,bottom))
  clim_list3_2 <- subset(read.csv(grid_path_list$ssp3_bc),select = -c(left, top,right,bottom))
  
  all_grid_clim3_raw <- merge(x = clim_list3_1, y = clim_list3_2, by = "id")
  occ_grid_clim3_values <- all_grid_clim3_raw[complete.cases(all_grid_clim3_raw), ]
  clim3_cell_id <- occ_grid_clim3_values$id
  clim3_all_area_grid_no_id <- subset(occ_grid_clim3_values,select = -c(id))
  clim3_all_area_grid_no_id <- predict(pp_pca, newdata = clim3_all_area_grid_no_id)
  ped3 <- predict(mod,clim3_all_area_grid_no_id)
  # save csv file
  ped3['id'] <- clim3_cell_id
  save_csv_path3 <- paste(maxent_raster_dir_this,"/ssp370_predict_",this_bug,'.csv',sep = '')
  write.csv(ped3, file = save_csv_path3)
  ########################################
  # predictions for the future   SSP5    #
  ########################################
  
  clim_list5_1 <- subset(read.csv(grid_path_list$ssp5_lc),select = -c(left, top,right,bottom))
  clim_list5_2 <- subset(read.csv(grid_path_list$ssp5_bc),select = -c(left, top,right,bottom))
  
  all_grid_clim5_raw <- merge(x = clim_list5_1, y = clim_list5_2, by = "id")
  occ_grid_clim5_values <- all_grid_clim5_raw[complete.cases(all_grid_clim5_raw), ]
  clim5_cell_id <- occ_grid_clim5_values$id
  clim5_all_area_grid_no_id <- subset(occ_grid_clim5_values,select = -c(id))
  clim5_all_area_grid_no_id <- predict(pp_pca, newdata = clim5_all_area_grid_no_id)
  ped5 <- predict(mod,clim5_all_area_grid_no_id)
  # save csv file
  ped5['id'] <- clim5_cell_id
  save_csv_path5 <- paste(maxent_raster_dir_this,"/ssp585_predict_",this_bug,'.csv',sep = '')
  write.csv(ped5, file = save_csv_path5)
  
  endTime <- Sys.time()
  print(endTime-startTime)
}

run_maxent_model_prediction_basic_grid_pca <- function(mod,grid_path_list,dir_sub_name,pp_pca,maxent_raster_dir){
  # take one trained maxent model
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  cat("\nbasic prediction")
  startTime <- Sys.time()
  
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
  #  
  startTime <- Sys.time()
  run_maxent_model_prediction_single_pca(mod=mod,
                                        this_item_name=paste("historical_predict","_allinput",sep = ''),
                                        grid_path_list_this = grid_path_list$historical_all,
                                        maxent_raster_dir_this=maxent_raster_dir_this,
                                        pp_pca=pp_pca,
                                        historical=T)
  
  print("historical_clim")
  
  endTime <- Sys.time()
  print(endTime-startTime)
  
  ########################################
  # predictions for the future   SSP1    #
  ########################################
  
  startTime <- Sys.time()
  run_maxent_model_prediction_single_pca(mod=mod,
                                         this_item_name=paste("ssp126","_allinput",sep = ''),
                                         grid_path_list_this = c(grid_path_list$ssp1_lc,grid_path_list$ssp1_bc),
                                         maxent_raster_dir_this=maxent_raster_dir_this,
                                         pp_pca=pp_pca,
                                         historical=F)
  
  print("ssp1")
  endTime <- Sys.time()
  print(endTime-startTime)
  
  ########################################
  # predictions for the future   SSP2    #
  ########################################
  
  startTime <- Sys.time()
  run_maxent_model_prediction_single_pca(mod=mod,
                                         this_item_name=paste("ssp245","_allinput",sep = ''),
                                         grid_path_list_this = c(grid_path_list$ssp2_lc,grid_path_list$ssp2_bc),
                                         maxent_raster_dir_this=maxent_raster_dir_this,
                                         pp_pca=pp_pca,
                                         historical=F)
  
  print("ssp2")
  endTime <- Sys.time()
  print(endTime-startTime)
  
  ########################################
  # predictions for the future   SSP3    #
  ########################################
  
  startTime <- Sys.time()
  run_maxent_model_prediction_single_pca(mod=mod,
                                         this_item_name=paste("ssp370","_allinput",sep = ''),
                                         grid_path_list_this = c(grid_path_list$ssp3_lc,grid_path_list$ssp3_bc),
                                         maxent_raster_dir_this=maxent_raster_dir_this,
                                         pp_pca=pp_pca,
                                         historical=F)
  
  print("ssp3")
  endTime <- Sys.time()
  print(endTime-startTime)
  
  ########################################
  # predictions for the future   SSP5    #
  ########################################
  
  startTime <- Sys.time()
  run_maxent_model_prediction_single_pca(mod=mod,
                                         this_item_name=paste("ssp585","_allinput",sep = ''),
                                         grid_path_list_this = c(grid_path_list$ssp5_lc,grid_path_list$ssp5_bc),
                                         maxent_raster_dir_this=maxent_raster_dir_this,
                                         pp_pca=pp_pca,
                                         historical=F)
  print("ssp5")
  endTime <- Sys.time()
  print(endTime-startTime)
  
}

run_maxent_model_prediction_list_grid_pca_old <- function(mod_list,grid_path_list,dir_sub_name,number_replicate,pp_pca,maxent_raster_dir){
  # take a list of trained maxent models
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  
  # mod_list: list of trained maxent models
  # clim: historical bioclimatic rasters (aligned)
  # maxent_raster_dir: the directory under which the bioclimatic rasters (aligned) are stored, with rasters for ssp1 - ssp5 stored under corresponding subfolders.
  
  cat("\nmodel-list prediction")
  startTime <- Sys.time()
  # dir_resample_mask_ssp1 <- paste(dir_resample_mask,'/ssp126_2071_2100/',sep = '')
  # dir_resample_mask_ssp2 <- paste(dir_resample_mask,'/ssp245_2071_2100/',sep = '')
  # dir_resample_mask_ssp3 <- paste(dir_resample_mask,'/ssp370_2071_2100/',sep = '')
  # dir_resample_mask_ssp5 <- paste(dir_resample_mask,'/ssp585_2071_2100/',sep = '')
  # 
  maxent_raster_dir_this <- paste(maxent_raster_dir,'/',dir_sub_name,sep = '')
  print(maxent_raster_dir_this)
  if (!dir.exists(maxent_raster_dir_this)){
    dir.create(maxent_raster_dir_this)
  }else{
    print("dir exists")
  }
  for (i in 1:number_replicate){
    cat("\n",i)
    mod <- mod_list[[i]]
    ########################################
    # historical prediction                #
    ########################################
    #  
    startTime <- Sys.time()
    print("historical_clim")
    historical_grid <- read.csv(grid_path_list$historical_all)
    historical_all_area_grid <- subset(historical_grid[complete.cases(historical_grid), ],select = -c(left, top,right,bottom,X,forest_grassland))
    historical_cell_id <-  historical_all_area_grid$id
    historical_all_area_grid_no_id <- subset(historical_all_area_grid,select = -c(id))
    historical_all_area_grid_no_id <- predict(pp_pca, newdata = historical_all_area_grid_no_id)
    ped <- predict(mod,historical_all_area_grid_no_id)
    # save csv file
    ped['id'] <- historical_cell_id
    save_csv_path <- paste(maxent_raster_dir_this,"/historical_predict_",this_bug,'_',i,'.csv',sep = '')
    write.csv(ped, file = save_csv_path)
    
    endTime <- Sys.time()
    print(endTime-startTime)
    ########################################
    # predictions for the future   SSP1    #
    ########################################
    
    clim_list1_1 <- subset(read.csv(grid_path_list$ssp1_lc),select = -c(left, top,right,bottom))
    clim_list1_2 <- subset(read.csv(grid_path_list$ssp1_bc),select = -c(left, top,right,bottom))
    
    all_grid_clim1_raw <- merge(x = clim_list1_1, y = clim_list1_2, by = "id")
    occ_grid_clim1_values <- all_grid_clim1_raw[complete.cases(all_grid_clim1_raw), ]
    clim1_cell_id <- occ_grid_clim1_values$id
    clim1_all_area_grid_no_id <- subset(occ_grid_clim1_values,select = -c(id))
    clim1_all_area_grid_no_id <- predict(pp_pca, newdata = clim1_all_area_grid_no_id)
    ped1 <- predict(mod,clim1_all_area_grid_no_id)
    # save csv file
    ped1['id'] <- clim1_cell_id
    save_csv_path1 <- paste(maxent_raster_dir_this,"/ssp126_predict_",this_bug,'_',i,'.csv',sep = '')
    write.csv(ped1, file = save_csv_path1)
    
    ########################################
    # predictions for the future   SSP2    #
    ########################################
    
    clim_list2_1 <- subset(read.csv(grid_path_list$ssp2_lc),select = -c(left, top,right,bottom))
    clim_list2_2 <- subset(read.csv(grid_path_list$ssp2_bc),select = -c(left, top,right,bottom))
    
    all_grid_clim2_raw <- merge(x = clim_list2_1, y = clim_list2_2, by = "id")
    occ_grid_clim2_values <- all_grid_clim2_raw[complete.cases(all_grid_clim2_raw), ]
    clim2_cell_id <- occ_grid_clim2_values$id
    clim2_all_area_grid_no_id <- subset(occ_grid_clim2_values,select = -c(id))
    clim2_all_area_grid_no_id <- predict(pp_pca, newdata = clim2_all_area_grid_no_id)
    ped2 <- predict(mod,clim2_all_area_grid_no_id)
    # save csv file
    ped2['id'] <- clim2_cell_id
    save_csv_path2 <- paste(maxent_raster_dir_this,"/ssp245_predict_",this_bug,'_',i,'.csv',sep = '')
    write.csv(ped2, file = save_csv_path2)
    
    ########################################
    # predictions for the future   SSP3    #
    ########################################
    
    clim_list3_1 <- subset(read.csv(grid_path_list$ssp3_lc),select = -c(left, top,right,bottom))
    clim_list3_2 <- subset(read.csv(grid_path_list$ssp3_bc),select = -c(left, top,right,bottom))
    
    all_grid_clim3_raw <- merge(x = clim_list3_1, y = clim_list3_2, by = "id")
    occ_grid_clim3_values <- all_grid_clim3_raw[complete.cases(all_grid_clim3_raw), ]
    clim3_cell_id <- occ_grid_clim3_values$id
    clim3_all_area_grid_no_id <- subset(occ_grid_clim3_values,select = -c(id))
    clim3_all_area_grid_no_id <- predict(pp_pca, newdata = clim3_all_area_grid_no_id)
    ped3 <- predict(mod,clim3_all_area_grid_no_id)
    # save csv file
    ped3['id'] <- clim3_cell_id
    save_csv_path3 <- paste(maxent_raster_dir_this,"/ssp370_predict_",this_bug,'_',i,'.csv',sep = '')
    write.csv(ped3, file = save_csv_path3)
    ########################################
    # predictions for the future   SSP5    #
    ########################################
    
    clim_list5_1 <- subset(read.csv(grid_path_list$ssp5_lc),select = -c(left, top,right,bottom))
    clim_list5_2 <- subset(read.csv(grid_path_list$ssp5_bc),select = -c(left, top,right,bottom))
    
    all_grid_clim5_raw <- merge(x = clim_list5_1, y = clim_list5_2, by = "id")
    occ_grid_clim5_values <- all_grid_clim5_raw[complete.cases(all_grid_clim5_raw), ]
    clim5_cell_id <- occ_grid_clim5_values$id
    clim5_all_area_grid_no_id <- subset(occ_grid_clim5_values,select = -c(id))
    clim5_all_area_grid_no_id <- predict(pp_pca, newdata = clim5_all_area_grid_no_id)
    ped5 <- predict(mod,clim5_all_area_grid_no_id)
    # save csv file
    ped5['id'] <- clim5_cell_id
    save_csv_path5 <- paste(maxent_raster_dir_this,"/ssp585_predict_",this_bug,'_',i,'.csv',sep = '')
    write.csv(ped5, file = save_csv_path5)
  }
  endTime <- Sys.time()
  print(endTime-startTime)
}

run_maxent_model_prediction_list_grid_pca <- function(mod_list,grid_path_list,dir_sub_name,number_replicate,pp_pca,maxent_raster_dir){
  # take a list of trained maxent models
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  
  # mod_list: list of trained maxent models
  # clim: historical bioclimatic rasters (aligned)
  # maxent_raster_dir: the directory under which the bioclimatic rasters (aligned) are stored, with rasters for ssp1 - ssp5 stored under corresponding subfolders.
  
  cat("\nmodel-list prediction")
  startTime <- Sys.time()
  # dir_resample_mask_ssp1 <- paste(dir_resample_mask,'/ssp126_2071_2100/',sep = '')
  # dir_resample_mask_ssp2 <- paste(dir_resample_mask,'/ssp245_2071_2100/',sep = '')
  # dir_resample_mask_ssp3 <- paste(dir_resample_mask,'/ssp370_2071_2100/',sep = '')
  # dir_resample_mask_ssp5 <- paste(dir_resample_mask,'/ssp585_2071_2100/',sep = '')
  # 
  maxent_raster_dir_this <- paste(maxent_raster_dir,'/',dir_sub_name,sep = '')
  print(maxent_raster_dir_this)
  if (!dir.exists(maxent_raster_dir_this)){
    dir.create(maxent_raster_dir_this)
  }else{
    print("dir exists")
  }
  for (i in 1:number_replicate){
    cat("\n",i)
    mod <- mod_list[[i]]
    ########################################
    # historical prediction                #
    ########################################
    #  
    startTime <- Sys.time()
    run_maxent_model_prediction_single_pca(mod=mod,
                                           this_item_name=paste("historical_predict_",i,sep = ''),
                                           grid_path_list_this = grid_path_list$historical_all,
                                           maxent_raster_dir_this=maxent_raster_dir_this,
                                           pp_pca=pp_pca,
                                           historical=T)
    
    print("historical_clim")
    
    endTime <- Sys.time()
    print(endTime-startTime)
    
    ########################################
    # predictions for the future   SSP1    #
    ########################################
    
    startTime <- Sys.time()
    run_maxent_model_prediction_single_pca(mod=mod,
                                           this_item_name=paste("ssp126_",i,sep = ''),
                                           grid_path_list_this = c(grid_path_list$ssp1_lc,grid_path_list$ssp1_bc),
                                           maxent_raster_dir_this=maxent_raster_dir_this,
                                           pp_pca=pp_pca,
                                           historical=F)
    
    print("ssp1")
    endTime <- Sys.time()
    print(endTime-startTime)
    
    ########################################
    # predictions for the future   SSP2    #
    ########################################
    
    startTime <- Sys.time()
    run_maxent_model_prediction_single_pca(mod=mod,
                                           this_item_name=paste("ssp245_",i,sep = ''),
                                           grid_path_list_this = c(grid_path_list$ssp2_lc,grid_path_list$ssp2_bc),
                                           maxent_raster_dir_this=maxent_raster_dir_this,
                                           pp_pca=pp_pca,
                                           historical=F)
    
    print("ssp2")
    endTime <- Sys.time()
    print(endTime-startTime)
    
    ########################################
    # predictions for the future   SSP3    #
    ########################################
    
    startTime <- Sys.time()
    run_maxent_model_prediction_single_pca(mod=mod,
                                           this_item_name=paste("ssp370_",i,sep = ''),
                                           grid_path_list_this = c(grid_path_list$ssp3_lc,grid_path_list$ssp3_bc),
                                           maxent_raster_dir_this=maxent_raster_dir_this,
                                           pp_pca=pp_pca,
                                           historical=F)
    
    print("ssp3")
    endTime <- Sys.time()
    print(endTime-startTime)
    
    ########################################
    # predictions for the future   SSP5    #
    ########################################
    
    startTime <- Sys.time()
    run_maxent_model_prediction_single_pca(mod=mod,
                                           this_item_name=paste("ssp585_",i,sep = ''),
                                           grid_path_list_this = c(grid_path_list$ssp5_lc,grid_path_list$ssp5_bc),
                                           maxent_raster_dir_this=maxent_raster_dir_this,
                                           pp_pca=pp_pca,
                                           historical=F)
    print("ssp5")
    endTime <- Sys.time()
    print(endTime-startTime)
  }
  endTime <- Sys.time()
  print(endTime-startTime)
}

# this_return <- prepare_input_data_kfold_grid(occ_grid_path="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/cell/San.csv",
#                                              clim_grid_path="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/historical/5km.csv",
#                                              number_of_folds=10,
#                                              maxent_result_dir='')

# this_return <- prepare_input_data_kfold_buffer_grid(occ_grid_path="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/cell/San.csv",
#                                              clim_grid_path="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/historical/5km.csv",
#                                              buffer_grid_path="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/buffer/San.csv",
#                                              number_of_folds=10,
#                                              maxent_result_dir='')

run_maxent_kfold_cv_grid_pca <- function(this_bug,number_replicate,top_file_dir,occ_grid_path,clim_grid_path){
  ########################################
  # all the saving paths                 #
  ########################################
  
  #/output folder saves all the output
  all_path_stack <- all_saving_paths(top_file_dir=top_file_dir,this_bug=this_bug)
  
  ########################################
  #       prepare the data.              #
  ########################################
  
  input_data_stack_with_folds <- prepare_input_data_kfold_grid(occ_grid_path=occ_grid_path,
                                                               clim_grid_path=clim_grid_path,
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

overall_startTime <- Sys.time()

this_bug = 'San'
number_replicate = 10
top_file_dir = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/kfold_grid_buffer_process"
occ_grid_path = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/cell/San.csv"
clim_grid_path = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/historical/5km.csv"
buffer_grid_path = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/buffer/San.csv"

#lc land cover, bc bioclimatic
grid_path_list <- list("historical_all"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/historical/5km.csv",
                       "ssp1_lc"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/ssp1/global_SSP1_RCP26_2085/5km.csv",
                       "ssp1_bc"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/ssp1/ssp126_2071_2100/5km.csv",
                       "ssp2_lc"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/ssp2/global_SSP2_RCP45_2085/5km.csv",
                       "ssp2_bc"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/ssp2/ssp245_2071_2100/5km.csv",
                       "ssp3_lc"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/ssp3/global_SSP3_RCP70_2085/5km.csv",
                       "ssp3_bc"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/ssp3/ssp370_2071_2100/5km.csv",
                       "ssp5_lc"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/ssp5/global_SSP5_RCP85_2085/5km.csv",
                       "ssp5_bc"="/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/bioclimatic/ssp5/ssp585_2071_2100/5km.csv")
########################################
# all the saving paths                 #
########################################

#/output folder saves all the output
all_path_stack <- all_saving_paths(top_file_dir=top_file_dir,this_bug=this_bug)

########################################
#       prepare the data.              #
########################################


# this_input_data_stack <- prepare_input_data_kfold_grid_pca(occ_grid_path=occ_grid_path,
#                                                            clim_grid_path=clim_grid_path,
#                                                            number_of_folds=number_replicate,
#                                                            maxent_result_dir=all_path_stack$maxent_result_dir)

this_input_data_stack <- prepare_input_data_kfold_buffer_grid_pca(occ_grid_path=occ_grid_path,
                                                                  clim_grid_path=clim_grid_path,
                                                                  buffer_grid_path=buffer_grid_path,
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
cv_result_list <- run_maxent_model_cv_grid(list_x_train_full=this_input_data_stack$list_x_train_full,
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
this_model <- run_maxent_model_training_all_grid(maxent_evaluate_dir=all_path_stack$maxent_evaluate_dir,
                                                 all_x_full=this_input_data_stack$all_x_full,
                                                 all_pa=this_input_data_stack$all_pa,
                                                 maxent_result_path=all_path_stack$maxent_result_path,
                                                 dir_sub_name="kfold_all_input",
                                                 maxent_model_dir=all_path_stack$maxent_model_dir,
                                                 model_saving=T)



########################################
# predictions              #
########################################
pp_pca <- this_input_data_stack$pp_pca
rm(this_input_data_stack)

# perform predictions on all the cv models
run_maxent_model_prediction_list_grid_pca(mod_list=cv_result_list$model_list,
                                          grid_path_list=grid_path_list,
                                          dir_sub_name='cross_validation',
                                          number_replicate=number_replicate,
                                          pp_pca=pp_pca,
                                          maxent_raster_dir=all_path_stack$maxent_raster_dir)
# perform predictions on the model trained with all input data
run_maxent_model_prediction_basic_grid_pca(mod=this_model,
                                           grid_path_list=grid_path_list,
                                           dir_sub_name="kfold_all_input",
                                           pp_pca=pp_pca,
                                           maxent_raster_dir=all_path_stack$maxent_raster_dir)

overall_endTime <- Sys.time()
cat("overall running time")
print(overall_endTime - overall_startTime)
# historical_grid <- read.csv(grid_path_list$historical_all)
# historical_all_area_grid <- subset(historical_grid[complete.cases(historical_grid), ],select = -c(left, top,right,bottom,X,forest_grassland))
# historical_cell_id <-  historical_all_area_grid$id
# column_names <- names(input_data_stack_with_folds$pp_pca$mean)
# #historical_all_area_grid_no_id_clean <- subset(historical_all_area_grid_no_id,select = column_names)
# 
# historical_all_area_grid_no_id <- subset(historical_all_area_grid,select = column_names)#subset(historical_all_area_grid,select = -c(id))
# historical_all_area_grid_no_id <- predict(input_data_stack_with_folds$pp_pca, newdata = historical_all_area_grid_no_id)
# ped <- predict(this_model,historical_all_area_grid_no_id)
# # save csv file
# final_prediction <- as.data.frame(list(id = historical_cell_id,
#                                        prediction = ped))
# 
# save_csv_path <- paste(maxent_raster_dir_this,"/historical_predict_",this_bug,'.csv',sep = '')

