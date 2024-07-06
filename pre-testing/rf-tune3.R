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


rf_evaluate <- function(p, a, model, x, tr) {
  # modified from dismo::evaluate function to make it compatible with the output format of 'prob' type prediction.
  # The probability of '1' is used as the final prediction.
  if (! missing(x) ) {
    p <- predict(model, data.frame(extract(x, p)), type = "prob")[, 2]
    a <- predict(model, data.frame(extract(x, a)), type = "prob")[, 2]
  } else if (is.vector(p) & is.vector(a)) {
    # do nothing
  } else {
    p <- predict(model, data.frame(p), type = "prob")[, 2]
    a <- predict(model, data.frame(a), type = "prob")[, 2]
  }
  p <- stats::na.omit(p)
  a <- stats::na.omit(a)
  np <- length(p)
  na <- length(a)
  if (na == 0 | np == 0) {
    stop('cannot evaluate a model without absence and presence data that are not NA')
  }
  
  if (missing(tr)) {
    if (length(p) > 1000) {
      tr <- as.vector(quantile(p, 0:1000/1000))
    } else {
      tr <- p
    }
    if (length(a) > 1000) {
      tr <- c(tr, as.vector(quantile(a, 0:1000/1000)))
    } else {
      tr <- c(tr, a)
    }
    tr <- sort(unique(round(tr, 8)))
    tr <- c(tr - 0.0001, tr[length(tr)] + c(0, 0.0001))
  } else {
    tr <- sort(as.vector(tr))
  }
  
  N <- na + np
  
  xc <- new('ModelEvaluation')
  xc@presence = p
  xc@absence = a
  
  R <- sum(rank(c(p, a))[1:np]) - (np*(np+1)/2)
  xc@auc <- R / (as.numeric(na) * as.numeric(np))
  
  cr <- try(cor.test(c(p, a), c(rep(1, length(p)), rep(0, length(a)))), silent = TRUE)
  if (!inherits(cr, 'try-error')) {
    xc@cor <- cr$estimate
    xc@pcor <- cr$p.value
  }
  
  res <- matrix(ncol = 4, nrow = length(tr))
  colnames(res) <- c('tp', 'fp', 'fn', 'tn')
  xc@t <- tr
  for (i in 1:length(tr)) {
    res[i, 1] <- length(p[p >= tr[i]])  # a  true positives
    res[i, 2] <- length(a[a >= tr[i]])  # b  false positives
    res[i, 3] <- length(p[p < tr[i]])    # c  false negatives
    res[i, 4] <- length(a[a < tr[i]])    # d  true negatives
  }
  xc@confusion = res
  a = res[, 1]
  b = res[, 2]
  c = res[, 3]
  d = res[, 4]
  # after Fielding and Bell  
  xc@np <- as.integer(np)
  xc@na <- as.integer(na)
  xc@prevalence = (a + c) / N
  xc@ODP = (b + d) / N
  xc@CCR = (a + d) / N
  xc@TPR = a / (a + c)
  xc@TNR = d / (b + d)
  xc@FPR = b / (b + d)
  xc@FNR = c / (a + c)
  xc@PPP = a / (a + b)
  xc@NPP = d / (c + d)
  xc@MCR = (b + c) / N
  xc@OR = (a * d) / (c * b)
  
  prA = (a + d) / N
  prY = (a + b) / N * (a + c) / N
  prN = (c + d) / N * (b + d) / N
  prE = prY + prN
  xc@kappa = (prA - prE) / (1 - prE)
  return(xc)
}

all_ml_saving_paths <- function(top_file_dir,this_bug){
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
  
  
  
  #subfolder under save_all_output_path/result/ for model output
  ml_result_dir <- paste(save_all_output_dir,'/result',sep = '')
  print(3)
  print(ml_result_dir)
  if (!dir.exists(ml_result_dir)){
    dir.create(ml_result_dir)
  }else{
    print("dir exists")
  }
  
  #subfolder under save_all_output_path/result/ for model output
  ml_evaluate_dir <- paste(save_all_output_dir,'/evaluate',sep = '')
  print(4)
  print(ml_evaluate_dir)
  if (!dir.exists(ml_evaluate_dir)){
    dir.create(ml_evaluate_dir)
  }else{
    print("dir exists")
  }
  
  
  #subfolder under save_all_output_path/model/ for model model
  ml_model_dir <- paste(ml_result_dir,"/model",sep = '')
  print(5)
  print(ml_model_dir)
  if (!dir.exists(ml_model_dir)){
    dir.create(ml_model_dir)
  }else{
    print("dir exists")
  }
  
  #subfolder under save_all_output_path/output_raster/ for model model
  ml_raster_dir <- paste(ml_result_dir,"/output_raster",sep = '')
  print(6)
  print(ml_raster_dir)
  if (!dir.exists(ml_raster_dir)){
    dir.create(ml_raster_dir)
  }else{
    print("dir exists")
  }
  
  
  list_to_return <- list("ml_result_dir"=ml_result_dir, "ml_evaluate_dir"=ml_evaluate_dir,"ml_model_dir"=ml_model_dir,"ml_raster_dir"=ml_raster_dir )
  return(list_to_return)
}


# Function to tune Random Forest parameters using bias as the evaluation metric
tune_rf_model <- function(all_x_full, all_pa, max_ntree = 1000) { #changedByGpt
  # Combine the datasets into a single data frame
  combined_data <- data.frame(occ = all_pa, subset(all_x_full, select = -c(longitudes, latitudes)))
  combined_data$occ <- as.factor(combined_data$occ)
  
  # Define grid of parameters to tune
  tune_grid <- expand.grid(
    mtry = seq(1, ncol(combined_data) - 1, by = 1), # tune mtry from 1 to number of features
    ntree = seq(100, max_ntree, by = 100) # tune ntree from 100 to max_ntree in steps of 100
  ) #changedByGpt
  
  best_bias <- Inf #changedByGpt
  best_params <- list(mtry = NA, ntree = NA) #changedByGpt
  
  for (i in 1:nrow(tune_grid)) { #changedByGpt
    set.seed(123) # for reproducibility #changedByGpt
    mod <- randomForest(occ ~ ., data = combined_data, 
                        mtry = tune_grid$mtry[i], 
                        ntree = tune_grid$ntree[i],
                        sampsize = c("0" = sum(combined_data$occ == "1"), "1" = sum(combined_data$occ == "1")),
                        replace = TRUE)
    
    # Predict on training data
    preds <- predict(mod, combined_data, type = "prob")[, 2]
    current_bias <- bias(as.numeric(combined_data$occ) - 1, preds)
    
    if (abs(current_bias) < abs(best_bias)) {
      best_bias <- current_bias
      best_params$mtry <- tune_grid$mtry[i]
      best_params$ntree <- tune_grid$ntree[i]
    }
  }
  
  return(best_params)
}

# Modified run_rf_model_cv function to include tuned parameters
run_rf_model_cv <- function(list_x_train_full, list_x_test_full, list_pa_train, list_pa_test, list_p_train, list_a_train, list_p_test, list_a_test, ml_evaluate_dir, number_replicate, ml_model_dir, tuned_params, metric_saving = TRUE, model_saving = TRUE) { #changedByGpt
  cat("\nstart to calculate cv")
  startTime <- Sys.time()
  
  replicate_string <- paste("replicates=", number_replicate, sep = '')
  metric_result_list <- list()
  model_list <- list()
  variable_importance_list <- list()
  
  for (i in 1:number_replicate) {
    cat("\n", i)
    pder_train <- subset(list_x_train_full[[i]], select = -c(longitudes, latitudes))
    pa_train <- list_pa_train[[i]]
    prNum <- as.numeric(table(pa_train)["1"]) # number of presence records
    spsize <- c("0" = prNum, "1" = prNum) # sample size for both classes
    
    # Combine the datasets into a single data frame
    combined_data <- data.frame(occ = pa_train, pder_train)
    combined_data$occ <- as.factor(combined_data$occ)
    
    mod <- randomForest(occ ~ .,
                        data = combined_data,
                        ntree = tuned_params$ntree, #changedByGpt
                        mtry = tuned_params$mtry, #changedByGpt
                        sampsize = spsize,
                        replace = TRUE, # make sure samples are with replacement (default)
                        importance = TRUE)
    
    actual_results <- list_pa_test[[i]]
    predicted_results0 <- predict(mod, subset(list_x_test_full[[i]], select = -c(longitudes, latitudes)), type = "prob")
    predicted_results <- predicted_results0[, 2]
    this_auc <- auc(actual_results, predicted_results)
    this_bias <- bias(actual_results, predicted_results)
    this_mae <- mae(actual_results, predicted_results)
    
    # calculate TSS
    # get threshold from training set and then use this threshold in testing set
    this_p_train <- subset(list_p_train[[i]], select = -c(longitudes, latitudes))
    this_a_train <- subset(list_a_train[[i]], select = -c(longitudes, latitudes))
    
    this_evaluate_train <- rf_evaluate(p = this_p_train, a = this_a_train, model = mod)
    this_tss_original_list <- this_evaluate_train@TPR + this_evaluate_train@TNR - 1.0
    this_threshold_index <- which(this_tss_original_list == max(this_tss_original_list))[[1]]
    this_threshold <- this_evaluate_train@t[this_threshold_index]
    
    this_p_test <- subset(list_p_test[[i]], select = -c(longitudes, latitudes))
    this_a_test <- subset(list_a_test[[i]], select = -c(longitudes, latitudes))
    
    this_evaluate_test <- rf_evaluate(p = this_p_test, a = this_a_test, model = mod, tr = this_threshold)
    this_test_tss <- max(this_evaluate_test@TPR + this_evaluate_test@TNR - 1.0)
    print(this_evaluate_test@auc)
    
    this_metric_list = list("mae" = this_mae, "tss" = this_test_tss, "bias" = this_bias, "auc" = this_auc, "tss_threshold" = this_threshold)
    metric_result_list[[i]] <- this_metric_list
    model_list[[i]] <- mod
    variable_importance_list[[i]] <- importance(mod)
  }
  
  if (metric_saving) {
    csv_saving_path <- paste(ml_evaluate_dir, '/cv_metric_results.csv', sep = '')
    write.csv(metric_result_list, file = csv_saving_path)
    rds_saving_path <- paste(ml_evaluate_dir, '/cv_metric_results.RDS', sep = '')
    saveRDS(metric_result_list, file = rds_saving_path)
    # importance
    csv_saving_path_importance <- paste(ml_evaluate_dir, '/cv_importance.csv', sep = '')
    write.csv(variable_importance_list, file = csv_saving_path_importance)
    rds_saving_path_importance <- paste(ml_evaluate_dir, '/cv_importance.RDS', sep = '')
    saveRDS(variable_importance_list, file = rds_saving_path_importance)
  }
  
  if (model_saving) {
    model_saving_path <- paste(ml_evaluate_dir, '/cv_models.RDS', sep = '')
    saveRDS(model_list, file = model_saving_path)
  }
  endTime <- Sys.time()
  print(endTime - startTime)
  return(list("metric_result_list" = metric_result_list, "model_list" = model_list))
}

# Modified run_rf_model_training_all function to include tuned parameters
run_rf_model_training_all <- function(ml_evaluate_dir, all_x_full, all_pa, ml_result_path, dir_sub_name, ml_model_dir, tuned_params, model_saving = TRUE) { #changedByGpt
  # run RF model with all input data (hence no testing sets available for evaluation metric calculation)
  # ml_evaluate_dir: the directory to save results
  # all_pa: list of 0 and 1, 0 indicates absence, 1 indicates presence, for all input points
  # all_x_full: data frame with all the variable sampling for the all input points (presence + absence) and coordinates
  # model_saving = TRUE: if true, save the model to .RDS file
  # return the trained model
  cat("\nall-data training")
  startTime <- Sys.time()
  pder_train <- subset(all_x_full, select = -c(longitudes, latitudes))
  pa_train <- all_pa
  ml_evaluate_dir_this <- paste(ml_evaluate_dir, '/', dir_sub_name, sep = '')
  print(ml_evaluate_dir_this)
  if (!dir.exists(ml_evaluate_dir_this)) {
    dir.create(ml_evaluate_dir_this)
  } else {
    print("dir exists")
  }
  prNum <- as.numeric(table(pa_train)["1"]) # number of presence records
  print(as.numeric(table(pa_train)["0"]))
  spsize <- c("0" = prNum, "1" = prNum) # sample size for both classes
  # Combine the datasets into a single data frame
  combined_data <- data.frame(occ = pa_train, pder_train)
  combined_data$occ <- as.factor(combined_data$occ)
  
  mod <- randomForest(occ ~ .,
                      data = combined_data,
                      ntree = tuned_params$ntree, #changedByGpt
                      mtry = tuned_params$mtry, #changedByGpt
                      sampsize = spsize,
                      replace = TRUE, # make sure samples are with replacement (default)
                      importance = TRUE)
  
  if (model_saving) {
    # also save the model
    ml_model_path <- paste(ml_model_dir, '/', dir_sub_name, '_final_model_training_all.RDS', sep = '')
    saveRDS(mod, file = ml_model_path)
    variable_importance <- importance(mod)
    csv_saving_path_importance <- paste(ml_model_dir, '/', dir_sub_name, '_final_model_training_all_importance.csv', sep = '')
    write.csv(variable_importance, file = csv_saving_path_importance)
    importance_path <- paste(ml_model_dir, '/', dir_sub_name, '_final_model_training_all_importance.RDS', sep = '')
    saveRDS(variable_importance, file = importance_path)
  }
  endTime <- Sys.time()
  print(endTime - startTime)
  return(mod)
}


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


# The Actual Run.
bug_list <- list("Lon","Maz","Mex")#,"Neo","Pro","Rub")
number_replicate <- 10
top_file_dir <- "/Users/liting/Documents/GitHub/r_chagasM/output/rf-tune/pixel_buffer_off" # add an rf layer
species_input_dir <- "/Users/liting/Documents/GitHub/r_chagasM/output/pixel_buffer_off"
input_file_dir <- "/Users/liting/Documents/GitHub/r_chagasM"
clim_dir <- "/Users/liting/Documents/data/resample_mask/historical/"

for (this_bug in bug_list) {
  # 1 create all saving path
  all_path_stack <- all_ml_saving_paths(top_file_dir, this_bug)
  # 2 read all the input bug data
  species_input_path <- paste(species_input_dir, "/", this_bug, "/result/kfold_buffer_input_data_", this_bug, ".RDS", sep = '')
  this_input_data_stack <- readRDS(species_input_path)
  
  # Tune RF model with all data
  cat("\nTuning RF model for", this_bug)
  start_time <- Sys.time()
  tuned_params <- tune_rf_model(all_x_full = this_input_data_stack$all_x_full, all_pa = this_input_data_stack$all_pa, max_ntree = 1000) #changedByGpt
  print(paste("Tuned parameters for", this_bug, ": mtry =", tuned_params$mtry, ", ntree =", tuned_params$ntree))
  end_time <- Sys.time()
  print(end_time - start_time)
  # 3 RF run_rf_model_cv
  # perform cross-validation
  cv_result_list <- run_rf_model_cv(list_x_train_full = this_input_data_stack$list_x_train_full,
                                    list_x_test_full = this_input_data_stack$list_x_test_full,
                                    list_pa_train = this_input_data_stack$list_pa_train,
                                    list_pa_test = this_input_data_stack$list_pa_test,
                                    list_p_train = this_input_data_stack$list_p_train,
                                    list_a_train = this_input_data_stack$list_a_train,
                                    list_p_test = this_input_data_stack$list_p_test,
                                    list_a_test = this_input_data_stack$list_a_test,
                                    ml_evaluate_dir = all_path_stack$ml_evaluate_dir,
                                    number_replicate = number_replicate,
                                    ml_model_dir = all_path_stack$ml_model_dir,
                                    tuned_params = tuned_params, #changedByGpt
                                    metric_saving = TRUE,
                                    model_saving = TRUE)
  
  # 4 RF run_rf_model_training_all
  # train the model (for prediction)
  this_model <- run_rf_model_training_all(ml_evaluate_dir = all_path_stack$ml_evaluate_dir,
                                          all_x_full = this_input_data_stack$all_x_full,
                                          all_pa = this_input_data_stack$all_pa,
                                          ml_result_path = all_path_stack$ml_result_path,
                                          dir_sub_name = "all_input",
                                          ml_model_dir = all_path_stack$ml_model_dir,
                                          tuned_params = tuned_params, #changedByGpt
                                          model_saving = TRUE)
  
  # 5 run_rf_model_prediction_basic
  dir_resample_mask <- "/Users/liting/Documents/data/resample_mask"
  this_model_path <- paste(top_file_dir, '/', this_bug, '/result/model/all_input_final_model_training_all.RDS', sep = '')
  
  # perform predictions on the model trained with all input data
  # run_rf_model_prediction_basic(mod_path = this_model_path,
  #                               clim_dir = clim_dir,
  #                               ml_raster_dir = paste(top_file_dir, '/', this_bug, '/result/output_raster', sep = ''),
  #                               dir_resample_mask = dir_resample_mask,
  #                               dir_sub_name = "all_input")
}
