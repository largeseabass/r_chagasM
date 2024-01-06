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

prepare_input_data_kfold_grid <- function(occ_grid_path,clim_grid_path,number_of_folds,maxent_result_dir){
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
  
  
  cat("number of replicates: ",number_of_folds)
  ########################################
  # read kissing bug related csv files   #
  ########################################
  #read occurence data
  occ_grid <- read.csv(occ_grid_path)
  
  #read clim data
  clim_grid <- read.csv(clim_grid_path)
  
  
  
  # ########################################
  # # sample the background points         #
  # ########################################
  # startTime <- Sys.time()
  # occ_final <- occ_only
  # 
  # studyArea <- clim
  # 
  # set.seed(1) 
  # cat('\nselect background points')
  # bg <- sampleRandom(x=studyArea,
  #                    size=100,
  #                    sp=T) # return spatial points
  # 
  # set.seed(1)
  # endTime <- Sys.time()
  # print(endTime-startTime)
  # 
  # 
  # ########################################
  # # all input data                     #
  # ########################################
  # # extracting env conditions for all occurrence points
  # all_p <- extract(clim, occ_only,sp = T)
  # # extracting env conditions for all background points
  # all_a <- bg
  # # prepare data for training
  # all_pa <- c(rep(1, nrow(all_p)), rep(0, nrow(all_a)))
  # all_x_full <- as.data.frame(rbind(all_p, all_a))
  # ########################################
  # # k-fold p and a.                      #
  # ########################################
  # # list[number_of_folds] of index to be hold out for each fold
  # occ_only_dataframe <- as.data.frame(all_p)
  # bg_dataframe <- as.data.frame(all_a)
  # names(bg_dataframe)[names(bg_dataframe) == "x"] <- "longitudes"
  # names(bg_dataframe)[names(bg_dataframe) == "y"] <- "latitudes"
  # 
  # print(typeof(occ_only_dataframe[[1]]))
  # 
  # occ_kfold <- createFolds(occ_only_dataframe[[1]],k=number_of_folds) 
  # bg_kfold <- createFolds(bg_dataframe[[1]],k=number_of_folds) 
  # 
  # 
  # list_pa_train <- c()
  # list_pa_test <- c()
  # list_x_train_full <- c()
  # list_x_test_full <- c()
  # list_p_train <- c()
  # list_a_train <- c()
  # list_p_test <- c()
  # list_a_test <- c()
  # for (i in 1:number_of_folds){
  #   p_train <- occ_only_dataframe[-occ_kfold[[i]], ]  # this is the selection to be used for model training
  #   p_test <- occ_only_dataframe[occ_kfold[[i]], ]  # this is the opposite of the selection which will be used for model testing
  #   
  #   a_train <- bg_dataframe[-bg_kfold[[i]],]
  #   a_test <- bg_dataframe[bg_kfold[[i]],]
  #   
  #   pa_train <- c(rep(1, nrow(p_train)), rep(0, nrow(a_train)))
  #   pa_test <- c(rep(1, nrow(p_test)), rep(0, nrow(a_test)))
  #   x_train_full <- as.data.frame(rbind(p_train, a_train))
  #   x_test_full <- as.data.frame(rbind(p_test, a_test))
  #   
  #   list_pa_train <- c(list_pa_train,list(pa_train))
  #   list_pa_test <- c(list_pa_test,list(pa_test))
  #   list_x_train_full <- c(list_x_train_full,list(x_train_full))
  #   list_x_test_full <- c(list_x_test_full,list(x_test_full))
  #   list_p_train <- c(list_p_train,list(p_train))
  #   list_a_train <- c(list_a_train,list(a_train))
  #   list_p_test <- c(list_p_test,list(p_test))
  #   list_a_test <- c(list_a_test,list(a_test))
  # }
  # 
  # list_to_return <- list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"all_pa" =all_pa, "all_p"=all_p, "all_a"=all_a,"all_x_full"=all_x_full)
  # saveRDS(list_to_return, file = paste(maxent_result_dir,"/kfold_input_data_",this_bug,".RDS",sep = ''))
  # return(list_to_return)
}
