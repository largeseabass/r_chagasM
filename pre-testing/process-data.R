library("sp")
library("rJava")
library("raster")
library("dismo")
library('sf')
library("knitr")
library("rprojroot")
library("caret")

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

prepare_input_data_basic_old <- function(occ_raw_path,clim,maxent_result_dir,split_ratio=0.90){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # split_ratio: percentage of training data, e.g. 0.75
  #
  # return:  
  # list of items (note: pa_train and pa_test has the same order of rows as x_train and x_test)
  # "pa_train"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for training points
  # "pa_test"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for testing points
  # "x_train"  data frame with all the variable sampling for the training points (presence + absence)
  # "x_test"  data frame with all the variable sampling for the testing points (presence + absence)
  # "x_train_full"  data frame with all the variable sampling for the training points (presence + absence) and coordinates
  # "x_test_full"  data frame with all the variable sampling for the testing points (presence + absence) and coordinates
  # "a_train" data frame with all the variable sampling for the absence training points and coordinates
  # "a_test" data frame with all the variable sampling for the absence testing points and coordinates
  # "p_train" data frame with all the variable sampling for the presence training points and coordinates
  # "p_test" data frame with all the variable sampling for the presence testing points and coordinates
  cat("\nprepare input data")
  startTime <- Sys.time()
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data and make it a spatial object
  occ_raw <- read.csv(occ_raw_path)
  
  occ_only<- occ_raw[,c('DecimalLon',"DecimalLat")]
  colnames(occ_only) <- c('longitudes',"latitudes")
  coordinates(occ_only) <- ~longitudes + latitudes
  
  #plot(clim[[1]])  # to the first layer of the bioclim layers as a reference
  #plot(occ_only, add = TRUE) 
  
  proj4string(occ_only)<- CRS("+proj=longlat +datum=WGS84 +no_defs") #this CRS is consistent with that of the generated background points
  
  
  ########################################
  # sample the background points         #
  ########################################
  occ_final <- occ_only
  
  studyArea <- clim

  set.seed(1) 
  bg <- sampleRandom(x=studyArea,
                     size=10000,
                     na.rm=T, #removes the 'Not Applicable' points  
                     sp=T) # return spatial points 
  

  #plot(studyArea[[1]])
  #plot(bg,add=T) 
  #plot(occ_final,add=T,col="red")
  set.seed(1)

  ########################################
  # training & testing sets              #
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
  
  bg <- as.data.frame(bg)
  this_inspect_index_bg <- unique(which(is.na(as.data.frame(bg)), arr.ind=TRUE)[,1])
  if(length(this_inspect_index_bg) > 0){
    cat("\nNA values in background points!")
    cat("\nRows with NA values are removed. Row indexs:",this_inspect_index_bg)
    cat("\nRows with NA values are removed. Rows:")
    print(bg[this_inspect_index_bg,])
    bg <- bg[-this_inspect_index_bg,]
    cat("\nPlease double check input raster layers.")
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
  
  # randomly select 75% for training
  selected <- sample(1:length(all_p), length(all_p) * split_ratio)
  p_train <- all_p[selected, ]  # this is the selection to be used for model training
  p_test <- all_p[-selected, ]  # this is the opposite of the selection which will be used for model testing
  
  # extracting env conditions for background
  bg_selected <-  sample(1:length(bg), length(bg) * split_ratio)
  a_train <- bg[bg_selected,]
  a_test <- bg[-bg_selected,]
  # To create final training set: 
  # (presence) repeat the number 1 as many numbers as the number of rows in p
  # (absence) repeat 0 as the rows of background points
  pa_train <- c(rep(1, nrow(p_train)), rep(0, nrow(a_train)))
  pa_test <- c(rep(1, nrow(p_test)), rep(0, nrow(a_test)))
  

  # To create training and testing set, with (_full) and without decimal longitudes and latitudes.
  x_train_full <- as.data.frame(rbind(p_train, a_train))
  x_test_full <- as.data.frame(rbind(p_test, a_test))
  
  x_train <- subset(x_train_full,select = -c(longitudes, latitudes))
  x_test <- subset(x_test_full,select = -c(longitudes, latitudes))
  list_to_return <- list("pa_train" =pa_train, "pa_test"=pa_test, "x_train"=x_train,"x_test"=x_test,"x_train_full"=x_train_full,"x_test_full"=x_test_full,"a_train"=a_train,"a_test"=a_test,"p_train"=p_train,"p_test"=p_test)
  saveRDS(list_to_return, file = paste(maxent_result_dir,"/basic_input_data_",this_bug,".RDS",sep = ''))
  endTime <- Sys.time()
  print(endTime-startTime)
  return(list_to_return)
}

prepare_input_data_basic <- function(occ_raw_path,clim_dir,maxent_result_dir,split_ratio=0.90){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # split_ratio: percentage of training data, e.g. 0.75
  #
  # return:  
  # list of items (note: pa_train and pa_test has the same order of rows as x_train and x_test)
  # "pa_train"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for training points
  # "pa_test"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for testing points
  # "x_train"  data frame with all the variable sampling for the training points (presence + absence)
  # "x_test"  data frame with all the variable sampling for the testing points (presence + absence)
  # "x_train_full"  data frame with all the variable sampling for the training points (presence + absence) and coordinates
  # "x_test_full"  data frame with all the variable sampling for the testing points (presence + absence) and coordinates
  # "a_train" data frame with all the variable sampling for the absence training points and coordinates
  # "a_test" data frame with all the variable sampling for the absence testing points and coordinates
  # "p_train" data frame with all the variable sampling for the presence training points and coordinates
  # "p_test" data frame with all the variable sampling for the presence testing points and coordinates
  cat("\nprepare input data")
  startTime <- Sys.time()
  
  #### read clim
  clim_list <- list.files(clim_dir, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  
  
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data and make it a spatial object
  occ_raw <- read.csv(occ_raw_path)
  
  occ_only<- occ_raw[,c('DecimalLon',"DecimalLat")]
  colnames(occ_only) <- c('longitudes',"latitudes")
  coordinates(occ_only) <- ~longitudes + latitudes
  
  #plot(clim[[1]])  # to the first layer of the bioclim layers as a reference
  #plot(occ_only, add = TRUE) 
  
  proj4string(occ_only)<- CRS("+proj=longlat +datum=WGS84 +no_defs") #this CRS is consistent with that of the generated background points
  
  
  ########################################
  # sample the background points         #
  ########################################
  occ_final <- occ_only
  
  studyArea <- clim
  
  set.seed(1) 
  bg <- sampleRandom(x=studyArea,
                     size=10000,
                     na.rm=T, #removes the 'Not Applicable' points  
                     sp=T) # return spatial points 
  
  
  #plot(studyArea[[1]])
  #plot(bg,add=T) 
  #plot(occ_final,add=T,col="red")
  set.seed(1)
  
  ########################################
  # training & testing sets              #
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
  
  bg <- as.data.frame(bg)
  this_inspect_index_bg <- unique(which(is.na(as.data.frame(bg)), arr.ind=TRUE)[,1])
  if(length(this_inspect_index_bg) > 0){
    cat("\nNA values in background points!")
    cat("\nRows with NA values are removed. Row indexs:",this_inspect_index_bg)
    cat("\nRows with NA values are removed. Rows:")
    print(bg[this_inspect_index_bg,])
    bg <- bg[-this_inspect_index_bg,]
    cat("\nPlease double check input raster layers.")
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
  
  # randomly select 75% for training
  selected <- sample(1:length(all_p), length(all_p) * split_ratio)
  p_train <- all_p[selected, ]  # this is the selection to be used for model training
  p_test <- all_p[-selected, ]  # this is the opposite of the selection which will be used for model testing
  
  # extracting env conditions for background
  bg_selected <-  sample(1:length(bg), length(bg) * split_ratio)
  a_train <- bg[bg_selected,]
  a_test <- bg[-bg_selected,]
  # To create final training set: 
  # (presence) repeat the number 1 as many numbers as the number of rows in p
  # (absence) repeat 0 as the rows of background points
  pa_train <- c(rep(1, nrow(p_train)), rep(0, nrow(a_train)))
  pa_test <- c(rep(1, nrow(p_test)), rep(0, nrow(a_test)))
  
  
  # To create training and testing set, with (_full) and without decimal longitudes and latitudes.
  x_train_full <- as.data.frame(rbind(p_train, a_train))
  x_test_full <- as.data.frame(rbind(p_test, a_test))
  
  x_train <- subset(x_train_full,select = -c(longitudes, latitudes))
  x_test <- subset(x_test_full,select = -c(longitudes, latitudes))
  list_to_return <- list("pa_train" =pa_train, "pa_test"=pa_test, "x_train"=x_train,"x_test"=x_test,"x_train_full"=x_train_full,"x_test_full"=x_test_full,"a_train"=a_train,"a_test"=a_test,"p_train"=p_train,"p_test"=p_test)
  saveRDS(list_to_return, file = paste(maxent_result_dir,"/basic_input_data_",this_bug,".RDS",sep = ''))
  endTime <- Sys.time()
  print(endTime-startTime)
  return(list_to_return)
}


prepare_input_data_kfold_old1 <- function(occ_raw_path,clim,number_of_folds,maxent_result_dir){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  # split_ratio=0.75: percentage of input data to put into the training set
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
 
  # "final_pa_train"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for training points
  # "final_pa_test"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for testing points
  # "final_x_train"  data frame with all the variable sampling for the training points (presence + absence)
  # "final_x_test"  data frame with all the variable sampling for the testing points (presence + absence)
  # "final_x_train_full"  data frame with all the variable sampling for the training points (presence + absence) and coordinates
  # "final_x_test_full"  data frame with all the variable sampling for the testing points (presence + absence) and coordinates
  # "final_a_train" data frame with all the variable sampling for the absence training points and coordinates
  # "final_a_test" data frame with all the variable sampling for the absence testing points and coordinates
  # "final_p_train" data frame with all the variable sampling for the presence training points and coordinates
  # "final_p_test" data frame with all the variable sampling for the presence testing points and coordinates
  
  
  cat("number of replicates: ",number_of_folds)
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data
  occ_raw <- read.csv(occ_raw_path)
  
  occ_only<- occ_raw[,c('DecimalLon',"DecimalLat")]
  colnames(occ_only) <- c('longitudes',"latitudes")
  coordinates(occ_only) <- ~longitudes + latitudes
  
  proj4string(occ_only)<- CRS("+proj=longlat +datum=WGS84 +no_defs")#CRS("+init=epsg:4326")
  
  
  ########################################
  # sample the background points         #
  ########################################
  startTime <- Sys.time()
  occ_final <- occ_only
  
  studyArea <- clim
  
  set.seed(1) 
  cat('\nselect background points')
  bg <- sampleRandom(x=studyArea,
                     size=100,
                     sp=T) # return spatial points

  set.seed(1)
  endTime <- Sys.time()
  print(endTime-startTime)
  
  
  ########################################
  # train-test split                      #
  ########################################
  # randomly select 75% for training
  selected <- sample(1:length(occ_only), length(occ_only) * split_ratio)
  occ_train <- occ_only[selected, ]  # this is the selection to be used for model training
  occ_test <- occ_only[-selected, ]  # this is the opposite of the selection which will be used for model testing
  # extracting env conditions for training and testing occ from the raster stack; a data frame is returned (i.e multiple columns)
  final_p_train <- extract(clim, occ_train,sp = T)
  final_p_test <- extract(clim, occ_test,sp = T)
  # extracting env conditions for background
  bg_selected <-  sample(1:length(bg), length(bg) * split_ratio)
  final_a_train <- bg[bg_selected,]
  final_a_test <- bg[-bg_selected,]
  # To create final training set: 
  # (presence) repeat the number 1 as many numbers as the number of rows in p
  # (absence) repeat 0 as the rows of background points
  final_pa_train <- c(rep(1, nrow(final_p_train)), rep(0, nrow(final_a_train)))
  final_pa_test <- c(rep(1, nrow(final_p_test)), rep(0, nrow(final_a_test)))
  # To create training and testing set, with (_full) and without decimal longitudes and latitudes.
  final_x_train_full <- as.data.frame(rbind(final_p_train, final_a_train))
  final_x_test_full <- as.data.frame(rbind(final_p_test, final_a_test))
  final_x_train <- subset(final_x_train_full,select = -c(longitudes, latitudes))
  final_x_test <- subset(final_x_test_full,select = -c(longitudes, latitudes))
  
  ########################################
  # k-fold p and a.                      #
  ########################################
  # list[number_of_folds] of index to be hold out for each fold
  occ_only_dataframe <- as.data.frame(final_p_train)
  bg_dataframe <- as.data.frame(final_a_train)
  names(bg_dataframe)[names(bg_dataframe) == "x"] <- "longitudes"
  names(bg_dataframe)[names(bg_dataframe) == "y"] <- "latitudes"
  
  print(typeof(occ_only_dataframe[[1]]))
  
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
    # occ_train <- occ_only_dataframe[-occ_kfold[[i]], ]  # this is the selection to be used for model training
    # occ_test <- occ_only_dataframe[occ_kfold[[i]], ]  # this is the opposite of the selection which will be used for model testing
    # p_train <- cbind(extract(clim, occ_train,sp = T),occ_only_dataframe[-occ_kfold[[i]],])
    # p_test <- cbind(extract(clim, occ_test,sp = T),occ_only_dataframe[occ_kfold[[i]],])
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
  
  list_to_return <- list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"final_pa_train" =final_pa_train, "final_pa_test"=final_pa_test, "final_x_train"=final_x_train,"final_x_test"=final_x_test,"final_x_train_full"=final_x_train_full,"final_x_test_full"=final_x_test_full,"final_a_train"=final_a_train,"final_a_test"=final_a_test,"final_p_train"=final_p_train,"final_p_test"=final_p_test)
  saveRDS(list_to_return, file = paste(maxent_result_dir,"/input_data_",this_bug,".RDS",sep = ''))
  return(list_to_return)
}

prepare_input_data_kfold_old2 <- function(occ_raw_path,clim,number_of_folds,maxent_result_dir){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  # split_ratio=0.75: percentage of input data to put into the training set
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
  
  
  cat("\nprepare input data...number of replicates: ",number_of_folds)
  startTime <- Sys.time()
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data
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
  
  list_to_return <- list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"all_pa" =all_pa, "all_p"=all_p, "all_a"=all_a,"all_x_full"=all_x_full)
  saveRDS(list_to_return, file = paste(maxent_result_dir,"/kfold_input_data_",this_bug,".RDS",sep = ''))
  endTime <- Sys.time()
  print(endTime-startTime)
  return(list_to_return)
}

prepare_input_data_kfold <- function(occ_raw_path,clim_dir,number_of_folds,maxent_result_dir){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  # split_ratio=0.75: percentage of input data to put into the training set
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
  
  
  cat("\nprepare input data...number of replicates: ",number_of_folds)
  startTime <- Sys.time()
  
  #### read clim
  clim_list <- list.files(clim_dir, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data
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
  
  list_to_return <- list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"all_pa" =all_pa, "all_p"=all_p, "all_a"=all_a,"all_x_full"=all_x_full)
  saveRDS(list_to_return, file = paste(maxent_result_dir,"/kfold_input_data_",this_bug,".RDS",sep = ''))
  endTime <- Sys.time()
  print(endTime-startTime)
  return(list_to_return)
}


prepare_input_data_kfold_buffer_old1 <- function(occ_raw_path,clim,number_of_folds,shapefile_path,maxent_result_dir,buff_width = 55555){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  # buff_width: width of the buffer around occurence points (in decimal degrees)
  # save_input_data_path: path to save the return list
  #
  # idea of this function:
  # split the input data into training-testing sets
  # select background points from the the region outside buffers around the occurence points
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
  
  # "final_pa_train"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for training points
  # "final_pa_test"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for testing points
  # "final_x_train"  data frame with all the variable sampling for the training points (presence + absence)
  # "final_x_test"  data frame with all the variable sampling for the testing points (presence + absence)
  # "final_x_train_full"  data frame with all the variable sampling for the training points (presence + absence) and coordinates
  # "final_x_test_full"  data frame with all the variable sampling for the testing points (presence + absence) and coordinates
  # "final_a_train" data frame with all the variable sampling for the absence training points and coordinates
  # "final_a_test" data frame with all the variable sampling for the absence testing points and coordinates
  # "final_p_train" data frame with all the variable sampling for the presence training points and coordinates
  # "final_p_test" data frame with all the variable sampling for the presence testing points and coordinates
  
  cat("number of replicates: ",number_of_folds)
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data
  occ_raw <- read.csv(occ_raw_path)
  
  occ_only<- occ_raw[,c('DecimalLon',"DecimalLat")]
  colnames(occ_only) <- c('longitudes',"latitudes")
  coordinates(occ_only) <- ~longitudes + latitudes
  
  proj4string(occ_only)<- CRS("+proj=longlat +datum=WGS84 +no_defs")#CRS("+init=epsg:4326")
  
  
  ########################################
  # sample the background points         #
  ########################################
  startTime <- Sys.time()
  occ_final <- occ_only
  
  studyArea <- clim
  
  buffer_around_points <- buffer(occ_only, buff_width, dissolve=TRUE) #create buffers around presence (occurrence) points
  #shapefile(buffer_around_points, '/Users/vivianhuang/Desktop/buffer.shp',overwrite=TRUE)
  #plot(buffer_around_points)
  studyArea_polygon <- shapefile(shapefile_path) #create a mask for the studyArea by getting the first raster out, select all pixels with non-none values
  #plot(studyArea_polygon)
  studyArea_mask <- erase(studyArea_polygon, buffer_around_points) #create a mask for regions covered by the raster layer but outside the buffers around presence points
  #shapefile(studyArea_mask, '/Users/vivianhuang/Desktop/test.shp',overwrite=TRUE)
  #par(mar = c(1, 1, 1, 1))
  #plot(studyArea_mask,col="red")
  studyArea <- crop(studyArea,extent(studyArea_mask))  
  studyArea <- mask(studyArea,studyArea_mask)
  #par(mar = c(1, 1, 1, 1))
  #plot(studyArea[[1]],xlim=c(25,39),ylim=c(-102,-80))
  
  set.seed(1) 
  cat('\nselect background points')
  bg <- sampleRandom(x=studyArea,
                     size=10000,
                     sp=T) # return spatial points
  
  set.seed(1)
  endTime <- Sys.time()
  print(endTime-startTime)
  #par(mar = c(1, 1, 1, 1))
  #plot(studyArea[[1]])
  #plot(bg, col = "black",add = TRUE)
  #plot(occ_final, col = "red",add = TRUE)
  
  
  ########################################
  # train-test split                      #
  ########################################
  # randomly select 75% for training
  selected <- sample(1:length(occ_only), length(occ_only) * split_ratio)
  occ_train <- occ_only[selected, ]  # this is the selection to be used for model training
  occ_test <- occ_only[-selected, ]  # this is the opposite of the selection which will be used for model testing
  # extracting env conditions for training and testing occ from the raster stack; a data frame is returned (i.e multiple columns)
  final_p_train <- extract(clim, occ_train,sp = T)
  final_p_test <- extract(clim, occ_test,sp = T)
  # extracting env conditions for background
  bg_selected <-  sample(1:length(bg), length(bg) * split_ratio)
  final_a_train <- bg[bg_selected,]
  final_a_test <- bg[-bg_selected,]
  # To create final training set: 
  # (presence) repeat the number 1 as many numbers as the number of rows in p
  # (absence) repeat 0 as the rows of background points
  final_pa_train <- c(rep(1, nrow(final_p_train)), rep(0, nrow(final_a_train)))
  final_pa_test <- c(rep(1, nrow(final_p_test)), rep(0, nrow(final_a_test)))
  # To create training and testing set, with (_full) and without decimal longitudes and latitudes.
  final_x_train_full <- as.data.frame(rbind(final_p_train, final_a_train))
  final_x_test_full <- as.data.frame(rbind(final_p_test, final_a_test))
  final_x_train <- subset(final_x_train_full,select = -c(longitudes, latitudes))
  final_x_test <- subset(final_x_test_full,select = -c(longitudes, latitudes))
  
  ########################################
  # k-fold p and a.                      #
  ########################################
  # list[number_of_folds] of index to be hold out for each fold
  occ_only_dataframe <- as.data.frame(final_p_train)
  bg_dataframe <- as.data.frame(final_a_train)
  names(bg_dataframe)[names(bg_dataframe) == "x"] <- "longitudes"
  names(bg_dataframe)[names(bg_dataframe) == "y"] <- "latitudes"
  
  print(typeof(occ_only_dataframe[[1]]))
  
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
    # occ_train <- occ_only_dataframe[-occ_kfold[[i]], ]  # this is the selection to be used for model training
    # occ_test <- occ_only_dataframe[occ_kfold[[i]], ]  # this is the opposite of the selection which will be used for model testing
    # p_train <- cbind(extract(clim, occ_train,sp = T),occ_only_dataframe[-occ_kfold[[i]],])
    # p_test <- cbind(extract(clim, occ_test,sp = T),occ_only_dataframe[occ_kfold[[i]],])
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
  list_to_return <- list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"final_pa_train" =final_pa_train, "final_pa_test"=final_pa_test, "final_x_train"=final_x_train,"final_x_test"=final_x_test,"final_x_train_full"=final_x_train_full,"final_x_test_full"=final_x_test_full,"final_a_train"=final_a_train,"final_a_test"=final_a_test,"final_p_train"=final_p_train,"final_p_test"=final_p_test)
  saveRDS(list_to_return, file = paste(maxent_result_dir,"/input_data_",this_bug,".RDS",sep = ''))
  return(list_to_return)
}

prepare_input_data_kfold_buffer <- function(occ_raw_path,clim_dir,number_of_folds,shapefile_path,maxent_result_dir,buff_width = 55555){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  # buff_width: width of the buffer around occurence points (in decimal degrees)
  # save_input_data_path: path to save the return list
  #
  # idea of this function:
  # split the input data into training-testing sets
  # select background points from the the region outside buffers around the occurence points
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
  
  
  cat("\nprepare input data...number of replicates: ",number_of_folds)
  startTime <- Sys.time()
  
  #### read clim
  clim_list <- list.files(clim_dir, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data
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
  
  buffer_around_points <- buffer(occ_only, buff_width, dissolve=TRUE) #create buffers around presence (occurrence) points
  #shapefile(buffer_around_points, '/Users/vivianhuang/Desktop/buffer.shp',overwrite=TRUE)
  #plot(buffer_around_points)
  studyArea_polygon <- shapefile(shapefile_path) #create a mask for the studyArea by getting the first raster out, select all pixels with non-none values
  #plot(studyArea_polygon)
  studyArea_mask <- erase(studyArea_polygon, buffer_around_points) #create a mask for regions covered by the raster layer but outside the buffers around presence points
  #shapefile(studyArea_mask, '/Users/vivianhuang/Desktop/test.shp',overwrite=TRUE)
  #par(mar = c(1, 1, 1, 1))
  #plot(studyArea_mask,col="red")
  studyArea <- crop(studyArea,extent(studyArea_mask))  
  studyArea <- mask(studyArea,studyArea_mask)
  #par(mar = c(1, 1, 1, 1))
  #plot(studyArea[[1]],xlim=c(25,39),ylim=c(-102,-80))
  
  set.seed(1) 
  cat('\nselect background points')
  bg <- sampleRandom(x=studyArea,
                     size=10000,
                     sp=T) # return spatial points
  
  set.seed(1)
  #par(mar = c(1, 1, 1, 1))
  #plot(studyArea[[1]])
  #plot(bg, col = "black",add = TRUE)
  #plot(occ_final, col = "red",add = TRUE)
  
  
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
  list_to_return <- list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"all_pa" =all_pa, "all_p"=all_p, "all_a"=all_a,"all_x_full"=all_x_full)
  saveRDS(list_to_return, file = paste(maxent_result_dir,"/kfold_buffer_input_data_",this_bug,".RDS",sep = ''))
  endTime <- Sys.time()
  print(endTime-startTime)
  return(list_to_return)
}


run_maxent_model_cv <- function(list_x_train_full,list_x_test_full,list_pa_train,list_pa_test,list_p_train,list_a_train,list_p_test,list_a_test,maxent_evaluate_dir,number_replicate,maxent_model_dir,metric_saving=T,model_saving=T){
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
    pder_train <- subset(list_x_train_full[[i]],select = -c(longitudes, latitudes))
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
    predicted_results <- predict(mod,subset(list_x_test_full[[i]],select = -c(longitudes, latitudes)))
    this_auc <- auc(actual_results, predicted_results)
    this_bias <- bias(actual_results, predicted_results)
    this_mae <- mae(actual_results, predicted_results)
    # calculate TSS
    # get threshold from training set and then use this threshold in testing set
    this_p_train <- subset(list_p_train[[i]],select = -c(longitudes, latitudes))
    this_a_train <- subset(list_a_train[[i]],select = -c(longitudes, latitudes))
    this_evaluate_train <- dismo::evaluate(p = this_p_train, a = this_a_train, model = mod)
    this_tss_original_list <-this_evaluate_train@TPR + this_evaluate_train@TNR - 1.0
    this_threshold_index <- which(this_tss_original_list == max(this_tss_original_list))[[1]]
    this_threshold <- this_evaluate_train@t[this_threshold_index]
    this_p_test <- subset(list_p_test[[i]],select = -c(longitudes, latitudes))
    this_a_test <- subset(list_a_test[[i]],select = -c(longitudes, latitudes))
    this_evaluate_test <- dismo::evaluate(p = this_p_test, a = this_a_test, model = mod,tr=this_threshold)
    this_test_tss <- max(this_evaluate_test@TPR + this_evaluate_test@TNR - 1.0)
    print(this_evaluate_test@auc)
    
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

run_maxent_model_training_basic <- function(maxent_evaluate_dir,final_x_train,final_x_test,final_pa_train,final_pa_test,final_p_train,final_a_train,final_p_test,final_a_test,maxent_model_dir,metric_saving=T,model_saving=T){
  # run maxent model with given training set and calculate evaluation metric (with testing set)
  # maxent_evaluate_dir: the directory to save results
  # final_x_train,final_x_test,final_pa_train,final_pa_test,final_p_train,final_a_train,final_p_test,final_a_test,maxent_model_dir: : see prepare_input_data_kfold or prepare_input_data_kfold_buffer
  # metric_saving=T: if true, save the evaluation metrices to .RDS and .csv files
  # model_saving=T: if true, save the model to .RDS file
  # return the trained model
  cat("\nbasic training")
  startTime <- Sys.time()
  pder_train <- subset(final_x_train,select = -c(longitudes, latitudes))
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
  predicted_results <- predict(mod,subset(final_x_test,select = -c(longitudes, latitudes)))
  this_auc <- auc(actual_results, predicted_results)
  this_bias <- bias(actual_results, predicted_results)
  this_mae <- mae(actual_results, predicted_results)
  # calculate TSS
  # get threshold from training set and then use this threshold in testing set
  this_p_train <- subset(final_p_train,select = -c(longitudes, latitudes))
  this_a_train <- subset(final_a_train,select = -c(longitudes, latitudes))
  this_evaluate_train <- dismo::evaluate(p = this_p_train, a = this_a_train, model = mod)
  this_tss_original_list <-this_evaluate_train@TPR + this_evaluate_train@TNR - 1.0
  this_threshold_index <- which(this_tss_original_list == max(this_tss_original_list))[[1]]
  this_threshold <- this_evaluate_train@t[this_threshold_index]
  this_p_test <- subset(final_p_test,select = -c(longitudes, latitudes))
  this_a_test <- subset(final_a_test,select = -c(longitudes, latitudes))
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

run_maxent_model_training_all <- function(maxent_evaluate_dir,all_x_full,all_pa,maxent_result_path,dir_sub_name,maxent_model_dir,model_saving=T){
  # run maxent model with all input data (hence no testing sets available for evaluation metric calculation)
  # maxent_evaluate_dir: the directory to save results
  # "all_pa"  list of 0 and 1, 0 indicates absence, 1 indicates presence, for all input points
  # "all_x_full"  data frame with all the variable sampling for the all input points (presence + absence) and coordinates
  # model_saving=T: if true, save the model to .RDS file
  # return the trained model
  cat("\nall-data training")
  startTime <- Sys.time()
  pder_train <- subset(all_x_full,select = -c(longitudes, latitudes))
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
  if (model_saving){
    # also save the model
    maxent_model_path <- paste(maxent_model_dir,'/',dir_sub_name,'_final_model_training_all.RDS',sep = '')
    saveRDS(mod, file = maxent_model_path)
  }
  endTime <- Sys.time()
  print(endTime-startTime)
  return(mod)
}


run_maxent_model_prediction_single <- function(mod,this_item_name,clim_dir,maxent_raster_dir_this){
  startTime <- Sys.time()
  print(this_item_name)
  clim_list <- list.files(clim_dir, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  clim <- raster::stack(clim_list)
  print("make prediction")
  ped <- predict(mod,clim)
  save_raster_path <- paste(maxent_raster_dir_this,"/",this_item_name,"_",this_bug,'.tif',sep = '')
  writeRaster(ped, filename =save_raster_path, format = "GTiff")
  endTime <- Sys.time()
  print(endTime-startTime)
}

run_maxent_model_prediction_basic_old <- function(mod,clim,maxent_raster_dir,dir_resample_mask,dir_sub_name){
  # take one trained maxent model
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  cat("\nbasic prediction")
  startTime <- Sys.time()
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
  #  
  startTime <- Sys.time()
  print("historical_clim")
  
  ped <- predict(mod,clim)
  # save raster
  save_raster_path <- paste(maxent_raster_dir_this,"/historical_predict_",this_bug,'.tif',sep = '')
  writeRaster(ped, filename =save_raster_path, format = "GTiff")
  #plot(ped)
  endTime <- Sys.time()
  print(endTime-startTime)
  ########################################
  # predictions for the future   SSP1    #
  ########################################
  
  clim_list1 <- list.files(dir_resample_mask_ssp1, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  
  #check if the list of rasters are what we want
  print("clim_list1")
  
  clim1 <- raster::stack(clim_list1)
  
  ped1 <- predict(mod,clim1)
  save_raster_path1<- paste(maxent_raster_dir_this,"/ssp126_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path1)
  writeRaster(ped1, filename =save_raster_path1, format = "GTiff")
  ########################################
  # predictions for the future   SSP2    #
  ########################################
  
  clim_list2 <- list.files(dir_resample_mask_ssp2, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  
  #check if the list of rasters are what we want
  print("clim_list2")
  
  clim2 <- raster::stack(clim_list2)
  ped2 <- predict(mod,clim2)
  save_raster_path2<- paste(maxent_raster_dir_this,"/ssp245_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path2)
  writeRaster(ped2, filename =save_raster_path2, format = "GTiff")
  ########################################
  # predictions for the future   SSP3    #
  ########################################
  
  clim_list3 <- list.files(dir_resample_mask_ssp3, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  
  #check if the list of rasters are what we want
  print("clim_list3")
  
  clim3 <- raster::stack(clim_list3)
  ped3 <- predict(mod,clim3)
  save_raster_path3<- paste(maxent_raster_dir_this,"/ssp370_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path3)
  writeRaster(ped3, filename =save_raster_path3, format = "GTiff")
  ########################################
  # predictions for the future   SSP5    #
  ########################################
  
  clim_list5 <- list.files(dir_resample_mask_ssp5, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
  
  #check if the list of rasters are what we want
  print("clim_list5")
  
  clim5 <- raster::stack(clim_list5)
  ped5 <- predict(mod,clim5)
  save_raster_path5<- paste(maxent_raster_dir_this,"/ssp585_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path5)
  writeRaster(ped5, filename =save_raster_path5, format = "GTiff")
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


run_maxent_model_prediction_list_old <- function(mod_list,clim,maxent_raster_dir,dir_resample_mask,dir_sub_name,number_replicate){
  # take a list of trained maxent models
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  
  # mod_list: list of trained maxent models
  # clim: historical bioclimatic rasters (aligned)
  # maxent_raster_dir: the directory under which the bioclimatic rasters (aligned) are stored, with rasters for ssp1 - ssp5 stored under corresponding subfolders.
  
  cat("\nmodel-list prediction")
  startTime <- Sys.time()
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
  for (i in 1:number_replicate){
    cat("\n",i)
    mod <- mod_list[[i]]
    ########################################
    # historical prediction                #
    ########################################
    #  
    startTime <- Sys.time()
    print("historical_clim")
    
    ped <- predict(mod,clim)
    # save raster
    save_raster_path <- paste(maxent_raster_dir_this,"/historical_predict_",this_bug,'_',i,'.tif',sep = '')
    writeRaster(ped, filename =save_raster_path, format = "GTiff")
    #plot(ped)
    endTime <- Sys.time()
    print(endTime-startTime)
    ########################################
    # predictions for the future   SSP1    #
    ########################################
    
    clim_list1 <- list.files(dir_resample_mask_ssp1, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
    
    #check if the list of rasters are what we want
    print("clim_list1")
    
    clim1 <- raster::stack(clim_list1)
    
    ped1 <- predict(mod,clim1)
    save_raster_path1<- paste(maxent_raster_dir_this,"/ssp126_predict_",this_bug,'_',i,'.tif',sep = '')
    print(save_raster_path1)
    writeRaster(ped1, filename =save_raster_path1, format = "GTiff")
    ########################################
    # predictions for the future   SSP2    #
    ########################################
    
    clim_list2 <- list.files(dir_resample_mask_ssp2, pattern = '.tif$', full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
    
    #check if the list of rasters are what we want
    print("clim_list2")
    
    clim2 <- raster::stack(clim_list2)
    ped2 <- predict(mod,clim2)
    save_raster_path2<- paste(maxent_raster_dir_this,"/ssp245_predict_",this_bug,'_',i,'.tif',sep = '')
    print(save_raster_path2)
    writeRaster(ped2, filename =save_raster_path2, format = "GTiff")
    ########################################
    # predictions for the future   SSP3    #
    ########################################
    
    clim_list3 <- list.files(dir_resample_mask_ssp3, pattern = '.tif$', full.names = T) 
    #check if the list of rasters are what we want
    print("clim_list3")
    
    clim3 <- raster::stack(clim_list3)
    ped3 <- predict(mod,clim3)
    save_raster_path3<- paste(maxent_raster_dir_this,"/ssp370_predict_",this_bug,'_',i,'.tif',sep = '')
    print(save_raster_path3)
    writeRaster(ped3, filename =save_raster_path3, format = "GTiff")
    ########################################
    # predictions for the future   SSP5    #
    ########################################
    
    clim_list5 <- list.files(dir_resample_mask_ssp5, pattern = '.tif$', full.names = T)  
    
    #check if the list of rasters are what we want
    print("clim_list5")
    
    clim5 <- raster::stack(clim_list5)
    ped5 <- predict(mod,clim5)
    save_raster_path5<- paste(maxent_raster_dir_this,"/ssp585_predict_",this_bug,'_',i,'.tif',sep = '')
    print(save_raster_path5)
    writeRaster(ped5, filename =save_raster_path5, format = "GTiff")
  }
  endTime <- Sys.time()
  print(endTime-startTime)
}


run_maxent_model_prediction_list <- function(mod_list_path,clim,maxent_raster_dir,dir_resample_mask,dir_sub_name,number_replicate){
  # take a list of trained maxent models
  # perform prediction on historical and future projected 2071-2100 ssp1, ssp2, ssp3, ssp5 rasters (after aligned)
  # save the results as .tif files
  
  # mod_list: list of trained maxent models
  # clim: historical bioclimatic rasters (aligned)
  # maxent_raster_dir: the directory under which the bioclimatic rasters (aligned) are stored, with rasters for ssp1 - ssp5 stored under corresponding subfolders.
  
  cat("\nmodel-list prediction")
  startTime <- Sys.time()
  mod_list <- readRDS(mod_list_path)
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
  for (i in 1:number_replicate){
    cat("\n",i)
    mod <- mod_list[[i]]
    ########################################
    # historical prediction                #
    ########################################
    
    run_maxent_model_prediction_single(mod=mod,
                                       this_item_name=paste("historical_predict_",i,sep = ''),
                                       clim_dir = clim_dir,
                                       maxent_raster_dir_this=maxent_raster_dir_this)
    
    ########################################
    # predictions for the future   SSP1    #
    ########################################
    run_maxent_model_prediction_single(mod=mod,
                                       this_item_name=paste("ssp126_predict_",i,sep = ''),
                                       clim_dir = dir_resample_mask_ssp1,
                                       maxent_raster_dir_this=maxent_raster_dir_this)
    
    ########################################
    # predictions for the future   SSP2    #
    ########################################
    
    run_maxent_model_prediction_single(mod=mod,
                                       this_item_name=paste("ssp245_predict_",i,sep = ''),
                                       clim_dir = dir_resample_mask_ssp2,
                                       maxent_raster_dir_this=maxent_raster_dir_this)
    
    ########################################
    # predictions for the future   SSP3    #
    ########################################
    
    run_maxent_model_prediction_single(mod=mod,
                                       this_item_name=paste("ssp370_predict_",i,sep = ''),
                                       clim_dir = dir_resample_mask_ssp3,
                                       maxent_raster_dir_this=maxent_raster_dir_this)
    
    ########################################
    # predictions for the future   SSP5    #
    ########################################
    
    run_maxent_model_prediction_single(mod=mod,
                                       this_item_name=paste("ssp585_predict_",i,sep = ''),
                                       clim_dir = dir_resample_mask_ssp5,
                                       maxent_raster_dir_this=maxent_raster_dir_this)
  }
  endTime <- Sys.time()
  print(endTime-startTime)
}
