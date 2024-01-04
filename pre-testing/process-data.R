library("sp")
library("rJava")
library("raster")
library("dismo")
library("rgeos")
library('sf')

library("knitr")
library("rprojroot")

library("caret")

all_saving_paths <- function(top_file_dir,this_bug){
  ###########
  # example all_saving_paths("/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/","San")
  # top_file_dir: the directory to save all the output files
  # this_bug: specify which bug we are interested at, choose from: Dim, Ger, Ind, Lec, Lon, Max, Mex, Neo, Pro, Rec, Rub, San
  # return: a stack specify all the paths to save output files under top_file_dir, c(maxent_result_dir, maxent_model_dir,maxent_model_path, save_raster_path, p_path,p_test_path,a_train_path,a_test_path)
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
  save_all_output_dir <- paste(top_file_dir,this_bug,sep = '')
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

  
  #subfolder under save_all_output_path/model/ for maxent model
  maxent_model_dir <- paste(maxent_result_dir,"/model",sep = '')
  print(4)
  print(maxent_model_dir)
  if (!dir.exists(maxent_model_dir)){
    dir.create(maxent_model_dir)
  }else{
    print("dir exists")
  }
  maxent_model_path <- paste(maxent_model_dir,'/',this_bug,'.RDS',sep = '')
  print(maxent_model_path)
  
  #path under save_all_output_path for maxent historical prediction output
  save_raster_path<- paste(maxent_result_dir,"/historical_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path)
  
  #path under save_all_output_path for maxent training and testing sets (include the ones for cross validation)
  save_input_data_path<- paste(maxent_result_dir,"/input_data_",this_bug,'.RDS',sep = '')
  print(save_input_data_path)
  
  #path under save_all_output_path for maxent future projected prediction output for SSP126 
  save_raster_path_ssp1<- paste(maxent_result_dir,"/ssp126_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path_ssp1)
  
  #path under save_all_output_path for maxent future projected prediction output for SSP245
  save_raster_path_ssp2<- paste(maxent_result_dir,"/ssp245_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path_ssp2)
  
  #path under save_all_output_path for maxent future projected prediction output for SSP370 
  save_raster_path_ssp3<- paste(maxent_result_dir,"/ssp370_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path_ssp3)
  
  #path under save_all_output_path for maxent future projected prediction output for SSP585
  save_raster_path_ssp5<- paste(maxent_result_dir,"/ssp585_predict_",this_bug,'.tif',sep = '')
  print(save_raster_path_ssp5)

  
  return(list("maxent_result_dir"=maxent_result_dir, "maxent_model_dir"=maxent_model_dir,"maxent_model_path"=maxent_model_path, "save_raster_path"=save_raster_path, "save_input_data_path"=save_input_data_path,"save_raster_path_ssp1"=save_raster_path_ssp1,"save_raster_path_ssp2"=save_raster_path_ssp2,"save_raster_path_ssp3"=save_raster_path_ssp3, "save_raster_path_ssp5"=save_raster_path_ssp5))
}



prepare_input_data <- function(occ_raw_path,clim,split_ratio=0.90){
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
  
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data and make it a spatial object
  occ_raw <- read.csv(occ_raw_path)
  
  occ_only<- occ_raw[,c('DecimalLon',"DecimalLat")]
  colnames(occ_only) <- c('longitudes',"latitudes")
  coordinates(occ_only) <- ~longitudes + latitudes
  
  plot(clim[[1]])  # to the first layer of the bioclim layers as a reference
  plot(occ_only, add = TRUE) 
  
  proj4string(occ_only)<- CRS("+proj=longlat +datum=WGS84 +no_defs") #this CRS is consistent with that of the generated background points
  
  
  ########################################
  # sample the background points         #
  ########################################
  startTime <- Sys.time()
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
  endTime <- Sys.time()
  print(endTime-startTime)
  ########################################
  # training & testing sets              #
  ########################################
  
  # randomly select 75% for training
  selected <- sample(1:length(occ_only), length(occ_only) * split_ratio)
  
  occ_train <- occ_only[selected, ]  # this is the selection to be used for model training
  
  occ_test <- occ_only[-selected, ]  # this is the opposite of the selection which will be used for model testing
  
  
  # extracting env conditions for training occ from the raster
  # stack; a data frame is returned (i.e multiple columns)
  p_train <- extract(clim, occ_train,sp = T)
  # extracting env conditions for testing occ
  p_test <- extract(clim, occ_test,sp = T)
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
  
  
  return(list("pa_train" =pa_train, "pa_test"=pa_test, "x_train"=x_train,"x_test"=x_test,"x_train_full"=x_train_full,"x_test_full"=x_test_full,"a_train"=a_train,"a_test"=a_test,"p_train"=p_train,"p_test"=p_test))
  
}

prepare_input_data_kfold <- function(occ_raw_path,clim,number_of_folds,split_ratio=0.90){
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
  # "p_train" list of data frames with all the variable sampling for the presence training points and coordinates
  # "p_test" list of data frames with all the variable sampling for the presence testing points and coordinates
 
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
  
  return(list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"final_pa_train" =final_pa_train, "final_pa_test"=final_pa_test, "final_x_train"=final_x_train,"final_x_test"=final_x_test,"final_x_train_full"=final_x_train_full,"final_x_test_full"=final_x_test_full,"final_a_train"=final_a_train,"final_a_test"=final_a_test,"final_p_train"=final_p_train,"final_p_test"=final_p_test))

  
}


prepare_input_data_kfold_buffer <- function(occ_raw_path,clim,number_of_folds,shapefile_path,buff_width = 0.5){
  # input:
  # occ_raw_path: the path of the occurence data (in .csv format, with decimal longitude and latitude columns have column names 'DecimalLon',"DecimalLat")
  # clim: the stack of all input training raster data (after reprojection and alignment)
  # number_of_folds: specify the number of folds to split the data (for k-fold cross validation)
  # buff_width: width of the buffer around occurence points (in decimal degrees)
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
  # "p_train" list of data frames with all the variable sampling for the presence training points and coordinates
  # "p_test" list of data frames with all the variable sampling for the presence testing points and coordinates
  
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
  
  buffer_around_points <- buffer(occ_only, width=buff_width, dissolve=TRUE) #create buffers around presence (occurrence) points
  #plot(buffer_around_points)
  studyArea_polygon <- shapefile(shapefile_path) #create a mask for the studyArea by getting the first raster out, select all pixels with non-none values
  #plot(studyArea_polygon)
  studyArea_mask <- erase(studyArea_polygon, buffer_around_points) #create a mask for regions covered by the raster layer but outside the buffers around presence points
  #plot(studyArea_mask)
  studyArea <- mask(studyArea,studyArea_mask)
  #plot(studyArea[[1]])
  set.seed(1) 
  cat('\nselect background points')
  bg <- sampleRandom(x=studyArea,
                     size=100,
                     sp=T) # return spatial points
  
  set.seed(1)
  endTime <- Sys.time()
  print(endTime-startTime)
  
  plot(studyArea[[1]])
  plot(bg, add = TRUE)
  
  
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
  
  return(list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"final_pa_train" =final_pa_train, "final_pa_test"=final_pa_test, "final_x_train"=final_x_train,"final_x_test"=final_x_test,"final_x_train_full"=final_x_train_full,"final_x_test_full"=final_x_test_full,"final_a_train"=final_a_train,"final_a_test"=final_a_test,"final_p_train"=final_p_train,"final_p_test"=final_p_test))
  
  
}



run_maxent_model_cv <- function(list_x_train_full,list_pa_train,maxent_model_dir,number_replicate,csv_saving_path){
  ### run maxent model with cross validations
  # return the model
  # save all the metrics in a csv file
  
  startTime <- Sys.time()
  
  replicate_string <- paste("replicates=",number_replicate,sep='')
  list_x_train_full <- c()
  for (i in 1:number_replicate){
    pder_train <- subset(list_x_train_full[[i]],select = -c(longitudes, latitudes))
    pa_train <- list_pa_train[[i]]
    maxent_result_path_this <- maxent_model_dir
    mod <- maxent(x=pder_train, ## env conditions
                  p=pa_train,   ## 1:presence or 0:absence; occurence data + background points
                  path=paste0(maxent_result_path), ## folder for maxent output; 
                  # if we do not specify a folder R will put the results in a temp file, 
                  # and it gets messy to read those. . .
                  args=c("responsecurves","jackknife") ## parameter specification
    )
  }
  endTime <- Sys.time()
  print(endTime-startTime)
  
  return(mod)
}

run_maxent_model_training <- function(pder_train,pa_train,maxent_result_path){
  #run maxent model with all training set
}

