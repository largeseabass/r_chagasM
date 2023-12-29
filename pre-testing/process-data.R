library("sp")
library("rJava")
library("raster")
library("dismo")
library("rgeos")


library("knitr")
library("rprojroot")

library("caret")

all_saving_paths <- function(top_file_dir,this_bug){
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
  #path under save_all_output_path for maxent historical training set
  p_path<- paste(maxent_result_dir,"/historical_p_train_",this_bug,'.RDS',sep = '')
  print(p_path)
  #path under save_all_output_path for maxent historical testing set
  p_test_path<- paste(maxent_result_dir,"/historical_p_test_",this_bug,'.RDS',sep = '')
  print(p_test_path)
  
  #path under save_all_output_path for maxent historical training set
  a_train_path<- paste(maxent_result_dir,"/historical_a_train_",this_bug,'.RDS',sep = '')
  print(a_train_path)
  
  #path under save_all_output_path for maxent historical testing set
  a_test_path<- paste(maxent_result_dir,"/historical_a_test_",this_bug,'.RDS',sep = '')
  print(a_test_path)
  
  return(c(maxent_result_dir, maxent_model_dir,maxent_model_path, save_raster_path, p_path,p_test_path,a_train_path,a_test_path))
}



prepare_input_data <- function(occ_raw_path,clim){
  ########################################
  # read kissing bug presence points     #
  ########################################
  
  #read occurence data
  occ_raw <- read.csv(occ_raw_path)
  
  occ_only<- occ_raw[,c('DecimalLon',"DecimalLat")]
  colnames(occ_only) <- c('longitudes',"latitudes")
  coordinates(occ_only) <- ~longitudes + latitudes
  
  plot(clim[[1]])  # to the first layer of the bioclim layers as a reference
  plot(occ_only, add = TRUE) 
  
  proj4string(occ_only)<- CRS("+proj=longlat +datum=WGS84 +no_defs")#CRS("+init=epsg:4326")
  
  
  ########################################
  # sample the background points         #
  ########################################
  startTime <- Sys.time()
  occ_final <- occ_only
  
  ###### make buffers #######
  #occ_buff <- buffer(occ_final, 5)
  #plot(clim[[1]])
  #plot(occ_buff,add =TRUE)
  studyArea <- clim
  # crop study area to a manageable extent (rectangle shaped)
  #studyArea <- crop(clim,extent(occ_buff))  
  # the 'study area' created by extracting the buffer area from the raster stack
  #studyArea <- mask(studyArea,occ_buff)
  #plot(studyArea[[1]])
  
  set.seed(1) 
  bg <- sampleRandom(x=studyArea,
                     size=10000,
                     na.rm=T, #removes the 'Not Applicable' points  
                     sp=T) # return spatial points 
  
  #bg <- spTransform(bg, CRS("+init=epsg:4326"))
  plot(studyArea[[1]])
  # add the background points to the plotted raster
  plot(bg,add=T) 
  # add the occurrence data to the plotted raster
  plot(occ_final,add=T,col="red")
  set.seed(1)
  endTime <- Sys.time()
  print(endTime-startTime)
  ########################################
  # training & testing sets              #
  ########################################
  
  # randomly select 75% for training
  selected <- sample(1:length(occ_only), length(occ_only) * 0.75)
  
  occ_train <- occ_only[selected, ]  # this is the selection to be used for model training
  
  occ_test <- occ_only[-selected, ]  # this is the opposite of the selection which will be used for model testing
  
  
  # extracting env conditions for training occ from the raster
  # stack; a data frame is returned (i.e multiple columns)
  p_train <- extract(clim, occ_train,sp = T)
  # env conditions for testing occ
  p_test <- extract(clim, occ_test,sp = T)
  # extracting env conditions for background
  bg_selected <-  sample(1:length(bg), length(bg) * 0.75)
  a_train <- bg[bg_selected,]
  a_test <- bg[-bg_selected,]
  #bg_train <- bg[bg_selected,]
  #bg_test <-bg[-bg_selected,]
  #a_train<- extract(clim, bg_train,sp = T)  # this is the selection to be used for model training
  #a_test<- extract(clim, bg_test,sp = T)  # this is the selection to be used for model training
  # repeat the number 1 as many numbers as the number of rows
  # in p, and repeat 0 as the rows of background points
  # final training set
  pa_train <- c(rep(1, nrow(p_train)), rep(0, nrow(a_train)))
  pa_test <- c(rep(1, nrow(p_test)), rep(0, nrow(a_test)))
  
  # (rep(1,nrow(p)) creating the number of rows as the p data
  # set to have the number '1' as the indicator for presence;
  # rep(0,nrow(a)) creating the number of rows as the a data
  # set to have the number '0' as the indicator for absence;
  # the c combines these ones and zeros into a new vector that
  # can be added to the Maxent table data frame with the
  # environmental attributes of the presence and absence
  # locations
  x_train <- subset(as.data.frame(rbind(p_train, a_train)),select = -c(longitudes, latitudes))
  x_test <- subset(as.data.frame(rbind(p_test, a_test)),select = -c(longitudes, latitudes))
  
  
  return(list("pa_train" =pa_train, "pa_test"=pa_test, "x_train"=x_train,"x_test"=x_test,"a_train"=a_train,"a_test"=a_test,"p_train"=p_train,"p_test"=p_test))
  
}

prepare_input_data_kfold <- function(occ_raw_path,clim,number_of_folds){
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
                     size=10000,
                     sp=T) # return spatial points

  set.seed(1)
  endTime <- Sys.time()
  print(endTime-startTime)
  
  
  ########################################
  # k-fold p and a.                      #
  ########################################

  # list[number_of_folds] of index to be hold out for each fold
  occ_only_dataframe <- as.data.frame(occ_only)
  occ_kfold <- createFolds(occ_only_dataframe[[1]],k=number_of_folds) 
  
  bg_dataframe <- as.data.frame(bg)
  names(bg_dataframe)[names(bg_dataframe) == "x"] <- "longitudes"
  names(bg_dataframe)[names(bg_dataframe) == "y"] <- "latitudes"
  bg_kfold <- createFolds(bg_dataframe[[1]],k=number_of_folds) 
  
  
  list_pa_train <- c()
  list_pa_test <- c()
  list_x_train <- c()
  list_x_test <- c()
  list_p_test <- c()
  list_a_test <- c()
  for (i in 1:number_of_folds){
    occ_train <- occ_only_dataframe[-occ_kfold[[i]], ]  # this is the selection to be used for model training
    occ_test <- occ_only_dataframe[occ_kfold[[i]], ]  # this is the opposite of the selection which will be used for model testing
    p_train <- cbind(extract(clim, occ_train,sp = T),occ_only_dataframe[-occ_kfold[[i]],])
    p_test <- cbind(extract(clim, occ_test,sp = T),occ_only_dataframe[occ_kfold[[i]],])
    
    a_train <- bg_dataframe[-bg_kfold[[i]],]
    a_test <- bg_dataframe[bg_kfold[[i]],]
    
    pa_train <- c(rep(1, nrow(p_train)), rep(0, nrow(a_train)))
    pa_test <- c(rep(1, nrow(p_test)), rep(0, nrow(a_test)))
    x_train <- as.data.frame(rbind(p_train, a_train))
    x_test <- as.data.frame(rbind(p_test, a_test))
    
    list_pa_train <- c(list_pa_train,list(pa_train))
    list_pa_test <- c(list_pa_test,list(pa_test))
    list_x_train <- c(list_x_train,list(x_train))
    list_x_test <- c(list_x_test,list(x_test))
    list_p_test <- c(list_p_test,list(p_test))
    list_a_test <- c(list_a_test,list(a_test))
  }
  
  return(list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train"=list_x_train,"list_x_test"=list_x_test,"list_p_test"=list_p_test,"list_a_test"=list_a_test))
  
}


prepare_input_data_kfold_buffer <- function(occ_raw_path,clim,number_of_folds){
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
  
  buffer_around_points <- buffer(occ_only, width=0.5, dissolve=TRUE)
  plot(buffer_around_points)

  
  studyArea_onevalue <- studyArea[[1]] > -Inf
  studyArea_polygon <- rasterToPolygons(studyArea_onevalue, dissolve=TRUE)
  studyArea_mask <- gDifference(studyArea_polygon, buffer_around_points)
  plot(studyArea_mask)
  studyArea <- mask(studyArea,studyArea_mask)
  
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
  # k-fold p and a.                      #
  ########################################
  
  # list[number_of_folds] of index to be hold out for each fold
  occ_only_dataframe <- as.data.frame(occ_only)
  occ_kfold <- createFolds(occ_only_dataframe[[1]],k=number_of_folds) 
  
  bg_dataframe <- as.data.frame(bg)
  names(bg_dataframe)[names(bg_dataframe) == "x"] <- "longitudes"
  names(bg_dataframe)[names(bg_dataframe) == "y"] <- "latitudes"
  bg_kfold <- createFolds(bg_dataframe[[1]],k=number_of_folds) 
  
  
  list_pa_train <- c()
  list_pa_test <- c()
  list_x_train <- c()
  list_x_test <- c()
  list_p_test <- c()
  list_a_test <- c()
  for (i in 1:number_of_folds){
    occ_train <- occ_only_dataframe[-occ_kfold[[i]], ]  # this is the selection to be used for model training
    occ_test <- occ_only_dataframe[occ_kfold[[i]], ]  # this is the opposite of the selection which will be used for model testing
    p_train <- cbind(extract(clim, occ_train,sp = T),occ_only_dataframe[-occ_kfold[[i]],])
    p_test <- cbind(extract(clim, occ_test,sp = T),occ_only_dataframe[occ_kfold[[i]],])
    
    a_train <- bg_dataframe[-bg_kfold[[i]],]
    a_test <- bg_dataframe[bg_kfold[[i]],]
    
    pa_train <- c(rep(1, nrow(p_train)), rep(0, nrow(a_train)))
    pa_test <- c(rep(1, nrow(p_test)), rep(0, nrow(a_test)))
    x_train <- as.data.frame(rbind(p_train, a_train))
    x_test <- as.data.frame(rbind(p_test, a_test))
    
    list_pa_train <- c(list_pa_train,list(pa_train))
    list_pa_test <- c(list_pa_test,list(pa_test))
    list_x_train <- c(list_x_train,list(x_train))
    list_x_test <- c(list_x_test,list(x_test))
    list_p_test <- c(list_p_test,list(p_test))
    list_a_test <- c(list_a_test,list(a_test))
  }
  
  return(list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train"=list_x_train,"list_x_test"=list_x_test,"list_p_test"=list_p_test,"list_a_test"=list_a_test))
  
}



run_maxent_model <- function(pder_train,pa_train,maxent_result_path,number_replicate){
  startTime <- Sys.time()
  
  replicate_string <- paste("replicates=",number_replicate,sep='')
  
  mod <- maxent(x=pder_train, ## env conditions
                p=pa_train,   ## 1:presence or 0:absence; occurence data + background points
                path=paste0(maxent_result_path), ## folder for maxent output; 
                # if we do not specify a folder R will put the results in a temp file, 
                # and it gets messy to read those. . .
                args=c(replicate_string,"responsecurves","jackknife","replicatetype=crossvalidate") ## parameter specification
  )
  
  endTime <- Sys.time()
  print(endTime-startTime)
  return(mod)
}
