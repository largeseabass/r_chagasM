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


prepare_input_data_kfold_pca <- function(occ_raw_path,clim,number_of_folds,maxent_result_dir){
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
                     size=100,
                     sp=T) # return spatial points
  
  set.seed(1)
  
  
  ########################################
  # all input data                     #
  ########################################
  # extracting env conditions for all occurrence points
  all_p <- extract(clim, occ_only,sp = T)
  # extracting env conditions for all background points
  all_a <- bg
  
  ########################################
  # process the input data in 3 steps    #
  ########################################
  # zero or near-zero variance
  nearZeroVar(mdrrDescr, saveMetrics= TRUE)
  
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
  names(bg_dataframe)[names(bg_dataframe) == "x"] <- "longitudes"
  names(bg_dataframe)[names(bg_dataframe) == "y"] <- "latitudes"
  
  
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




this_bug = 'San'
number_replicate = 10
top_file_dir = "/Users/vivianhuang/Desktop/R-modeling-scripts/r_chagasM/output/kfold_process"
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

number_of_folds <- number_replicate
maxent_result_dir <- 
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

list_to_return <- list("list_pa_train" =list_pa_train, "list_pa_test"=list_pa_test, "list_x_train_full"=list_x_train_full,"list_x_test_full"=list_x_test_full,"list_p_train"=list_p_train,"list_a_train"=list_a_train,"list_p_test"=list_p_test,"list_a_test"=list_a_test,"all_pa" =all_pa, "all_p"=all_p, "all_a"=all_a,"all_x_full"=all_x_full)
saveRDS(list_to_return, file = paste(maxent_result_dir,"/kfold_input_data_",this_bug,".RDS",sep = ''))
endTime <- Sys.time()
print(endTime-startTime)
