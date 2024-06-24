library("raster")

bug_list <- list("San","Ger","Rec","Dim","Ind","Lec","Lon","Maz","Mex","Neo","Pro","Rub")
top_maxent_file_dir <- "/Users/liting/Documents/GitHub/r_chagasM/output/pixel_buffer_off"
top_rf_file_dir <- "/Users/liting/Documents/GitHub/r_chagasM/output/rf/pixel_buffer_off"
top_diff_file_dir <- "/Users/liting/Documents/GitHub/r_chagasM/output/figures/final_plot/difference"
for (this_bug in bug_list){
  top_file_dir <- paste(top_diff_file_dir,'/',this_bug,sep = '')
  print(top_file_dir)
  if (!dir.exists(top_file_dir)){
    dir.create(top_file_dir)
  }else{
    print("dir exists")
  }
  cat("Processing bug: ", this_bug, "\n")
  cat("Maxent file path: ", top_maxent_file_dir, "\n")
  maxent_result_path_historical <- paste(top_maxent_file_dir,'/',this_bug,'/result/output_raster/all_input/historical_predict_allinput_',this_bug,'.tif',sep = '')
  maxent_result_path_ssp126 <- paste(top_maxent_file_dir,'/',this_bug,'/result/output_raster/all_input/ssp126_predict_allinput_',this_bug,'.tif',sep = '')
  maxent_result_path_ssp245 <- paste(top_maxent_file_dir,'/',this_bug,'/result/output_raster/all_input/ssp245_predict_allinput_',this_bug,'.tif',sep = '')
  maxent_result_path_ssp337 <- paste(top_maxent_file_dir,'/',this_bug,'/result/output_raster/all_input/ssp370_predict_allinput_',this_bug,'.tif',sep = '')
  maxent_result_path_ssp585 <- paste(top_maxent_file_dir,'/',this_bug,'/result/output_raster/all_input/ssp585_predict_allinput_',this_bug,'.tif',sep = '')
  cat("Maxent result path: ", maxent_result_path_historical, "\n")
  rf_result_path_historical <- paste(top_rf_file_dir,'/',this_bug,'/result/output_raster/all_input/historical_predict_allinput_',this_bug,'.tif',sep = '')
  rf_result_path_ssp126 <- paste(top_rf_file_dir,'/',this_bug,'/result/output_raster/all_input/ssp126_predict_allinput_',this_bug,'.tif',sep = '')
  rf_result_path_ssp245 <- paste(top_rf_file_dir,'/',this_bug,'/result/output_raster/all_input/ssp245_predict_allinput_',this_bug,'.tif',sep = '')
  rf_result_path_ssp337 <- paste(top_rf_file_dir,'/',this_bug,'/result/output_raster/all_input/ssp370_predict_allinput_',this_bug,'.tif',sep = '')
  rf_result_path_ssp585 <- paste(top_rf_file_dir,'/',this_bug,'/result/output_raster/all_input/ssp585_predict_allinput_',this_bug,'.tif',sep = '')
  cat("RF result path: ", rf_result_path_historical, "\n")
  maxent_raster_historical <- raster(maxent_result_path_historical)
  maxent_raster_ssp126 <- raster(maxent_result_path_ssp126)
  maxent_raster_ssp245 <- raster(maxent_result_path_ssp245)
  maxent_raster_ssp337 <- raster(maxent_result_path_ssp337)
  maxent_raster_ssp585 <- raster(maxent_result_path_ssp585)
  cat("Maxent raster loaded: ", maxent_result_path_historical, "\n")
  rf_raster_historical <- raster(rf_result_path_historical)
  rf_raster_ssp126 <- raster(rf_result_path_ssp126)
  rf_raster_ssp245 <- raster(rf_result_path_ssp245)
  rf_raster_ssp337 <- raster(rf_result_path_ssp337)
  rf_raster_ssp585 <- raster(rf_result_path_ssp585)
  cat("RF raster loaded: ", rf_result_path_historical, "\n")
  startTime <- Sys.time()
  diff_raster_historical <- maxent_raster_historical - rf_raster_historical
  endTime <- Sys.time()
  cat("Difference raster calculated time: ", "\n")
  print(endTime-startTime)
  diff_raster_ssp126 <- maxent_raster_ssp126 - rf_raster_ssp126
  diff_raster_ssp245 <- maxent_raster_ssp245 - rf_raster_ssp245
  diff_raster_ssp337 <- maxent_raster_ssp337 - rf_raster_ssp337
  diff_raster_ssp585 <- maxent_raster_ssp585 - rf_raster_ssp585
  cat("Difference raster calculated: ", "\n")
  writeRaster(diff_raster_historical, paste(top_diff_file_dir,'/',this_bug,'/diff_historical.tif',sep = ''), format="GTiff", overwrite=TRUE)
  writeRaster(diff_raster_ssp126, paste(top_diff_file_dir,'/',this_bug,'/diff_ssp126.tif',sep = ''), format="GTiff", overwrite=TRUE)
  writeRaster(diff_raster_ssp245, paste(top_diff_file_dir,'/',this_bug,'/diff_ssp245.tif',sep = ''), format="GTiff", overwrite=TRUE)
  writeRaster(diff_raster_ssp337, paste(top_diff_file_dir,'/',this_bug,'/diff_ssp337.tif',sep = ''), format="GTiff", overwrite=TRUE)
  writeRaster(diff_raster_ssp585, paste(top_diff_file_dir,'/',this_bug,'/diff_ssp585.tif',sep = ''), format="GTiff", overwrite=TRUE)
  cat("Difference raster saved: ", "\n")
  startTime <- Sys.time()
  sum_maxent_raster_historical <- cellStats(maxent_raster_historical, sum)
  endTime <- Sys.time()
  cat("Sum raster calculated time: ", "\n")
  print(endTime-startTime)
  sum_rf_raster_historical <- cellStats(rf_raster_historical, sum)
  sum_diff_raster_historical <- cellStats(diff_raster_historical, sum)
  cat("Sum calculated: ", "\n")
  sum_maxent_raster_ssp126 <- cellStats(maxent_raster_ssp126, sum)
  sum_rf_raster_ssp126 <- cellStats(rf_raster_ssp126, sum)
  sum_diff_raster_ssp126 <- cellStats(diff_raster_ssp126, sum)
  
  sum_maxent_raster_ssp245 <- cellStats(maxent_raster_ssp245, sum)
  sum_rf_raster_ssp245 <- cellStats(rf_raster_ssp245, sum)
  sum_diff_raster_ssp245 <- cellStats(diff_raster_ssp245, sum)
  
  sum_maxent_raster_ssp337 <- cellStats(maxent_raster_ssp337, sum)
  sum_rf_raster_ssp337 <- cellStats(rf_raster_ssp337, sum)
  sum_diff_raster_ssp337 <- cellStats(diff_raster_ssp337, sum)
  
  sum_maxent_raster_ssp585 <- cellStats(maxent_raster_ssp585, sum)
  sum_rf_raster_ssp585 <- cellStats(rf_raster_ssp585, sum)
  sum_diff_raster_ssp585 <- cellStats(diff_raster_ssp585, sum)

  all_sum_results <- list("sum_maxent_raster_historical" = sum_maxent_raster_historical,
                           "sum_rf_raster_historical" = sum_rf_raster_historical,
                           "sum_diff_raster_historical" = sum_diff_raster_historical,
                           "sum_maxent_raster_ssp126" = sum_maxent_raster_ssp126,
                           "sum_rf_raster_ssp126" = sum_rf_raster_ssp126,
                           "sum_diff_raster_ssp126" = sum_diff_raster_ssp126,
                           "sum_maxent_raster_ssp245" = sum_maxent_raster_ssp245,
                           "sum_rf_raster_ssp245" = sum_rf_raster_ssp245,
                           "sum_diff_raster_ssp245" = sum_diff_raster_ssp245,
                           "sum_maxent_raster_ssp337" = sum_maxent_raster_ssp337,
                           "sum_rf_raster_ssp337" = sum_rf_raster_ssp337,
                           "sum_diff_raster_ssp337" = sum_diff_raster_ssp337,
                           "sum_maxent_raster_ssp585" = sum_maxent_raster_ssp585,
                           "sum_rf_raster_ssp585" = sum_rf_raster_ssp585,
                           "sum_diff_raster_ssp585" = sum_diff_raster_ssp585)
  
  saveRDS(all_sum_results, paste(top_diff_file_dir,'/',this_bug,'/sum_results.rds',sep = ''))
  
  csv_saving_path_sum <- paste(top_diff_file_dir,'/',this_bug,'/sum_results.csv',sep = '')
  write.csv(all_sum_results, file = csv_saving_path_sum)
  cat("Sum results saved: ", csv_saving_path_sum, "\n")
}


