library("raster")

# we want to iterate through all the input raster files, calculate the mean and standard deviation of each file

clim_dir <- "/Users/liting/Documents/data/resample_mask/historical/"
dir_resample_mask <- "/Users/liting/Documents/data/resample_mask"
dir_resample_mask_ssp1 <- paste(dir_resample_mask,'/ssp126_2071_2100/',sep = '')
dir_resample_mask_ssp2 <- paste(dir_resample_mask,'/ssp245_2071_2100/',sep = '')
dir_resample_mask_ssp3 <- paste(dir_resample_mask,'/ssp370_2071_2100/',sep = '')
dir_resample_mask_ssp5 <- paste(dir_resample_mask,'/ssp585_2071_2100/',sep = '')

dir_list <- list(clim_dir, dir_resample_mask_ssp1, dir_resample_mask_ssp2, dir_resample_mask_ssp3, dir_resample_mask_ssp5)

for (this_dir in dir_list){
  clim_list <- list.files(this_dir, pattern = '.tif$', full.names = T)
  print(clim_list)
  save_df <- data.frame(clim_name = character(), clim_mean = numeric(), clim_sd = numeric())
  for (this_clim in clim_list){
    print(this_clim)
    clim_raster <- raster(this_clim)
    clim_name <- substr(basename(this_clim), 1, nchar(basename(this_clim))-4)
    print(clim_name)
    start_time <- Sys.time()
    clim_mean <- cellStats(clim_raster, mean)
    end_time <- Sys.time()
    print(end_time-start_time)
    start_time <- Sys.time()
    clim_sd <- cellStats(clim_raster, sd)
    end_time <- Sys.time()
    print(end_time-start_time)
    print(clim_mean)
    print(clim_sd)
    save_df <- rbind(save_df, data.frame(clim_name, clim_mean, clim_sd))
  }
  
  RDS_saving_path <- paste(this_dir,'/mean_sd.rds',sep = '')
  saveRDS(save_df, RDS_saving_path)
  csv_saving_path <- paste(this_dir,'/mean_sd.csv',sep = '')
  write.csv(save_df, csv_saving_path)
}

