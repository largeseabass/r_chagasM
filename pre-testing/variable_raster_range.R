# calculate variable range

library(raster)
library(dplyr)


# List of variable names
variables <- c("Tave_sp", "Tave_sm", "Tave_at", "Tave_wt", "MAT", "MCMT", "MWMT", "TD", "EMT", "EXT", "DD_0", "DD5", "DD_18", "DD18", "DD1040", "FFP", "bFFP", "eFFP", "PAS", "NFFD", "Eref", "CMD", "CMI", "AHM", "SHM", "PPT_sp", "PPT_sm", "PPT_at", "PPT_wt", "RH", "MAP", "MSP", "Barren", "cropland", "forest", "grassland", "permanent snow and ice", "urban", "water")
# Paths to folders
folders <- c("/Users/liting/Documents/data/resample_mask/historical", 
             "/Users/liting/Documents/data/resample_mask/ssp126_2071_2100", 
             "/Users/liting/Documents/data/resample_mask/ssp245_2071_2100", 
             "/Users/liting/Documents/data/resample_mask/ssp370_2071_2100", 
             "/Users/liting/Documents/data/resample_mask/ssp585_2071_2100")

# Prepare a data frame to store the results
results_df <- data.frame(variable = variables)

# Calculate the min and max values for each variable in each folder
for (folder in folders) {
  # Create a column for the current folder to store range as (min, max)
  results_df[[basename(folder)]] <- NA
  
  for (variable in variables) {
    raster_path <- file.path(folder, paste0(variable, ".tif"))
    
    if (file.exists(raster_path)) {
      r <- raster(raster_path)
      min_val <- min(values(r), na.rm = TRUE)
      max_val <- max(values(r), na.rm = TRUE)
      # Store the range in the format (min, max)
      results_df[results_df$variable == variable, basename(folder)] <- paste0("(", min_val, ", ", max_val, ")")
      cat("Variable: ", variable, " Folder: ", basename(folder), " Min: ", min_val, " Max: ", max_val, "\n")
    } else {
      # If the file does not exist, perhaps indicate it in the results
      results_df[results_df$variable == variable, basename(folder)] <- "NA"
    }
  }
}


write.csv(results, "/Users/liting/Documents/GitHub/r_chagasM/output/figures/raster_ranges.csv", row.names = FALSE)
