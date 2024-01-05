# r_chagasM

## saving file locations

- top_file_dir

- save_all_output_dir: top_file_dir/this_bug/

- maxent_evaluate_dir: top_file_dir/this_bug/evaluate/
- maxent_result_dir: top_file_dir/this_bug/results/
- maxent_model_dir: top_file_dir/this_bug/results/model/
- maxent_raster_dir: top_file_dir/this_bug/results/output_raster/

save the final model (from which we generate prediction). The models used in cross-validation are not saved.
- maxent_model_path: top_file_dir/this_bug/results/model/thisbug.RDS
- save_raster_path: top_file_dir/this_bug/results/historical_predict_this_bug.tif
- save_input_data_path: top_file_dir/this_bug/results/input_data_this_bug.RDS
- save_raster_path_ssp1: top_file_dir/this_bug/results/ssp126_predict_this_bug.tif
- save_raster_path_ssp2: top_file_dir/this_bug/results/ssp245_predict_this_bug.tif
- save_raster_path_ssp3: top_file_dir/this_bug/results/ssp370_predict_this_bug.tif
- save_raster_path_ssp5: top_file_dir/this_bug/results/ssp585_predict_this_bug.tif

- maxent evaluation results (cross validation, i indicates the number of run): top_file_dir/this_bug/evaluate/i
- maxent evaluation results (for the final model used in prediction): top_file_dir/this_bug/evaluate/final

## list of functions

### all the file

all_saving_paths(top_file_dir,this_bug)
- return a list of paths for saving all the outputs from the model.

### prepare input data

prepare_input_data(occ_raw_path,clim)
- return train-test split data for MaxEnt

prepare_input_data_kfold(occ_raw_path,clim,number_of_folds,shapefile_dir, shapefile_name,split_ratio=0.90)
- return a list of train-test split data for MaxEnt based on split_ratio, split_ratio of the data is further divided into groups to perform (number-replicate)-fold cross-validation.

prepare_input_data_kfold_buffer(occ_raw_path,clim,number_of_folds,buff_width = 0.5)
- return a list of train-test split data for MaxEnt based on split_ratio, split_ratio of the data is further divided into groups to perform (number-replicate)-fold cross-validation.
- Different from the prepare_input_data_kfold, the background points here are selected from the region but outside the buffers of buff_width degree radius around each occurence points.


