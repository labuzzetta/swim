# How to SWIM

### 1. Run `collect_data.js` on Google Earth Engine

- Define study region polygon
- Adjust parameters in "User Specification Required"
- Run: approximately 5 - 10 minutes
- Store landsat_images.tif file, landsat_training.tif file, and names landsat_names.csv file in directory for use in R

### 2. Run `convert_TIF_for_GAPFILL.R`

- Open R and source convert_TIF_for_GAPFILL.R script
- Set working directory to find landsat_images.tif and landsat_names.csv file from above
- Run `convert_TIF_for_GAPFILL(getwd(), "landsat_images.tif", dates = "landsat_names.csv", out = "landsat_gapfill")`
- A .csv file for each landsat band will be produced (this can take a while depending on the total # of Landsat images downloaded)

### 3. Impute missing data via `gapfill.R`

- For SWIM, missing data from at least bands 2, 4, and 5 must be imputed
- Use `gapfill.R` to impute missing values in the .csv representations of these data from above
- See case study scripts for an examples of using gapfill.R for imputation. These scripts are:
  - `case_studies/colo_river/analysis/colo_river_gapfill.R`
  - `case_studies/devils_lake/analysis/devils_lake_gapfill.R`
  - `case_studies/new_orleans/analysis/new_orleans.R`
- Additionally, see `gapfill_example.R`

### 4. Train classifier and classify imputed imagery

- The .R function `train_and_predict_RF.R` can be used to train a Random Forest classifier according to the SWIM methodology
- Many files and parameters are needed as input for this function. These can be obtained from collect_data.js and previous steps

### 5. Analyze classifications

- The classified images from `train_and_predict_RF.R` will be output in raster brick format. 
- These images may be analyzed in a manner similar to that presented in the case study analysis scripts:
  - `case_studies/colo_river/analysis/colo_river.Rmd`
  - `case_studies/devils_lake/analysis/devils_lake.Rmd`
  - `case_studies/new_orleans/analysis/new_orleans.Rmd`
