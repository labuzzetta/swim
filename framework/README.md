# How to SWIM

1. Run collect_data.js on Google Earth Engine

- Define study region polygon
- Adjust parameters in User Specification Required section
- Run: approximately 5 - 10 minutes
- Store landsat_images.tif file and names landsat_names.csv file in directory for use in R

2. Run convert_TIF_for_GAPFILL.R

- Open R and source convert_TIF_for_GAPFILL.R script
- Set working directory to find .tif and .csv file from above
- Run `convert_TIF_for_GAPFILL(getwd(), "landsat_images.tif", dates = "landsat_names.csv", out = "landsat_gapfill")`
- Files will 

3. Impute missing data via gapfill_landsat2.R

- For SWIM, missing data in bands 2, 4, and 5 must be imputed
- Use gapfill_landsat2.R to impute missing values in the .csv representations of these data from above
- See case studies for an examples of using gapfill_landsat2.R for imputation

4. Train classifier
