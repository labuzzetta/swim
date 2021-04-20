# Case Studies

Three regions were selected to provide case studies of the SWIM Classification Framework in Labuzzetta et al. (2021). The rationale for the selection of each location and a discussion of the results can be found in this paper. In this directory, each case study contains files and scripts essential to reproducing these results. Each case study directory is organized as follows:

1. `analysis`: Most importantly, this directory contains an R markdown file (.Rmd) containing the script used to create figures used to analyze changes in water extent in the respective region of interest. Code used to produce the figures for the paper is included. This directory also includes a .csv file of total water extent based on the SWIM and JRC methods in each available region over the study time period. Additionally, .R scripts used to perform imputation for each region are provided.

2. `classification`: A .tif file of the SWIM framework classification results for each Landsat image is provided.

3. `imputated_data`: Imputation results for the 2nd, 4th, and 5th bands of each Landsat image are provided as .rds files. 

4. `jrc_data`: A .tif file of the JRC classifications associated with each Landsat image is provided.

5. `landsat_data`: The ...names.csv file contains metadata information on each Landsat image. There is a .tif file containing the 6 bands from all Landsat images of the region of interest.

6. `training_data`: A .tif of the three Landsat images used for training the SWIM classification framework random forest classifier is provided.
