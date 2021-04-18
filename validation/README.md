# Validation

`validation_geometries`:

The hand-drawn validation polygons for the water extent of each sampled small-waterbody in the validation study are provided as .js files. Each of these files can be loaded into Google Earth Engine. These scripts also contain the date of the Landsat / NAIP imagery that was used for comparison. 

`validation.js`: This script can be used for each validation polygon to train a random forest classifier to predict the surface water extent in the respective sampled Landsat image according to the SWIM classification framework. The script compares the accuracy, sensitivity, and specificity of the SWIM and JRC classification methods to the hand-drawn polygon. The validation_geometry for each sample needs to be added to this script and the prediction range edited to include the respective Landsat image. The variable `cluster_prop` should be adjusted to test various lambda values as detailed in Labuzzetta et al. (2021). 
