var NHD = ee.FeatureCollection("users/chalab13/LWB_IA_all_years_geo"),
    NAIP = ee.ImageCollection("USDA/NAIP/DOQQ");

//////////////////////////////
// BEGIN User Specification //
//////////////////////////////

//Enter the id string for the hand-drawn validation polygon
var L0000e3d0456fb1363e13 = // Paste geometry from validation_geometries file here //
var valid_geo = L0000e3d0456fb1363e13; //Update geometry name
var string_geo_id = "0000e3d0456fb1363e13"; // Remove "L" from name of geometry and save as string //

//Enter the date range for the validation imagery given in validation_geometries file
var predict_range_1 = '2007-09-22';
var predict_range_2 = '2007-09-24';

//Enter desired value of the CSSM tuning parameter
var cluster_prop = 0.5

////////////////////////////
// END User Specification //
////////////////////////////

/////////////////////////////////////////////////////////////////
// The following code should work without adjustment for the   //
// validation cases. The only exceptions are as follows:       //
//  - The number of trees used in the Random Forest training   //
//    for polygon L0000bb56059bed1defc3 should be 100, from 500//
//  - The variable poly for the training region for the        //
//    polygon L0000bb56059bed1defc3 should be changed to       //
//    var poly = NHD.geometry().centroid().buffer(1600)        //
// These changes are made to adjust for memory limits on GEE   //
/////////////////////////////////////////////////////////////////

//Polygon string form of geo_id
var geo_id = string_geo_id

//State to calculate water-body size within
var state = "Iowa";

//Beginning and end of range to find training image
var train_range_1 = '1984-01-01';
var train_range_2 = '2011-12-31';
var train_doy_1 = 120;
var train_doy_2 = 180;
var train_doy_3 = 240;
var train_doy_4 = 300;
var predict_doy_1 = 120;
var predict_doy_2 = 300;

//Proportion Area Specification (Greater than)
//Image must contain valid pixels for greater than this proportion
var area_prop = 0.95;

//Proportion Cloud Speciication (Less than)
//Image much contain less than this proportion of clouds
var cloud_prop = 0.05;

//Retrieve geometry of state border
var state_geo = ee.FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')
  .filter(ee.Filter.eq('Name', state))
  .geometry()

//Find waterbody matching filters by id within state
var NHD = NHD
         //.filterBounds(state_geo)
         .filter(ee.Filter.eq("system:index", geo_id));

//Center on selected waterbody
Map.centerObject(NHD)

//Function to calculate date for end of the month selected
//GEE Functional version - needed for .map() internal operations
var date_range_algorithm = function(month, year) {
var end_range = "";
month = ee.String(month)
year = ee.String(year)
ee.Algorithms.If(
  (month.compareTo("09").eq(0)).or(month.compareTo("04").eq(0)).or(month.compareTo("06").eq(0)).or(month.compareTo("09").eq(0)),
  ee.String(year).cat("-").cat(month).cat("-").cat("30"),
  ee.Algorithms.If(month.compareTo("02"),
    ee.Algorithms.If(ee.Number.parse(year).mod(4).eq(1),
    ee.String(year).cat("-").cat(month).cat("-").cat("29"),
    ee.String(year).cat("-").cat(month).cat("-").cat("28")),
  ee.String(year).cat("-").cat(month).cat("-").cat("31"))
  )
} //end function date_range_algorithm()

//Create the buffered polygon around the waterbody for training
var poly = NHD.geometry().buffer(800)

//Create the buffered polygon around the waterbody for prediction
var predict_poly = NHD.geometry().buffer(800)

//Function to calculate amount of clouds and percent area of land image covers
var calculate_clouds = function(image) {

//Select pixels which do not contain any of the following bit-flag sums
var cloud = image.select(['pixel_qa'],['cloud']).eq(66)
                  .or(image.select(['pixel_qa'],['cloud']).eq(68))
                  .or(image.select(['pixel_qa'],['cloud']).eq(130))
                  .or(image.select(['pixel_qa'],['cloud']).eq(132))
                  .or(image.select(['pixel_qa'],['cloud']).eq(321))
                  .or(image.select(['pixel_qa'],['cloud']).eq(385))
                  .or(image.select(['pixel_qa'],['cloud']).eq(833))
                  .or(image.select(['pixel_qa'],['cloud']).eq(897))
                  .or(image.select(['pixel_qa'],['cloud']).eq(1345))
                  .or(image.select(['pixel_qa'],['cloud']).eq(323))
                  .or(image.select(['pixel_qa'],['cloud']).eq(387))
                  .or(image.select(['pixel_qa'],['cloud']).eq(835))
                  .or(image.select(['pixel_qa'],['cloud']).eq(899))
                  .or(image.select(['pixel_qa'],['cloud']).eq(1347))

                  .and((image.select(['sr_cloud_qa'],['cloud']).eq(0))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(1))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(32))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(3))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(65))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(129))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(67))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(99))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(131))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(163))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(193))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(223))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(159))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(227))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(95))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(31))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(131))).not()

//Calculate the proportion of pixels labeled with a 'cloudy' flag sum
var cloudiness = cloud.reduceRegion({
  reducer: 'mean',
  geometry: poly,
  scale: 30,
});

//Calculate the percent area of the polygon the image contains
var area_pct = image.clip(poly).geometry().area().divide(poly.area());

//Given the image capture date, calculate the corresponding month dates for JRC
var JRC_range_1 = ee.String(ee.Date(image.date()).get('year')).cat("-").cat(ee.String(ee.Date(image.date()).get('month'))).cat("-").cat("01");
var JRC_range_2 = date_range_algorithm(ee.String(ee.Date(image.date()).get('month')), ee.String(ee.Date(image.date()).get('year')));

//Find the matching JRC image for the LANDSAT image
var JRC = ee.Image(ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
  .filterBounds(poly)
  .filterDate(JRC_range_1, JRC_range_2).first())

//Store the JRC image as a band in the LANDSAT image
var temp_truth = JRC.clip(poly).select('water').subtract(ee.Image.constant(1));

//calculate the number and size of water pixels - from the JRC data
var JRC_lakeArea = temp_truth.select(['water']).eq(1);
var JRC_areaImage = JRC_lakeArea.multiply(ee.Image.pixelArea());

//reduce the image to calculate the size of the lake
var JRC_stats = JRC_areaImage.reduceRegion({
  reducer: ee.Reducer.sum(),
  geometry: poly,
  scale:30,
  maxPixels: 1e9
});

//Store the cloud percent, area percent, and total amount of water as statistics in the LANDSAT image
return image.set(cloudiness).set(JRC_stats).set(ee.Dictionary(['area',area_pct]));

} //end function calculate_clouds

//Same function as above altered for images which are not used in
//training but used after training the classifier
var calculate_clouds_test = function(image) {

//Select pixels which do not contain any of the following bit-flag sums
var cloud = image.select(['pixel_qa'],['cloud']).eq(66)
                  .or(image.select(['pixel_qa'],['cloud']).eq(68))
                  .or(image.select(['pixel_qa'],['cloud']).eq(130))
                  .or(image.select(['pixel_qa'],['cloud']).eq(132))
                  .or(image.select(['pixel_qa'],['cloud']).eq(321))
                  .or(image.select(['pixel_qa'],['cloud']).eq(385))
                  .or(image.select(['pixel_qa'],['cloud']).eq(833))
                  .or(image.select(['pixel_qa'],['cloud']).eq(897))
                  .or(image.select(['pixel_qa'],['cloud']).eq(1345))
                  .or(image.select(['pixel_qa'],['cloud']).eq(323))
                  .or(image.select(['pixel_qa'],['cloud']).eq(387))
                  .or(image.select(['pixel_qa'],['cloud']).eq(835))
                  .or(image.select(['pixel_qa'],['cloud']).eq(899))
                  .or(image.select(['pixel_qa'],['cloud']).eq(1347))

                  .and((image.select(['sr_cloud_qa'],['cloud']).eq(0))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(1))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(32))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(3))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(65))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(129))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(67))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(99))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(131))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(163))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(193))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(223))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(159))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(227))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(95))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(31))
                  .or(image.select(['sr_cloud_qa'],['cloud']).eq(131))).not()

//Calculate the proportion of pixels labeled with a 'cloudy' flag sum
var cloudiness = cloud.reduceRegion({
  reducer: 'mean',
  geometry: predict_poly,
  scale: 30,
});

//Calculate the percent area of the polygon the image contains
var area_pct = image.clip(predict_poly).geometry().area().divide(predict_poly.area());

//Store the cloud percent, area percent, and total amount of water as statistics in the LANDSAT image
return image.set(cloudiness).set(ee.Dictionary(['area',area_pct]));

} //end function calculate_clouds

//Collect Landsat 5 images
var images5 = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
.filterBounds(predict_poly)
.filterDate(predict_range_1, predict_range_2)
.filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2));

//Collect Landsat 7 images
var images7 = ee.ImageCollection("LANDSAT/LE07/C01/T1_SR")
.filterBounds(predict_poly)
.filterDate(predict_range_1, predict_range_2)
.filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2))
.filterMetadata('system:index', 'not_equals', 'LE70270322015225');

//Collect Landsat 8 images
var images8 = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR")
.filterBounds(predict_poly)
.filterDate(predict_range_1, predict_range_2)
.filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2));

//Add fake quality band for landsat 8
var addQualityBand = function(image){
return image.addBands(image.select(['sr_aerosol'],['edit_sr_aerosol']).subtract(1))
            .addBands(image.select(['pixel_qa'],['edit_pixel_qa']).subtract(1))
            .select(['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'edit_sr_aerosol', 'edit_pixel_qa'],['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'sr_cloud_qa', 'pixel_qa']);

}

//Change the names of the qa bands for Landsat 8 and subtract 1 to differentiate values from Landsat 5/7 versions
images8 = images8.map(addQualityBand)

//Calculate the NDWI, MNDWI, and MIR indices and add as bands to the image
var addFeatures = function(image){
return image.addBands(ee.Image(ee.Image(image.select('B2')).subtract(ee.Image(image.select('B5'))))
                      .divide(ee.Image(ee.Image(image.select('B2')).add(ee.Image(image.select('B5'))))))
       .addBands(ee.Image(ee.Image(image.select('B3')).subtract(ee.Image(image.select('B4'))))
                      .divide(ee.Image(ee.Image(image.select('B3')).add(ee.Image(image.select('B4'))))))
       .addBands(ee.Image(ee.Image(image.select('B4')).subtract(ee.Image(image.select('B2'))))
                      .divide(ee.Image(ee.Image(image.select('B4')).add(ee.Image(image.select('B2'))))))
       .select(['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'sr_cloud_qa', 'pixel_qa', 'B2_1', 'B3_1', 'B4_1'],['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'sr_cloud_qa', 'pixel_qa', 'MNDWI', 'NDVI', 'NDWI'])
}

//Merge collections
var images = ee.ImageCollection(images5.merge(images7));
var image = ee.Image(ee.ImageCollection(images.merge(images8).map(addFeatures)).first());

//Load all images from the Landsat 5 satellite which have less than desired
//percent cloud cover throughout the training period for the polygon
var imagesA = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
.filterBounds(poly)
.filterDate(train_range_1, train_range_2)
.filter(ee.Filter.dayOfYear(train_doy_1, train_doy_2))
.map(addFeatures)

//Use the calculate_clouds function to add desired statistics to each image
var with_cloudiness = imagesA.map(calculate_clouds);

//List of bands used for prediction in dataset
var bands = ['B5', 'MNDWI', 'NDWI'];

//Select the first image meeting the filter criteria within the 90th percent of water extent
//Clip the image to the region and select the bands that will be used as features
var median = with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).sort('water', false).size().multiply(0.9).ceil()
var train_image_needs_mask = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).reproject('EPSG:4326', null, 30).clip(poly);

//Calculate the corresponding month of the JRC image from the training image
var train_date = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).date();
var JRC_range_year = ee.String(train_date.get('year'));
var JRC_range_month = ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10), ee.String("0").cat(ee.String(train_date.get('month'))), ee.String(train_date.get('month')));

//Produce the range of dates for the month of the JRC image
var JRC_range_1 = JRC_range_year.cat("-").cat(JRC_range_month).cat("-").cat("01");
var JRC_range_2 = date_range_algorithm(ee.String(JRC_range_month), JRC_range_year);

//Load the JRC dataset
var JRC = ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
.filterBounds(poly)
.filterDate(JRC_range_1, JRC_range_2);
var JRC_image = ee.Image(JRC.first());

//Clip the JRC data to the region and create labels (1) land, (2) water
var truth_needs_mask = JRC_image.clip(poly).select('water');

//Invalid pixel of a collection for Truth
var trainMaskBadData = function(image) {
var valid = truth_needs_mask.select('water').gt(0)
var clean = image.mask(valid);
return clean;
};

//Store data for the predictions within this polygon
var prediction_list = ee.List([]);

//Remove any NA in the truth data from the training image
var data = trainMaskBadData(train_image_needs_mask).select(bands);

//Remove an NA data from the truth image by getting rid of (0) labeled pixels and then subtract 1
var truth = trainMaskBadData(truth_needs_mask).select('water').subtract(ee.Image.constant(1));

//Add the truth labels to the LANDSAT training image
data = data.addBands(truth)

//Number of total pixels in data image
var total_pixels = ee.Number(data.sample({factor:1}).size())

//Select only the data bands, not the JRC band for clustering
var clusterImage = data.select(bands)

//Create a feature collection for the clustering step
var cluster_training = clusterImage.select(clusterImage.bandNames()).sample({
region: poly,
scale: 30,
factor:1
});

//Instantiate the clusterer and train it, let Xmeans select
//between 10 and 100 clusters
var clusterer = ee.Clusterer.wekaXMeans({distanceFunction:"Euclidean", minClusters:10, maxClusters:100}).train({features:cluster_training, inputProperties:clusterImage.bandNames()});

//Cluster the input using the trained clusterer.
var result = data.select(bands).cluster(clusterer);

//Add the JRC truth band back to the cluster image
var results = result.addBands(truth)

//Calculate the number of water pixels or pixels with a neighboring water pixel, in each cluster
var waterImage = ee.Image(ee.Dictionary(results.mask(results.reduceNeighborhood(ee.Reducer.max(), ee.Kernel.fixed(3,3,[[0,1,0],[1,1,1],[0,1,0]])).select(['water_max'],['water']).eq(1)).reduceRegion(ee.Reducer.frequencyHistogram(), poly, 30).get('cluster')).toImage())

//Calculate the total number of pixels in each cluster
var allImage = ee.Image(ee.Dictionary(results.reduceRegion({reducer:ee.Reducer.frequencyHistogram(), geometry:poly, scale:30}).get('cluster')).toImage())

//Calculate the ratio of water to total number of pixels in each cluster
var ratios = waterImage
.divide(allImage.select(waterImage.bandNames()))

//Identify the maximum water proportion of the clusters
//Simply used becuase some future steps cannot work on null objects
//Guaranteeing maxCluster is greater than cluster_prop will occur in the trainer_combined step
var maxCluster = ee.List(ratios.reduceRegion(ee.Reducer.max(), poly, 30).toArray().toList()).reduce(ee.Reducer.max());

//Return the cluster number of all clusters with more than desired percent water pixels, otherwise null
var whichWater = function(key, value) {
return ee.Algorithms.If(ee.Number(value).gt(cluster_prop).or(ee.Number(value).eq(maxCluster)), ee.Number.parse(key), null);
};

//Make one image with all of the clusters with greater than cluster_prop water labeled with cluster number
var waterBandImage = ratios.reduceRegion(ee.Reducer.max(), poly, 30).map(whichWater).toImage()

//Match the image to that of the original cluster image. If cluster name matches, save as a 1 for water.
var cluster = waterBandImage.eq(result);

//If there is water in the pixel, return 1, otherwise return 0
cluster = ee.Image(ee.Algorithms.If(waterBandImage.bandNames().length().gt(0), cluster.reduce(ee.Reducer.max()).rename('cluster'), ee.Image.constant(0).rename(['cluster'])))

//Subtract the cluster water truth image and the JRC water truth images from each other
var cluster_diff = cluster.select('cluster').neq(truth.select('water')).rename('diff');

//Mask all pixels that are labeled the same
var maskNonDiffData = function(image) {
var diff = image.select('diff').eq(1);
return image.mask(diff);
};

//Mask all pixels that are labeled differently
var maskDiffData = function(image) {
var diff = image.select('diff').eq(0);
return image.mask(diff);
};

//Create an image of differently labeled pixels and an image of non-differently labeled pixels
var diff_pixels = maskNonDiffData(cluster_diff.addBands(cluster.select(['cluster'],['water']))
                    .addBands(data.select(bands))).select(ee.List(['water']).cat(bands));

//Calculate the number of pixels that are labeled differently
var diff_size = ee.Number(diff_pixels.sample({factor:1}).size());

//Create the training dataset
var training = data;

// Sample the input imagery to get a FeatureCollection of training data.
var trainer = training.sample({
region:poly,
factor: 1,
scale:30
});

// Sample the input imagery again to get a second FeatureCollection of the diff_pixels
var binomial_trainer = diff_pixels.sample({
region:poly,
factor: 1,
scale: 30
})

// Generate the new training image with the cluster_diff pixels added
var trainer_combined =  ee.Algorithms.If(ee.Number(maxCluster).gt(cluster_prop), ee.FeatureCollection(trainer.merge(binomial_trainer)), ee.FeatureCollection(trainer));

//Repeat above section with second training image:
var imagesB = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
.filterBounds(poly)
.filterDate(train_range_1, train_range_2)
.filter(ee.Filter.dayOfYear(train_doy_2, train_doy_3))
.map(addFeatures)

var with_cloudiness = imagesB.map(calculate_clouds);

var bands = ['B5', 'MNDWI', 'NDWI'];

var median = with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).sort('water', false).size().multiply(0.9).ceil()
var train_image_needs_mask = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).reproject('EPSG:4326', null, 30).clip(poly);

var train_date = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).date();
var JRC_range_year = ee.String(train_date.get('year'));
var JRC_range_month = ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10), ee.String("0").cat(ee.String(train_date.get('month'))), ee.String(train_date.get('month')));

var JRC_range_1 = JRC_range_year.cat("-").cat(JRC_range_month).cat("-").cat("01");
var JRC_range_2 = date_range_algorithm(ee.String(JRC_range_month), JRC_range_year);

var JRC = ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
.filterBounds(poly)
.filterDate(JRC_range_1, JRC_range_2);

var JRC_image = ee.Image(JRC.first());
var truth_needs_mask = JRC_image.clip(poly).select('water');

var trainMaskBadData = function(image) {
var valid = truth_needs_mask.select('water').gt(0)
var clean = image.mask(valid);
return clean;
};

var prediction_list = ee.List([]);

var data = trainMaskBadData(train_image_needs_mask).select(bands);

var truth = trainMaskBadData(truth_needs_mask).select('water').subtract(ee.Image.constant(1));

data = data.addBands(truth)

var total_pixels = ee.Number(data.sample({factor:1}).size())

var clusterImage = data.select(bands)

var cluster_training = clusterImage.select(clusterImage.bandNames()).sample({
region: poly,
scale: 30,
factor:1
});

var clusterer = ee.Clusterer.wekaXMeans({distanceFunction:"Euclidean", minClusters:10, maxClusters:100}).train({features:cluster_training, inputProperties:clusterImage.bandNames()});

var result = data.select(bands).cluster(clusterer);

var results = result.addBands(truth)

var waterImage = ee.Image(ee.Dictionary(results.mask(results.reduceNeighborhood(ee.Reducer.max(), ee.Kernel.fixed(3,3,[[0,1,0],[1,1,1],[0,1,0]])).select(['water_max'],['water']).eq(1)).reduceRegion(ee.Reducer.frequencyHistogram(), poly, 30).get('cluster')).toImage())

var allImage = ee.Image(ee.Dictionary(results.reduceRegion({reducer:ee.Reducer.frequencyHistogram(), geometry:poly, scale:30}).get('cluster')).toImage())

var ratios = waterImage
.divide(allImage.select(waterImage.bandNames()))

var maxCluster = ee.List(ratios.reduceRegion(ee.Reducer.max(), poly, 30).toArray().toList()).reduce(ee.Reducer.max());

var whichWater = function(key, value) {
return ee.Algorithms.If(ee.Number(value).gt(cluster_prop).or(ee.Number(value).eq(maxCluster)), ee.Number.parse(key), null);
};

var waterBandImage = ratios.reduceRegion(ee.Reducer.max(), poly, 30).map(whichWater).toImage()

var cluster = waterBandImage.eq(result);

cluster = ee.Image(ee.Algorithms.If(waterBandImage.bandNames().length().gt(0), cluster.reduce(ee.Reducer.max()).rename('cluster'), ee.Image.constant(0).rename(['cluster'])))

var cluster_diff = cluster.select('cluster').neq(truth.select('water')).rename('diff');

var maskNonDiffData = function(image) {
var diff = image.select('diff').eq(1);
return image.mask(diff);
};

var maskDiffData = function(image) {
var diff = image.select('diff').eq(0);
return image.mask(diff);
};

var diff_pixels = maskNonDiffData(cluster_diff.addBands(cluster.select(['cluster'],['water']))
                    .addBands(data.select(bands))).select(ee.List(['water']).cat(bands));

var diff_size = ee.Number(diff_pixels.sample({factor:1}).size());

var training = data;

var trainer = training.sample({
region:poly,
factor: 1,
scale:30
});

var binomial_trainer = diff_pixels.sample({
region:poly,
factor: 1,
scale: 30
})

var trainer_combined_2 =  ee.Algorithms.If(ee.Number(maxCluster).gt(cluster_prop), ee.FeatureCollection(trainer.merge(binomial_trainer)), ee.FeatureCollection(trainer));

//Merge the 2 edited training images together
var trainer_combined_3 = ee.FeatureCollection(ee.FeatureCollection(trainer_combined).merge(trainer_combined_2))

//Repeat the process a third time for the final training image
var imagesC = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
.filterBounds(poly)
.filterDate(train_range_1, train_range_2)
.filter(ee.Filter.dayOfYear(train_doy_3, train_doy_4))
.map(addFeatures)

var with_cloudiness = imagesC.map(calculate_clouds);

var bands = ['B5', 'MNDWI', 'NDWI'];

var median = with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).sort('water', false).size().multiply(0.9).ceil()
var train_image_needs_mask = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).reproject('EPSG:4326', null, 30).clip(poly);

var train_date = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).date();
var JRC_range_year = ee.String(train_date.get('year'));
var JRC_range_month = ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10), ee.String("0").cat(ee.String(train_date.get('month'))), ee.String(train_date.get('month')));

var JRC_range_1 = JRC_range_year.cat("-").cat(JRC_range_month).cat("-").cat("01");
var JRC_range_2 = date_range_algorithm(ee.String(JRC_range_month), JRC_range_year);

var JRC = ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
.filterBounds(poly)
.filterDate(JRC_range_1, JRC_range_2);

var JRC_image = ee.Image(JRC.first());

var truth_needs_mask = JRC_image.clip(poly).select('water');

var trainMaskBadData = function(image) {
var valid = truth_needs_mask.select('water').gt(0)
var clean = image.mask(valid);
return clean;
};

var prediction_list = ee.List([]);

var data = trainMaskBadData(train_image_needs_mask).select(bands);

var truth = trainMaskBadData(truth_needs_mask).select('water').subtract(ee.Image.constant(1));

data = data.addBands(truth)

var total_pixels = ee.Number(data.sample({factor:1}).size())

var clusterImage = data.select(bands)

var cluster_training = clusterImage.select(clusterImage.bandNames()).sample({
region: poly,
scale: 30,
factor:1
});

var clusterer = ee.Clusterer.wekaXMeans({distanceFunction:"Euclidean", minClusters:10, maxClusters:100}).train({features:cluster_training, inputProperties:clusterImage.bandNames()});

var result = data.select(bands).cluster(clusterer);

var results = result.addBands(truth)

var waterImage = ee.Image(ee.Dictionary(results.mask(results.reduceNeighborhood(ee.Reducer.max(), ee.Kernel.fixed(3,3,[[0,1,0],[1,1,1],[0,1,0]])).select(['water_max'],['water']).eq(1)).reduceRegion(ee.Reducer.frequencyHistogram(), poly, 30).get('cluster')).toImage())

var allImage = ee.Image(ee.Dictionary(results.reduceRegion({reducer:ee.Reducer.frequencyHistogram(), geometry:poly, scale:30}).get('cluster')).toImage())

var ratios = waterImage
.divide(allImage.select(waterImage.bandNames()))

var maxCluster = ee.List(ratios.reduceRegion(ee.Reducer.max(), poly, 30).toArray().toList()).reduce(ee.Reducer.max());

var whichWater = function(key, value) {
return ee.Algorithms.If(ee.Number(value).gt(cluster_prop).or(ee.Number(value).eq(maxCluster)), ee.Number.parse(key), null);
};

var waterBandImage = ratios.reduceRegion(ee.Reducer.max(), poly, 30).map(whichWater).toImage()

var cluster = waterBandImage.eq(result);

cluster = ee.Image(ee.Algorithms.If(waterBandImage.bandNames().length().gt(0), cluster.reduce(ee.Reducer.max()).rename('cluster'), ee.Image.constant(0).rename(['cluster'])))

var cluster_diff = cluster.select('cluster').neq(truth.select('water')).rename('diff');

var maskNonDiffData = function(image) {
var diff = image.select('diff').eq(1);
return image.mask(diff);
};

var maskDiffData = function(image) {
var diff = image.select('diff').eq(0);
return image.mask(diff);
};

var diff_pixels = maskNonDiffData(cluster_diff.addBands(cluster.select(['cluster'],['water']))
                    .addBands(data.select(bands))).select(ee.List(['water']).cat(bands));

var diff_size = ee.Number(diff_pixels.sample({factor:1}).size());

var training = data;

var trainer = training.sample({
region:poly,
factor: 1,
scale:30
});

var binomial_trainer = diff_pixels.sample({
region:poly,
factor: 1,
scale: 30
})

var trainer_combined_4 =  ee.Algorithms.If(ee.Number(maxCluster).gt(cluster_prop), ee.FeatureCollection(trainer.merge(binomial_trainer)), ee.FeatureCollection(trainer));

//Merge all three edited training images into one feature collection
var trainer_combined = ee.FeatureCollection(ee.FeatureCollection(trainer_combined_3).merge(trainer_combined_4))

//Make a Random Forest classifier and train it.
var classifier = ee.Classifier.randomForest(500)
.train(trainer_combined, 'water', bands);

//Classify the input imagery.
var classified = training.select().classify(classifier);

//Get a confusion matrix representing resubstitution accuracy.
var trainAccuracy = classifier.confusionMatrix();

//Add in the statistics for the test imagery using the test version of withCloudiness
var test_withCloudiness = ee.ImageCollection(ee.List([image])).map(calculate_clouds_test);

//Find the image acquisition date
var test_image_date = ee.Image(test_withCloudiness.filter(ee.Filter.gte('area', area_prop)).sort('cloud').first()).date();
var test_date = ee.Date.fromYMD(test_image_date.get('year'),
                              test_image_date.get('month'),
                              test_image_date.get('day'));

//Calculate the corresponding month of the JRC image from the training image
var test_image_date = ee.Image(test_withCloudiness.filter(ee.Filter.gte('area', area_prop)).sort('cloud').first()).date();
var JRC_range_year = ee.String(test_image_date.get('year'));
var JRC_range_month = ee.Algorithms.If(ee.Number(test_image_date.get('month')).lt(10), ee.String("0").cat(ee.String(test_image_date.get('month'))), ee.String(test_image_date.get('month')));

//Produce the range of dates for the month of the JRC image
var JRC_range_1 = JRC_range_year.cat("-").cat(JRC_range_month).cat("-").cat("01");
var JRC_range_2 = date_range_algorithm(ee.String(JRC_range_month), JRC_range_year);

//Clip the image to the region and select the bands that will be used as features
var test_image_needs_mask = ee.Image(test_withCloudiness.filter(ee.Filter.gte('area', area_prop)).sort('cloud').first()).reproject('EPSG:4326', null, 30).clip(predict_poly);

//Load the JRC dataset
var JRC = ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
.filterBounds(predict_poly)
.filterDate(JRC_range_1, JRC_range_2);

//Select the JRC data and reproject
var JRC_july_2002_test = ee.Image(JRC.first());

//Clip the JRC data to the region and create labels
var truth_needs_mask_test = JRC_july_2002_test.clip(predict_poly).select('water');

//Some JRC data does not block clouds.... it actually blocks valid pixels - use following to skip mask
var test_image = test_image_needs_mask.select(bands);
var truth_test = truth_needs_mask_test.select('water').subtract(ee.Image.constant(1)).gt(0);

Map.addLayer(test_image.select(["B5"]), {min:0, max:1200}, "test image B5")

//Create a feature collection from the prediction data
var prediction = test_image.addBands(truth_test).sample({
factor: 1,
region: predict_poly,
scale: 30
})

//Use the trained classifier to predict the labels
var predicted = prediction.classify(classifier);

//Measure prediction accuracy against the JRC-truth
var predictAccuracy = predicted.errorMatrix('water', 'classification');

//Create an image that has the prediction labels stored with it
var prediction_image = test_image.classify(classifier);

//Find any National Agriculture Imagery Program images for high-resolution imagery
var NAIP = ee.ImageCollection('USDA/NAIP/DOQQ')
.filterBounds(predict_poly)
.filterDate(JRC_range_1, JRC_range_2);

//Count the number of images found within the date range
var NAIP_images = NAIP.size();

//Calculate the number and size of water pixels - from the prediction
var prediction_lakeArea = prediction_image.select(['classification']).eq(1);
var prediction_areaImage = prediction_lakeArea.multiply(ee.Image.pixelArea());

//Reduce the image to calculate the size of the lake
var prediction_stats = prediction_areaImage.reduceRegion({
reducer: ee.Reducer.sum(),
geometry: predict_poly,
scale:30,
maxPixels: 1e9
});

//Calculate the number and size of water pixels - from the JRC data
var JRC_lakeArea = truth_test.select(['water']).eq(1);
var JRC_areaImage = JRC_lakeArea.multiply(ee.Image.pixelArea());

//Reduce the image to calculate the size of the lake
var JRC_stats = JRC_areaImage.reduceRegion({
reducer: ee.Reducer.sum(),
geometry: predict_poly,
scale:30,
maxPixels: 1e9
});

//Calculate the size of the watervody
var NHD_geometry = NHD.filterBounds(predict_poly).filterMetadata(ee.String('P').cat(test_image_date.get('year')), 'equals', '1').geometry()
var NHD_size = predict_poly.intersection(NHD_geometry).area();

//Return data from prediction
var result = image.clip(predict_poly).addBands(prediction_image).addBands(truth_test);

Map.addLayer(result, {bands: ['water', 'classification', 'classification'], min:0, max:1})

//Find NAIP image used for validation polygon
var NAIP_image = NAIP.filterBounds(predict_poly)
  .filterDate(JRC_range_1, JRC_range_2)
  .mosaic().clip(predict_poly)

Map.addLayer(NAIP_image)

//Calculate values for quantifying CSSM method sensitivity
var mask = result.select("classification");
var water_area = ee.Image.pixelArea().multiply(mask).rename('water_area');
var land_area = ee.Image.pixelArea().multiply(mask.eq(0)).rename('incorrect_land_area')
var water = water_area.addBands(land_area).reduceRegion({
reducer: ee.Reducer.sum(),
geometry: valid_geo,
scale: 30
});

//Calculate values for quantifying CSSM method specificity
var land_geo = predict_poly.union(valid_geo).difference(valid_geo)
var mask = result.select("classification");
var water_area = ee.Image.pixelArea().multiply(mask).rename('incorrect_water_area');
var land_area = ee.Image.pixelArea().multiply(mask.eq(0)).rename('land_area')
var land = water_area.addBands(land_area).reduceRegion({
reducer: ee.Reducer.sum(),
geometry: land_geo,
scale: 30
});

//Output CSSM accuracy statistics
print('CSSM Accuracy: ', (ee.Number(water.get("water_area")).add(ee.Number(land.get("land_area")))
                    .divide(ee.Number(water.get('incorrect_land_area')).add(ee.Number(water.get("water_area"))).add(ee.Number(land.get('incorrect_water_area'))).add(ee.Number(land.get('land_area'))))))
print('CSSM Sensitivity: ', (ee.Number(water.get('water_area'))
                    .divide(ee.Number(water.get('incorrect_land_area')).add(ee.Number(water.get("water_area"))))))
print('CSSM Specificity: ', (ee.Number(land.get('land_area'))
                    .divide(ee.Number(land.get('incorrect_water_area')).add(ee.Number(land.get("land_area"))))))

//Calculate values for quantifying JRC method sensitivity
var mask = result.select("water");
var water_area = ee.Image.pixelArea().multiply(mask).rename('water_area');
var land_area = ee.Image.pixelArea().multiply(mask.eq(0)).rename('incorrect_land_area')
var water = water_area.addBands(land_area).reduceRegion({
reducer: ee.Reducer.sum(),
geometry: valid_geo,
scale: 30
});

//Calculate values for quantifying JRC method specificity
var land_geo = predict_poly.union(valid_geo).difference(valid_geo)
var mask = result.select("water");
var water_area = ee.Image.pixelArea().multiply(mask).rename('incorrect_water_area');
var land_area = ee.Image.pixelArea().multiply(mask.eq(0)).rename('land_area')
var land = water_area.addBands(land_area).reduceRegion({
reducer: ee.Reducer.sum(),
geometry: land_geo,
scale: 30
});

//Output JRC accuracy statistics
print('JRC Accuracy: ', (ee.Number(water.get("water_area")).abs().add(ee.Number(land.get("land_area")).abs())
                    .divide(ee.Number(water.get('incorrect_land_area')).abs().add(ee.Number(water.get("water_area")).abs()).add(ee.Number(land.get('incorrect_water_area')).abs()).add(ee.Number(land.get('land_area')).abs()))))
print('JRC Sensitivity: ', (ee.Number(water.get('water_area')).abs()
                    .divide(ee.Number(water.get('incorrect_land_area')).abs().add(ee.Number(water.get("water_area")).abs()))))
print('JRC Specificity: ', (ee.Number(land.get('land_area'))
                    .divide(ee.Number(land.get('incorrect_water_area')).abs().add(ee.Number(land.get("land_area")).abs()))))

//Calculate prediction polygon area
var total_area = predict_poly.union(valid_geo);

//Calculate water polygon area from CSSM classification
var mask = result.select("classification");
var water_area = ee.Image.pixelArea().multiply(mask).rename('water_area');
var water = water_area.reduceRegion({
reducer: ee.Reducer.sum(),
geometry: total_area,
scale: 30
});

//Calculate water polygon area from JRC classification
var mask = result.select("water");
var water_area = ee.Image.pixelArea().multiply(mask).rename('water_area');
var jrc_water = water_area.reduceRegion({
reducer: ee.Reducer.sum(),
geometry: total_area,
scale: 30
});

//Output values for predicted proportion water statistics
print('Valid water %: ', valid_geo.area().divide(total_area.area()))
print('CSSM water %: ', ee.Number(water.get('water_area')).divide(total_area.area()))
print('JRC water %: ', ee.Number(jrc_water.get('water_area')).divide(total_area.area()))
