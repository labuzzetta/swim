var new_orleans_pred = /* color: #d63000 */ee.Geometry.Polygon(
        [[[-90.45822552238303, 30.023402487169445],
          [-90.45822828111125, 29.99434768896399],
          [-90.4247465281112, 29.99453888240871],
          [-90.424749268695, 30.023402487171193]]]),
    devils_lake_pred = /* color: #d63000 */ee.Geometry.Polygon(
        [[[-99.26189390735397, 48.089727214109104],
          [-99.26189276513912, 48.06067241581544],
          [-99.21850528508463, 48.06086282320035],
          [-99.21850180767149, 48.089726427966866],
          [-99.26189390735397, 48.089727214109104]]]),
    pothole_pred = /* color: #ffc82d */ee.Geometry.Polygon(
        [[[-98.29844091956329, 46.16715643218263],
          [-98.29844043320668, 46.13810163388089],
          [-98.25659199187595, 46.138292147552555],
          [-98.25658955471226, 46.1671557523632],
          [-98.29844091956329, 46.16715643218263]]]),
    colo_river_pred = /* color: #d63000 */ee.Geometry.Polygon(
        [[[-110.45318780454625, 37.82622514894149],
          [-110.45309187010344, 37.797015477497936],
          [-110.41640311007609, 37.797238480959884],
          [-110.41649535618944, 37.82607652470161]]]);

///////////////////////////////////
/// USER SPECIFICATION REQUIRED ///
///////////////////////////////////

//Beginning and end of range to find training image
var train_range_1 = '1984-01-01';
var train_range_2 = '2015-12-31';
//Split training into 120-180, 180-240, 240-300
var train_doy_1 = 120;
var train_doy_2 = 180;
var train_doy_3 = 240;
var train_doy_4 = 300;
//Beginning and end of range to select prediction images
var predict_range_1 = '1984-01-01';
var predict_range_2 = '2018-08-11';
var predict_doy_1 = 0;
var predict_doy_2 = 365;

//Proportion Area Specification (Greater than)
//Image must contain valid pixels for greater than this proportion
var area_prop = 0.95;
//Proportion Cloud Speciication (Less than)
//Image much contain less than this proportion of clouds
var cloud_prop = 0.05;

//Polygon for region of interest (ROI)
var poly = new_orleans_pred

///////////////////////////////////
/// END USER SPECIFICATION AREA ///
///////////////////////////////////


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

//Stores data that will be returned for each water-body
var area_list = ee.List([]);

//function to calculate amount of clouds and percent area of land image covers
var calculate_clouds = function(image) {
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

  var cloudiness = cloud.reduceRegion({
    reducer: 'mean',
    geometry: poly,
    scale: 30,
  });
  var area_pct = image.clip(poly).geometry().area(30).divide(poly.area(30));
  var missingness = image.select(['pixel_qa'],['missing']).eq(0).reduceRegion({
    reducer: 'sum',
    geometry: poly,
    scale: 30,
  });

  var JRC_range_1 = ee.String(ee.Date(image.date()).get('year')).cat("-").cat(ee.String(ee.Date(image.date()).get('month'))).cat("-").cat("01");
  var JRC_range_2 = date_range_algorithm(ee.String(ee.Date(image.date()).get('month')), ee.String(ee.Date(image.date()).get('year')));

  var JRC = ee.Image(ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
  .filterBounds(poly)
  .filterDate(JRC_range_1, JRC_range_2).first())

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

  return image.set(cloudiness).set(missingness).set(JRC_stats).set(ee.Dictionary(['area',area_pct]));
} //end function calculate_clouds

//version for prediction region!
var calculate_clouds_test = function(image) {
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

  var cloudiness = cloud.reduceRegion({
    reducer: 'mean',
    geometry: poly,
    scale: 30,
  });
  var area_pct = image.clip(poly).geometry().area(30).divide(poly.area(30));
  var missingness = image.select(['pixel_qa'],['missing']).eq(0).reduceRegion({
    reducer: 'sum',
    geometry: poly,
    scale: 30,
  });
  return image.set(cloudiness).set(missingness).set(ee.Dictionary(['area',area_pct]));
} //end function calculate_clouds

//Collect Landsat 5 images
var images5 = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(predict_range_1, predict_range_2)
  .filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2));

//Collect Landsat 7 images
var images7 = ee.ImageCollection("LANDSAT/LE07/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(predict_range_1, predict_range_2)
  .filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2));

//Collect Landsat 8 images
var images8 = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(predict_range_1, predict_range_2)
  .filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2));

//Function to add edited quality band for Landsat 8
var addQualityBand = function(image){
  return image.addBands(image.select(['sr_aerosol'],['edit_sr_aerosol']).subtract(1))
              .addBands(image.select(['pixel_qa'],['edit_pixel_qa']).subtract(1))
    .select(['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'edit_sr_aerosol', 'edit_pixel_qa'],['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'sr_cloud_qa', 'pixel_qa']);

}
//Add edited quality band for Landsat 8 imagery
images8 = images8.map(addQualityBand)

//Merge collections to collect all Landsat Images
var images = ee.ImageCollection(images5.merge(images7));
var image = ee.ImageCollection(images.merge(images8));

var bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'water']

var test_withCloudiness = ee.ImageCollection(image).map(calculate_clouds_test);

var test_images = test_withCloudiness

var trim_images = function(image){

  var train_date = image.date();
  var JRC_range_year = ee.String(train_date.get('year'));
  var JRC_range_month = ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10), ee.String("0").cat(ee.String(train_date.get('month'))), ee.String(train_date.get('month')));

  //Produce the rnage of dates for the month of the JRC image
  var JRC_range_1 = JRC_range_year.cat("-").cat(JRC_range_month).cat("-").cat("01");
  var JRC_range_2 = date_range_algorithm(ee.String(JRC_range_month), JRC_range_year);

  image = image.updateMask(image.select(['pixel_qa'],['cloud']).eq(66)
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
            .or(image.select(['sr_cloud_qa'],['cloud']).eq(131))))

  //Load the JRC dataset
  var JRC = ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
    .filterBounds(poly)
    .filterDate(JRC_range_1, JRC_range_2);

  var JRC_missing = ee.Image(ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
    .filterBounds(poly)
    .filterDate("1984-06-01", "1984-06-30").first()).multiply(0);

  var JRC = JRC.merge(ee.ImageCollection(JRC_missing));

  var JRC_image = ee.Image(JRC.first());
  //Clip the JRC data to the region and create labels (1) land, (2) water

  var truth_needs_mask = JRC_image.select('water');

  image = image.addBands(truth_needs_mask)

  return image.clip(poly)

}

test_images = test_images.map(trim_images)

var first = ee.List([
  ee.Image()
]);

print(test_images)

var accumulate = function(image, list) {

  var previous = ee.Image(ee.List(list).get(0));
      image = image.select(bands,
      ee.List([
      ee.String('B1_').cat(image.get("system:index")),
      ee.String('B2_').cat(image.get("system:index")),
      ee.String('B3_').cat(image.get("system:index")),
      ee.String('B4_').cat(image.get("system:index")),
      ee.String('B5_').cat(image.get("system:index")),
      ee.String('B7_').cat(image.get("system:index")),
      ee.String('water_').cat(image.get("system:index"))
    ]))

  // Add the current anomaly to make a new cumulative anomaly image.
  var added = previous.addBands(image)
  return ee.List([added]);
};

// Create an ImageCollection of cumulative anomaly images by iterating.
// Since the return type of iterate is unknown, it needs to be cast to a List.
var cumulative = ee.Image(ee.ImageCollection(ee.List(test_images.iterate(accumulate, first))).first()).toFloat();

var names = ee.FeatureCollection(test_images.map(function(image){return ee.Feature(poly).set("id",image.get("system:index")).setGeometry(null)}))

print(names)

Export.image.toDrive({image:cumulative, description:"landsat_images", folder:"r_data", scale:30, region:poly})

Export.table.toDrive({collection:names, description:"landsat_names", folder:"r_data"})

///////////////////////////////////
/// USER SPECIFICATION REQUIRED ///
///////////////////////////////////

//Beginning and end of range to find training image
var train_range_1 = '1984-01-01';
var train_range_2 = '2015-12-31';
//Split training into 120-180, 180-240, 240-300
var train_doy_1 = 120; 
var train_doy_2 = 180;
var train_doy_3 = 240;
var train_doy_4 = 300;
//Beginning and end of range to select prediction images
var predict_range_1 = '1984-01-01';
var predict_range_2 = '2018-08-11';
var predict_doy_1 = 0; 
var predict_doy_2 = 365;

//Proportion Area Specification (Greater than)
//Image must contain valid pixels for greater than this proportion
var area_prop = 0.95;
//Proportion Cloud Speciication (Less than)
//Image much contain less than this proportion of clouds
var cloud_prop = 0.05;

//Polygon for region of interest (ROI)
var poly = new_orleans_pred

///////////////////////////////////
/// END USER SPECIFICATION AREA ///
///////////////////////////////////

       
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
  
//Stores data that will be returned for each water-body
var area_list = ee.List([]);
  
//function to calculate amount of clouds and percent area of land image covers
var calculate_clouds = function(image) {
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
                   
  var cloudiness = cloud.reduceRegion({
    reducer: 'mean', 
    geometry: poly, 
    scale: 30,
  });
  var area_pct = image.clip(poly).geometry().area(30).divide(poly.area(30));
  var missingness = image.select(['pixel_qa'],['missing']).eq(0).reduceRegion({
    reducer: 'sum', 
    geometry: poly, 
    scale: 30,
  });
    
  var JRC_range_1 = ee.String(ee.Date(image.date()).get('year')).cat("-").cat(ee.String(ee.Date(image.date()).get('month'))).cat("-").cat("01");
  var JRC_range_2 = date_range_algorithm(ee.String(ee.Date(image.date()).get('month')), ee.String(ee.Date(image.date()).get('year')));
    
  var JRC = ee.Image(ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
  .filterBounds(poly)
  .filterDate(JRC_range_1, JRC_range_2).first())

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
    
  return image.set(cloudiness).set(missingness).set(JRC_stats).set(ee.Dictionary(['area',area_pct]));
} //end function calculate_clouds
  
//version for prediction region!
var calculate_clouds_test = function(image) {
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
                   
  var cloudiness = cloud.reduceRegion({
    reducer: 'mean', 
    geometry: poly, 
    scale: 30,
  });
  var area_pct = image.clip(poly).geometry().area(30).divide(poly.area(30));
  var missingness = image.select(['pixel_qa'],['missing']).eq(0).reduceRegion({
    reducer: 'sum', 
    geometry: poly, 
    scale: 30,
  });
  return image.set(cloudiness).set(missingness).set(ee.Dictionary(['area',area_pct]));
} //end function calculate_clouds
  
//Collect Landsat 5 images
var images5 = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(predict_range_1, predict_range_2)
  .filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2));
  
//Collect Landsat 7 images
var images7 = ee.ImageCollection("LANDSAT/LE07/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(predict_range_1, predict_range_2)
  .filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2));
  
//Collect Landsat 8 images
var images8 = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(predict_range_1, predict_range_2)
  .filter(ee.Filter.dayOfYear(predict_doy_1, predict_doy_2));

//Function to add edited quality band for Landsat 8
var addQualityBand = function(image){
  return image.addBands(image.select(['sr_aerosol'],['edit_sr_aerosol']).subtract(1))
              .addBands(image.select(['pixel_qa'],['edit_pixel_qa']).subtract(1))
    .select(['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'edit_sr_aerosol', 'edit_pixel_qa'],['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'sr_cloud_qa', 'pixel_qa']);

}
//Add edited quality band for Landsat 8 imagery
images8 = images8.map(addQualityBand)
  
//Merge collections to collect all Landsat Images
var images = ee.ImageCollection(images5.merge(images7));
var image = ee.ImageCollection(images.merge(images8));

var bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'water']

var test_withCloudiness = ee.ImageCollection(image).map(calculate_clouds_test);

var test_images = test_withCloudiness

var trim_images = function(image){
  
  var train_date = image.date();
  var JRC_range_year = ee.String(train_date.get('year'));
  var JRC_range_month = ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10), ee.String("0").cat(ee.String(train_date.get('month'))), ee.String(train_date.get('month')));
  
  //Produce the rnage of dates for the month of the JRC image
  var JRC_range_1 = JRC_range_year.cat("-").cat(JRC_range_month).cat("-").cat("01");
  var JRC_range_2 = date_range_algorithm(ee.String(JRC_range_month), JRC_range_year);

  image = image.updateMask(image.select(['pixel_qa'],['cloud']).eq(66)
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
            .or(image.select(['sr_cloud_qa'],['cloud']).eq(131))))

  //Load the JRC dataset
  var JRC = ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
    .filterBounds(poly)
    .filterDate(JRC_range_1, JRC_range_2);
  
  var JRC_missing = ee.Image(ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
    .filterBounds(poly)
    .filterDate("1984-06-01", "1984-06-30").first()).multiply(0);
  
  var JRC = JRC.merge(ee.ImageCollection(JRC_missing));
  
  var JRC_image = ee.Image(JRC.first());
  //Clip the JRC data to the region and create labels (1) land, (2) water
  
  var truth_needs_mask = JRC_image.select('water');
  
  image = image.addBands(truth_needs_mask)  
  
  return image.clip(poly)
  
} 

test_images = test_images.map(trim_images)

var first = ee.List([
  ee.Image()
]);

print(test_images)

var accumulate = function(image, list) {

  var previous = ee.Image(ee.List(list).get(0));
      image = image.select(bands,
      ee.List([
      ee.String('B1_').cat(image.get("system:index")),
      ee.String('B2_').cat(image.get("system:index")),
      ee.String('B3_').cat(image.get("system:index")),
      ee.String('B4_').cat(image.get("system:index")),
      ee.String('B5_').cat(image.get("system:index")),
      ee.String('B7_').cat(image.get("system:index")),
      ee.String('water_').cat(image.get("system:index"))
    ]))
  
  // Add the current anomaly to make a new cumulative anomaly image.
  var added = previous.addBands(image)
  return ee.List([added]);
};

// Create an ImageCollection of cumulative anomaly images by iterating.
// Since the return type of iterate is unknown, it needs to be cast to a List.
var cumulative = ee.Image(ee.ImageCollection(ee.List(test_images.iterate(accumulate, first))).first()).toFloat();
          
var names = ee.FeatureCollection(test_images.map(function(image){return ee.Feature(poly).set("id",image.get("system:index")).setGeometry(null)}))
            
print(names)            
            
Export.image.toDrive({image:cumulative, description:"landsat_images", folder:"r_data", scale:30, region:poly})

Export.table.toDrive({collection:names, description:"landsat_names", folder:"r_data"})

//////////////////////////////////////////////////////
// Select and store the training images for download//
//////////////////////////////////////////////////////

//Load all images from the Landsat 5 satellite which have less than 10
//percent cloud cover throughout the training period for the polygon
var imagesA = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(train_range_1, train_range_2)
  .filter(ee.Filter.dayOfYear(train_doy_1, train_doy_2))

//For Version 2.0
//var imagesA = ee.ImageCollection(ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
//.merge(ee.ImageCollection("LANDSAT/LE07/C01/T1_SR"))
//.merge(ee.ImageCollection("LANDSAT/LC08/C01/T1_SR")))
//  .filterBounds(poly)
//  .filterDate(train_range_1, train_range_2)
//  .filter(ee.Filter.dayOfYear(train_doy_2, train_doy_3))

var with_cloudiness = imagesA.map(calculate_clouds);

//List of bands used for prediction in dataset
var bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7'];
      
//Clip the image to the region and select the bands that will be used as features
var median = with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).sort('water', false).size().multiply(0.9).ceil()
var train_image_needs_mask = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).reproject('EPSG:4326', null, 30).clip(poly);
      
//Calculate the corresponding month of the JRC image from the training image
var train_date = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).date();
var JRC_range_year = ee.String(train_date.get('year'));
var JRC_range_month = ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10), ee.String("0").cat(ee.String(train_date.get('month'))), ee.String(train_date.get('month')));

print("Train date 1:")
print(train_date)

//Produce the rnage of dates for the month of the JRC image
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

//Remove any NA in the truth data from the training image
var data = trainMaskBadData(train_image_needs_mask).select(bands);
      
//Remove an NA data from the truth image by getting rid of (0) labeled pixels and then subtract 1
var truth = trainMaskBadData(truth_needs_mask).select('water').subtract(ee.Image.constant(1));

//add the truth labels to the LANDSAT training image
data = data.addBands(truth)
      
var train_data_compile = data;
      
//----------------

//Load all images from the Landsat 5 satellite which have less than 10
//percent cloud cover throughout the training period for the polygon
var imagesA = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(train_range_1, train_range_2)
  .filter(ee.Filter.dayOfYear(train_doy_2, train_doy_3))

var with_cloudiness = imagesA.map(calculate_clouds);

//Clip the image to the region and select the bands that will be used as features
//print(with_cloudiness.size())
var median = with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).sort('water', false).size().multiply(0.9).ceil()
var train_image_needs_mask = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).reproject('EPSG:4326', null, 30).clip(poly);

//Calculate the corresponding month of the JRC image from the training image
var train_date = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).date();
var JRC_range_year = ee.String(train_date.get('year'));
var JRC_range_month = ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10), ee.String("0").cat(ee.String(train_date.get('month'))), ee.String(train_date.get('month')));

print("Train date 2:")
print(train_date)

//Produce the rnage of dates for the month of the JRC image
var JRC_range_1 = JRC_range_year.cat("-").cat(JRC_range_month).cat("-").cat("01");
var JRC_range_2 = date_range_algorithm(ee.String(JRC_range_month), JRC_range_year);

//Load the JRC dataset
var JRC = ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
  .filterBounds(poly)
  .filterDate(JRC_range_1, JRC_range_2);

var JRC_image = ee.Image(JRC.first());
//Clip the JRC data to the region and create labels (1) land, (2) water
var truth_needs_mask = JRC_image.clip(poly).select('water');

//Remove any NA in the truth data from the training image
var data = trainMaskBadData(train_image_needs_mask).select(bands);

//Remove an NA data from the truth image by getting rid of (0) labeled pixels and then subtract 1
var truth = trainMaskBadData(truth_needs_mask).select('water').subtract(ee.Image.constant(1));

//add the truth labels to the LANDSAT training image
data = data.addBands(truth)

train_data_compile = train_data_compile.addBands(data)

//----------------

//Load all images from the Landsat 5 satellite which have less than 10
//percent cloud cover throughout the training period for the polygon
var imagesA = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
  .filterBounds(poly)
  .filterDate(train_range_1, train_range_2)
  .filter(ee.Filter.dayOfYear(train_doy_3, train_doy_4))

var with_cloudiness = imagesA.map(calculate_clouds);

//Clip the image to the region and select the bands that will be used as features
var median = with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).sort('water', false).size().multiply(0.9).ceil()
var train_image_needs_mask = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).reproject('EPSG:4326', null, 30).clip(poly);

//Calculate the corresponding month of the JRC image from the training image
var train_date = ee.Image(with_cloudiness.filter(ee.Filter.gte('area', area_prop)).filter(ee.Filter.lte('cloud', cloud_prop)).limit(median, 'water').sort('water', false).first()).date();
var JRC_range_year = ee.String(train_date.get('year'));
var JRC_range_month = ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10), ee.String("0").cat(ee.String(train_date.get('month'))), ee.String(train_date.get('month')));

print("Train date 3:")
print(train_date)

//Produce the rnage of dates for the month of the JRC image
var JRC_range_1 = JRC_range_year.cat("-").cat(JRC_range_month).cat("-").cat("01");
var JRC_range_2 = date_range_algorithm(ee.String(JRC_range_month), JRC_range_year);

//Load the JRC dataset
var JRC = ee.ImageCollection('JRC/GSW1_0/MonthlyHistory')
  .filterBounds(poly)
  .filterDate(JRC_range_1, JRC_range_2);

var JRC_image = ee.Image(JRC.first());
//Clip the JRC data to the region and create labels (1) land, (2) water
var truth_needs_mask = JRC_image.clip(poly).select('water');

//Remove any NA in the truth data from the training image
var data = trainMaskBadData(train_image_needs_mask).select(bands);

//Remove an NA data from the truth image by getting rid of (0) labeled pixels and then subtract 1
var truth = trainMaskBadData(truth_needs_mask).select('water').subtract(ee.Image.constant(1));

//add the truth labels to the LANDSAT training image
data = data.addBands(truth)

train_data_compile = ee.Image(train_data_compile.addBands(data)).toFloat();
      
Export.image.toDrive({image:train_data_compile, description:"landsat_training", folder:"r_data", scale:30, region:poly})

