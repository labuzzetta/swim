var L00009bd5a536a60b1f02 = /* color: #00ff00 */ee.Geometry.MultiPolygon(
        [[[[-96.17873132228851, 42.03272729463239],
           [-96.17879569530487, 42.03273127912081],
           [-96.17879033088684, 42.03265955829124]]],
         [[[-96.17832899093628, 42.03236470514163],
           [-96.17832899093628, 42.032320875502634],
           [-96.17828607559204, 42.032316890988504],
           [-96.17828071117401, 42.03224516969135],
           [-96.17823243141174, 42.03225313872835],
           [-96.17822170257568, 42.03236072063025]]],
         [[[-96.17874205112457, 42.03270737218662],
           [-96.17877423763275, 42.03266752727632],
           [-96.17869913578033, 42.032655573798344],
           [-96.17865085601807, 42.032643620318105],
           [-96.17861330509186, 42.032687449734574]]],
         [[[-96.17892444133759, 42.03310183545101],
           [-96.17882251739502, 42.03314566455151],
           [-96.17888152599335, 42.033149649013666],
           [-96.17892444133759, 42.03314168008906]]],
         [[[-96.1783504486084, 42.03053379584679],
           [-96.17834776639938, 42.03049594188817],
           [-96.17830216884613, 42.03049195725964],
           [-96.17829144001007, 42.0305437574111],
           [-96.17834508419037, 42.03055571128621]]]]);


//Change the following for each lake
var polygon = "00009bd5a536a60b1f02"
var train_date = ee.Date("2005-07-31")

print(L00009bd5a536a60b1f02.area())

//-------------Stays the same for each lake--------------//
//Find the polygon from the NRI database
var NRI = ee.FeatureCollection("users/chalab13/LWB_IA_all_years_geo"),
    NAIP = ee.ImageCollection("USDA/NAIP/DOQQ");

var predict_poly = NRI
  .filterMetadata("system:index", "equals", polygon)
  .geometry()

//Determine month and year of training date
var NAIP_range_year = ee.String(train_date.get('year'));
var NAIP_range_month = ee.String(ee.Algorithms.If(ee.Number(train_date.get('month')).lt(10),
          ee.String("0").cat(ee.String(train_date.get('month'))),
          ee.String(train_date.get('month'))));

//Function to calculate date for end of the month selected
//Non - GEE Functional version
var date_range = function(month, year) {
  var end_range = "";
  if(month == "09" || month == "04" || month == "06" || month == "11"){
    end_range = ee.String(year).cat("-").cat(month).cat("-").cat("30")
  } else {
    if(month == "02"){
      if(year % 4 === 0){
          end_range = ee.String(year).cat("-").cat(month).cat("-").cat("29")
      } else {
          end_range = ee.String(year).cat("-").cat(month).cat("-").cat("28")
      }
    } else {
      end_range = ee.String(year).cat("-").cat(month).cat("-").cat("31")
    }
  }
  return end_range
} // end function date_range()

//Produce a range of dates for the month of the NAIP image (1st through 30th of the month)
var NAIP_range_1 = NAIP_range_year.cat("-").cat(NAIP_range_month).cat("-").cat("01");
var NAIP_range_2 = date_range(NAIP_range_month.getInfo(), NAIP_range_year.getInfo());

//Collect NAIP images
var NAIP = NAIP
  .filterBounds(predict_poly)
  .filterDate(NAIP_range_1, NAIP_range_2)

//Add to map
Map.addLayer(predict_poly.buffer(800))
Map.addLayer(NAIP.select('R','G','B').mosaic().clip(predict_poly.buffer(800)))
Map.centerObject(NAIP, 14)
