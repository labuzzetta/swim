train_and_predict_RF <- function(training_file, predict_images_file, prediction_dates_file, jrc_file,
                              missing_image, imputed_images_list = c(), lambda = 0.5, method = "cluster", plot = T) {
  
  #Empty list to add imputed datasets as read
  imputed_datasets <- list()
  
  #Read in each imputed RDS file, only bands 2,4,5 needed
  for(f in 1:length(imputed_images_list)){
    d <- readRDS(imputed_images_list[f])
    imputed_datasets[[f]] <- d 
  }
  
  #Read in the file containing prediction images
  images <- brick(predict_images_file)
  
  #Drop the first layer, as it will be empty by design
  images <- dropLayer(images, 1)
  
  #Generate the classifier from training files
  rf <- train_cluster_RF(training_images = training_file, method = method, lambda)
  
  #Calculate the row and column of each pixel listed
  img_row <- c()
  img_col <- c()
  for(i in 1:(ncol(images)*nrow(images))) {
    img_row[i] = ceiling(i/ncol(images)); 
    img_col[i] = i - floor(i/(ncol(images)))*ncol(images)
  }
  img_col[img_col==0] <- ncol(images)
  
  #For storing table of classification statistics
  predictions = c()
  
  #For storing image classifications
  aniBrick <- brick()
  
  #Iterate through the images which will be classified
  for(i in 1:nrow(imputed_datasets[[1]])) {
    
    #Print iteration
    print(i)
    
    #Derive raster from imputed datasets
    image_bands <- c()
    for(j in 1:length(imputed_datasets)){
      data <- imputed_datasets[[j]]
      image_bands <- c(image_bands, suppressWarnings(flip(rasterFromXYZ(cbind(data.frame(img_col), data.frame(img_row), data[i,])), direction="y")))
    }
    
    #Create a brick from raster layers of imputed data
    pred_image <- brick(image_bands)
    
    #Calculate the NDWI, MNDWI, and MIR values
    pred_image <- brick((pred_image[[1]] - pred_image[[2]])/(pred_image[[1]] + pred_image[[2]]),
                        (pred_image[[1]] - pred_image[[3]])/(pred_image[[1]] + pred_image[[3]]),
                         pred_image[[3]]
                        ) 
    names(pred_image) <- c("NDWI", "MNDWI", "MIR")

    #Classify the image
    predOutcome <- suppressWarnings(predict(pred_image, model=rf, na.rm=TRUE))
    
    #Calculate the number of pixels classified as water
    predictions <- rbind(predictions, c(i, sum(values(predOutcome), na.rm=TRUE)))
    
    #Plot classifications as completed:
    if(plot){
      extent(predOutcome) <- extent(images[[1]])
      projection(predOutcome) <- projection(images[[1]])
      aniBrick <- addLayer(aniBrick, predOutcome)
      plot(predOutcome, legend = F, axes = F, box = F)
    }
    
  }
  
  #Calculate dates from missing_image file
  dates <- read.csv(missing_image)[,1:2]
  year <- dates$year
  doy <- dates$doy
  
  #Create table with important statistics for each image
  library(dplyr)
  predictions <- data.frame(cbind(year, doy, predictions))
  df <- data.frame(matrix(unlist(predictions), nrow = length(year), byrow=F))
  colnames(df) <- c("year", "doy", "row", "pred")
  years <- apply(df, 1, function(x){lubridate::year(strptime(paste(x[1], x[2]), format="%Y %j"))})
  month <- apply(df, 1, function(x){lubridate::month(strptime(paste(x[1], x[2]), format="%Y %j"))})
  day <- apply(df, 1, function(x){lubridate::day(strptime(paste(x[1], x[2]), format="%Y %j"))})
  df <- cbind(years, month, day, df)
  dates <- apply(df, 1, function(x){paste(x[1], x[2], x[3], sep="-")})
  df <- data.frame(df) %>% mutate(dates = lubridate::ymd(dates))
  cloud <- percent_missing(missing_image)
  df <- cbind(df, cloud)
  jrc <- brick(jrc_file)
  jrc <- readAll(jrc)
  print("Read All JRC File")
  totals <- c(); 
  for(i in 1:nlayers(jrc)){totals <- c(totals, sum(getValues(jrc[[i]] > 1),na.rm=T))}
  df <- cbind(df, jrc = totals)
  df <- dplyr::arrange(df, dates)
  
  #Create an R object to output from classification process
  #Store table as $predict_table
  #Store classified images as $predict_images
  to_return <- list()
  to_return$predict_table <- df
  to_return$predict_images <- aniBrick
  
  #Output classifications
  return(to_return)
  
}