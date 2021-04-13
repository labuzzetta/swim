#' Convert TIF file to GAPFILL Format
#'
#' Converts a TIF file containing 6 band layers per image downloaded from
#' Google Earth Engine into a set of 6 .csv files in GAPFILL format
#'
#' @param path directory to search for datasets i.e. ~/GEE/data/new_orleans
#' @param prediction filename for .tif file of google earth image layers
#         i.e. new_orleans_prediction_images.tif
#' @param dates filename for .csv file of LANDSAT ID values
#'        i.e. new_orleans_prediction_names.csv
#' @param out prefix for .csv matrix files for bands of prediction images
#' @return Generates 6 .csv files for input to GAPFILL algorithm
#' @import dplyr, lubridate, raster
#' @export

convert_TIF_for_GAPFILL <- function(path, prediction, dates, out) {
  
  library(raster)
  
  #Read in prediction images from .tif file
  p <- brick(paste(path, prediction, sep="/"))
  #Drop empty first layer (unavoidable product of GEE code)
  p <- dropLayer(p, 1)
  
  #Read in .csv file of Landsat ID's which contain dates for images
  names <- read.table(paste(path, dates, sep="/"), sep = ",", header = T, as.is = T)
  
  #Produce vector of days from string
  day <- cbind(lapply(names$id, function(x){return(as.integer(substr(x, nchar(x)-1, nchar(x))))}))
  colnames(day) <- "day"
  
  #Produce vector of months from string
  month <- cbind(lapply(names$id, function(x){return(as.integer(substr(x, nchar(x)-3, nchar(x)-2)))}))
  colnames(month) <- "month"
  
  #Produce vector of years from string
  year <- cbind(lapply(names$id, function(x){return(as.integer(substr(x, nchar(x)-7, nchar(x)-4)))}))
  colnames(year) <- "year"
  
  #Calculate doy's from month, day, and year vectors
  doy <- cbind(lubridate::yday(lubridate::mdy(paste(month, day, year, "-"))))
  colnames(doy) <- "doy"
  
  b1_rows <- c()
  b2_rows <- c()
  b3_rows <- c()
  b4_rows <- c()
  b5_rows <- c()
  b6_rows <- c()
  
  #Get matrices of vector of pixels values for each image
  #WARNING: Computationally expensive!
  values <- getValues(p)
  
  #Bind vectors for each band into matrices of band values (columns) for each image (rows)
  for(i in 1:(nlayers(p)/7)){
    
    b1_rows <- rbind(b1_rows, values[,(i-1)*7+1])
    b2_rows <- rbind(b2_rows, values[,(i-1)*7+2])
    b3_rows <- rbind(b3_rows, values[,(i-1)*7+3])
    b4_rows <- rbind(b4_rows, values[,(i-1)*7+4])
    b5_rows <- rbind(b5_rows, values[,(i-1)*7+5])
    b6_rows <- rbind(b6_rows, values[,(i-1)*7+6])
    
  }
  
  #Produce pixel names for columns
  cols <- c()
  for(i in 1:nrow(values)){ cols <- c(cols, paste("pixel",i,sep="")) }
  
  #Ensure NA standardization
  b1_rows[is.na(b1_rows)] <- NA
  b2_rows[is.na(b2_rows)] <- NA
  b3_rows[is.na(b3_rows)] <- NA
  b4_rows[is.na(b4_rows)] <- NA
  b5_rows[is.na(b5_rows)] <- NA
  b6_rows[is.na(b6_rows)] <- NA
  
  #Add dates to images values in row
  b1_rows <- cbind(year, doy, b1_rows)
  b2_rows <- cbind(year, doy, b2_rows)
  b3_rows <- cbind(year, doy, b3_rows)
  b4_rows <- cbind(year, doy, b4_rows)
  b5_rows <- cbind(year, doy, b5_rows)
  b6_rows <- cbind(year, doy, b6_rows)
  
  #Edit column names
  colnames(b1_rows) <- c("year", "doy", cols)
  colnames(b2_rows) <- c("year", "doy", cols)
  colnames(b3_rows) <- c("year", "doy", cols)
  colnames(b4_rows) <- c("year", "doy", cols)
  colnames(b5_rows) <- c("year", "doy", cols)
  colnames(b6_rows) <- c("year", "doy", cols)
  
  #Ensure integer standardization
  mode(b1_rows) <- 'integer'
  mode(b2_rows) <- 'integer'
  mode(b3_rows) <- 'integer'
  mode(b4_rows) <- 'integer'
  mode(b5_rows) <- 'integer'
  mode(b6_rows) <- 'integer'
  
  #Create output files
  write.table(b1_rows, file = paste(path, paste(out, "b1.csv", sep = "_"), sep="/"), sep = ",")
  write.table(b2_rows, file = paste(path, paste(out, "b2.csv", sep = "_"), sep="/"), sep = ",")
  write.table(b3_rows, file = paste(path, paste(out, "b3.csv", sep = "_"), sep="/"), sep = ",")
  write.table(b4_rows, file = paste(path, paste(out, "b4.csv", sep = "_"), sep="/"), sep = ",")
  write.table(b5_rows, file = paste(path, paste(out, "b5.csv", sep = "_"), sep="/"), sep = ",")
  write.table(b6_rows, file = paste(path, paste(out, "b6.csv", sep = "_"), sep="/"), sep = ",")
  
}