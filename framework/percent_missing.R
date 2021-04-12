#' Calculate the percent of pixels missing in each image
#'
#' Input a set of prediction images and calculate the percent of each
#' image that contains missing values
#'
#' @param to_impute_file A .csv file of images which is passed to gapfill
#' @return A vector of percentages of missing pixels in images
#' @export

percent_missing <- function(file){
  
  f <- read.csv(file)
  
  total <- ncol(f) - 2

  return(apply(f, 1, function(x){sum(is.na(x))/total * 100}))
  
}