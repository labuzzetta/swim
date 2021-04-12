#' Uses a TIF of 3 layered training images to train Random Forest Classifier
#'
#' Imports a training dataset of 3 training images layered together in one
#' file with training labels and trains a Random Forest Classifier
#'
#' @param training_images filepath for .tif file of training images
#' @return A trained  Random Forest Classifier Model
#' @import sf, sp, raster
#' @export

train_cluster_RF <- function(training_images, method = "cluster", lambda) {
  
  library(raster)
  
  #Subset the training image file into three images, transform the bands to NDWI, MNDWI, MIR
  trn <- brick(training_images)
  trn1 <- subset(trn, 1:7)
  trn1 <- brick((trn1[[2]] - trn1[[4]])/(trn1[[2]] + trn1[[4]]), (trn1[[2]] - trn1[[5]])/(trn1[[2]] + trn1[[5]]), trn1[[5]], trn1[[7]]) 
  trn2 <- subset(trn, 8:14)
  trn2 <- brick((trn2[[2]] - trn2[[4]])/(trn2[[2]] + trn2[[4]]), (trn2[[2]] - trn2[[5]])/(trn2[[2]] + trn2[[5]]), trn2[[5]], trn2[[7]]) 
  trn3 <- subset(trn, 15:21)
  trn3 <- brick((trn3[[2]] - trn3[[4]])/(trn3[[2]] + trn3[[4]]), (trn3[[2]] - trn3[[5]])/(trn3[[2]] + trn3[[5]]), trn3[[5]], trn3[[7]]) 
  
  #Create matrices of pixel values for each training image
  library(dplyr)
  values1 <- getValues(trn1)
  values2 <- getValues(trn2)
  values3 <- getValues(trn3)
  valuetable <- data.frame(rbind(values1, values2, values3))
  
  #Calculate the row and column of each pixel in the original image
  row <- c()
  col <- c()
  for(i in 1:nrow(values1)) {
    row[i] = ceiling(i/ncol(trn1)); 
    col[i] = i - floor(i/(ncol(trn1)))*ncol(trn1)
  }
  col[col==0] <- ncol(trn1)
  values1 <- cbind(values1, row, col)
  values2 <- cbind(values2, row, col)
  values3 <- cbind(values3, row, col)
  
  library(RWeka)
  WPM("refresh-cache")
  WPM("install-package", "XMeans")
  
  #Remove NA values which are not valueable for training
  values1 <- na.omit(values1)
  values2 <- na.omit(values2)
  values3 <- na.omit(values3)
  
  #If using the CSSM cluster method, complete the following steps
  if(method == "cluster"){
    
    #Perform XMeans clustering for each training image
    xmeans1 <- XMeans(values1[, 1:3], c("-L",10,"-H", 100))
    values1 <- cbind(values1, predict(xmeans1))
    xmeans2 <- XMeans(values2[, 1:3], c("-L",10,"-H", 100))
    values2 <- cbind(values2, predict(xmeans2))
    xmeans3 <- XMeans(values3[, 1:3], c("-L",10,"-H", 100))
    values3 <- cbind(values3, predict(xmeans3))
    
    #For each image, calculate if the pixel or one of it's neighbors
    #(in the vertical or horizontal direction) have a JRC water label
    neighbor1 <- c()
    neighbor2 <- c()
    neighbor3 <- c()
    
    for(i in 1:nrow(values1)){
      
      n1 = if(!is.na(values1[which(values1[,5]==(values1[i,5]-1) & values1[,6]==values1[i,6]),][4])){
        values1[which(values1[,5]==(values1[i,5]-1) & values1[,6]==values1[i,6]),][4] == 1
      } else {FALSE}
      n2 = if(!is.na(values1[which(values1[,5]==(values1[i,5]+1) & values1[,6]==values1[i,6]),][4])){
        values1[which(values1[,5]==(values1[i,5]+1) & values1[,6]==values1[i,6]),][4] == 1
      } else {FALSE}
      n3 = if(!is.na(values1[which(values1[,5]==values1[i,5] & values1[,6]==(values1[i,6]-1)),][4])){
        values1[which(values1[,5]==values1[i,5] & values1[,6]==(values1[i,6]-1)),][4] == 1
      } else {FALSE}
      n4 = if(!is.na(values1[which(values1[,5]==values1[i,5] & values1[,6]==(values1[i,6]+1)),][4])){
        values1[which(values1[,5]==values1[i,5] & values1[,6]==(values1[i,6]+1)),][4] == 1
      } else {FALSE}
      loc = values1[i,4] == 1
      
      neighbor1[i] = n1 | n2 | n3 | n4 | loc
      
    }
    
    for(i in 1:nrow(values2)){
      
      n1 = if(!is.na(values2[which(values2[,5]==(values2[i,5]-1) & values2[,6]==values2[i,6]),][4])){
        values2[which(values2[,5]==(values2[i,5]-1) & values2[,6]==values2[i,6]),][4] == 1
      } else {FALSE}
      n2 = if(!is.na(values2[which(values2[,5]==(values2[i,5]+1) & values2[,6]==values2[i,6]),][4])){
        values2[which(values2[,5]==(values2[i,5]+1) & values2[,6]==values2[i,6]),][4] == 1
      } else {FALSE}
      n3 = if(!is.na(values2[which(values2[,5]==values2[i,5] & values2[,6]==(values2[i,6]-1)),][4])){
        values2[which(values2[,5]==values2[i,5] & values2[,6]==(values2[i,6]-1)),][4] == 1
      } else {FALSE}
      n4 = if(!is.na(values2[which(values2[,5]==values2[i,5] & values2[,6]==(values2[i,6]+1)),][4])){
        values2[which(values2[,5]==values2[i,5] & values2[,6]==(values2[i,6]+1)),][4] == 1
      } else {FALSE}
      loc = values2[i,4] == 1
      
      neighbor2[i] = n1 | n2 | n3 | n4 | loc
      
    }
    
    for(i in 1:nrow(values3)){
      
      n1 = if(!is.na(values3[which(values3[,5]==(values3[i,5]-1) & values3[,6]==values3[i,6]),][4])){
        values3[which(values3[,5]==(values3[i,5]-1) & values3[,6]==values3[i,6]),][4] == 1
      } else {FALSE}
      n2 = if(!is.na(values3[which(values3[,5]==(values3[i,5]+1) & values3[,6]==values3[i,6]),][4])){
        values3[which(values3[,5]==(values3[i,5]+1) & values3[,6]==values3[i,6]),][4] == 1
      } else {FALSE}
      n3 = if(!is.na(values3[which(values3[,5]==values3[i,5] & values3[,6]==(values3[i,6]-1)),][4])){
        values3[which(values3[,5]==values3[i,5] & values3[,6]==(values3[i,6]-1)),][4] == 1
      } else {FALSE}
      n4 = if(!is.na(values3[which(values3[,5]==values3[i,5] & values3[,6]==(values3[i,6]+1)),][4])){
        values3[which(values3[,5]==values3[i,5] & values3[,6]==(values3[i,6]+1)),][4] == 1
      } else {FALSE}
      loc = values3[i,4] == 1
      
      neighbor3[i] = n1 | n2 | n3 | n4 | loc
      
    }
    
    values1 <- data.frame(cbind(values1, neighbor1))
    values2 <- data.frame(cbind(values2, neighbor2))
    values3 <- data.frame(cbind(values3, neighbor3))
    
    #Rename the bands collected in each column for the training matrices
    colnames(values1) <- c("NDWI", "MNDWI", "MIR", "l", "row", "col", "cluster", "neighbor")
    colnames(values2) <- c("NDWI", "MNDWI", "MIR", "l", "row", "col", "cluster", "neighbor")
    colnames(values3) <- c("NDWI", "MNDWI", "MIR", "l", "row", "col", "cluster", "neighbor")
    
    #Calculate which clusters have a proportion water that is greater than lambda
    clusters1 <- (values1 %>% 
                    mutate(cluster = factor(cluster)) %>% 
                    group_by(cluster, neighbor) %>% 
                    summarise(n = n()) %>% 
                    mutate(p = n / sum(n)) %>% 
                    filter(neighbor == 1 & p > lambda))$cluster
    clusters2 <- (values2 %>% 
                    mutate(cluster = factor(cluster)) %>% 
                    group_by(cluster, neighbor) %>% 
                    summarise(n = n()) %>% 
                    mutate(p = n / sum(n)) %>% 
                    filter(neighbor == 1 & p > lambda))$cluster
    clusters3 <- (values3 %>% 
                    mutate(cluster = factor(cluster)) %>% 
                    group_by(cluster, neighbor) %>% 
                    summarise(n = n()) %>% 
                    mutate(p = n / sum(n)) %>% 
                    filter(neighbor == 1 & p > lambda))$cluster
    
    #If pixel belongs to a cluster with p > lambda, save temp label as 1
    cgl1 <- c()
    cgl2 <- c()
    cgl3 <- c()
    
    for(i in 1:nrow(values1)){
      if(values1[i,'cluster'] %in% clusters1){
        cgl1[i] = 1
      } else {
        cgl1[i] = 0
      }
    }
    
    for(i in 1:nrow(values2)){
      if(values2[i,'cluster'] %in% clusters2){
        cgl2[i] = 1
      } else {
        cgl2[i] = 0
      }
    }
    
    for(i in 1:nrow(values3)){
      if(values3[i,'cluster'] %in% clusters3){
        cgl3[i] = 1
      } else {
        cgl3[i] = 0
      }
    }
    
    values1 <- cbind(values1, cgl1)
    values2 <- cbind(values2, cgl2)
    values3 <- cbind(values3, cgl3)
    
    #Determine if the cluster labels and jrc labels match, 
    #If the labels match, add the pixel with the jrc to the training dataset
    #If the labels do not match, add two copies of the pixel to the training dataset, once with each label
    trn_values1 <- c()
    trn_values2 <- c()
    trn_values3 <- c()
    
    for(i in 1:nrow(values1)){
      if(values1[i,'l'] == values1[i,'cgl1']){
        trn_values1 <- rbind(trn_values1, values1[i,1:4])
      } else {
        trn_values1 <- rbind(trn_values1, c(values1[i, 1:3],"l"=0), c(values1[i, 1:3],"l"=1))
      }
    }
    
    for(i in 1:nrow(values2)){
      if(values2[i,'l'] == values2[i,'cgl2']){
        trn_values2 <- rbind(trn_values2, values2[i,1:4])
      } else {
        trn_values2 <- rbind(trn_values2, c(values2[i, 1:3],"l"=0), c(values2[i, 1:3],"l"=1))
      }
    }
    
    for(i in 1:nrow(values3)){
      if(values3[i,'l'] == values3[i,'cgl3']){
        trn_values3 <- rbind(trn_values3, values3[i,1:4])
      } else {
        trn_values3 <- rbind(trn_values3, c(values3[i, 1:3],"l"=0), c(values3[i, 1:3],"l"=1))
      }
    }
    
  } else if(method == "JRC") {
    
    #If simply using the JRC method, do not alter the training data
    trn_values1 <- data.frame(values1)
    colnames(trn_values1) <- c("NDWI", "MNDWI", "MIR", "l")
    trn_values2 <- data.frame(values2)
    colnames(trn_values2) <- c("NDWI", "MNDWI", "MIR", "l")
    trn_values3 <- data.frame(values3)
    colnames(trn_values3) <- c("NDWI", "MNDWI", "MIR", "l")
    
  }
  
  #Format the training dataset columns for use with the randomForest package
  training <- rbind(trn_values1, trn_values2, trn_values3)
  training <- data.frame(matrix(unlist(training), ncol = 4, byrow = F))
  colnames(training) <- c("NDWI", "MNDWI", "MIR", "l")
  training$l <- factor(training$l)
  training_bands <- training[,1:3]
  colnames(training_bands) <- c("MNDWI", "NDWI", "MIR")
  
  #Return the trained random forest classifier
  library(randomForest)
  return(randomForest(x = training[,1:3], y = training$l, ntree = 500))
  
}