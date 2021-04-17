###################################
#### Example Landsat gapfill.R ####
###################################

#Load required libraries
library(doParallel)
library(Matrix)
library(raster)
library(rasterVis)
library(fda)
library(stfit)
library(dplyr)

#Load gapfill.R script
source("../gapfill.R")

#Load the landsat data
data <- read.csv("landsat_to_gapfill_b2.csv")

#The region is 109 rows x 125 columns
#Load these values from the input imagery
img <- brick("landsat_images.tif")
n.row <- raster::nrow(img)
n.col <- raster::ncol(img)

#Trim year and doy columns, enumerate and clip values
mat <- as.matrix(data[, -c(1,2)])
mat[!is.na(mat)] <- as.numeric(mat[!is.na(mat)])
mat[mat > 2000 | mat < 0] <- NA

#Store year and doy vectors
year <- data$year
doy <- data$doy
day.eval <- sort(unique(data$doy))

#Parallelize if desirable
registerDoParallel(cores = 2) #Recommend 12 - 18 if available

#Edit spline regression function in stfit package
spreg2 <- function(x, y, x.eval, minimum.num.obs = 4, basis = "fourier", 
                   rangeval = c(min(x.eval)-1, max(x.eval)), nbasis = 7, ...){
  nonna.idx = !is.na(y)
  basis = match.arg(basis)
  if(sum(nonna.idx) > minimum.num.obs){
    x = x[nonna.idx]
    y = y[nonna.idx]
    if(basis == "fourier"){
      bs = fda::create.fourier.basis(rangeval=rangeval, nbasis=nbasis, ...)
    } else
      if(basis == "bspline"){
        bs = fda::create.bspline.basis(rangeval=rangeval, nbasis=nbasis, ...)
      }
    X = fda::eval.basis(x, bs)
    lmfit = lm.fit(X, y)
    return(fda::eval.basis(x.eval, bs) %*% lmfit$coefficients)
  } else{
    return(rep(NA, length(x.eval)))
  }
}
stfit::opts_stfit$set(temporal_mean_est = spreg2)

#Set path to store intermediate gapfill function data
path = paste0(getwd(), "/b2_imputation/")

#############################
### Level one imputation ####
#############################
cat("level one imputation....\n")

#In this case, sample every 5 pixels in row and every 4 pixels in column
#This will create a 22 by 32 image to represent the whole image
idx1 = c(t(outer(seq(2, n.row, by = 5), seq(1, n.col, by = 4),
                 FUN = function(ridx, cidx){
                   (ridx-1) * 125 + cidx
                 })))
mat1 = mat[,idx1]

#Impute the sub-sampled image
#Note: May need to adjust h.tcov and clipRange values
res1 = gapfill(year, doy, mat1, ceiling(n.row / 5), ceiling(n.col / 4), doyeval = day.eval,
                     h.tcov = 1000, clipRange = c(0, 2000), use.intermediate.result = TRUE, outlier.action = "keep",
                     intermediate.dir = paste0(path, "lvl1/"))

#Fill in the imputed pixels
na.idx1 = is.na(mat1)
mat[,idx1][na.idx1] = res1$imat[na.idx1]
saveRDS(mat, paste0(path, "lvl1/", "lvl1_impute_outlier_removed.rds"))

################################
### Level two imputation########
################################
cat("level two imputation....\n")

#Separate the whole image into 3x3 subimages: 36, 36, 37 and 42, 42, 41
row.grids <- c(0, 36, 72, 109)
col.grids <- c(0, 42, 84, 125)

#Parallelize imputation of subimages
res.list = foreach(n=1:9) %dopar% {
  ii = floor((n-1)/3) + 1
  jj = (n-1) %% 3 + 1
  #Block index
  bIdx <- c(t(outer(seq(row.grids[ii]+1, row.grids[ii+1]), seq(col.grids[jj]+1, col.grids[jj+1]),
                    FUN = function(ridx, cidx){
                      (ridx-1) * 125 + cidx
                    })))
  mat2 <- mat[, bIdx]
  gapfill(year, doy, mat2, row.grids[ii+1]-row.grids[ii], col.grids[jj+1]-col.grids[jj], doyeval = day.eval, 
                  h.tcov = 200, clipRange = c(0, 2000), use.intermediate.result = TRUE, outlier.action = "keep",
                  intermediate.dir = paste0(path, "lvl2/block", n, "/"))$imat
}
saveRDS(res.list, file = paste0(path, "res.list.rds"))

#Recombine imputed images
for(n in 1:9){
  ii = floor((n-1)/3) + 1
  jj = (n-1) %% 3 + 1
  #Block index
  bIdx <- c(t(outer(seq(row.grids[ii]+1, row.grids[ii+1]), seq(col.grids[jj]+1, col.grids[jj+1]),
                    FUN = function(ridx, cidx){
                      (ridx-1) * 125 + cidx
                    })))
  mat[, bIdx] <- res.list[[n]]
}

#Save final imputed output
#Use this output for this Band in next step: train_and_predict_RF
#Need to impute at least b2, b4, and b5 files
saveRDS(mat, file = paste0(path, "mat_imputed_b2_new.rds"))
