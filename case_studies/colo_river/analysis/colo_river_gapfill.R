# landsat gapfill
# Thu Jul 12 22:11:35 2018 ------------------------------
library(doParallel)
library(Matrix)
library(raster)
library(rasterVis)
library(fda)
library(stfit)
library(dplyr)
colthm = RdBuTheme()
colthm$regions$col = rev(colthm$regions$col)
source("../gapfill_landsat2.R")

# read the landsat data
# The region is 109 rows by 138 columns with 15768 pixels in total.
# 1111 images
data <- read.csv("/vol/data/zhuz/xinyuechang/LandsatImputation/Datasets/colo_river_new_mask_b2.csv")
mat <- as.matrix(data[, -c(1,2)])
mat[!is.na(mat)] <- as.numeric(mat[!is.na(mat)])
mat[mat > 3000 | mat < 0] <- NA
year <- data$year
doy <- data$doy
day.eval <- sort(unique(data$doy))

registerDoParallel(cores = 18)
stfit::opts$set(temporal_mean_est = stfit::spreg)

path = "~/ColoradoRiver/Band2/"
#############################
### Level one imputation ####
#############################
cat("level one imputation....\n")
# sample every 5 pixels in row and every 5 pixels in column
# use a 22 by 30 image to represent the whole image
idx1 = c(t(outer(seq(2, 109, by = 5), seq(1, 138, by = 5),
                 FUN = function(ridx, cidx){
                   (ridx-1) * 138 + cidx
                 })))
mat1 = mat[,idx1]
# impute the sampled image
res1 = gapfill_landsat2(year, doy, mat1, 22, 28, doyeval = day.eval, outlier.action = "keep",
                     h.tcov = 200, clipRange = c(0, 2400), use.intermediate.result = TRUE, 
                     intermediate.dir = paste0(path, "lvl1/"))
# fill in the imputed pixels
na.idx1 = is.na(mat1)
mat[,idx1][na.idx1] = res1$imat[na.idx1]
saveRDS(mat, paste0(path, "lvl1/", "lvl1_impute_outlier_removed.rds"))

################################
### Level two imputation########
################################
cat("level two imputation....\n")

# separate the whole image into 3x3 subimages: 36, 36, 37 and 46, 46, 46
res.list = foreach(n=1:9) %dopar% {
  ii = floor((n-1)/3) + 1
  jj = (n-1) %% 3 + 1
  ## block index
  if (ii < 3) {
    bIdx = c(t(outer(seq((ii-1)*36+1, ii*36), seq((jj-1)*46+1, jj*46),
                   FUN = function(ridx, cidx){
                     (ridx-1) * 138 + cidx
                   })))
    mat2 = mat[, bIdx]
    gapfill_landsat2(year, doy, mat2, 36, 46, doyeval = day.eval, outlier.action = "keep", 
                   h.tcov = 200, clipRange = c(0, 2400), use.intermediate.result = FALSE,
                   intermediate.dir = paste0(path, "lvl2/block", n, "/"))$imat
  }else {
    bIdx = c(t(outer(seq((ii-1)*36+1, 109), seq((jj-1)*46+1, jj*46),
                   FUN = function(ridx, cidx){
                     (ridx-1) * 138 + cidx
                   })))
    mat2 = mat[,bIdx]
    gapfill_landsat2(year, doy, mat2, 37, 46, doyeval = day.eval, outlier.action = "keep",
                   h.tcov = 200, clipRange = c(0, 2400), use.intermediate.result = FALSE, 
                   intermediate.dir = paste0(path, "lvl2/block", n, "/"))$imat
  }
}
saveRDS(res.list, file = paste0(path, "res.list.rds"))

for(n in 1:9){
  ii = floor((n-1)/3) + 1
  jj = (n-1) %% 3 + 1
  ## block index
  if (ii < 3) {
    bIdx = c(t(outer(seq((ii-1)*36+1, ii*36), seq((jj-1)*46+1, jj*46),
                   FUN = function(ridx, cidx){
                     (ridx-1) * 138 + cidx
                   })))
    mat[,bIdx] = res.list[[n]]
  }else {
    bIdx = c(t(outer(seq((ii-1)*36+1, 109), seq((jj-1)*46+1, jj*46),
                   FUN = function(ridx, cidx){
                     (ridx-1) * 138 + cidx
                   })))
    mat[,bIdx] = res.list[[n]]
  }
}

saveRDS(mat, file = paste0(path, "mat_imputed_b2.rds"))
