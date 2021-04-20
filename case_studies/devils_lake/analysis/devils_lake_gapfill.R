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
source("gapfill.R")

# read the landsat data
# The region is 108 rows by 146 columns with 15768 pixels in total.
# 786 images
data <- read.csv("dl_to_gapfill_b2.csv")
mat <- as.matrix(data[, -c(1,2)])
mat[!is.na(mat)] <- as.numeric(mat[!is.na(mat)])
mat[mat > 2000 | mat < 0] <- NA
year <- data$year
doy <- data$doy
day.eval <- sort(unique(data$doy))

registerDoParallel(cores = 18)
stfit::opts$set(temporal_mean_est = stfit::spreg)

path = "./DevilsLake/Band2"
#############################
### Level one imputation ####
#############################
cat("level one imputation....\n")
# sample every 5 pixels in row and every 5 pixels in column
# use a 22 by 30 image to represent the whole image
idx1 = c(t(outer(seq(2, 109, by = 5), seq(1, 162, by = 5),
                 FUN = function(ridx, cidx){
                   (ridx-1) * 162 + cidx
                 })))
mat1 = mat[,idx1]
# impute the sampled image
res1 = gapfill(year, doy, mat1, 22, 33, doyeval = day.eval, outlier.action = "keep",
                     h.tcov = 200, clipRange = c(0, 1700), use.intermediate.result = TRUE, 
                     intermediate.dir = paste0(path, "lvl1/"))
# fill in the imputed pixels
na.idx1 = is.na(mat1)
mat[,idx1][na.idx1] = res1$imat[na.idx1]
saveRDS(mat, paste0(path, "lvl1/", "lvl1_impute_outlier_removed.rds"))

################################
### Level two imputation########
################################
cat("level two imputation....\n")

# separate the whole image into 3x3 subimages: 36, 36, 37 and 54, 54, 54
res.list = foreach(n=1:9) %dopar% {
  ii = floor((n-1)/3) + 1
  jj = (n-1) %% 3 + 1
  ## block index
  if (ii < 3) {
    bIdx = c(t(outer(seq((ii-1)*36+1, ii*36), seq((jj-1)*54+1, jj*54),
                   FUN = function(ridx, cidx){
                     (ridx-1) * 162 + cidx
                   })))
    mat2 = mat[, bIdx]
    gapfill(year, doy, mat2, 36, 54, doyeval = day.eval, outlier.action = "keep", 
                   h.tcov = 200, clipRange = c(0, 1700), use.intermediate.result = TRUE,
                   intermediate.dir = paste0(path, "lvl2/block", n, "/"))$imat
  }else {
    bIdx = c(t(outer(seq((ii-1)*36+1, 109), seq((jj-1)*54+1, jj*54),
                   FUN = function(ridx, cidx){
                     (ridx-1) * 162 + cidx
                   })))
    mat2 = mat[,bIdx]
    gapfill(year, doy, mat2, 37, 54, doyeval = day.eval, outlier.action = "keep",
                   h.tcov = 200, clipRange = c(0, 1700), use.intermediate.result = TRUE, 
                   intermediate.dir = paste0(path, "lvl2/block", n, "/"))$imat
  }
}
saveRDS(res.list, file = paste0(path, "res.list.rds"))

for(n in 1:9){
  ii = floor((n-1)/3) + 1
  jj = (n-1) %% 3 + 1
  ## block index
  if (ii < 3) {
    bIdx = c(t(outer(seq((ii-1)*36+1, ii*36), seq((jj-1)*54+1, jj*54),
                   FUN = function(ridx, cidx){
                     (ridx-1) * 162 + cidx
                   })))
    mat[,bIdx] = res.list[[n]]
  }else {
    bIdx = c(t(outer(seq((ii-1)*36+1, 109), seq((jj-1)*54+1, jj*54),
                   FUN = function(ridx, cidx){
                     (ridx-1) * 162 + cidx
                   })))
    mat[,bIdx] = res.list[[n]]
  }
}

saveRDS(mat, file = paste0(path, "mat_imputed_b2_new.rds"))
