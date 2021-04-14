#Gapfill function for imputing Landsat imagery
#for every 2 years, calculate mean function and temporal effect
#for completely missing images, add spatial effect obtained from linear interpolation among nearest two images
gapfill <- function(year, doy, mat, img.nrow, img.ncol, doyeval = 1:365,  h.tcov = 100, h.tsigma2 = 300,
                    h.scov = 2, h.ssigma2 = 2, nnr = 10, outlier.action = "keep", outlier.tol = 0.2,
                    intermediate.save = TRUE, intermediate.dir = "./output/",
                    use.intermediate.result = TRUE, doy.break = NULL, b2e.con = FALSE, t.grid.num = 50,
                    clipRange = c(0, 1800), clipMethod = "nnr") {
  
  #Load intermediately saved data for efficiency if present
  if(intermediate.save){
    if(!dir.exists(intermediate.dir)){
      cat(paste0("Folder", intermediate.dir, "is created to save intermediate results."))
      dir.create(intermediate.dir, recursive = TRUE)
    }
  }
  
  #Data in date by pixel format from .csv file
  imat = mat
  
  ###################################
  #### 1. Overall mean estimaton ####
  ###################################
  
  #Calculate the number of 2 year segments
  yeareval <- sort(unique(year))
  num.segs <- length(yeareval)%/%2
  
  #Load intermediately saved data if present
  if(use.intermediate.result & file.exists(paste0(intermediate.dir, "meanest.rds"))){
    meanest = readRDS(paste0(intermediate.dir, "meanest.rds"))
  } else {
    #Divide into intervals every 2 years, if 1 year is left, then added to last interval
    meanest <- list()
    for (g in 1:num.segs) {
      #Determine indicies for segment
      yrs <- yeareval[(2*g-1):(2*g)]
      if(length(yeareval) == (2*g+1)) {
        yrs <- yeareval[(2*g-1):(2*g+1)]
      }
      #Subset data
      sub.idx <- which(year %in% yrs)
      #Complete mean function estimation for 2-year segment
      msk <- stfit:::getMask(mat[sub.idx, ])
      meanest[[g]] = stfit::meanEst(doy[sub.idx], mat[sub.idx, ], doyeval = sort(unique(doy[sub.idx])),
                             clipRange = clipRange, msk = msk, clipMethod = clipMethod, outlier.tol = outlier.tol, 
                             img.nrow = img.nrow, img.ncol = img.ncol)
    }
    #Save intermediate data
    if(intermediate.save)
      saveRDS(meanest, paste0(intermediate.dir, "meanest.rds"))
  }
  
  ###################################
  #### 2. Time effect estimation ####
  ###################################
  
  #Matrix to hold temporary data
  rmat <- matrix(NA, ncol = ncol(mat), nrow = nrow(mat)) 
  
  #For each 2-year segment
  for (g in 1:num.segs) {
    yrs <- yeareval[(2*g-1):(2*g)]
    if(length(yeareval) == (2*g+1)) {
      yrs <- yeareval[(2*g-1):(2*g+1)]
    }
    #Remove outlier pixels
    sub.idx <- which(year %in% yrs)
    meanestg <- meanest[[g]]
    for (i in 1:length(meanestg$outlier$outidx)) {
      gidx <- sub.idx[meanestg$outlier$outidx[i]]
      mat[gidx, meanest$outlier$outlst[[i]]] <- NA 
    }
    #Remove outlier images
    outlier.img.idx = sub.idx[meanestg$idx$idx.outlier]
    for(i in outlier.img.idx){
      mat[outlier.img.idx,] = NA
    }
    rmat[sub.idx, ] = mat[sub.idx, ] - meanestg$meanmat[unlist(lapply(doy[sub.idx], function(x,y) which(y == x), y = meanestg$doyeval)), ]
  }
  
  #Estimate the temporal effect using residuals
  #Result is a 3d array with the first dimension year, second dimension doy and third dimension pixel index
  if(use.intermediate.result & file.exists(paste0(intermediate.dir, "teffarray.rds"))){
    teffarray = readRDS(paste0(intermediate.dir, "teffarray.rds"))
  } else {
    teffarray = stfit:::teffEst(year, doy, rmat, doyeval = doyeval, h.cov = h.tcov, h.sigma2 = h.tsigma2, t.grid.num = t.grid.num)
    if(intermediate.save)
      saveRDS(teffarray, paste0(intermediate.dir, "teffarray.rds"))
  }
  
  ######################################
  #### 3. Spatial effect estimation ####
  ######################################
  
  #Calculate residuals after removing temporal effect
  yearidx = unlist(lapply(year, function(x, y)
    which(y == x), y = as.numeric(dimnames(teffarray$teff_array)[[1]])))
  doyidx = unlist(lapply(doy, function(x, y)
    which(y == x), y = as.numeric(dimnames(teffarray$teff_array)[[2]])))
  for (i in 1:nrow(rmat)) {
    rmat[i, ] = rmat[i, ] - teffarray$teff_array[yearidx[i], doyidx[i], ]
  }
  
  #Estimate the spatial effect using residuals
  #Result is a 3d array with the first dimension year, second dimension doy and third dimension pixel index
  if(use.intermediate.result & file.exists(paste0(intermediate.dir, "seffmat.rds"))){
    seffmat = readRDS(paste0(intermediate.dir, "seffmat.rds"))
  } else {
    if(is.null(doy.break)){
      seffmat = seffEst(rmat, img.nrow, img.ncol, nnr = nnr, h.cov = h.scov, h.sigma2 = h.ssigma2 )$seff_mat
    }
    if(intermediate.save)
      saveRDS(seffmat, paste0(intermediate.dir, "seffmat.rds"))
  }
  
  #######################
  #### 4. Gapfilling ####
  #######################
  
  #Partially missing images: mean + time effect + spatial effect 
  #All missing images: mean + time effect
  #First calculate the theoretically imputed mat
  mat_imputed <- matrix(0, ncol = ncol(mat), nrow = nrow(mat))
  for (g in 1:num.segs) {
    yrs <- yeareval[(2*g-1):(2*g)]
    if(length(yeareval) == (2*g+1)) {
      yrs <- yeareval[(2*g-1):(2*g+1)]
    }
    sub.idx <- which(year %in% yrs)
    meanestg <- meanest[[g]]
    mat_imputed[sub.idx, ] <- meanestg$meanmat[unlist(lapply(doy[sub.idx], function(x, y) which(y == x), y = meanestg$doyeval)), ]
  }
  
  #Add temporal effect estimation
  for(i in 1:nrow(mat_imputed)){
      mat_imputed[i,] = mat_imputed[i,] + teffarray$teff_array[yearidx[i], doyidx[i],]
  }
  
  #Add spatial effect estimation
  pct_missing = apply(seffmat, 1, function(x) {length(which(x==0))/length(x)})
  all.miss.idx <- which(pct_missing == 1)
  
  for (i in 1:length(all.miss.idx)) {
    year.miss <- year[all.miss.idx[i]]
    doys <- doy[year == year.miss & pct_missing < 0.5]
    seffmat.temp <- seffmat[year == year.miss & pct_missing < 0.5, ]
    doy.miss <- doy[all.miss.idx[i]]
    #Select the nearest two images
    #nbr1 <- which.min(doy.miss - doys[doys < doy.miss])
    if (all(doy.miss <= doys) | all(doy.miss >= doys)) {
      doy1 <- which.min(abs(doy.miss - doys))
      seffmat[all.miss.idx[i], ] <- seffmat.temp[doy1, ]
    } else {
      doy1 <- doys[doys <= doy.miss][which.min(doy.miss - doys[doys <= doy.miss])]
      doy2 <- doys[doys > doy.miss][which.min(doys[doys > doy.miss] - doy.miss)]
      seffmat1 <- colMeans(seffmat[doy==doy1 & year==year.miss, , drop = FALSE])
      seffmat2 <- colMeans(seffmat[doy==doy2 & year==year.miss, , drop = FALSE])
      #Weighted average of the nearest two images
      seffmat[all.miss.idx[i], ] <- ((doy.miss-doy1)/(doy2-doy1))*seffmat2 + ((doy2-doy.miss)/(doy2-doy1))*seffmat1
    }
  }
  
  mat_imputed = mat_imputed + seffmat

  #Final imputation
  outlier.action = match.arg(outlier.action)
  if(outlier.action == "keep") {
    #If keep originally observed values
    imat[is.na(imat)] = mat_imputed[is.na(imat)] 
    imat[imat < 0] <- 0 # truncate results at 0
    } else if (outlier.action == "remove") {
      #If remove outliers
      imat = mat
      imat[is.na(imat)] = mat_imputed[is.na(imat)]
    }
  return(list(imat = imat, idx = meanest$idx, outlier = meanest$outlier))
  
}
  