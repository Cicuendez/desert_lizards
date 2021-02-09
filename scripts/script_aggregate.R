# This script receives a raster file at high resolution and aggregates the
# pixels by mean and standard deviation to a coarser resolution. It generates
# three files from a single raster:
#   1) Original values: aggregation with mean function to the target resolution
#   2) Standard Deviation: aggregation with SDEV func. to the target resolution
#   3) Neighborhood Standard deviation: It aggregates with SDEV to the target
#      resolution but uses additional cells on the neighborhood.
# For the production of the 3rd file, the script uses a parallelized version
# of the aggregation algorithm but it might still take a significant amount of
# time to process.

library(raster)
library(foreach)
library(doParallel)

# INPUT DATA ###################################################################
srcFile <- "rasters/NDVI.tif"
tgtDir <- "processedRasters"
prefix <- "NDVI"

ncores <- 3
tgt.res <- 0.166666666666666
neigh.rad <- 2

# SCRIPT #######################################################################

if (is.na(prefix)) {
    prefix <- strsplit(basename(srcFile), "\\.")[[1]][1]
}

registerDoParallel(ncores)

# Search Window
w <- matrix(1, neigh.rad*2+1, neigh.rad*2+1)

# read source raster
src.rst <- raster(srcFile)

# Try to find the best aggregation factor to target resolution
agg.factor <- round(tgt.res/res(src.rst)[1], 5)

if (agg.factor > 1 & agg.factor%%1==0) {

    mean.rst <- aggregate(src.rst, agg.factor, mean, na.rm=T)
    fname <- paste0(prefix, "_avg.tif")
    writeRaster(mean.rst, file.path(tgtDir, fname)) 
    
    sdev.rst <- aggregate(src.rst, agg.factor, sd, na.rm=T)
    fname <- paste0(prefix, "_sdev.tif")
    writeRaster(sdev.rst, file.path(tgtDir, fname))
    
    larg.rst <- mean.rst * NA
    crd <- coordinates(mean.rst)
    half.side <- neigh.rad*tgt.res+tgt.res/2
    n <- nrow(crd)
    ind <- which(!is.na(mean.rst[]))
    
    dt <- foreach(i=ind, .combine='c') %dopar% {
        v <- mean.rst[i]
        pnt <- unlist(crd[i,])
        ext <- extent(pnt[1]-half.side, pnt[1]+half.side,
                      pnt[2]-half.side, pnt[2]+half.side)
        nv <- extract(src.rst, ext)
        sd(nv, na.rm=T)
    }
    larg.rst[ind] <- dt
    fname <- paste0(prefix, "_sdevNeigh.tif")
    writeRaster(larg.rst, file.path(tgtDir, fname))
    
} else {
    stop("Aggregation factor either 1 or not integer")   
}






