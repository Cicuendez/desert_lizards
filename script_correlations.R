# This script calculates the Kendall correlation between species richness and
# a variable divided in 3 different types: 
#   1) original variable values (e.g. elevation)
#   2) within variation (e.g. elevation aggregated by standard deviation)
#   3) neighborhood variation: (e.g. standard deviation of the neighbors around
#      the focal pixel)
# The correlations are calculated per area as defined as spatial polygons (e.g.
# a polygon shapefile). 
# For each area, a random subsample (with replacement) of "rep.size" pixels from
# species richness and variables is extracted and Kendall correlations 
# calculated. This is repeated 'nreps' times and a confidence interval is 
# extracted for the correlation.
# The script produces a CSV file with all results and a plot.

library(rgdal)
library(raster)
load("results_pedro/R_session_deserts.RData")

inDir <- "data/rasters"
predictors <- c("AnnualPrec", "AnnualTemp", "AvgMaxNDVI", "Elevation", 
          "MeanDiurnalRange", "PrecDriestMonth", "Aridity", "Pet")

# INPUT DATA ###################################################################

## Area vector file 
deserts <- readOGR("data/vector/all_deserts_dissolved.shp")

# Descriptive field of unique areas in vector data
field <- "desert"

## Richness raster
rich <- raster("data/rasters/lizard_richness_raster.tif")
rich <- readAll(rich)
results <- vector('list', length = length(predictors))
names(results) <- predictors
vars <- vector('list', length = length(predictors))
names(vars) <- predictors
var.files <- vector('list', length = length(predictors))
names(var.files) <- predictors

for (predictor in predictors){
    ## Variables (File names are expected in the format VDIR/VARNAME_VTYPE.tif)
    vdir <- "data/rasters"
    varname <- predictor
    print(predictor)
    vtypes <- c("avg" , "sdev", "sdevLarge_5neighs")
    var.files[[predictor]] <- file.path(vdir, paste0(varname, "_", vtypes, "_10min.tif"))
    vars[[predictor]] <- stack(var.files[[predictor]])
    vars[[predictor]] <- readAll(vars[[predictor]])
    
    # INPUT VARIABLES ##############################################################
    
    nreps <- 1000
    rep.size <- 250
    
    # OUTPUT FILES #################################################################
    csvfile <- paste("results/", varname, "_results.csv", sep="")
    plotfile <- paste("results/", varname, "_plot.png", sep="")
    
    
    # SCRIPT #######################################################################
    results[[predictor]] <- data.frame(expand.grid(desert=deserts[[field]], var=predictor, type=vtypes),
                          n.px=NA, low.ci=NA, mean=NA, up.ci=NA, sd=NA)    
    
    for (d in 1:length(deserts)) {
        desert <- deserts[d,]
        dname <- desert[[field]]
        
        # data: values of the environmental variable and of richness extracted for the desert area.
        data <- na.exclude(data.frame(extract(vars[[predictor]], desert)[[1]], 
                                      rich=extract(rich, desert)[[1]]))
        
        print(paste("Working on", dname))
        
        # Replicates and correlation
        # rep.data: 1,000 correlation coefficients for each variable type (avg, sdev, sdevLarge_5neighs).
        rep.data <- matrix(NA, nreps, length(vtypes))
        for (r in 1:nreps) {
            # dt.rep: sampling of 'rep.size' pixels from the environmental variable and richness (subset of 'data')
            #dt.rep[[predictor]] <- data[sample(nrow(data), rep.size, replace=TRUE),]
            dt.rep <- data[sample(nrow(data), rep.size, replace=TRUE),]
            for (v in 1:nlayers(vars[[predictor]])) {
                #rep.data[r,v] <- cor(dt.rep[[predictor]][,v], dt.rep[[predictor]]$rich, method="kendall")
                rep.data[r,v] <- cor(dt.rep[,v], dt.rep$rich, method="kendall")
            }
        }
        
        # C.I.
        m <- colMeans(rep.data, na.rm=T)
        s <- apply(rep.data, 2, sd, na.rm=T)
        ci.low <- m-1.96*s
        ci.up <- m+1.96*s
        
        for (v in 1:length(vtypes)) {
            row.filter <- results[[predictor]]$desert==dname & results[[predictor]]$type==vtypes[v]
            results[[predictor]][row.filter, 4:8] <- c(nrow(data), ci.low[v], m[v], ci.up[v], s[v])
        }
    }
    
    write.table(results[[predictor]], csvfile, sep=";", col.names=T, row.names=F, quote=F)
    
    
    # PLOTTING #####################################################################
    nd <- length(deserts[[field]])
    col <- c("#a62103", "#f25116", "#7a97ff", "#f2dd72", "#400c07", "#03a688",
             "#6a8c1f", "#f29e38")
    
    png(plotfile, height=600, width=1000, pointsize=18)
    plot.new()
    plot.window(c(1,3*nd), c(-0.65, 0.65))
    
    i <- 1
    for (vtp in vtypes) {
        for (d in 1:nd) {
            desert <- deserts[[field]][d]
            row.filter <- results[[predictor]]$desert==desert & results[[predictor]]$type==vtp
            vals <- unlist(results[[predictor]][row.filter,5:7])
            points(i, vals[2], pch=16, col=col[d])
            arrows(i, vals[1], i, vals[3], length=0.05, angle=90, code=3, lwd=2, col=col[d])
            i <- i +1
        }
    }
    abline(h=0)
    abline(v=c(nd+0.5, nd*2+0.5), lty=2)
    legend("bottomright", pch=16, col=col, legend=deserts$desert, ncol = 2, cex=0.6) 
    axis(2)
    title(main= varname, ylab=("Kendall correlation"))
    axis(1, at=c(nd/2, nd/2+nd, nd/2+2*nd), 
         labels=c("Original", "Within StdDev", "Neighbour StdDev"))
    box()
    dev.off()   
    
}

getwd()
save.image("R_session_deserts.RData")

############
############
############

## Variables (File names are expected in the format VDIR/VARNAME_VTYPE.tif)
vdir <- "data/rasters"
varname <- "Elevation"
vtypes <- c("avg" , "sdev", "sdevLarge_5neighs")
var.files <- file.path(vdir, paste0(varname, "_", vtypes, "_10min.tif"))
vars <- stack(var.files)

# INPUT VARIABLES ##############################################################

nreps <- 1000
rep.size <- 250

# OUTPUT FILES #################################################################
csvfile <- paste("results/", varname, "_results.csv", sep="")
plotfile <- paste("results/", varname, "_plot.png", sep="")


# SCRIPT #######################################################################
results <- data.frame(expand.grid(desert=deserts[[field]], type=vtypes),
                      n.px=NA, low.ci=NA, mean=NA, up.ci=NA, sd=NA)    
    
for (d in 1:length(deserts)) {
    desert <- deserts[d,]
    dname <- desert[[field]]

    data <- na.exclude(data.frame(extract(vars, desert)[[1]], 
                                  rich=extract(rich, desert)[[1]]))

    print(paste("Working on", dname))

    # Replicates and correlation
    rep.data <- matrix(NA, nreps, length(vtypes))
    for (r in 1:nreps) {
        dt.rep <- data[sample(nrow(data), rep.size, replace=TRUE),]
        for (v in 1:nlayers(vars)) {
            rep.data[r,v] <- cor(dt.rep[,v], dt.rep$rich, method="kendall")
        }
    }
    
    # C.I.
    m <- colMeans(rep.data, na.rm=T)
    s <- apply(rep.data, 2, sd, na.rm=T)
    ci.low <- m-1.96*s
    ci.up <- m+1.96*s
    
    for (v in 1:length(vtypes)) {
        row.filter <- results$desert==dname & results$type==vtypes[v]
        results[row.filter, 3:7] <- c(nrow(data), ci.low[v], m[v], ci.up[v], s[v])
    }
}

write.table(results, csvfile, sep=";", col.names=T, row.names=F, quote=F)


# PLOTTING #####################################################################
nd <- length(deserts[[field]])
col <- c("#a62103", "#f25116", "#7a97ff", "#f2dd72", "#400c07", "#03a688",
         "#6a8c1f", "#f29e38")

png(plotfile, height=600, width=1000, pointsize=18)
plot.new()
plot.window(c(1,3*nd), c(-0.65, 0.65))

i <- 1
for (vtp in vtypes) {
    for (d in 1:nd) {
        desert <- deserts[[field]][d]
        row.filter <- results$desert==desert & results$type==vtp
        vals <- unlist(results[row.filter,4:6])
        points(i, vals[2], pch=16, col=col[d])
        arrows(i, vals[1], i, vals[3], length=0.05, angle=90, code=3, lwd=2, col=col[d])
        i <- i +1
    }
}
abline(h=0)
abline(v=c(nd+0.5, nd*2+0.5), lty=2)
legend("bottomright", pch=16, col=col, legend=deserts$desert, ncol = 2, cex=0.6) 
axis(2)
title(main= varname, ylab=("Kendall correlation"))
axis(1, at=c(nd/2, nd/2+nd, nd/2+2*nd), 
     labels=c("Original", "Within StdDev", "Neighbour StdDev"))
box()
dev.off()   



