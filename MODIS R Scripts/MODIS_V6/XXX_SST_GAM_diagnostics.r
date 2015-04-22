# ---------------------------------------------------------------------------------------------
# --- This script is intended to provide initial diagnostics for MODIS SST estimates
# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Install required packages ----

if (!require(lubridate)) {install.packages("lubridate"); require(lubridate)}
if (!require(stringr)) {install.packages("stringr"); require(stringr)}
if (!require(maps)) {install.packages("maps"); require(maps)}
if (!require(maptools)) {install.packages("maptools"); require(maptools)}
if (!require(robust)) {install.packages("robust"); require(robust)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(reshape)) {install.packages("reshape"); require(reshape)}
if (!require(raster)) {install.packages("raster"); require(raster)}
if (!require(IDPmisc)) {install.packages("IDPmisc"); require(IDPmisc)}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define sensor and calibration collection on which we will be working ----

#sensor <- "AQUA"        # Select MODIS onboard AQUA or TERRA
sensor <- "TERRA"  			# Select MODIS onboard AQUA or TERRA

collection <- 6				  # Calibration collection

cat("\n Working on MODIS onboard",sensor,"collection",collection,"...\n");
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define geophysical variable worked on (ie, SST or SST4), algorithm type, etc. ----
# --- and other relevant quantities.

geophys.var <- "SST"				  # Geophysical variable
sst.algo <- "latband1"				# Type of SST algorithm

algo.coeffs.version <- "2.0"	# Version of algorithm coefficients

# -- Define matchups version and input format (old or new)

matchup.version  <- paste("collection_",collection, sep="")	  # Version of matchups

matchup.format <- "OLD"
#matchup.format <- "NEW"
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Check if object "orig" exists ----
# --- This object contains the data frame with matchup records, and has been created
# --- by previously-run scripts.

if (exists("orig")) {
  cat("Object orig exists...\n") 
} else {
  stop("ERROR:Object orig DOES NOT exist\n") 
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Find out what operating system we are using ----

op.sys <- .Platform$OS.type  											# Get operating system

if (regexpr("^[Ww]in*", op.sys) == 1) {
  op.sys <- "Windows"															# Windows
} else if (regexpr("^[LlIiNnUuXx]*", op.sys) == 1) {
  op.sys <- "Linux"																# Linux
}
cat(paste("We are running on a",op.sys,"system...   \n"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Build names of directories for outputs (coefficients, figures) ----

if (op.sys == "Windows") {
  
  # --- Directory where algorithm coefficients will be placed
  coeff.outdir <- paste("D:/Matchups/MODIS/", sensor,"/",
    matchup.version, "/coeffs","/", sep="")
  tt0 <- file.info(coeff.outdir)
  if(!tt0$isdir)
    stop("ERROR: Directory", coeff.outdir,"does not exist...\n")
  
  # --- Directory where output graphics will be placed
  graph.outdir <- paste("D:/Matchups/MODIS/", sensor,"/",
    matchup.version,"/figs/", sep="")
  tt0 <- file.info(graph.outdir)
  if(!tt0$isdir)
    stop("ERROR: Directory", graph.outdir,"does not exist...\n")
  
  # --- Directory where processed data.frame will be placed
  results.outdir <- paste("D:/Matchups/MODIS/", sensor,"/",
    matchup.version,"/results/", sep="")
  tt0 <- file.info(results.outdir)
  if(!tt0$isdir)
    stop("ERROR: Directory", results.outdir,"does not exist...\n")
  
  # --- Directory where quality hypercubes will be placed
  cube.outdir <- paste("D:/Matchups/MODIS/", sensor,"/",
    matchup.version,"/hypercubes/", sep="")
  tt0 <- file.info(cube.outdir)
  if(!tt0$isdir)
    stop("ERROR: Directory", cube.outdir,"does not exist...\n")
  
}	# End of checking if operating system is Windows

rm(op.sys, tt0); gc()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# --- BEGIN DIAGNOSTICS
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

plot.diagnostics <- TRUE    # Plot diagnostics

# ---------------------------------------------------------------------------------------#
# --- How many matchups available for SST coefficient estimation? ----

if (plot.diagnostics) {
 
  outfile <- "barplot_usable_matchups_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
     width = 640, height = 480, units = "px", pointsize = 12,
     bg = "white", res = NA)
  barplot(prop.table(xtabs(~ orig$use.4.coeffs.SST)),
    names=c("NOT usable","Usable"),
    main=paste(sensor,"SST - Matchups usable for SST coeff estimation"),
    ylab="Proportion of matchups",
    col="lemonchiffon2", las=1)
  box()
  dev.off()
  
}

rm(outfile); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Level plot of availability of usable matchups ----
# --- by latitude band and month of the year

xx0 <- aggregate(sat.timedate ~ sat.mon + latband, data=orig,
  subset=use.4.coeffs.SST, FUN=length)

colnames(xx0) <- c("mon","lat","n")

bbb <- pretty(xx0$n, n=6)
ccc <- brewer.pal(length(bbb)-1, "RdYlBu")
cc2 <- ccc[length(ccc):1]		# flip palette so it goes from blue to red

if (plot.diagnostics) {
  
  outfile <- "levelplot_usable_matchups_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  
  png(filename = outfile,
    width = 640, height = 640, units = "px", pointsize = 12,
    bg = "white", res = NA)

  levelplot(n ~ mon * lat, data=xx0,
    pretty=TRUE, at=bbb,
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Matchups usable for SST coeff estimation"),
    ylim=c(0.75, 6.25),
    col.regions=cc2,
    scales = list( x= list(cex=1.0, rot=0, at=seq(1,12,1), labels=month.abb),
    y = list(cex=0.8, rot=90, at=seq(1,6,1),
    labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5, v=0.5:11.5, col="white", lwd=1)},
    colorkey=list(space="right", col=cc2, at=bbb,
      labels=list(at=bbb, cex=0.8), height=0.8, width=2))
  
  dev.off()

}
  
rm(xx0,bbb,ccc,cc2);gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#



geo.dir <- "D:/ENSO-Data/Other Data/Natural Earth/"

tt0 <- file.info(geo.dir)     # get info about input directory
if (!tt0$isdir)               # Is it a directory?
  stop("ERROR: Specified input directory does not exist... verify name...\n")
rm(tt0); gc()

file <- "ne_110m_land.shp"              # Name of file with geographic info
infile <- paste(geo.dir, file, sep="")    # Build complete (long) input file name

if (!exists("infile")) {
  stop("ERROR: Specified input file  does not exist...\n") }

land <- readShapeSpatial(infile, repair=TRUE,force_ring=TRUE,
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

land2 <- as(land, "SpatialPolygons")

land.layout=list("sp.polygons", land2, col="black", fill="grey")


# --- Count the number of matchups per 5-degree cell ----

tt2 <- tapply(orig$buoy.sst, INDEX=orig$cell5deg, FUN=length, simplify=TRUE)

# --- Create a raster with the count

# --- Create a raster object with 5-degree pixels

grid5deg <- raster(ncol=72, nrow=36,
  xmn=-180, xmx=180,
  ymn=-90, ymx=90,
  crs="+proj=longlat +ellps=WGS84 +datum=WGS84")

count5deg <- grid5deg
values(count5deg) <- NA # Fill the raster with NAs

cell.ids <- as.integer(names(tt2))

count5deg[cell.ids] <- tt2

uu1 <- quantile(count5deg, probs=c(0.0,0.2,0.4,0.6,0.8,1.0))

image(count5deg,
  main="Number of Terra matchups",
  xlab="Longitude", ylab="Latitude",
  ylim=c(-89, 89),
  breaks=uu1,
  col=brewer.pal('YlOrRd', n=5))
lines(world.sp)
box()

uu1 <- quantile(count5deg, probs=c(0.0,0.2,0.4,0.6,0.8,1.0))


library(rasterVis)
levelplot(count5deg, margin=FALSE,
  main="Number of Terra matchups",
  colorkey=TRUE,
  at=uu1,
  col.regions=brewer.pal('YlOrRd', n=5))

# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot SST residuals as a function of satellite zenith angle

#use <- orig$SST.tree.class == "P-Good" & abs(orig$SST.latband1.gam.res) < 5 &
#  orig$qsst.new == 0

use <- abs(orig$SST.latband1.gam.res + 0.17) < 1.5 

smoothScatter(orig$satz[use], orig$SST.latband1.gam.res[use],
  colramp = colorRampPalette(c("white",brewer.pal(7,"Greens"))),
  nbin=120,
  xlim=c(-60,60),
  ylim=c(-4,4),
  xlab="Satellite Zenith Angle",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST GAM residuals"))

abline(h=-0.17, col="white")

library(IDPmisc)

iplot(orig$satz[use], orig$SST.latband1.gam.res[use],
  xlab="Satellite Zenith Angle",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST GAM residuals"))

abline(h=-0.17, col="white")



use <- abs(orig$SST.latband1.res + 0.17) < 2.0 

smoothScatter(orig$satz[use], orig$SST.latband1.res[use],
  colramp = colorRampPalette(c("white",brewer.pal(7,"Greens"))),
  nbin=120,
  xlim=c(-60,60),
  ylim=c(-4,4),
  xlab="Satellite Zenith Angle",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST Latband1 residuals"))

abline(h=-0.17, col="white")

iplot(orig$satz[use], orig$SST.latband1.res[use],
  xlab="Satellite Zenith Angle",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST Latband1 residuals"))

abline(h=-0.17, col="white")


# --- Boxplots of SST residuals as a function of satellite zenith angle

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.gam.res + 0.17) < 1.5

tt2 <- cut(orig$satz[use], breaks=seq(-60,60,10), include.lowest=TRUE)

boxplot(split(orig$SST.latband1.gam.res[use], tt2),
  main=paste(sensor,"SST GAM Residuals"),
  xlab="Satellite zenith angle intervals",
  ylab="SST residuals", col="cadetblue1",
  ylim=c(-0.8,0.8), par(cex.lab=1.0, cex.axis=0.7, las=2))
abline(h=-0.17, col="tomato", lwd=2)


boxplot(split(orig$SST.latband1.res[use], tt2),
  main=paste(sensor,"SST Linear Regression Residuals"),
  xlab="Satellite zenith angle intervals",
  ylab="SST residuals", col="cadetblue1",
  ylim=c(-0.8,0.8), par(cex.lab=1.0, cex.axis=0.7, las=2))
abline(h=-0.17, col="tomato", lwd=2)

rm(tt2, use); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#

# --- Plot SST residuals as a function of buoy SST

#use <- orig$SST.tree.class == "P-Good" & abs(orig$SST.latband1.gam.res) < 5 &
#  orig$qsst.new == 0

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.gam.res + 0.17) < 2.0 

smoothScatter(orig$buoy.sst[use], orig$SST.latband1.gam.res[use],
  colramp = colorRampPalette(c("white",brewer.pal(7,"Greens"))),
  nbin=120,
  ylim=c(-2, 2),
  xlab="Buoy SST",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST GAM residuals"))

abline(h=-0.17, col="white")

library(IDPmisc)

iplot(orig$buoy.sst[use], orig$SST.latband1.gam.res[use],
  xlab="Buoy SST",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST GAM residuals"))

abline(h=-0.17, col="grey40")

# --- Now for Linear regression estimates

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.res + 0.17) < 2.0 

smoothScatter(orig$buoy.sst[use], orig$SST.latband1.res[use],
  colramp = colorRampPalette(c("white",brewer.pal(7,"Greens"))),
  nbin=120,
  ylim=c(-2, 2),
  xlab="Buoy SST",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST LR residuals"))

abline(h=-0.17, col="white")

library(IDPmisc)

iplot(orig$buoy.sst[use], orig$SST.latband1.res[use],
  xlab="Buoy SST",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST LR residuals"))

abline(h=-0.17, col="grey40")




# --- Plot GAM SST residuals as a function of BT difference

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.gam.res + 0.17) < 2.0 

smoothScatter(orig$cen.11000[use] - orig$cen.12000[use],
  orig$SST.latband1.gam.res[use],
  colramp = colorRampPalette(c("white",brewer.pal(7,"Greens"))),
  nbin=120,
  ylim=c(-2, 2),
  xlab="BT31 - BT32 difference",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST LR residuals"))

abline(h=-0.17, col="grey40")


iplot(orig$cen.11000[use] - orig$cen.12000[use],
  orig$SST.latband1.gam.res[use],
  xlab="BT31 - BT32 difference",
  ylab="SST Residuals",
  main=paste("MODIS ",sensor,"\n SST LR residuals"))

abline(h=-0.17, col="grey40")






# ---------------------------------------------------------------------------------------#
# --- Plot time series of mean of SST residuals ----
# --- by latitude band and month.
# --- Lines are plotted for GAM latband1 algorithm (reddish lines)
# --- and for regression latband1 algorithm (blue lines).

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month")
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date); gc()

# --- Compute monthly mean of SST residuals
# --- We use only matchups that can be used for coefficient estimation
# --- and those which passed the cloud tree
# --- and have quality 0

use <- abs(orig$SST.latband1.gam.res + 0.17) < 2.0 

#use <- orig$use.4.coeffs.SST &
#  orig$SST.tree.class == "P-Good" &
#  orig$qsst.new == 0

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.gam.res[use],
  INDEX=list(tt0[use], orig$latband[use]),
  FUN=mean,
  na.rm=TRUE, simplify=TRUE)      

tt2 <- data.frame(t=seq.months, tt1)
colnames(tt2) <- c("date", levels(orig$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable_name="latband")

tt3b <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("GAM", times=length(tt3$date)),
  SST.res=tt3$value)
colnames(tt3b) <- c("date","latband","SST.algo","SST.res")


tt4 <- tapply(X=orig$SST.latband1.res[use],
  INDEX=list(tt0[use], orig$latband[use]),
  FUN=mean,
  na.rm=TRUE, simplify=TRUE)

tt5 <- data.frame(t=seq.months, tt4)
colnames(tt5) <- c("date",levels(orig$latband))
rownames(tt5) <- NULL

tt6 <- melt(tt5, id.vars=c("date"), variable_name="latband")

tt6b <- data.frame(tt6$date,
  tt6$latband,
  SST.algo=rep("latband1", times=length(tt6$date)),
  SST.res=tt6$value)
colnames(tt6b) <- c("date","latband","SST.algo","SST.res")

tt7 <- rbind(tt3b, tt6b)

if (plot.diagnostics) {
  
  outfile <- "mean_SST_resids_ts"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
      width = 640, height = 640, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  xyplot(SST.res ~ date | latband, data = tt7,
         type = c("l"),
         main=paste(sensor, "Mean of SST residuals"),
         xlab="Time",
         ylab="SST residuals",
         col=c("orange","steelblue3"), lwd=2,
         groups = SST.algo, 
         panel = panel.superpose, 
         panel.groups = function(x, y, ...) { 
           panel.abline(h=-0.17, col="grey", lwd=1)
           panel.xyplot(x, y, ...) },
         layout=c(1,6))
  
  dev.off()
  
}  

rm(tt0,tt1,tt2,tt3,tt3b,tt4,tt5,tt6,tt6b,tt7); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot time series of median of SST residuals ----
# --- by latitude band and month.
# --- Lines are plotted for latband1 algorithm (reddish lines)
# --- and for v5 algorithm (blue lines).

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month")
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date)

# --- Compute monthly median of SST residuals
# --- We use only matchups that can be used for coefficient estimation
# --- and those which passed the cloud tree
# --- and have quality 0

#use <- orig$use.4.coeffs.SST &
#  orig$SST.tree.class == "P-Good" &
#  orig$qsst.new == 0

use <- abs(orig$SST.latband1.gam.res + 0.17) < 1.5 

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.gam.res[use],
  INDEX=list(tt0[use], orig$latband[use]),
  FUN=median,
  na.rm=TRUE, simplify=TRUE)      

tt2 <- data.frame(t=seq.months, tt1)
colnames(tt2) <- c("date", levels(orig$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable_name="latband")

tt3b <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("GAM", times=length(tt3$date)),
  SST.res=tt3$value)
colnames(tt3b) <- c("date","latband","SST.algo","SST.res")

tt4 <- tapply(X=orig$SST.latband1.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=median,
  na.rm=TRUE, simplify=TRUE)

tt5 <- data.frame(t=seq.months, tt4)
colnames(tt5) <- c("date",levels(orig$latband))
rownames(tt5) <- NULL

tt6 <- melt(tt5, id.vars=c("date"), variable_name="latband")

tt6b <- data.frame(tt6$date,
  tt6$latband,
  SST.algo=rep("latband", times=length(tt6$date)),
  SST.res=tt6$value)
colnames(tt6b) <- c("date","latband","SST.algo","SST.res")

tt7 <- rbind(tt3b, tt6b)

if (plot.diagnostics) {
  
  outfile <- "median_SST_resids_ts"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
      width = 640, height = 640, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  xyplot(SST.res ~ date | latband, data = tt7,
    type = c("l"),
    main=paste(sensor, "Median of SST residuals"),
    xlab="Time",
    ylab="SST residuals",
    col=c("orange","steelblue3"), lwd=2,
    groups = SST.algo, 
    panel = panel.superpose, 
    panel.groups = function(x, y, ...) { 
      panel.abline(h=-0.17, col="grey", lwd=1)
      panel.xyplot(x, y, ...) },
    layout=c(1,6))
  
  dev.off()
  
}  
  
rm(tt0,tt1,tt2,tt3,tt3b,tt4,tt5,tt6,tt6b,tt7); gc()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Plot time series of standard deviation of SST residuals
# --- by latitude band and month.
# --- Lines are plotted for latband1 algorithm (reddish lines)
# --- and for v5 algorithm (blue lines).

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date)

# --- Compute monthly SD of SST residuals
# --- We use only matchups that can be used for coefficient estimation
# --- and those which passed the cloud tree
# --- and have quality 0

#use <- orig$use.4.coeffs.SST &
#  orig$SST.tree.class == "P-Good"

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.gam.res + 0.17) < 2.0

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.gam.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=sd,
  na.rm=TRUE, simplify=TRUE)      

tt2 <- data.frame(t=seq.months, tt1)
colnames(tt2) <- c("date",levels(orig$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable_name="latband")

tt3b <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("GAM", times=length(tt3$date)),
  SST.res=tt3$value)
colnames(tt3b) <- c("date","latband","SST.algo","SST.res")



tt4 <- tapply(X=orig$SST.latband1.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=sd,
  na.rm=TRUE, simplify=TRUE)      

tt5 <- data.frame(t=seq.months, tt4)
colnames(tt5) <- c("date",levels(orig$latband))
rownames(tt5) <- NULL

tt6 <- melt(tt5, id.vars=c("date"), variable_name="latband")

tt6b <- data.frame(tt6$date,
  tt6$latband,
  SST.algo=rep("latband1", times=length(tt6$date)),
  SST.res=tt6$value)
colnames(tt6b) <- c("date","latband","SST.algo","SST.res")

tt7 <- rbind(tt3b, tt6b)

if (plot.diagnostics) {
    
  outfile <- "stdev_SST_resids_ts"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
    width = 640, height = 640, units = "px", pointsize = 12,
    bg = "white", res = NA)
  
  xyplot(SST.res ~ date | latband, data = tt7, 
    type = c("l"),
    main=paste(sensor, "Std Dev of SST residuals"),
    xlab="Time",
    ylab="Std. dev. SST residuals",
    col=c("orange","steelblue3"), lwd=2,
    groups = SST.algo, 
    panel = panel.superpose, 
    panel.groups = function(x, y, ...) { 
      panel.xyplot(x, y, ...) },
    layout=c(1,6))
  
  dev.off()
}

rm(tt0,tt1,tt2,tt3,tt3b,tt4,tt5,tt6,tt6b,tt7); gc()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Plot time series of MAD of SST residuals
# --- by latitude band and month.
# --- Lines are plotted for latband1 algorithm (reddish lines)
# --- and for v5 algorithm (blue lines).

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date)

# --- Compute monthly MAD  SST residuals
# --- We use only matchups that can be used for coefficient estimation
# --- and those which passed the cloud tree
# --- and have quality 0

#use <- orig$use.4.coeffs.SST &
#  orig$SST.tree.class == "P-Good" &
#  orig$qsst.new == 0

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.gam.res + 0.17) < 2.0

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.gam.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=mad,
  na.rm=TRUE, simplify=TRUE)      

tt2 <- data.frame(t=seq.months, tt1)
colnames(tt2) <- c("date",levels(orig$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable_name="latband")

tt3b <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("GAM", times=length(tt3$date)),
  SST.res=tt3$value)
colnames(tt3b) <- c("date","latband","SST.algo","SST.res")


tt4 <- tapply(X=orig$SST.latband1.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=mad,
  na.rm=TRUE, simplify=TRUE)      

tt5 <- data.frame(t=seq.months, tt4)
colnames(tt5) <- c("date",levels(orig$latband))
rownames(tt5) <- NULL

tt6 <- melt(tt5, id.vars=c("date"), variable_name="latband")

tt6b <- data.frame(tt6$date,
  tt6$latband,
  SST.algo=rep("latband1", times=length(tt6$date)),
  SST.res=tt6$value)
colnames(tt6b) <- c("date","latband","SST.algo","SST.res")

tt7 <- rbind(tt3b, tt6b)

if (plot.diagnostics) {
    
  outfile <- "MAD_SST_resids_ts"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
    width = 640, height = 640, units = "px", pointsize = 12,
    bg = "white", res = NA)
  
  xyplot(SST.res ~ date | latband, data = tt7, 
    type = c("l"),
    main=paste(sensor, "MAD of SST residuals"),
    xlab="Time",
    ylab="MAD of SST residuals",
    col=c("orange","steelblue3"), lwd=2,
    groups = SST.algo, 
    panel = panel.superpose, 
    panel.groups = function(x, y, ...) { 
      panel.xyplot(x, y, ...) },
    layout=c(1,6))
  
  dev.off()

}

rm(tt0,tt1,tt2,tt3,tt3b,tt4,tt5,tt6,tt6b,tt7); gc()
# ----------------------------------------------------------------------------------------
  
# ----------------------------------------------------------------------------------------
# --- 3. Hovmoller plots of SST residuals
# ----------------------------------------------------------------------------------------
  
# ----------------------------------------------------------------------------------------
# --- 3.a Hovmoller plot of median SST residual statistics by month/year and latband
 
# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")
seq.months <- format(seq.months, "%b-%Y")
  
rm(min.date, max.date)

# --- We use only matchups that can be used for coefficient estimation
# --- and those which passed the cloud tree
# --- and have quality 0

#use <- orig$use.4.coeffs.SST &
#  orig$SST.tree.class == "P-Good" &
#  orig$qsst.new == 0

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.gam.res + 0.17) < 2.0

# --- Select various statistics for SST residuals
  
xx1 <- aggregate((SST.latband1.gam.res + 0.17) ~ sat.moyr + latband,
  data=orig,
  subset = use,
  FUN=mean,
  na.action=na.omit, simplify=TRUE)
  
xx2 <- aggregate((SST.latband1.gam.res + 0.17) ~ sat.moyr + latband,
  data=orig,
  subset = use,
  FUN=median,
  na.action=na.omit, simplify=TRUE)
  
xx3 <- aggregate((SST.latband1.gam.res + 0.17) ~ sat.moyr + latband,
  data=orig,
  subset = use,
  FUN=sd,
  na.action=na.omit, simplify=TRUE)

xx4 <- aggregate((SST.latband1.gam.res + 0.17) ~ sat.moyr + latband,
  data=orig,
  subset = use,
  FUN=mad,
  na.action=na.omit, simplify=TRUE)

xx5 <- aggregate((SST.latband1.gam.res + 0.17) ~ sat.moyr + latband,
  data=orig,
  subset = use,
  FUN=length,
  na.action=na.omit, simplify=TRUE)


# --- Join all data frames with statistics
  
aa2 <- join(xx1, xx2,
  by=c("sat.moyr", "latband"),
  type="left", match="all")
  
aa3 <- join(aa2, xx3,
  by=c("sat.moyr", "latband"),
  type="left", match="all") 
  
aa4 <- join(aa3, xx4,
  by=c("sat.moyr", "latband"),
  type="left", match="all")

aa5 <- join(aa4, xx5,
  by=c("sat.moyr", "latband"),
  type="left", match="all")
  
colnames(aa5) <- c("mon", "lat",
  "res.mean","res.median","res.sd","res.mad","res.n")

rm(xx1,xx2,xx3,xx4,xx5,aa2,aa3,aa4); gc()  


# --- SST latband1 statistics for Linear Regression approach

xx1b <- aggregate((SST.latband1.res + 0.17) ~ sat.moyr + latband,
  data=orig, subset = use, FUN=mean, na.action=na.omit, simplify=TRUE)

xx2b <- aggregate((SST.latband1.res + 0.17) ~ sat.moyr + latband,
  data=orig, subset = use, FUN=median, na.action=na.omit, simplify=TRUE)

xx3b <- aggregate((SST.latband1.res + 0.17) ~ sat.moyr + latband,
  data=orig, subset = use, FUN=sd, na.action=na.omit, simplify=TRUE)

xx4b <- aggregate((SST.latband1.res + 0.17) ~ sat.moyr + latband,
  data=orig, subset = use, FUN=mad, na.action=na.omit, simplify=TRUE)

xx5b <- aggregate((SST.latband1.res + 0.17) ~ sat.moyr + latband,
  data=orig, subset = use, FUN=length, na.action=na.omit, simplify=TRUE)

# --- Join all data frames with statistics
  
aa2b <- join(xx1b, xx2b, by=c("sat.moyr", "latband"),
  type="left", match="all")
  
aa3b <- join(aa2b, xx3b, by=c("sat.moyr", "latband"),
  type="left", match="all") 
  
aa4b <- join(aa3b, xx4b, by=c("sat.moyr", "latband"),
  type="left", match="all")

aa5b <- join(aa4b, xx5b, by=c("sat.moyr", "latband"),
  type="left", match="all")
  
colnames(aa5b) <- c("mon", "lat",
  "res.mean.lr","res.median.lr","res.sd.lr","res.mad.lr","res.n.lr")

rm(xx1b,xx2b,xx3b,xx4b,xx5b,aa2b,aa3b,aa4b); gc()  

# --- Build object with differences...

uu1 <- join(aa5, aa5b, by=c("mon","lat"), type="full")

uu2 <- data.frame(mon=uu1$mon, lat=uu1$lat,
  mean.diff=uu1$res.mean - uu1$res.mean.lr,
  median.diff=uu1$res.median - uu1$res.median.lr,
  sd.diff=uu1$res.sd - uu1$res.sd.lr,
  mad.diff=uu1$res.mad - uu1$res.mad.lr)


# --- Build intervals for mean and median of residuals, and color palette
  
#bb1 <- range(pretty(c(aa5$res.mean, aa5$res.median), n=8))
#bb2 <- round(seq(from=bb1[1], to=bb1[2], 0.050), 2)

bb2 <- seq(from=-0.20, to=0.20, by=0.050)
bb3 <- brewer.pal(length(bb2[bb2 < 0])-1, "Blues")
bb3 <- bb3[length(bb3):1]
bb4 <- brewer.pal(length(bb2[bb2 > 0])-1, "Reds")
bb5 <- c(bb3,"lightgoldenrodyellow", "lightgoldenrodyellow", bb4)  

rm(bb1,bb3,bb4)

plot.diagnostics <- TRUE
  
if (plot.diagnostics) {
    
  # --- Hovmoller of mean of SST residuals
  
  outfile <- "HM_mean_resids_GAM_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
      width = 640, height = 640, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  levelplot(res.mean ~ mon * lat,
    data=aa5,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Mean of SST GAM residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    
  
  dev.off()
  
  outfile <- "HM_mean_resids_LR_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
      width = 640, height = 640, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  levelplot(res.mean.lr ~ mon * lat,
    data=aa5b,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Mean of SST LR latband1 residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    
  
  dev.off()
  
  # --- Hovmoller of median of SST residuals
  
  outfile <- "HM_median_resids_GAM_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
    width = 640, height = 640, units = "px", pointsize = 12,
    bg = "white", res = NA)
    
  levelplot(res.median ~ mon * lat,
    data=aa5,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Median of SST GAM residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    
  
  dev.off()
  
  outfile <- "HM_median_resids_LR_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
    width = 640, height = 640, units = "px", pointsize = 12,
    bg = "white", res = NA)
    
  levelplot(res.median.lr ~ mon * lat,
    data=aa5b,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Median of SST LR latband1 residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    
  
  dev.off()
  
}

# --- Build intervals for sdev and mad of residuals, and color palette
  
bb1 <- range(pretty(c(aa5$res.sd, aa5$res.mad), n=8))
if (bb1[length(bb1)] > 1.0) {
  bb2 <- c(0.0, seq(0.3, 1.0, 0.1), bb1[length(bb1)])
} else {
  bb2 <- c(0.0, seq(0.3, 1.0, 0.1))
}
bb5 <- brewer.pal(length(bb2)-1, "PuRd")  

if (plot.diagnostics) {
    
  # --- Hovmoller of standard deviation of SST residuals
  
  outfile <- "HM_sd_resids_GAM_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
      width = 640, height = 640, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  levelplot(res.sd ~ mon * lat,
    data=aa5,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Std. Dev. of SST GAM residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    

  dev.off()
  
  outfile <- "HM_sd_resids_LR_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
      width = 640, height = 640, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  levelplot(res.sd.lr ~ mon * lat,
    data=aa5b,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Std. Dev. of SST LR latband1 residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    

  dev.off()
  
  # --- Hovmoller of MAD of SST residuals
  
  outfile <- "HM_mad_resids_GAM_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
    width = 640, height = 640, units = "px", pointsize = 12,
    bg = "white", res = NA)
  
  levelplot(res.mad ~ mon * lat,
    data=aa5,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "MAD of SST GAM residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    
  
  dev.off()
  
  outfile <- "HM_mad_resids_LR_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
    width = 640, height = 640, units = "px", pointsize = 12,
    bg = "white", res = NA)
  
  levelplot(res.mad.lr ~ mon * lat,
    data=aa5b,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "MAD of SST LR latband1 residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    
  
  dev.off()

}
  
# --- Build intervals for N of residuals, and color palette
  
bb2 <- round(quantile(aa5$res.n, probs=seq(0, 1, 0.1)),0)
bb5 <- c("grey80", brewer.pal(9, "RdPu"))

if (plot.diagnostics) {    
  
  # --- Hovmoller of number of SST residuals
  
  outfile <- "HM_number_resids_GAM_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
      width = 640, height = 640, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  levelplot(res.n ~ mon * lat,
    data=aa5,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Number of SST GAM residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    
  
  dev.off()
  
  outfile <- "HM_number_resids_LR_SST"
  outfile <- paste(graph.outdir, outfile,".png", sep="")  
  png(filename = outfile,
      width = 640, height = 640, units = "px", pointsize = 12,
      bg = "white", res = NA)
  
  levelplot(res.n ~ mon * lat,
    data=aa5b,
    pretty=TRUE, at=bb2,          
    xlab="Month", ylab="Latitude band",
    main= paste(sensor, "Number of SST latband1 residuals"),
    ylim=c(0.5, 6.5),
    col.regions=bb5,
    scales = list(x= list(cex=0.7, rot=0, at=seq(1,length(seq.months),12),
      labels=seq.months[seq(1,length(seq.months),12)]),
      y = list(cex=0.8, rot=90, at=seq(1,6,1),
      labels=levels(orig$latband))),
    panel= function(x, y,...) {
      panel.levelplot(x,y,...)
      panel.abline(h=0.5:5.5,  col="white", lwd=1)},
    colorkey=list(space="right", col=bb5, at=bb2,
    labels=list(at=bb2, cex=0.8), height=0.8, width=2))    
  
  dev.off()
  
}  
# ---------------------------------------------------------------------------------------- 

# ----------------------------------------------------------------------------------------  
# --- Boxplot of SST latband1 residuals by month/year

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics
  
min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")
seq.months <- format(seq.months, "%b-%Y")
  
rm(min.date, max.date)
  
# --- We use only matchups that can be used for coefficient estimation
# --- and those which passed the cloud tree
# --- and have quality 0
  
#use <- orig$use.4.coeffs.SST &
#  orig$SST.tree.class == "P-Good" &
#  orig$qsst.new == 0

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.gam.res + 0.17) < 2.0

# --- Boxplot of ALL usable SST residuals by month/year
  
outfile <- "Boxplot_resids_GAM_SST"
outfile <- paste(graph.outdir, outfile,".png", sep="")  
png(filename = outfile,
  width = 640, height = 640, units = "px", pointsize = 12,
  bg = "white", res = NA)
  
bwplot(SST.latband1.gam.res ~ sat.moyr,
  data=orig,
  subset=use,
  ylim=c(-0.8, 0.6),
  ylab="SST residuals",
  xlab="Time",
  main=paste(sensor," - SST GAM residuals"),
  panel = function(x,y,...) { 
    panel.bwplot(x,y,...)
    panel.abline(h=c(-0.27,-0.17,-0.07), col="tomato", lwd=2, lty=c(2,1,2)) },
  scales=list(x=list(at=seq(1,length(seq.months),12),
    cex=0.8, labels=seq.months[seq(1,length(seq.months),12)])))

dev.off()

outfile <- "Boxplot_resids_LR_SST"
outfile <- paste(graph.outdir, outfile,".png", sep="")  
png(filename = outfile,
  width = 640, height = 640, units = "px", pointsize = 12,
  bg = "white", res = NA)

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.res + 0.17) < 2.0

bwplot(SST.latband1.res ~ sat.moyr,
  data=orig,
  subset=use,
  ylim=c(-0.8, 0.6),
  ylab="SST residuals",
  xlab="Time",
  main=paste(sensor," - SST LR latband1 residuals"),
  panel = function(x,y,...) { 
    panel.bwplot(x,y,...)
    panel.abline(h=c(-0.27,-0.17,-0.07), col="tomato", lwd=2, lty=c(2,1,2)) },
  scales=list(x=list(at=seq(1,length(seq.months),12),
    cex=0.8, labels=seq.months[seq(1,length(seq.months),12)])))

dev.off()






  
# --- Boxplot of ALL usable SST residuals by month/year and latitude band
  
outfile <- "Boxplot_latband_resids_GAM__SST"
outfile <- paste(graph.outdir, outfile,".png", sep="")  
png(filename = outfile,
  width = 640, height = 800, units = "px", pointsize = 12,
  bg = "white", res = NA)

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.gam.res + 0.17) < 2.0

bwplot(SST.latband1.gam.res ~ sat.moyr | latband,
  data=orig,
  subset=use,
  ylim=c(-0.8,0.6),
  ylab="SST residuals",
  xlab="Time",
  main=paste(sensor," - SST GAM residuals"),
  panel = function(x,y,...) { 
    panel.bwplot(x,y,...)
    panel.abline(h=-0.17, col="tomato", lwd=1) },
  scales=list(x=list(at=seq(1,length(seq.months),12),
    cex=0.8, labels=seq.months[seq(1,length(seq.months),12)])),
  layout=c(1,6))

dev.off()

outfile <- "Boxplot_latband_resids_GAM__SST"
outfile <- paste(graph.outdir, outfile,".png", sep="")  
png(filename = outfile,
  width = 640, height = 800, units = "px", pointsize = 12,
  bg = "white", res = NA)

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.res + 0.17) < 2.0

bwplot(SST.latband1.res ~ sat.moyr | latband,
  data=orig,
  subset=use,
  ylim=c(-0.8,0.6),
  ylab="SST residuals",
  xlab="Time",
  main=paste(sensor," - SST LR residuals"),
  panel = function(x,y,...) { 
    panel.bwplot(x,y,...)
    panel.abline(h=-0.17, col="tomato", lwd=1) },
  scales=list(x=list(at=seq(1,length(seq.months),12),
    cex=0.8, labels=seq.months[seq(1,length(seq.months),12)])),
  layout=c(1,6))

dev.off()
# ----------------------------------------------------------------------------------------  



# ----------------------------------------------------------------------------------------
# --- Plot of SST residuals as a function of BT31 - BT32 and buoy SST

library(hexbin)

#use.sst <- "GAM"
use.sst <- "LR"

if (use.sst == "GAM"){
  tt0 <- orig$SST.latband1.gam.res + 0.17
} else if (use.sst == "LR") {
  tt0 <- orig$SST.latband1.res + 0.17
}

use <- orig$use.4.coeffs.SST & abs(tt0) < 1.5

ddd <- data.frame(resids.gam = tt0[use],
  buoy.sst = orig$buoy.sst[use],
  BTdiff = (orig$cen.11000[use] - orig$cen.12000[use]),
  satz = orig$satz[use])

dd2 <- hexbin(x=ddd$buoy.sst, y=ddd$BTdiff, xbins = 40, shape = 1, IDs=TRUE)

# Add cell numbers to data frame "ddd"

ddd.b <- data.frame(ddd, cell=dd2@cID)

# Compute centroids of hexagonal bins, add coordinates to data frame

dd3 <- hcell2xy(dd2)
dd4 <- data.frame(cell=dd2@cell, x.coord=dd3$x, y.coord=dd3$y)

# Calculate median, MAD and number of residuals in each hexagonal bin

dd5 <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=median, na.rm=TRUE, simplify=TRUE)
dd5b <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=mad, na.rm=TRUE, simplify=TRUE)
dd5c <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=length, simplify=TRUE)

dd6 <- data.frame(cell=as.numeric(names(dd5)), med.res=dd5, mad.res=dd5b, n=dd5c, dd4)

# Eliminate bins with less than 5 matchups

dd7 <- subset(dd6, subset = n > 30)

rm(tt0,ddd,dd2,ddd.b,dd3,dd4,dd5,dd5b,dd5c,dd6); gc()
# ----------------------------------------------------------------------------------------  

# ----------------------------------------------------------------------------------------  
# --- Plot median of SST residuals as a function of Buoy SST and BT31 - BT32 

# Define intervals for median of SST residuals

tt1 <- c(-2,-0.3,-0.10, 0.10, 0.3, 2)
tt2 <- cut(dd7$med.res, breaks=tt1)

# Add factor with interval of residuals' median

dd8 <- data.frame(dd7, med.q=as.numeric(tt2))

# Plot residuals in (BT31 - BT32) vs. buoy SST space

tt3 <- rev(brewer.pal(5, "RdBu"))

plot(dd8$x.coord, dd8$y.coord,
  pch=21, bg=tt3[dd8$med.q], cex=1.4,
  col="grey80",
  ylim=c(-0.6, 3.2),
  xlab="Buoy SST",
  ylab="BT31 - BT32",
  main=paste(sensor, "- Median of SST",use.sst,"residuals"))

legend("topleft", fill=tt3, legend=levels(tt2))

rm(tt1,tt2,tt3); gc()
# ----------------------------------------------------------------------------------------  

# ----------------------------------------------------------------------------------------  
# --- Plot MAD of SST residuals

tt4 <- c(0.0, 0.3, 0.4, 0.5, 0.7, 1.5)
tt5 <- cut(dd7$mad.res, breaks=tt4)

dd9 <- data.frame(dd8, mad.q=as.numeric(tt5))

tt6 <- (brewer.pal(5, "YlGn"))

plot(dd9$x.coord, dd9$y.coord,
  pch=21, bg=tt6[dd9$mad.q], cex=1.3,
  col="grey80",
  xlab="Buoy SST",
  ylab="BT31 - BT32",
  main=paste(sensor, "- MAD of SST GAM residuals"))

legend("topleft", fill=tt6, legend=levels(tt5))

rm(tt4,tt5,tt6); gc()
# ----------------------------------------------------------------------------------------  


# ----------------------------------------------------------------------------------------
# --- Plot of SST residuals as a function of BT31 - BT32 and buoy SST

library(hexbin)

#use.sst <- "GAM"
use.sst <- "LR"

if (use.sst == "GAM"){
  tt0 <- orig$SST.latband1.gam.res + 0.17
} else if (use.sst == "LR") {
  tt0 <- orig$SST.latband1.res + 0.17
}

use <- orig$use.4.coeffs.SST & abs(tt0) < 1.5

ddd <- data.frame(resids.gam = tt0[use],
  buoy.sst = orig$buoy.sst[use],
  BTdiff = (orig$cen.11000[use] - orig$cen.12000[use]),
  satz = orig$satz[use])

dd2 <- hexbin(x=ddd$satz, y=ddd$BTdiff, xbins = 40, shape = 1, IDs=TRUE)

# Add cell numbers to data frame "ddd"

ddd.b <- data.frame(ddd, cell=dd2@cID)

# Compute centroids of hexagonal bins, add coordinates to data frame

dd3 <- hcell2xy(dd2)
dd4 <- data.frame(cell=dd2@cell, x.coord=dd3$x, y.coord=dd3$y)

# Calculate median, MAD and number of residuals in each hexagonal bin

dd5 <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=median, na.rm=TRUE, simplify=TRUE)
dd5b <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=mad, na.rm=TRUE, simplify=TRUE)
dd5c <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=length, simplify=TRUE)

dd6 <- data.frame(cell=as.numeric(names(dd5)), med.res=dd5, mad.res=dd5b, n=dd5c, dd4)

# Eliminate bins with less than 5 matchups

dd7 <- subset(dd6, subset = n > 30)

rm(ddd,dd2,ddd.b,dd3,dd4,dd5,dd5b,dd5c,dd6); gc()
# ----------------------------------------------------------------------------------------  

# ----------------------------------------------------------------------------------------  
# --- Plot median of SST residuals as a function of satz and BT31 - BT32 

# Define intervals for median of SST residuals

tt1 <- c(-2.0, -0.3, -0.1, 0.1, 0.3, 2.0)
tt2 <- cut(dd7$med.res, breaks=tt1)

# Add factor with interval of residuals' median

dd8 <- data.frame(dd7, med.q=as.numeric(tt2))

# Plot residuals in satellitye zenith angle vs (BT31 - BT32) space

tt3 <- rev(brewer.pal(5, "RdBu"))

plot(dd8$x.coord, dd8$y.coord,
  pch=21, bg=tt3[dd8$med.q], cex=1.2,
  col="grey70",
  xlab="Satellite zenith angle",
  ylab="BT31 - BT32",
  ylim=c(-0.5, 5),
  main=paste(sensor, "- Median of SST",use.sst,"residuals"))

legend("topleft", fill=tt3, legend=levels(tt2))
# ----------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------
# --- Plot of SST residuals as a function of buoy SST and satellite zenith angle

library(hexbin)

use.sst <- "GAM"
#use.sst <- "LR"

if (use.sst == "GAM"){
  tt0 <- orig$SST.latband1.gam.res + 0.17
} else if (use.sst == "LR") {
  tt0 <- orig$SST.latband1.res + 0.17
}

use <- orig$use.4.coeffs.SST & abs(tt0) < 1.5

ddd <- data.frame(resids.gam = tt0[use],
  buoy.sst = orig$buoy.sst[use],
  BTdiff = (orig$cen.11000[use] - orig$cen.12000[use]),
  satz = orig$satz[use])

dd2 <- hexbin(x=ddd$buoy.sst, y=ddd$satz, xbins = 40, shape = 1, IDs=TRUE)

# Add cell numbers to data frame "ddd"

ddd.b <- data.frame(ddd, cell=dd2@cID)

# Compute centroids of hexagonal bins, add coordinates to data frame

dd3 <- hcell2xy(dd2)
dd4 <- data.frame(cell=dd2@cell, x.coord=dd3$x, y.coord=dd3$y)

# Calculate median, MAD and number of residuals in each hexagonal bin

dd5 <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=median, na.rm=TRUE, simplify=TRUE)
dd5b <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=mad, na.rm=TRUE, simplify=TRUE)
dd5c <- tapply(X=ddd.b$resids.gam, INDEX=ddd.b$cell, FUN=length, simplify=TRUE)

dd6 <- data.frame(cell=as.numeric(names(dd5)), med.res=dd5, mad.res=dd5b, n=dd5c, dd4)

# Eliminate bins with less than 5 matchups

dd7 <- subset(dd6, subset = n > 30)

rm(ddd,dd2,ddd.b,dd3,dd4,dd5,dd5b,dd5c,dd6); gc()
# ----------------------------------------------------------------------------------------  

# ----------------------------------------------------------------------------------------  
# --- Plot median of SST residuals as a function of satz and BT31 - BT32 

# Define intervals for median of SST residuals

tt1 <- c(-2.0, -0.3, -0.1, 0.1, 0.3, 2.0)
tt2 <- cut(dd7$med.res, breaks=tt1)

# Add factor with interval of residuals' median

dd8 <- data.frame(dd7, med.q=as.numeric(tt2))

# Plot residuals in satellitye zenith angle vs (BT31 - BT32) space

tt3 <- rev(brewer.pal(5, "RdBu"))

plot(dd8$x.coord, dd8$y.coord,
  pch=21, bg=tt3[dd8$med.q], cex=1.4,
  col="grey70",
  xlab="Buoy SST",
  ylab="Satellite zenith angle",
  main=paste(sensor, "- Median of SST",use.sst,"residuals"))

legend("center", fill=tt3, legend=levels(tt2))
# ----------------------------------------------------------------------------------------


































plot(dd8$x.coord, dd8$y.coord, pch=16)





tt1 <- as.matrix(data.frame(dd8$x.coord, dd8$y.coord))


tt2 <- ahull(tt1, alpha=1.2)

plot.ahull(tt1)


# ----------------------------------------------------------------------------------------  

# ----------------------------------------------------------------------------------------
# --- Adjust loess fit to the binned medians of SST residuals

tt1 <- loess(med.res ~ x.coord + y.coord, data=dd7, span=0.1, degree=1)

tt2 <- range(pretty(dd7$x.coord, n = 20))
tt3 <- range(pretty(dd7$y.coord, n = 20))

sst.coords <- seq(from=tt2[1], to=tt2[2], by=0.5)
BTdiff.coords <- seq(from=tt3[1], to=tt3[2], by=0.1)
n.x <- length(sst.coords)
n.y <- length(BTdiff.coords)

tt4 <- data.frame(expand.grid(sst.coords, BTdiff.coords))
colnames(tt4) <- c("x.coord", "y.coord")

tt5 <- predict(tt1, newdata=tt4)

tt6 <- data.frame(tt4, med.res=tt5)


mat.dat <- round(matrix(tt5, nrow=n.x, ncol=n.y, byrow=FALSE), 3)

tt3 <- rev(brewer.pal(5, "RdBu"))


image(sst.coords, BTdiff.coords, mat.dat, col=heat.colors(20))

contour(sst.coords, BTdiff.coords, mat.dat, add=TRUE,
  levels=seq(-0.3, 0.3, 0.1))


tt3 <- rev(brewer.pal(5, "RdBu"))

plot(dd8$x.coord, dd8$y.coord,
  pch=21, bg=tt3[dd8$med.q], cex=1.3,
  col="grey80",
  xlab="Buoy SST",
  ylab="BT31 - BT32",
  main=paste(sensor, "- Median of SST GAM residuals"))























# ----------------------------------------------------------------------------------------  
  
use <- orig$use.4.coeffs.SST &
  orig$SST.tree.class == "P-Good" &
  orig$qsst.new == 0

xx <- orig$cen.11000[use] - orig$cen.12000[use]
yy <- abs(orig$satz[use])

xx2 <- pretty(xx, n=200)
yy2 <- pretty(yy, n=200)

grid <- expand.grid(x=xx2, y=yy2)

#library(MASS)
#zz <- kde2d(xx, yy, n=100)

library(sm)
zz2 <- sm.density(cbind(xx,yy), eval.grid=FALSE, eval.points=grid)

res <- data.frame(x=zz2$eval.points[,1],
  y=zz2$eval.points[,2],
  z=zz2$estimate)

tt2 <- pretty(res$z, n=10)

tt3 <- brewer.pal(9, "YlOrRd")

levelplot(z ~ x * y, data=res,
  xlab="BT31 - BT32",
  ylab="Satellite zenith angle",
  at=tt2,
  col.regions=tt3) 


# ------------------------

use <- orig$use.4.coeffs.SST &
  orig$SST.tree.class == "P-Good" &
  orig$qsst.new == 0

xx <- orig$cen.11000[use] - orig$cen.12000[use]
yy <- abs(orig$satz[use])
zz <- orig$SST.latband1.gam.res[use]

grid <- data.frame(x=xx, y=yy, z=zz)

fit <- loess(z ~ x + y, data=grid, span=0.25)

xx2 <- pretty(xx, n=200)
yy2 <- pretty(yy, n=200)

grid2 <- expand.grid(x=xx2, y=yy2)

pred <- predict(fit, newdata=grid2)

dim(pred$fitted)


image(pred)



# ---------------------------------------------------------------------------------------#
# --- Clean up all objects EXCEPT those with names ----
# --- equal to "orig" or starting with string "AQUA" or "TERRA

tt1 <- objects()
tt2a <- str_detect(tt1, "^orig$")
tt2b <- str_detect(tt1, "^AQUA")
tt2c <- str_detect(tt1, "^TERRA")
tt3 <- tt2a | tt2b | tt2c
tt4 <- tt1[!tt3]
rm(list=tt4)


rm(tt1,tt2a,tt2b,tt2c,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------



