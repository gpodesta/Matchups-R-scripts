# ---------------------------------------------------------------------------------------#
# --- BEGIN PRELIMINARY SECTION ON CHECKING COLLECTION 6 CALIBRATION ----
# --- FOR LWIR CHANNELS (used to compute SST) IN MODIS TERRA.
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Load required packages ----

if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(maps)) {install.packages("maps"); library(maps)}
if (!require(maptools)) {install.packages("maptools"); library(maptools)}
if (!require(robust)) {install.packages("robust"); library(robust)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
if (!require(sp)) {install.packages("sp"); library(sp)}
if (!require(raster)) {install.packages("raster"); library(raster)}
if (!require(xts)) {install.packages("xts"); library(xts)}
if (!require(xtsExtra)) {install.packages("xtsExtra"); library(xtsExtra)}
if (!require(zoo)) {install.packages("zoo"); library(zoo)}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define sensor and calibration collection on which we will be working ----

sensor <- "AQUA"      	# Select MODIS onboard AQUA or TERRA
#sensor <- "TERRA"			# Select MODIS onboard AQUA or TERRA

collection <- 6				  # Calibration collection

cat("\n Working on MODIS onboard",sensor,"collection",collection,"...\n");
# ----------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------#
# --- Define geophysical variable worked on (ie, SST or SST4), algorithm type, etc. ----
# --- and other relevant quantities.

geophys.var <- "SST"							# Geophysical variable
sst.algo <- "latband1"						# Type of SST algorithm
algo.coeffs.version <- "6.3"			# Version of algorithm coefficients

# -- Define matchups version and input format (old or new)

matchup.version  <- paste("collection_",collection, sep="")	  # Version of matchups

matchup.format <- "OLD"
#matchup.format <- "NEW"
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Check if object "orig" exists ----
# --- This object contains the dataframe with matchup records.
# --- If this object exists, we do not ned to read matchups again.

if (exists("orig")) {
  cat("Object orig exists...\n") 
} else {
  stop("ERROR: Object orig DOES NOT exist\n") 
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Find out what operating system we are using ----

op.sys <- .Platform$OS.type    										# Get operating system

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


# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- 1. Explore temporal discontinuities in time series of SST residuals ----
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# --- Compute median of SST residuals by month and year.

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date)

# --- Compute monthly medians and number of SST residuals
# --- We use only matchups that can be used for coefficient estimation

use <- orig$use.4.coeffs.SST

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.prelim.res[use],
  INDEX=tt0[use],
  FUN=median, na.rm=TRUE, simplify=TRUE)      # Calculate median by month

tt2 <- tapply(X=orig$SST.latband1.prelim.res[use],
  INDEX=tt0[use],
  FUN=length, simplify=TRUE)                  # Calculate N by month

# --- Assemble median and N of SST residuals in a data frame
# --- and create an XTS time series

tt3 <- data.frame(med=tt1, num=tt2)
tt4 <- xts(tt3, order.by=seq.months)

plot.zoo(tt4,
  type="l", lwd=3, col=c("tomato","orange"),
  main=paste(sensor,"- SST Residuals"),
  xlab="Time",
  ylab=c("Median","N"))

rm(use,tt0,tt1,tt2,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# --- APPARENTLY, FOR MODIS AQUA THERE IS NO NEED TO CORRECT
# --- FOR TEMPORAL EFFECTS on SST RESIDUALS.

# --- MODIS TERRA DOES NOT SHOW TEMPORAL PATTERNS EITHER.
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------#
# ----------------------------------------------------------------------------------------
# --- 2. Explore effects of satellite zenith angle on SST residuals ----
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# --- Adjust a line to the preliminary SST residuals as a function of satellite zenith angle.
# --- Fit is made for satellite zenith angles between -60 and +60 degrees.

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res) < 2.0

gg4 <- loess(orig$SST.latband1.prelim.res[use] ~ orig$satz[use], span=0.35)  # Fit trend
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))								# predict value for regular sequence
gg6 <- data.frame(sza=seq(-60.0, 60.0, 0.5), SSTres=gg5)	# make data frame

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res) < 1.5

gg9a <- loess(orig$SST.latband1.prelim.res[use] ~ orig$satz[use], span=0.35)  # Fit trend
gg9b <- predict(gg9a, seq(-60.0, 60.0, 0.5))      					# predict value for regular sequence
gg9c <- data.frame(sza=seq(-60.0, 60.0, 0.5), SSTres=gg9b)	# make data frame

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res) < 1.0

gg8a <- loess(orig$SST.latband1.prelim.res[use] ~ orig$satz[use], span=0.35)  # Fit trend
gg8b <- predict(gg8a, seq(-60.0, 60.0, 0.5))    						# predict value for regular sequence
gg8c <- data.frame(sza=seq(-60.0, 60.0, 0.5), SSTres=gg8b)	# make data frame

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res) < 0.5

gg7a <- loess(orig$SST.latband1.prelim.res[use] ~ orig$satz[use], span=0.35)  # Fit trend
gg7b <- predict(gg7a, seq(-60.0, 60.0, 0.5))    						# predict value for regular sequence
gg7c <- data.frame(sza=seq(-60.0, 60.0, 0.5), SSTres=gg7b)	# make data frame

tt1 <- brewer.pal(4,"Dark2")

plot(gg6$sza, gg6$SSTres, type="l",
  ylim=c(-0.4, 0.0),
  main=paste(sensor,"- SST Residuals vs. scan angle"),
  xlab="Satellite zenith angle",
  ylab="SST residuals",
  lwd=3, col=tt1[4])
lines(gg9c$sza, gg9c$SSTres, lwd=3, col=tt1[3])
lines(gg8c$sza, gg8c$SSTres, lwd=3, col=tt1[2])
lines(gg7c$sza, gg7c$SSTres, lwd=3, col=tt1[1])

legend("bottomright",
  legend = c("< 2.0", "< 1.5", "< 1.0","< 0.5"),
  lty=1, lwd=3,
  col=tt1[4:1],
  xjust = 1, yjust = 1,
  title = "abs(SST residuals)")

rm(use, gg4,gg5,gg6,gg7a,gg7b,gg7c,gg8a,gg8b,gg8c,gg9a,gg9b,gg9c); gc()


# --- Boxplots of SST residuals as a function of satellite zenith angle

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res) < 2.0

tt2 <- cut(orig$satz[use], breaks=seq(-60,60,10), include.lowest=TRUE)

boxplot(split(orig$SST.latband1.prelim.res[use], tt2),
  main=paste(sensor,"SST Residuals"),
  xlab="Satellite zenith angle intervals",
  ylab="SST residuals",
  ylim=c(-0.8,0.8), par(cex.lab=1.0, cex.axis=0.7, las=2))
abline(h=-0.17, col="tomato", lwd=2)

rm(tt2, use); gc()

# --- Does mirror side influence SZA effects on SST residuals?

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res) < 2.0 & orig$mirror == 1

gg4 <- loess(orig$SST.latband1.prelim.res[use] ~ orig$satz[use], span=0.35)
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))  							# predict value for regular sequence
gg6 <- data.frame(sza=seq(-60.0, 60.0, 0.5), SSTres=gg5)	# make data frame

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res) < 2.0 & orig$mirror == 2

gg4b <- loess(orig$SST.latband1.prelim.res[use] ~ orig$satz[use], span=0.35)
gg5b <- predict(gg4, seq(-60.0, 60.0, 0.5))    						# predict value for regular sequence
gg6b <- data.frame(sza=seq(-60.0, 60.0, 0.5), SSTres=gg5)	# make data frame

tt1 <- brewer.pal(4,"Dark2")

plot(gg6$sza, gg6$SSTres, type="l",
  ylim=c(-0.4, 0.0),
  main=paste(sensor,"- SST Residuals vs. scan angle\n Mirror side 1"),
  xlab="Satellite zenith angle",
  ylab="SST residuals",
  lwd=3, col=tt1[1])
lines(gg6b$sza, gg6b$SSTres+0.01, lwd=3, col=tt1[2])
abline(h=-0.17, col="tomato", lwd=2)

legend("bottomright",
  legend = c("1", "2"),
  lty=1, lwd=3,
  col=tt1[1:2],
  xjust = 1, yjust = 1,
  title = "Mirror side")

# --- Does latitude band influence SZA effects on SST residuals?

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res) < 2.0

xyplot(orig$SST.latband1.prelim.res[use] ~ orig$satz[use] | orig$latband[use],
  ylim=c(-0.30,0.00), xlab="Satellite Zenith Angle",
  ylab="SST Residuals (latband)",
  panel=function(x, y) {
  panel.xyplot(x,y, pch=".", col="lightsteelblue",  cex=0.3)
  panel.loess(x,y,span=0.4, lwd=3, col="tomato")
  panel.abline(h=-0.17, col="grey40")})

rm(use,gg4,gg5,gg6,gg4b,gg5b,gg6b,tt1); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ----------------------------------------------------------------------------------------
# --- 3. Explore effects of mirror side on SST residuals ----
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date)

# --- Compute monthly medians and number of SST4 residuals
# --- We use only matchups that can be used for coefficient estimation

use <- orig$use.4.coeffs.SST & abs(orig$SST.latband1.prelim.res + 0.17) < 2

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.prelim.res[use],
  INDEX=list(tt0[use], orig$mirror[use]),
  FUN=median, na.rm=TRUE, simplify=TRUE)      # Calculate median by month

tt2 <- xts(tt1, order.by=seq.months)

tt3 <- apply(tt2, MARGIN=2, FUN=median, na.rm=TRUE)

# --- Plot monthly median of SST residuals for each mirror side

plot.zoo(tt2[-1,],
  plot.type="single",
  type="l", lwd=2, col=c("tomato","steelblue"),
  ylim=range(pretty(coredata(tt2[-1,]))),
  main=paste(sensor,"- SST Residuals by mirror side"),
  xlab="Time",
  ylab=c("Median of SST residuals"))

abline(h=tt3, col=c("tomato","steelblue"))

legend("bottomright", legend = c("Mirror side 1", "Mirror side 2"),
  lty=1, lwd=3,col=c("tomato","steelblue"), xjust = 1, yjust = 1, cex=0.8)


cor(coredata(tt2[ ,1]), coredata(tt2[ ,2]), use="complete.obs")

# --- Plot time series of differences between residuals
# --- for mirror sides 2 minus 1

plot.zoo(tt2[-1,2] - tt2[-1,1],
  type="h",col=c("orange"),lwd=2,
  main=paste(sensor,"- SST resids diff between mirrors"),
  xlab="Time",
  ylab=c("SST residuals mirror side 2 minus 1"))
abline(h=c(0, median(tt2[ ,2] - tt2[ ,1], na.rm=TRUE)), col="grey30")

median(tt2[ ,1], na.rm=TRUE) # -0.2322514
median(tt2[ ,2], na.rm=TRUE) # -0.1239988

summary(tt2[-1, 2] - tt2[-1, 1])  #Median :  0.11187  Mean   : 0.10817 

rm(use, tt0, tt1, tt2); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save "orig" data frame into a data frame for the sensor in question... ----

if (sensor == "TERRA") {
  TERRA <- orig	
} else if (sensor == "AQUA") {
  AQUA <- orig
}	
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Write a text file with processed data frame ----
# --- Includes all variables for a sensor.

# --- First, convert buoy and satellite dates/times to character vectors
# --- so that they are written out surrounded by quotes and thus can be read in
# --- as a single string.

orig.write <- orig     # Create bogus object "orig.write" that will be written out

orig.write$buoy.timedate <- as.character(orig.write$buoy.timedate)
orig.write$sat.timedate <- as.character(orig.write$sat.timedate)

out.data.file <- paste(results.outdir, sensor,"_", matchup.version,"_results.txt", sep="")

write.table(orig.write, file=out.data.file, append=FALSE,
  sep=" ", na="NA", row.names=FALSE, col.names=TRUE, quote=TRUE)

# --- Also write out a text file using dput() that
# --- can be easily regenerated

out.data.file2 <- paste(results.outdir, sensor,"_", matchup.version,
  "_dputoutput.txt", sep="")

dput(orig, file=out.data.file2)

# uuu <- dget(out.data.file2)

rm(out.data.file, out.data.file2); gc()
# ----------------------------------------------------------------------------------------

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



# ---------------------------------------------------------------------------------------#
# --- Do this section only for Terra matchups - Correction for mirror side effects ----
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

if (sensor == "TERRA") {
  
  SST.latband1 <- rep(NA, times=length(orig$SST.latband1))
  SST.latband1.prelim.res <- rep(NA, times=length(orig$SST.latband1.prelim.res))
  
  # --- A preliminary correction for mirror side effects for TERRA.
  # --- There is a median difference of about 0.112 degrees in mirror 2 minus mirror 1
  # --- residuals. We split the difference between both mirror sides.
      
  SST.latband1 <- ifelse(orig$mirror == 2,
    orig$SST.latband1.prelim - 0.056,
    orig$SST.latband1.prelim + 0.056)
  
  SST.latband1.prelim.res <- ifelse (orig$mirror == 2,
    orig$SST.latband1.prelim.res - 0.056,
    orig$SST.latband1.prelim.res + 0.056)

  # --- Put SST values corrected for mirror side effects in "orig"
  
  orig$SST.latband1 <- SST.latband1
  orig$SST.latband1.prelim.res <- SST.latband1.prelim.res
  
  rm(SST.latband1, SST.latband1.prelim.res); gc()
  
} # End of preliminary SST corrections for mirror side -- TERRA only
# ----------------------------------------------------------------------------------------








