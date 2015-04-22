# ---------------------------------------------------------------------------------------#
# --- BEGIN PRELIMINARY SECTION ON CHECKING COLLECTION 6 CALIBRATION ----
# --- FOR SWIR CHANNELS (used to compute SST4) IN MODIS TERRA.

# --- NOTE: This is a preliminary section to be used for a quick exploration of
# --- calibration issues in MODIS-TERRA Collection 5 that resulted in a systematic trend
# --- in SST4 residuals between 2000 and early 2003.
# --- For these analyses we will use preliminary SST4 coefficients using matchups
# --- for 2003 and 2004.
# --- No training or validation sets will be defined for this initial analyses.
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Load required packages ----

if (!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
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

geophys.var <- "SST4"							# Geophysical variable
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
# ----------------------------------------------------------------------------------------
# --- 1. Explore temporal discontinuities in time series of SST4 residuals ----
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Compute median of preliminary SST4 residuals by month and year ----

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date)
  
# --- Compute monthly medians and number of SST4 residuals
# --- We use only matchups that can be used for coefficient estimation
  
use <- orig$use.4.coeffs.SST4
  
tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months
  
tt1 <- tapply(X=orig$SST4.latband1.prelim.res[use],
 INDEX=tt0[use],
  FUN=median, na.rm=TRUE, simplify=TRUE)      # Calculate median by month
  
tt2 <- tapply(X=orig$SST4.latband1.prelim.res[use],
  INDEX=tt0[use],
  FUN=length, simplify=TRUE)                  # Calculate N by month

# --- Assemble median and N of SST4 residuals in a data frame
# --- and create an XTS time series

tt3 <- data.frame(med=tt1, num=tt2)
tt4 <- xts(tt3, order.by=seq.months)

plot.zoo(tt4,
  type="l", lwd=3, col=c("tomato","orange"),
  main=paste(sensor,"- SST4 Residuals"),
  xlab="Time", las=1, cex.axis=0.7,
  ylab=c("Median","N"))

library(xtsExtra)
plot.xts(tt4$med,
  type="o",
  pch=16, col="steelblue", lwd=1,
  ylab="SST4 residuals",
  yax.loc="left",
  main=paste0(sensor," - SST4 Residuals"),
  auto.grid=FALSE, minor.ticks=FALSE,
  major.ticks="years", major.format = "%b\n%Y",
  events=list(time = c("2000-10-30", "2001-07-02"),
    label = c("Oct-00","Jul-01")))

# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- APPARENTLY, FOR MODIS AQUA THERE IS NO NEED TO CORRECT
# --- FOR TEMPORAL EFFECTS on SST4 RESIDUALS.
# --- INSTEAD, TERRA NEEDS TO BE FIXED
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- Do this section only for Terra matchups ----
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- Define epochs for anomalies in MODIS TERRA SST4 residuals.

# --- Data extracted from MCST site: http://mcst.gsfc.nasa.gov/index.php?section=15 .
# --- The MODIS instrument began taking data using side - A electronics.
# --- MODIS consistently produced data starting on 24 February 2000 except for a few periods.
# --- Science data was processed using B - side electronics between
# --- October 30, 2000 (data day 2000304) and June 15, 2001 (day 2001166). 
# --- MODIS-TERRA experienced a Power Supply 2 (PS2 = electronics side - B) shutdown anomaly
# --- and did not take science data during June 16, 2001 (day 2001166)
# --- to July 2, 2001 (day 2001183). 
# --- When MODIS recovered, it was commanded to take science mode data
# --- using Power Supply 1 and electronics side A. 

tt1a <- as.Date(floor_date(min(orig$sat.timedate), unit="month"))
tt1b <- as.Date(floor_date(ymd("2000-10-30"), unit="day"))
tt1c <- as.Date(floor_date(ymd("2001-07-02"), unit="day"))
tt1d <- as.Date(ceiling_date(max(orig$sat.timedate), unit="month"))
tt1 <- c(tt1a,tt1b,tt1c,tt1d)

rm(tt1a,tt1b,tt1c,tt1d); gc()

tt2 <- cut(as.Date(orig$buoy.timedate), breaks=tt1, include.lowest=FALSE,
  labels=c("P1","P2","P3"))

table(tt2, useNA="always")

tt3 <- tapply(X=orig$SST4.latband1.prelim.res[orig$use.4.coeffs.SST4],
  INDEX=tt2[orig$use.4.coeffs.SST4],
  FUN=median, simplify=TRUE)           

round(tt3, 4)

# --- Plot of time series of SST4 residuals, including the levels for each epoch

plot.zoo(tt4$med,
  type="l", lwd=2, col="steelblue",
  ylim=c(-0.75, 0),
  main=paste(sensor,"- SST4 Residuals"),
  xlab="Time", las=1, cex.axis=0.7,
  ylab="Median of SST4 Residuals")
points(tt4$med, pch=16, col="tomato")
abline(h=tt3, col="grey50")


# --- These values can be used to correct SST4 values, as the residuals seem to be
# --- fairly stable around the offset value for each epoch.    
# --- The difference between the median after July 2001 and the previous
# --- two epochs is about 0.44  and 0.25 degC, respectively. This value needs to be ADDED to
# --- SST4 values prior to 2001-07-01 in order to correct them. 
# --- *** NOTE The following code is only run for Terra:

if (sensor == "TERRA") {

  # --- The median of SST residuals for each epoch identified is shown below.
  #       P1         P2         P3 
  # -0.6174036 -0.4258075 -0.1723426 

  correct1 <- tt3["P3"] - tt3["P1"]     # 0.445061
  correct2 <- tt3["P3"] - tt3["P2"]     # 0.253465
  
  SST4.latband1.prelim <- rep(NA, times=length(orig$SST4.latband1.prelim))
  SST4.latband1.prelim.res <- rep(NA, times=length(orig$SST4.latband1.prelim.res))

  # --- Correct for period 1: Feb 2000 to 30 October 2000
  uu1 <- which(tt2 == "P1")
  SST4.latband1.prelim[uu1] <- orig$SST4.latband1.prelim[uu1] + correct1
  SST4.latband1.prelim.res[uu1] <- orig$SST4.latband1.prelim.res[uu1] + correct1

  # --- Correct for period 1: 30 October 2000 to 2 July 2001
  uu2 <- which(tt2 == "P2")
  SST4.latband1.prelim[uu2] <- orig$SST4.latband1.prelim[uu2] + correct2
  SST4.latband1.prelim.res[uu2] <- orig$SST4.latband1.prelim.res[uu2] + correct2
  
  # --- Correct for period 3: 2 July 2001 to end of record (no correction!)
  uu3 <- which(tt2 == "P3")
  SST4.latband1.prelim[uu3] <- orig$SST4.latband1.prelim[uu3] + 0.0
  SST4.latband1.prelim.res[uu3] <- orig$SST4.latband1.prelim.res[uu3] + 0.0
   
  # --- Put corrected SST4 values in "orig"
  
  orig$SST4.latband1.prelim <- SST4.latband1.prelim
  orig$SST4.latband1.prelim.res <- SST4.latband1.prelim.res

  rm(SST4.latband1.prelim, SST4.latband1.prelim.res, uu1,uu2,uu3); gc()

} # End of SST4 corrections for TERRA only


# --- Compute AGAIN monthly medians of SST4 residuals AFTER the correction.

use <- orig$use.4.coeffs.SST4

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST4.latband1.prelim.res[use],
  INDEX=tt0[use],
  FUN=median, na.rm=TRUE, simplify=TRUE)      # Calculate median by month

tt2 <- tapply(X=orig$SST4.latband1.prelim.res[use],
  INDEX=tt0[use],
  FUN=length, simplify=TRUE)                  # Calculate N by month

# --- Assemble median and N of SST4 residuals in a data frame
# --- and create an XTS time series

tt3 <- data.frame(med=tt1, num=tt2)
tt4 <- xts(tt3, order.by=seq.months)

plot.zoo(tt4$med,
  type="l", lwd=2, col="steelblue",
  ylim=c(-0.75, 0),
  main=paste(sensor,"- SST4 Residuals"),
  xlab="Time", las=1, cex.axis=0.7,
  ylab="Median of SST4 Residuals")
points(tt4$med, pch=16, col="tomato")
abline(h=median(tt4$med, na.rm=TRUE), col="grey50")

rm(use,tt0,tt1,tt2,tt3,tt4); gc()

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- End of section to be done ONLY for Terra matchups
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#


# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- 2. Explore effects of satellite zenith angle on SST4 residuals ----
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# --- Adjust a line to preliminary SST4 residuals as a function of satellite zenith angle.
# --- Fit is made for satellite zenith angles between -60 and +60 degrees.

use <- orig$use.4.coeffs.SST4 & abs(orig$SST4.latband1.prelim.res + 0.17) < 2.0

gg4 <- loess(orig$SST4.latband1.prelim.res[use] ~ orig$satz[use], span=0.35)  # Fit trend
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))								# predict value for regular sequence
gg6 <- data.frame(sza=seq(-60.0, 60.0, 0.5), SST4res=gg5)	# make data frame

plot(gg6$sza, gg6$SST4res, type="l",
  xlab="Satellite zenith angle",
  ylab="SST4 residuals",
  lwd=3, col="tomato")

rm(use,gg4,gg5,gg6); gc()
# ----------------------------------------------------------------------------------------




# ---------------------------------------------------------------------------------------#
# ----------------------------------------------------------------------------------------
# --- 3. Explore effects of mirror side on SST4 residuals ----
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date)

# --- Compute monthly medians and number of SST4 residuals
# --- We use only matchups that can be used for coefficient estimation

use <- orig$use.4.coeffs.SST4

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST4.latband1.prelim.res[use],
  INDEX=list(tt0[use], orig$mirror[use]),
  FUN=median, na.rm=TRUE, simplify=TRUE)      # Calculate median by month

tt2 <- xts(tt1, order.by=seq.months)

# --- Plot monthly median of SST4 residuals for each mirror side

plot.zoo(tt2,
  plot.type="single",
  type="l", lwd=2, col=c("tomato","steelblue"),
  ylim=range(pretty(coredata(tt2))),
  main=paste(sensor,"- SST4 Residuals by mirror side"),
  xlab="Time",
  ylab=c("Median of SST4 residuals"))
abline(h=-0.17, col="grey50")

legend("topleft", legend = c("side 1", "side 2"),
  lty=1, lwd=3,col=c("tomato","steelblue"), xjust = 1, yjust = 1,
  title = "Mirror")

cor(coredata(tt2[,1]), coredata(tt2[,2]), use="complete.obs")
cor(coredata(tt2[,1]), coredata(tt2[-(1:12),2]), use="complete.obs")

# --- Plot time series of differences between residuals
# --- for mirror sides 2 minus 1

plot.zoo(tt2[,2] - tt2[,1],
  type="h",col=c("tomato"),lwd=2,
  main=paste(sensor,"- SST4 Residuals - mirror difference"),
  xlab="Time",
  ylab=c("SST4 residuals mirror side 2 minus 1"))

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
tt2a <- str_detect(tt1, "^orig")
tt2b <- str_detect(tt1, "^AQUA")
tt2c <- str_detect(tt1, "^TERRA")
tt3 <- tt2a | tt2b | tt2c
tt4 <- tt1[!tt3]
rm(list=tt4)

rm(tt1,tt2a,tt2b,tt2c,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------


