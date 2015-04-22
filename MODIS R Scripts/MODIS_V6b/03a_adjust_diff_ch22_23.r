

# ---------------------------------------------------------------------------------------#
# --- Fit a new "reference difference" between MODIS channels 22 and 23 ----
# --- A reference curve had been fit earlier in the MODIS project,
# --- but it needs to be reestimated now that we have more matchups.

# --- First, we select matchups that pass some tests.
# --- The tests are similar to those that define use for coefficient estimation, but
# --- satellite zenith angles are extended to -60 and +60 degrees.

pass.filters2 <- orig$solz > 90 &
  abs(orig$satz) <= 60 &
  abs(orig$buoy.lat) < 60 &
  abs(orig$SST4.latband1.prelim.res) <= 2

table(pass.filters2, useNA="always")
  
# --- Extract records that pass the tests; only some variables are needed

orig2 <- subset(orig, subset=pass.filters2,
  select=c(cen.3959, cen.4050, satz, cen.39.40.ref))  

gg1 <- orig2$cen.3959 - orig2$cen.4050    # BT channel 22 minus BT channel 23 

# --- Adjust a line to the difference between channels 22 and 23
# --- as a function of satellite zenith angle.
# --- Fit is made for satellite zenith angles between -60 and +60 degrees.

gg4 <- loess(gg1 ~ orig2$satz, span = 0.35)								# Fit trend in dif as f(zenith angle)
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))								# predict value for regular sequence
gg6 <- data.frame(sza=seq(-60.0, 60.0, 0.5), d2223=gg5)		# make data frame
gg6 <- na.omit(gg6)                                       # Omit rows with NAs

# --- Now that we have a fit, calculate the standardized difference for all matchups.
# --- The standardized difference is the observed (BT22 - BT23) value minus
# --- the (BT22 - BT23) value fitted for the corresponding satellite zenith angle.

gg7 <-  predict(gg4, newdata = orig$satz)  	# fitted difference for ALL matchups

# --- Now calculate the standardized difference between
# --- the actual BT22 - BT23 minus the fitted "reference" line...

d2223ref.new <- (orig$cen.3959 - orig$cen.4050) - gg7

# --- Add variable created above to "orig" big data frame.

orig <- data.frame(orig, cen.39.40.ref.new=d2223ref.new)

# --- Finally, Sue needs fitted differences for zenith angles up to -65 and +65 degrees.
# --- Loess cannot extrapolate (only satz values <= 60 degrees are retained here), so we use
# --- splines to extrapolate out to 65 degrees.

gg6b <- smooth.spline(gg6[,1], gg6[,2])
gg6c <- predict(gg6b, seq(-65,65,0.5))
gg6d <- data.frame(sza=gg6c$x, d.22.23=gg6c$y)

# --- Write out the values of the fitted difference to a text file

out.data.file <- paste(results.outdir, sensor, "_d2223_fit.txt", sep="")

write.table(gg6d, file=out.data.file, append=FALSE,
  sep=" ", na="NA", row.names=FALSE, col.names=TRUE, quote=TRUE)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot the previous and newly fitted associations between BT22-BT23 and SZA ----

# --- Reference curve ORIGINALLY used in l2gen (data from Sue, original fit by Ed Kearns). 

uu1 <- seq(-52, 52, 1)

uu2 <- c(4.44, 4.22, 4.02, 3.97, 3.90, 3.73, 3.55, 3.44, 3.35, 3.29,
	3.18, 3.09, 3.03, 2.99, 2.93, 2.88, 2.81, 2.77, 2.74, 2.70,
	2.67, 2.64, 2.58, 2.56, 2.54, 2.51, 2.46, 2.46, 2.43, 2.41,
    2.38, 2.35, 2.35, 2.33, 2.32, 2.29, 2.27, 2.25, 2.24, 2.26,
    2.24, 2.21, 2.20, 2.21, 2.19, 2.19, 2.18, 2.17, 2.17, 2.16,
    2.17, 2.15, 2.16, 2.16, 2.15, 2.16, 2.16, 2.16, 2.16, 2.16,
    2.17, 2.15, 2.14, 2.14, 2.17, 2.21, 2.20, 2.19, 2.19, 2.19,
    2.18, 2.21, 2.26, 2.30, 2.31, 2.30, 2.34, 2.32, 2.35, 2.40,
    2.42, 2.45, 2.42, 2.50, 2.55, 2.58, 2.65, 2.71, 2.77, 2.82,
    2.89, 2.96, 3.02, 3.10, 3.16, 3.22, 3.31, 3.38, 3.45, 3.55,
    3.66, 3.80, 3.94, 4.08, 4.30)

# --- Plot  BT22 -BT23 differences
# --- Add lines for previous and new fit to BT22-BT23 differences.

smoothScatter(orig$satz[orig$use.4.coeffs.SST4], 
  orig$cen.3959[orig$use.4.coeffs.SST4] - orig$cen.4050[orig$use.4.coeffs.SST4],
  nbin=60,
  xlim=c(-60,60),
  xlab="Satellite Zenith Angle",
  ylab="BT22 - BT23",
  main=paste("MODIS ",sensor,"\n Standardized BT22 - BT23"))

lines(gg6[,1], gg6[,2], lwd=4, col="violet")  # New fit
lines(uu1, uu2, lwd=3, col="orange")          # Old fit

tt1 <- seq(from=-60, to=60, by=1)
tt2 <- (1/(cos(tt1 * ((2 * pi) / 360)))) - 1 + 2.15
lines(tt1,tt2, lwd=2, col="grey70")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot observed and standardized BT22 - BT23 differences ----

smoothScatter(orig$satz[orig$use.4.coeffs.SST4], 
  orig$cen.3959[orig$use.4.coeffs.SST4] - orig$cen.4050[orig$use.4.coeffs.SST4],
  nbin=60,
  xlim=c(-60,60),
  xlab="Satellite Zenith Angle",
  ylab="BT22 - BT23",
  main=paste("MODIS ",sensor,"\n Original BT22 - BT23"))

smoothScatter(orig$satz[orig$use.4.coeffs.SST4], 
  orig$cen.39.40.ref.new[orig$use.4.coeffs.SST4],
  nbin=60,
  xlim=c(-60,60),
  xlab="Satellite Zenith Angle",
  ylab="BT22 - BT23",
  main=paste("MODIS ",sensor,"\n Standardized BT22 - BT23"))
abline(h=0, col="grey80", lwd=3)

rm(pass.filters2,orig2,gg1,gg4,gg5,gg6,gg7,d2223ref.new)
rm(gg6b,gg6c,gg6c); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Explore presence of an association with SZA in BT31-BT32 ----
# --- i.e., the temperature difference between channels 31 and 32.

pass.filters2 <- orig$solz > 90 &
  abs(orig$satz) <= 60 &
  abs(orig$buoy.lat) < 60 &
  abs(orig$cen.sst4 - orig$buoy.sst) <= 2

table(pass.filters2, useNA="always")

# --- Extract records that pass the tests; only some variables are needed

orig2 <- subset(orig, subset=pass.filters2, select=c(cen.11000, cen.12000, satz))  

# --- Adjust a line to the difference between channels
# --- as a function of satellite zenith angle.
# --- Fit is made with satellite zenith angles between -60 and +60 degrees.

gg4 <- loess((orig2$cen.11000 - orig2$cen.12000) ~ orig2$satz, span=0.35)   # Fit trend
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))								# predict value for regular sequence
gg6 <- data.frame(sza=seq(-60.0, 60.0, 0.5), d3132=gg5)		# make data frame

smoothScatter(orig2$satz, (orig2$cen.11000 - orig2$cen.12000),
  xlim=c(-60,60),
  xlab="Satellite Zenith Angle",
  ylab="BT31 - BT32",
  main=paste("MODIS ",sensor,"\n Original BT31 - BT32"))

lines(gg6[,1], gg6[,2], lwd=4, col="tomato")  # New fit

# --- We check the range of fitted BT31 - BT32 as a function of satellite zenith angle
# --- INTERESTING: The fits are extremely similar for Aqua and TERRA

range(gg6[,2], na.rm=TRUE)
# 0.8931074 1.1451559 Aqua

rm(pass.filters2, orig2, gg1,gg2,gg4,gg5,gg6,gg6b,gg6c,gg6d,gg7);gc()
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


