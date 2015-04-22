# ---------------------------------------------------------------------------------------#
# --- Script to estimate "latband1" SST4 algorithm coefficients
# --- for MODIS matchups.
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Install required packages ----

if (!require(dataframe)) {install.packages("dataframe"); library(dataframe)}
if (!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(maps)) {install.packages("maps"); library(maps)}
if (!require(maptools)) {install.packages("maptools"); library(maptools)}
if (!require(robust)) {install.packages("robust"); library(robust)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
if (!require(sp)) {install.packages("sp"); library(sp)}
if (!require(raster)) {install.packages("raster"); library(raster)}
if (!require(caret)) {install.packages("caret"); library(caret)}
if (!require(mgcv)) {install.packages("mgcv"); library(mgcv)}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define sensor and calibration collection on which we will be working ----

sensor <- "AQUA"      	# Select MODIS onboard AQUA or TERRA
#sensor <- "TERRA"			# Select MODIS onboard AQUA or TERRA

collection <- 6				  # Calibration collection

cat("\n Working on MODIS onboard",sensor,"collection",collection,"...\n")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define geophysical variable worked on (ie, SST or SST4), algorithm type, etc. ----
# --- and other relevant quantities.

geophys.var <- "SST4"							# Geophysical variable
sst4.algo <- "latband1"						# Type of SST4 algorithm

algo.coeffs.version <- "6.3"			# Version of algorithm coefficients

# -- Define matchups version and input format (old or new)

matchup.version  <- paste("collection_",collection, sep="")	  # Version of matchups

matchup.format <- "OLD"
#matchup.format <- "NEW"
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
# --- Check if object "orig" exists ----
# --- This object contains the dat aframe with matchup records

if (exists("orig")) {
  cat("Object orig exists...\n") 
} else {
  stop("ERROR:Object orig DOES NOT exist\n") 
}
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
# --- Implement secant function where input x is in degrees -----

library(circular)
secant.deg <- function(x) {1 / (cos(rad(x)))}

#test <- seq(-60, 60, 0.5)
#plot(test, secant.deg(test), type="l")
#plot(test, secant.deg(test) - 1, type="l")
#rm(test)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Reserve some of the data to test SST4 algorithm performance.----
# --- Note we are using ONLY those matchups selected for coefficient estimation,
# --- which among other things include only night data.
# --- That is, divide the "use.4.coeffs.SST4" matchups
# --- into "training" and "validation" sets.

# --- First, label all records that can be used for coeff estimation
# --- as "Validate", all others as "NotUsed"

orig$train.sst4 <- ifelse(orig$use.4.coeffs.SST4, "Validate", "NotUsed")

# --- Define the proportion of sample to be used for training
# --- and the number of training records.

orig.use <- which(orig$use.4.coeffs.SST4)  						# Indices of records used for coeff estimation
n.orig.use <- length(orig.use)												# No of records for coeff estimation
prop.training <- 0.65																	# Proportion of matchups in the training set
n.training <- round((n.orig.use * prop.training), 0)	# No. of matchups to be used for training

set.seed(128)			# Setting the seed allows us to reproduce results if desired...
ind.training <- sort(sample(orig.use, size=n.training, replace=FALSE)) # Indices of training records
orig$train.sst4[ind.training] <- "Train"		# Training set

table(orig$train.sst4, useNA="ifany")

rm(orig.use, n.orig.use, n.training, ind.training, prop.training)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Prepare objects related to zonal bands ----

latband.names <- levels(orig$latband)
n.latbands <- length(latband.names)
lat.boundaries <- c(-90, -40, -20, 0, 20, 40, 90) 	# Fixed boundaries for latitude bands

if (n.latbands != length(lat.boundaries) - 1)
	stop("ERROR: Check definition of latitude bands and their boundaries...\n")	

# --- Prepare object holding boundaries for each latitude band.
# --- It will be used when exporting coefficients file.

lat.boundaries2 <- matrix(data=NA, nrow=n.latbands, ncol=3)
for (i in 1:n.latbands) {
	lat.boundaries2[i,] <- c(i, lat.boundaries[i],lat.boundaries[i+1])
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create matrices to store results from SST4 coeff estimation ----

bb1 <- list(month.abb, latband.names)														# Names of x and y dimensions

mad <- matrix(NA, ncol=n.latbands, nrow=12)											# MAD of LTS residuals

coef1 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef2 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef3 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef4 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression

xnumber <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)		# No. of records
xsigma <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)		# Std. dev of residuals
xrsq <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# R-squared
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create objects to store GAM estimates of SST4 and SST4 residuals ----

SST4.latband1.prelim.gam <- rep(NA, times=nrow(orig))

SST4.latband1.prelim.gam.res <- rep(NA, times=nrow(orig))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Assemble a data frame with variables to be used in regressions ----
# --- DO NOT filter by training records at this point.

reg.input <- data.frame(
  bsst = orig$buoy.sst,
  ch3959 = orig$cen.3959,
  BTdiff = (orig$cen.3959 - orig$cen.4050),
  satz = orig$satz,
  solz = orig$solz,
  train.sst4 = orig$train.sst4,
  latband = orig$latband,
  mon = orig$sat.mon)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Iterate through the data to estimate SST4 algorithm coefficients ----

for (i in 1:12) {		# Begin looping through months of the year with matchups in them

###  i <- 8; j <- 5
 
	# --- First, select the 5 months that will be used
	# --- to estimate coefficients for each given month of the year

	if (i == 1) {
		use.months <- c(11:12,1:3)	# For January use Nov,Dec,Jan,Feb,Mar
	} else if (i == 2) {
		use.months <- c(12,1:4)		  # For February use Dec,Jan,Feb,Mar,Apr
	} else if (i == 3) {
		use.months <- 1:5           # For March use Jan-May
	} else if (i == 4) {
		use.months <- 2:6           # For April use Feb-Jun
	} else if (i == 5) {
		use.months <- 3:7           # For May, use Mar-Jul 
	} else if (i == 6) {
		use.months <- 4:8           # For June, use Apr-Aug
	} else if (i == 7) {
		use.months <- 5:9           # For July, use May-Sep
	} else if (i == 8) {
		use.months <- 6:10          # For August, use Jun-Oct
	} else if (i == 9) {
		use.months <- 7:11          # For September, use Jul-Nov
	} else if (i == 10) {
		use.months <- 8:12          # For October use Aug-Dec
	} else if (i == 11) {
		use.months <- c(9:12,1)     # For November use Sep,Oct,Nov,Dec,Jan
	} else if (i == 12) {
		use.months <- c(10:12,1:2)  # For December use Oct,Nov,Dec,Jan,Feb
	} else {
		stop("ERROR: Check month in satellite dates! \n")
	}

	# --- Start iterating over latitude bands

	for (j in 1:n.latbands) {     		# Begin looping through atmospheric regimes or latitude bands
    	
		cat("\nCoefficients being estimated for", month.abb[i], "...")
		cat("\nCoefficients being estimated for latitude band", latband.names[j], "...\n")
		
    # Select matchups for estimating coefficients
    # that are (a) in training set
    # and (b) within month window and latband being worked on.
    # *** Additionally, for TERRA we start in 2003, as previous records may
    # have errors in SWIR channel values (due to electronics problems).
   
    in.month.window <- reg.input$mon %in% use.months 			
		in.latband <- as.numeric(reg.input$latband) == j
    in.training.set <- reg.input$train.sst4 == "Train"
    after.2003 <- orig$sat.timedate >= ymd("2003-01-01", tz="GMT")
    
    if (sensor == "TERRA") {
      use.est <- in.month.window & in.latband & in.training.set & after.2003
    } else if (sensor == "AQUA") {
      use.est <- in.month.window & in.latband & in.training.set
    } else {
      cat("Sensor name is incorrect...\n")
    }
   
    # Select matchups to be used for calculation
    # that are the central month and latband being worked on
    
    in.month <- reg.input$mon == i
    use.calc <- in.month & in.latband

		if (any(use.est)) {	# Check if there are any matchups within month and latitude band

      # --- Build second data frame with variables
      # --- Select those training records within month window and latband.
      # --- NOTE: FOr SST4 the secant term is NOT multiplied by a channel difference
      # --- (as it is done for SST estimation).
            
      reg.input.2 <- data.frame(
        bsst = reg.input$bsst[use.est],
        ch3959 = reg.input$ch3959[use.est],
        BTdiff = reg.input$BTdiff[use.est],
        satz = reg.input$satz[use.est],
        secterm = secant.deg(reg.input$satz[use.est]) - 1,
        solz = reg.input$solz[use.est],
        mon = reg.input$mon[use.est],
        latband = reg.input$latband[use.est])
 
			# --- Define weights for each record
			# --- (to be used in weighted regression)
		
			# TODO: Decide if we use 0.6 or 0.5 for months 1 and 5 in window
		
		  wts <- c(0.5, 0.8, 1.0, 0.8, 0.5)
			xwt <- wts[match(reg.input.2$mon, use.months)]

			# --- Perform a first iteration using a robust regression
		
      xreg.rob.MM <- lmRob(bsst ~ ch3959 + BTdiff + secterm,
        data=reg.input.2,
  			weights=xwt,
				control = lmRob.control(tlo = 1e-8, tua = 1.5e-09,
				mxr = 500, mxf = 500, mxs = 1000))	
		
			# Define weights based on SST4 residuals and bisquare function
			# Robustness weights based on 6 * MAD (see "Visualizing Data", p. 118)

      bb2 <- median(abs(xreg.rob.MM$residuals))		# MAD of residuals
			bb3 <- xreg.rob.MM$residuals / (6.0 * bb2)  # residuals / (6 * MAD)
      bb4 <- ( 1 - (bb3^2) )^2										# bisquare
      xwt2 <- ifelse(abs(bb3) < 1, bb4, 0)      	# This is the bisquare weight
	
			xwtf <- xwt * xwt2		# Robustness and month weights
			rm(bb2,bb3,bb4)
		
			# plot((xreg.rob.MM$residuals), xwt2)	# MAD weights vs. residuals
			# plot((xreg.rob.MM$residuals), xwtf)		# Final weights vs. residuals
      # plot(xreg.rob.MM$fitted, x1); abline(0,1, col="tomato")

			# --- Now, do a standard regression using weights determined above.
			# --- The weights are the product of the robustness and time weights.
			# --- Only use records with weights above a certain threshold
			# --- (excluded using the subset option).
			
			weight.threshold <- 0.0  	# Do not use records with weights below this value
		
      xreg <- lmRob(bsst ~ ch3959 + BTdiff + secterm,
        data=reg.input.2,
        weights = xwtf,
      	subset=(xwtf > weight.threshold),
        na.action = na.omit)
      
      coef1[i, j] <- xreg$coef[1]			# Store SST4 coefficients
      coef2[i, j] <- xreg$coef[2]
      coef3[i, j] <- xreg$coef[3]
      coef4[i, j] <- xreg$coef[4]
      	
			xnumber[i,j] <- length(xwtf > weight.threshold)      	
      
			qqq <- summary(xreg)
      xrsq[i,j] <- qqq$r.squared			# Overall r-squared
      xsigma[i,j] <- qqq$sigma        # Std. error of residuals
				
			# plot(x1, xreg$resid, pch=16, col=3, main=paste("Month: ",i,"Lat band:",j))
			# abline(h=0)
      
      # --------------------------------------------------------------------#
  		# --- While still in the loop, fit a GAM with smooth terms
      # --- for BT difference and satellite zenith angle.
      # --------------------------------------------------------------------#
      
      xreg.gam <- gam(bsst ~ ch3959 + s(BTdiff) + s(satz),
        data = reg.input.2,
        weights = xwtf,
        method = "REML",
        subset= (xwtf > weight.threshold),
        na.action = na.omit)
         
      summary(xreg.gam$residuals)
    
      predvals <- predict(xreg.gam, newdata=reg.input[use.calc,], type="response")      
      predvals.2 <- predict(xreg.gam, newdata=reg.input[use.calc,], type="terms")

      # Store estimated GAM SST4 values and SST4 residuals.
      # Remove 0.17 deg C to compute "pseudo-skin" SST4

      SST4.latband1.prelim.gam[which(use.calc)] <- (predvals - 0.17)
      SST4.latband1.prelim.gam.res[which(use.calc)] <- (predvals - 0.17) -
          orig$buoy.sst[which(use.calc)]
      
      # plot(reg.input[use.calc, "satz"], predvals.2[ ,3], type="p", pch=16)
      # plot(reg.input[use.calc, "BTdiff"], predvals.2[ ,2], type="p")
      		
      
		}	# End of test for availability of matchups within month and latitude band 		
	}		# End of looping through latitude bands
}			# End of looping through months
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save GAM SST4 estimates in object "orig" ----

nite <- orig$solz > 90

orig$SST4.latband1.prelim.gam <- ifelse(nite, SST4.latband1.prelim.gam, NA)
  
orig$SST4.latband1.prelim.gam.res <- ifelse(nite, SST4.latband1.prelim.gam.res, NA)

rm(nite); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create a data frame with prelim SST4 coeffs for each month and latitude band.----
# --- **** NOTE: We subtract 0.17 deg C from the constant (coeff1)
# --- **** so we effectively compute a "skin" SST.

mm1 <- rep(seq(1,length(month.abb),1), each=n.latbands)    
mm2 <- rep(seq(1,n.latbands,1), times=length(month.abb))   
mm3 <- as.vector(t(coef1)) - 0.17		# *** Here we apply the "skin" correction *** #
mm4 <- as.vector(t(coef2))
mm5 <- as.vector(t(coef3))
mm6 <- as.vector(t(coef4))
mm7 <- as.vector(t(xnumber))

coeffs.SST4.latband1.df <- data.frame(mon=mm1, lat=mm2,
	coeff1=mm3, coeff2=mm4, coeff3=mm5, coeff4=mm6, N=mm7) 
	
rm(mm1,mm2,mm3,mm4,mm5,mm6,mm7)
rm(xreg, xsigma, xwt, xwt2, xwtf, mad, xnumber,xrsq)
rm(wts,x1,x2,x3,x4,x5,x6)
rm(bb1,coef1,coef2,coef3,coef4, weight.threshold,xreg.rob.MM)
rm(in.latband, in.month, qqq, use,i,j); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Write a text file with SST4 latband1 algorithm coefficients ----
# --- to be used in calculation of global SST fields
# --- NOTE that the constant (coef1) already has the skin temperature correction incorporated.

mm1 <- rep(sensor, times=(n.latbands * length(month.abb)))
mm2 <- rep(seq(1,length(month.abb),1), each=n.latbands)
mm3 <- rep(seq(1,n.latbands,1), times=length(month.abb)) 
mm4 <- lat.boundaries2[match(mm3, lat.boundaries2[,1]), 2]
mm5 <- lat.boundaries2[match(mm3, lat.boundaries2[,1]), 3]

coeffs.export.frame <- data.frame(sat=mm1, mon=mm2,
	bot.lat=mm4, top.lat=mm5,
	coeffs.SST4.latband1.df[ ,-c(1,2)])

outfile <- paste(coeff.outdir, sensor,
	"_", matchup.version,"_",geophys.var,
	"_",sst4.algo,
	"_coeffs_v",algo.coeffs.version,
	"_prelim1.txt", sep="")

# --- Write a brief header for the coefficient file

cat("# Filename:", outfile,"\n", file=outfile, append=FALSE)
cat("# Coefficients estimated:",
  format(now(),"%Y-%m-%d %H:%M:%S\n"),
  file=outfile, append=TRUE)
cat("# Algorithm version 6.3 PRELIMINARY 1\n",file=outfile, append=TRUE)
cat("# Model: bsst ~ ch3959 + btdiff + sec(satz)\n",file=outfile, append=TRUE)

write.table(coeffs.export.frame,
  file=outfile, append=TRUE,
	col.names=FALSE, row.names=FALSE, sep=" ")

# --- Write out object "coeffs.SST4.latband1.df"
# --- to a text file that can be easily reconstructed

outfile2 <- paste(coeff.outdir, sensor,
	"_", matchup.version,"_",geophys.var,
	"_",sst4.algo,
	"_coeffs_v",algo.coeffs.version,
	"_dput_prelim1.txt", sep="")

dput(coeffs.SST4.latband1.df, file=outfile2)

rm(coeffs.export.frame)
rm(mm1,mm2,mm3,mm4,mm5,outfile, outfile2)
rm(lat.boundaries, lat.boundaries2,n.latbands)
rm(latband,BT3959,BTdiff,satz,bsst,mon); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- NOW THAT COEFFICIENTS HAVE BEEN ESTIMATED, COMPUTE LATBAND1 SST4s ----
# --- FOR ALL MATCHUPS.

# --- Fetch the SST4 coefficients corresponding to each month and latitude band
# --- Note: Many of the steps in this section are necessary because
# --- function merge() seems to get rows out of order, and satellite
# --- quantities and coefficients do not align.

orig2 <- orig 	# Create new working object "orig2"

# --- Add numeric values for mon and latband to "orig2"

orig2 <- data.frame(orig2, mon=orig2$sat.mon, lat=as.numeric(orig2$latband))

# --- Merge the orig2 data frame with the coefficients data frame
# --- Note we are using the join() command in the plyr library.
# --- Unlike merge(), join() does not change the order of the first data.frame to be merged.

library(plyr)
aa2 <- join(orig2, coeffs.SST4.latband1.df, by=c("mon","lat"), type="left", match="all")

# --- Verify that we have a single coefficient value for each month/latband combination

ttt <- tapply(aa2$coeff1, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value poer month and latband")

ttt <- tapply(aa2$coeff2, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value poer month and latband")

ttt <- tapply(aa2$coeff3, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value poer month and latband")

ttt <- tapply(aa2$coeff4, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value poer month and latband")

rm(ttt); gc()

# --- Extract vectors of coefficients

coef1 <- aa2$coeff1
coef2 <- aa2$coeff2
coef3 <- aa2$coeff3
coef4 <- aa2$coeff4

# --- Extract the independent variables used to compute SST4

x1 <- aa2$buoy.sst  									# Buoy SST (not used here for calculation)
x2 <- aa2$cen.3959  									# BT22 (brightness temperature for MODIS channel 22)
x3 <- aa2$cen.3959 - aa2$cen.4050  		# BT22 - BT23 (NOT multiplied by buoy SST)
x4 <- abs(aa2$satz)  									# Satellite zenith angle
x6 <- (secant.deg(x4) - 1)            # Secant term (NOT multiplied by BT diff)

# --- Compute "latband1" SST4s and SST4 residuals for ALL the data.

SST4.latband1.prelim <- coef1 + (coef2 * x2) + (coef3 * x3) + (coef4 * x6)

# --- Compute "latband1" SST4 residuals as (satellite SST4 minus buoy SST).
# --- NOTE: Be careful to use object "aa2" to subtract buoy SST,
# --- otherwise records may be misaligned with "orig".

SST4.latband1.prelim.res <- SST4.latband1.prelim - aa2$buoy.sst		# SST4 residuals

# --- Add computed latband1 SST4 and residuals to data frame "aa2"

aa2 <- data.frame(aa2,
  SST4.latband1.prelim = SST4.latband1.prelim,
	SST4.latband1.prelim.res = SST4.latband1.prelim.res)

# --- SST4 values are good only at night (solar zenith angle > 90)
# --- Set non-night SST4 and SST4 residual values to NA

aa2$SST4.latband1.prelim <- ifelse(aa2$solz > 90, aa2$SST4.latband1.prelim, NA)
aa2$SST4.latband1.prelim.res <- ifelse(aa2$solz > 90, aa2$SST4.latband1.prelim.res, NA)

# --- Eliminate unwanted columns from object "aa2"

tt1 <- names(aa2)
tt2 <- c("mon","lat","coeff1","coeff2","coeff3","coeff4","N") 	# Columns to eliminate
tt3 <- which(!(tt1 %in% tt2))		# Numbers of columns in aa2 to KEEP in "orig"

# --- Save wanted columns in NEW object "orig"

orig <- aa2[,tt3]		# Original object with SST latband1 and residuals added

rm(x1,x2,x3,x4,x5,x6,coef1,coef2,coef3,coef4,orig2,aa2,tt1,tt2,tt3); gc()
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

orig.write <- orig   	# Create bogus object "orig.write" that will be written out

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

rm(out.data.file, out.data.file2, orig.write); gc()
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
# --- Statistics for preliminary SST4 latband1 residuals in TRAINING set

rms <- function(x) {
	x <- x[!is.na(x)]
	x2 <- x * x
	result <- sqrt( sum(x2) / length(x) )
	return(result)
}

tt0 <- orig$SST4.latband1.prelim.res

if (sensor == "TERRA") {
  use <- orig$use.4.coeffs.SST4 &
    orig$sat.timedate >= ymd("2003-01-01", tz="GMT") & abs(tt0) < 2.0
  table(use, useNA="always")
} else if (sensor == "AQUA") {
  use <- orig$train.sst4 == "Train" & abs(tt0) < 2.0
  table(use, useNA="always")
} else {
  cat("Sensor name is incorrect...\n")
}

tt1 <- round(min(tt0[use], na.rm=TRUE), 3)
tt2 <- round(quantile(tt0[use], probs=0.25, na.rm=TRUE), 3)
tt3 <- round(median(tt0[use], na.rm=TRUE), 3)
tt4 <- round(mean(tt0[use], na.rm=TRUE), 3)
tt5 <- round(quantile(tt0[use], probs=0.75, na.rm=TRUE), 3)
tt6 <- round(max(tt0[use], na.rm=TRUE), 3)
tt7 <- round(rms(tt0[use]), 3)
tt8 <- round(sd(tt0[use], na.rm=TRUE), 3)
tt9 <- round(mad(tt0[use], na.rm=TRUE), 3)
tt10 <- length(tt0[use]) 

tt11 <- data.frame(min=tt1, q1=tt2, median=tt3, mean=tt4,
	q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10, row.names=NULL)



tt0 <- orig$SST4.latband1.prelim.gam.res

if (sensor == "TERRA") {
  use <- orig$use.4.coeffs.SST4 &
    orig$sat.timedate >= ymd("2003-01-01", tz="GMT") &
    abs(tt0) < 2.0
  table(use, useNA="always")
} else if (sensor == "AQUA") {
  use <- orig$train.sst4 == "Train" & abs(tt0) < 2.0
  table(use, useNA="always")
} else {
  cat("Sensor name is incorrect...\n")
}

tt1 <- round(min(tt0[use], na.rm=TRUE), 3)
tt2 <- round(quantile(tt0[use], probs=0.25, na.rm=TRUE), 3)
tt3 <- round(median(tt0[use], na.rm=TRUE), 3)
tt4 <- round(mean(tt0[use], na.rm=TRUE), 3)
tt5 <- round(quantile(tt0[use], probs=0.75, na.rm=TRUE), 3)
tt6 <- round(max(tt0[use], na.rm=TRUE), 3)
tt7 <- round(rms(tt0[use]), 3)
tt8 <- round(sd(tt0[use], na.rm=TRUE), 3)
tt9 <- round(mad(tt0[use], na.rm=TRUE), 3)
tt10 <- length(tt0[use]) 

tt11 <- data.frame(min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10, row.names=NULL)

rm(use,tt0,tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,tt11,rms); gc()
# ----------------------------------------------------------------------------------------


