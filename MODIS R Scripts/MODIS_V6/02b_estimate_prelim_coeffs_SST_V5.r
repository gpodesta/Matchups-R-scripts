# ---------------------------------------------------------------------------------------#
# --- Script to estimate Versionr 5 SST algorithm coefficients
# --- for MODIS Aqua or Terra matchups.
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Install required packages ----

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

sensor <- "AQUA"     # Select MODIS onboard AQUA or TERRA
#sensor <- "TERRA"			# Select MODIS onboard AQUA or TERRA

collection <- 6				# Calibration collection

cat("\n Working on MODIS onboard",sensor,"collection",collection,"...\n");
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define geophysical variable worked on (ie, SST or SST4), algorithm type, etc. ----
# --- and other relevant quantities.

geophys.var <- "SST"				  # Geophysical variable
sst.algo <- "V5"						  # Type of SST algorithm

algo.coeffs.version <- "6.3"	# Version of algorithm coefficients

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
# --- Implement secant function where input x is in degrees -----

library(circular)
secant.deg <- function(x) {1 / (cos(rad(x)))}

#test <- seq(-60, 60, 0.5)
#plot(test, secant.deg(test), type="l")
#plot(test, secant.deg(test) - 1, type="l")
#rm(test)
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
# --- Use training set to estimate SST V5 coefficients ----

# --- To estimate SST coefficients for V5 algorithm we use the training and validation sets
# --- defined earlier (when fitting "latband" (or V6) coefficients).
# --- Make sure that field "train.sst" exists in data.frame "orig".

tt1 <- names(orig)  # names of fields in object "orig"

if (!("train.sst" %in% tt1))
  stop("Field train.sst is not present in data frame orig...\n")

rm(tt1); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define boundaries of overlapping 5-month windows ----

# --- First go to the beginning of the month where the first date occurs,
# --- then go back two other months.
# --- Also, go forward three months (to form the upper boundary of a 5-month window).
# --- Operations in this section use package "lubridate".

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- ceiling_date(max(orig$sat.timedate, na.rm=TRUE), "month")

tt1 <- seq(from=min.date, to=max.date, by="months")

# --- Create object with the beginning date for the month in the center of the time window

window.centers.beg <- tt1[-length(tt1)]						# Eliminate last value (> max date) 

beg.of.window <- window.centers.beg + months(-2)	# Go back two months from beginning
end.of.window <- window.centers.beg + months(3)		# Go forward 3 months from end
n.of.windows <- length(beg.of.window)							# no. of windows for which coeffs can be estimated	

window.bounds <- data.frame(per=seq(1, n.of.windows, 1),
  beg=beg.of.window, end=end.of.window)

n.periods <- nrow(window.bounds)

rm(min.date, max.date, beg.of.window, end.of.window, tt1); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create matrices to store results from SST V5 coefficient estimation ----

n.BTdiff.regimes <- length(levels(orig$BTdiff.regime))		# Number of BTdiff regimes

coef1 <- matrix(data=NA, ncol=n.BTdiff.regimes, nrow=n.of.windows)  # Coeffs from weighted regression
coef2 <- matrix(data=NA, ncol=n.BTdiff.regimes, nrow=n.of.windows)  # Coeffs from weighted regression
coef3 <- matrix(data=NA, ncol=n.BTdiff.regimes, nrow=n.of.windows)  # Coeffs from weighted regression
coef4 <- matrix(data=NA, ncol=n.BTdiff.regimes, nrow=n.of.windows)  # Coeffs from weighted regression

xnumber <- matrix(data=NA, n.BTdiff.regimes, nrow=n.of.windows)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Assemble a data frame with variables to be used in regressions ----
# --- DO NOT filter by training records at this point.

reg.input <- data.frame(bsst = orig$buoy.sst,
  ch11000 = orig$cen.11000,
  BTdiff = (orig$cen.11000 - orig$cen.12000),
  satz = orig$satz,
  train = orig$train.sst,
  BTdiff.regime = orig$BTdiff.regime,
  sat.time = orig$sat.timedate)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Start iterating to estimate SST V5 algorithm coefficients ----

for (i in 1:n.of.windows) {			    # Begin looping through time windows    
	for (j in 1:n.BTdiff.regimes) {   # Begin looping through BTdiff regimes
  
		#   i <- 100; j <- 2
		
		cat("Estimating coefficients for period",i,"of",n.of.windows,"...\n") 
   	cat("Estimating coefficients for BTdiff regime",j,"...\n") 

		# -----------------------------------------------------------------------------------#
		# --- Define if records fall within time window and BTdiff regime being worked on,
    # --- and whether they are in th etraining set.
		# --- We use the satellite time to check if record is inside a time window.
		# -----------------------------------------------------------------------------------#
    
    in.training.set <- reg.input$train == "Train"
    
    in.period <- (reg.input$sat.time >= window.bounds$beg[i]) &
			(reg.input$sat.time < window.bounds$end[i])

		in.BTdiff.regime <- (reg.input$BTdiff.regime == levels(orig$BTdiff.regime)[j]) 
	
		use.est <- in.training.set & in.period & in.BTdiff.regime				
	
		# --- Check if there are ANY records in this time window and BTdiff regime
		# --- If not, set coefficients to NA and go to next window/BTdiff.regime
	
		if (any(use.est)) {
		  cat(paste("There are",length(which(use.est == TRUE)),
        "matchups inside window...\n\n")) 
		
      # --- Build second data frame with variables
      # --- Select those training records within month and latband
            
      reg.input.2 <- data.frame(
        bsst = reg.input$bsst[use.est],
        ch11000 = reg.input$ch11000[use.est],
        BTdiff = reg.input$BTdiff[use.est],
        satz = reg.input$satz[use.est],
        btd.sst = (reg.input$BTdiff[use.est] * reg.input$bsst[use.est]),
        secterm = (secant.deg(reg.input$satz[use.est]) - 1) *
          reg.input$BTdiff[use.est],
        BTdiff.regime = reg.input$BTdiff.regime[use.est],
        sat.time = reg.input$sat.time[use.est])
      
			# ------------------------------------------------------------------------#
			# --- Assign weights to months in 3, 4 or 5 month windows
			# ------------------------------------------------------------------------#

    	# Vector of weights, length equal to no. of records in window/BTdiff regime
      xwt <- rep(0, nrow(reg.input.2))  
      xt <- reg.input.2$sat.time
      
    	if ( i == 1 ) {      			# For time window 1
    		xwt <- ifelse( xt >= window.centers.beg[i]   & xt < window.centers.beg[i] + months(1), 1.0, xwt)
    		xwt <- ifelse( xt >= window.centers.beg[i+1] & xt < window.centers.beg[i+1] + months(1), 0.8, xwt)
    		xwt <- ifelse( xt >= window.centers.beg[i+2] & xt < window.centers.beg[i+2] + months(1), 0.5, xwt)
    	} else if ( i == 2 ) {    # For time window 2
    		xwt <- ifelse( xt >= window.centers.beg[i-1] & xt < window.centers.beg[i-1] + months(1), 0.8, xwt)
    		xwt <- ifelse( xt >= window.centers.beg[i]   & xt < window.centers.beg[1]   + months(1), 1.0, xwt)
    		xwt <- ifelse( xt >= window.centers.beg[i+1] & xt < window.centers.beg[i+1] + months(1), 0.8, xwt)
    		xwt <- ifelse( xt >= window.centers.beg[i+2] & xt < window.centers.beg[i+2] + months(1), 0.5, xwt)
    	} else if ( i >= 3 & i <= (n.of.windows - 2)) {
    														# For time windows 3 to N-2
      	xwt <- ifelse(xt >= window.centers.beg[i-2] & xt < window.centers.beg[i-2] + months(1), 0.5, xwt)
      	xwt <- ifelse(xt >= window.centers.beg[i-1] & xt < window.centers.beg[i-1] + months(1), 0.8, xwt)
      	xwt <- ifelse(xt >= window.centers.beg[i] 	& xt < window.centers.beg[i]  + months(1), 1.0, xwt)
      	xwt <- ifelse(xt >= window.centers.beg[i+1] & xt < window.centers.beg[i+1] + months(1), 0.8, xwt)
      	xwt <- ifelse(xt >= window.centers.beg[i+2] & xt < window.centers.beg[i+2] + months(1), 0.5, xwt)
   	 	} else if (i == (n.of.windows - 1)) {
   	 														# For time window N-1
    		xwt <- ifelse(xt >= window.centers.beg[i-2] & xt < window.centers.beg[i-2] + months(1), 0.5, xwt)
    		xwt <- ifelse(xt >= window.centers.beg[i-1] & xt < window.centers.beg[i-1] + months(1), 0.8, xwt)
    		xwt <- ifelse(xt >= window.centers.beg[i]   & xt < window.centers.beg[i] 	+ months(1), 1.0, xwt)
     		xwt <- ifelse(xt >= window.centers.beg[i+1] & xt < window.centers.beg[i+1] + months(1), 0.8, xwt)
    	} else if (i == n.of.windows) {
    														# For time window N
    		xwt <- ifelse(xt >= window.centers.beg[i-2] & xt < window.centers.beg[i-2] + months(1), 0.5, xwt)
    		xwt <- ifelse(xt >= window.centers.beg[i-1] & xt < window.centers.beg[i-1] + months(1), 0.8, xwt)
    		xwt <- ifelse(xt >= window.centers.beg[i]   & xt < window.centers.beg[i]   + months(1), 1.0, xwt)
    	}

			table(xwt, month(xt), useNA="ifany")
			
			# ------------------------------------------------------------------#
			# --- Now, do a first iteration using robust regression
			# ------------------------------------------------------------------#     
      
			# --- Perform a first iteration using a robust regression
			
			xreg.rob.MM <- lmRob(bsst ~ ch11000 + btd.sst + secterm,
        data = reg.input.2,
  			weights = xwt,
				control = lmRob.control(tlo = 1e-8, tua = 1.5e-09,
				mxr = 500, mxf = 500, mxs = 1000))	
			
			# Define weights based on SST residuals and bisquare function
			# Robustness weights based on 6 * MAD (see "Visualizing Data", p. 118)
			      
			bb2 <- median(abs(xreg.rob.MM$residuals))		# MAD of residuals
			bb3 <- xreg.rob.MM$residuals / (6 * bb2)  	# residuals / (6 * MAD)
			bb4 <- ( 1 - (bb3^2) )^2										# bisquare
			xwt2 <- ifelse(abs(bb3) < 1, bb4, 0)      	# This is the bisquare weight
			
			xwtf <- xwt * xwt2		# Robustness weights only (no monthly weights here)
			rm(bb2,bb3,bb4,xwt,xwt2); gc()
      
			# plot((xreg.rob.MM$residuals), xwt2)  # MAD weights vs. residuals
			# plot((xreg.rob.MM$residuals), xwtf)		# Final weights vs. residuals
			
			# ---------------------------------------------------------------#
			# --- Now, do a standard regression using weights determined above
			# --- Only use records with weights above a certain threshold
			# ---------------------------------------------------------------#
	
			weight.threshold <- 0.0		# Do not use records with weights below this value
			
			xreg <- lm(bsst ~ ch11000 + btd.sst + secterm,
        data = reg.input.2,
				weights=xwtf, subset=(xwtf > weight.threshold),
				na.action=na.omit)		
   			
   		#	--- Store coefficients
   	
			coef1[i,j] <- coef(xreg)[1]
			coef2[i,j] <- coef(xreg)[2]
			coef3[i,j] <- coef(xreg)[3]
			coef4[i,j] <- coef(xreg)[4]

			xnumber[i,j] <- length(xwtf > weight.threshold)		# Store no. of cases
		
		} else {			# No usable matchups within time window and BTdiff regime
			
			warning("No matchups in this window/BTdiff regime...\n")
			
      coef1[i,j] <- NA
			coef2[i,j] <- NA
			coef3[i,j] <- NA
			coef4[i,j] <- NA
			xnumber[i,j] <- 0
				
    }		# End of test for availability of matchups within month and BTdiff regime 		
 	}			# End of looping through BTdiff regimes
}				# End of looping through time windows 
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Write a data frame with coefficients for each time window and BTdiff regime. ----
# --- **** NOTE: We subtract 0.17 degC from the constant (coeff1)
# --- **** so we effectively compute a "skin" SST.

mm1 <- rep(seq(from=1, to=n.of.windows, by=1), each=n.BTdiff.regimes)    
mm2 <- rep(seq(from=1, to=n.BTdiff.regimes, by=1), times=n.of.windows)   
mm3 <- as.vector(t(coef1)) - 0.17		# *** Here we apply the "skin" correction *** #
mm4 <- as.vector(t(coef2))
mm5 <- as.vector(t(coef3))
mm6 <- as.vector(t(coef4))
mm7 <- as.vector(t(xnumber))

coeffs.SST.V5.df <- data.frame(per=mm1, BTdiff.reg=mm2,
	coeff1=mm3, coeff2=mm4, coeff3=mm5, coeff4=mm6, N=mm7) 

rm(mm1,mm2,mm3,mm4,mm5,mm6)
rm(xreg, xsigma, xwt, xwt2, xwtf, mad, xnumber)
rm(wts,x1,x2,x3,x4,x5,x6)
rm(coef1,coef2,coef3,coef4,xreg.rob.MM, weight.threshold)
rm(in.period, in.BTdiff.regime,in.training.set,i,j,use.est,xt); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Write a text file with SST V5 coefficients ----
# --- to be used in calculation of global SST fields.
# --- NOTE that the constant (coef1) already has the skin temperature correction incorporated.

# --- Prepare the data frame to be written out to file 

n.BTdiff.regimes <- length(levels(orig$BTdiff.regime))

mm1 <- rep(sensor, times=(n.of.windows * n.BTdiff.regimes));
mm2 <- rep(seq(from=1, to=n.of.windows, by=1), each=n.BTdiff.regimes);    
mm3 <- rep(seq(from=1, to=n.BTdiff.regimes, by=1), times=n.of.windows)

tt1 <- window.centers.beg
tt2 <- tt1 + months(1) - days(1)
mm4a <- as.integer(as.vector(year(tt1)))
mm4b <- sprintf("%03d", yday(tt1))
mm4c <- paste(mm4a, mm4b, sep="")
mm4 <- rep(mm4c, each=n.BTdiff.regimes)

mm5a <- as.integer(as.vector(year(tt2)))
mm5b <- sprintf("%03d", yday(tt2))
mm5c <- paste(mm5a, mm5b, sep="")
mm5 <- rep(mm5c, each=n.BTdiff.regimes)

coeffs.export.frame <- data.frame(mm1, mm4, mm5,
  coeffs.SST.V5.df[ ,c("coeff1","coeff2","coeff3","coeff4")], mm2, mm3)

head(coeffs.export.frame)

tt1 <- order(coeffs.export.frame$mm2, decreasing=TRUE)
coeffs.export.frame <- coeffs.export.frame[tt1,]

outfile <- paste(coeff.outdir, sensor,
	"_", matchup.version,"_",geophys.var,
	"_",sst.algo,
	"_coeffs_v",algo.coeffs.version,
	"_prelim1.txt", sep="")

cat("# Filename:", outfile,"\n", file=outfile, append=FALSE)
cat("# Coefficients estimated:",
  format(now(),"%Y-%m-%d %H:%M:%S\n"),
  file=outfile, append=TRUE)
cat("#",geophys.var, "Algorithm",sst.algo,"PRELIMINARY 1\n",file=outfile, append=TRUE)
cat("# Model: bsst ~ ch11000 + (ch11000 - ch12000) * bsst + sec(satz)*BTdifference\n",
    file=outfile, append=TRUE)

write.table(coeffs.export.frame,
	file=outfile, append=TRUE,
	col.names=FALSE, row.names=FALSE,  
	sep=" ")

# --- Write out object "coeffs.SST.V5.df"
# --- to a text file that can be easily reconstructed

outfile2 <- paste(coeff.outdir, sensor,
	"_", matchup.version,"_",geophys.var,
	"_",sst.algo,
	"_coeffs_v",algo.coeffs.version,
	"_dput_prelim1.txt", sep="");

dput(coeffs.SST.V5.df, file=outfile2)

rm(n.BTdiff.regimes,mm1,mm2,mm3,tt1,tt2,mm4a,mm4b,mm4c,mm4,mm5a,mm5b,mm5c,mm5)
rm(outfile, outfile2); gc()
# ----------------------------------------------------------------------------------------






# ---------------------------------------------------------------------------------------#
# --- NOW COMPUTE V5 SSTs FOR ALL MATCHUPS ----

# --- Fetch the SST coefficients corresponding to each period and T45 regime.
# --- Note: Many of the steps in this section are necessary because
# --- function merge() seems to get rows out of order, and satellite
# --- quantities and coefficients do not align.

# --- Create new working object "orig2"

orig2 <- orig 	

min.date <- floor_date(min(orig2$sat.timedate, na.rm=TRUE), "month") 
max.date <- ceiling_date(max(orig2$sat.timedate, na.rm=TRUE), "month")

tt1 <- seq(from=min.date, to=max.date, by="months")

period <- as.numeric(cut(orig2$sat.timedate, breaks=tt1, include.lowest=TRUE))
table(period, useNA="always")

rm(min.date, max.date, tt1); gc()

# --- BT difference regime as a numeric value

BTdiff.reg <- as.numeric(orig2$BTdiff.regime) 

# --- Add numeric values for period and BTdiff regime to "orig2"

orig2 <- data.frame(orig2, per=period, BTdiff.reg=BTdiff.reg)
table(orig2$per, orig2$BTdiff.reg)

# --- Merge the orig2 data frame with the coefficients data frame.
# --- Note we are using the join() command in the plyr library.
# --- Unlike merge(), join() does not change the order of the first data.frame to be merged.

library(plyr)
aa2 <- join(orig2, coeffs.SST.V5.df, by=c("per","BTdiff.reg"), type="left", match="all")

# --- Verify that we have a single coefficient value for each period/BTdiff regime combination

ttt <- tapply(aa2$coeff1, INDEX=list(aa2$per, aa2$BTdiff.reg), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per period and BTdiff regime")

ttt <- tapply(aa2$coeff2, INDEX=list(aa2$per, aa2$BTdiff.reg), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per period and BTdiff regime")

ttt <- tapply(aa2$coeff3, INDEX=list(aa2$per, aa2$BTdiff.reg), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per period and BTdiff regime")

ttt <- tapply(aa2$coeff4, INDEX=list(aa2$per, aa2$BTdiff.reg), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per period and BTdiff regime")

rm(ttt); gc()

# --- Extract vectors of coefficients from object "aa2"

coef1 <- aa2$coeff1
coef2 <- aa2$coeff2
coef3 <- aa2$coeff3
coef4 <- aa2$coeff4

# --- Extract the independent variables used in computing SST V5

x1 <- aa2$cen.11000  										  # BT31 (brigthness temperature for MODIS channel 31)
x2 <- aa2$cen.11000 - aa2$cen.12000			  # BTdiff = BT31 - BT32
x3 <- aa2$buoy.sst											  # Buoy SST
x4 <- aa2$satz											      # Satellite zenith angle
x5 <- x2 * x3														  # (BT31 - BT32) * buoy SST
x6 <- (secant.deg(x4) - 1) * x2	          # Secant(satz) * (BT31 - BT32)

# --- Compute "V5" SSTs and residuals for ALL the data.

SST.V5 <- coef1 + (coef2 * x1) + (coef3 * x5) + (coef4 * x6)

# --- Compute "V5" SST residuals as (satellite minus buoy).
# --- NOTE: Be careful to use object "aa2",
# --- otherwise records may be misaligned with "orig".

SST.V5.res <- SST.V5 - aa2$buoy.sst		# SST V5 residuals

# --- Add computed PF5 SST and residuals to data frame "aa2"

aa2 <- data.frame(aa2,
	SST.V5 = SST.V5,
	SST.V5.res = SST.V5.res)

# --- Eliminate unwanted columns from object "aa2"

tt1 <- names(aa2)
tt2 <- c("per","t45reg","coeff1","coeff2","coeff3","coeff4","N",
  "beg","end","BTdiff.reg") 	   # Columns to eliminate
tt3 <- which(!(tt1 %in% tt2))		# Numbers of columns in aa2 to KEEP in "orig"

# --- Save wanted columns in NEW object "orig"

orig <- aa2[, tt3]		# Original object with SST V5 and residuals added

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
tt2a <- str_detect(tt1, "^orig")
tt2b <- str_detect(tt1, "^AQUA")
tt2c <- str_detect(tt1, "^TERRA")
tt3 <- tt2a | tt2b | tt2c
tt4 <- tt1[!tt3]
rm(list=tt4)

rm(tt1,tt2a,tt2b,tt2c,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------










# ---------------------------------------------------------------------------------------#
# --- Statistics for SST V5 residuals in TRAINING set

rms <- function(x) {
	x <- x[!is.na(x)]
	x2 <- x * x
	result <- sqrt( sum(x2) / length(x) )
	return(result)
}

tt0 <- orig$SST.V5.res

use <- orig$use.4.coeffs.SST == TRUE &
	orig$train.sst == "Train" &
	abs(tt0) < 2.0

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
	q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10, row.names=NULL);



use <- orig$use.4.coeffs.SST == TRUE &
  orig$train.sst == "Validate" &
  abs(tt0) < 2.0

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
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10, row.names=NULL);

rm(use,tt0,tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,tt11,rms);gc()
# ---------------------------------------------------------------------------------------#



# ----------------------------------------------------------------------------------------
# --- Plot time series of median of SST residuals
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

use <- orig$use.4.coeffs.SST &
  orig$SST.tree.class == "P-Good" &
  orig$qsst.new == 0

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=median,
  na.rm=TRUE, simplify=TRUE)      

tt2 <- data.frame(t=seq.months, tt1)
colnames(tt2) <- c("date",levels(orig$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable_name="latband")

tt3b <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("latband1", times=length(tt3$date)),
  SST.res=tt3$value)
colnames(tt3b) <- c("date","latband","SST.algo","SST.res")


tt4 <- tapply(X=orig$SST.V5.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=median,
  na.rm=TRUE, simplify=TRUE)      

tt5 <- data.frame(t=seq.months, tt4)
colnames(tt5) <- c("date",levels(orig$latband))
rownames(tt5) <- NULL

tt6 <- melt(tt5, id.vars=c("date"), variable_name="latband")

tt6b <- data.frame(tt6$date,
  tt6$latband,
  SST.algo=rep("V5", times=length(tt6$date)),
  SST.res=tt6$value)
colnames(tt6b) <- c("date","latband","SST.algo","SST.res")

tt7 <- rbind(tt3b, tt6b)

xyplot(SST.res ~ date | latband, data = tt7, 
  type = c("l"),
  main=paste(sensor, "Median of SST residuals"),
  xlab="Time",
  ylab="SST residuals",
  col=c("orange","steelblue3"), lwd=2,
  groups = SST.algo, 
  panel = panel.superpose, 
  panel.groups = function(x, y, ...) { 
    panel.abline(h=-0.17, col="grey", lwd=2)
    panel.xyplot(x, y, ...) },
  layout=c(1,6))
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

use <- orig$use.4.coeffs.SST &
  orig$SST.tree.class == "P-Good"

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.prelim.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=median,
  na.rm=TRUE, simplify=TRUE)      

tt2 <- data.frame(t=seq.months, tt1)
colnames(tt2) <- c("date",levels(orig$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable_name="latband")

tt3b <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("latband1", times=length(tt3$date)),
  SST.res=tt3$value)
colnames(tt3b) <- c("date","latband","SST.algo","SST.res")


tt4 <- tapply(X=orig$SST.V5.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=sd,
  na.rm=TRUE, simplify=TRUE)      

tt5 <- data.frame(t=seq.months, tt4)
colnames(tt5) <- c("date",levels(orig$latband))
rownames(tt5) <- NULL

tt6 <- melt(tt5, id.vars=c("date"), variable_name="latband")

tt6b <- data.frame(tt6$date,
  tt6$latband,
  SST.algo=rep("V5", times=length(tt6$date)),
  SST.res=tt6$value)
colnames(tt6b) <- c("date","latband","SST.algo","SST.res")

tt7 <- rbind(tt3b, tt6b)

xyplot(SST.res ~ date | latband, data = tt7, 
  type = c("l"),
  main=paste(sensor, "Std Dev of SST residuals"),
  xlab="Time",
  ylab="ST dev SST residuals",
  col=c("orange","steelblue3"), lwd=2,
  groups = SST.algo, 
  panel = panel.superpose, 
  panel.groups = function(x, y, ...) { 
  panel.abline(h=-0.17, col="grey", lwd=2)
    panel.xyplot(x, y, ...) },
  layout=c(1,6))
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

use <- orig$use.4.coeffs.SST &
  orig$SST.tree.class == "P-Good" &
  orig$qsst.new == 0

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt1 <- tapply(X=orig$SST.latband1.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=mad,
  na.rm=TRUE, simplify=TRUE)      

tt2 <- data.frame(t=seq.months, tt1)
colnames(tt2) <- c("date",levels(orig$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable_name="latband")

tt3b <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("latband1", times=length(tt3$date)),
  SST.res=tt3$value)
colnames(tt3b) <- c("date","latband","SST.algo","SST.res")


tt4 <- tapply(X=orig$SST.V5.res[use],
  INDEX=list(tt0[use],orig$latband[use]),
  FUN=mad,
  na.rm=TRUE, simplify=TRUE)      

tt5 <- data.frame(t=seq.months, tt4)
colnames(tt5) <- c("date",levels(orig$latband))
rownames(tt5) <- NULL

tt6 <- melt(tt5, id.vars=c("date"), variable_name="latband")

tt6b <- data.frame(tt6$date,
  tt6$latband,
  SST.algo=rep("V5", times=length(tt6$date)),
  SST.res=tt6$value)
colnames(tt6b) <- c("date","latband","SST.algo","SST.res")

tt7 <- rbind(tt3b, tt6b)

xyplot(SST.res ~ date | latband, data = tt7, 
  type = c("l"),
  main=paste(sensor, "MAD of SST residuals"),
  xlab="Time",
  ylab="MAD of SST residuals",
  col=c("orange","steelblue3"), lwd=2,
  groups = SST.algo, 
  panel = panel.superpose, 
  panel.groups = function(x, y, ...) { 
    panel.abline(h=-0.17, col="grey", lwd=2)
    panel.xyplot(x, y, ...) },
  layout=c(1,6))
# ----------------------------------------------------------------------------------------











