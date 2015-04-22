# ----------------------------------------------------------------------------------------
# --- Script to re-estimate "latband1" SST algorithm coefficients
# --- for MODIS matchups AFTER identifying ADDITIONAL algorithm terms needed
# --- (e.g., mirror side correction, asymetric satelite zenith angle effect).
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Install required packages ----

if (!require(lubridate)) {install.packages("lubridate"); require(lubridate)}
if (!require(stringr)) {install.packages("stringr"); require(stringr)}
if (!require(maps)) {install.packages("maps"); require(maps)}
if (!require(robust)) {install.packages("robust"); require(robust)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(mgcv)) {install.packages("mgcv"); require(mgcv)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define sensor and calibration collection on which we will be working ----

sensor <- "AQUA"   # Select MODIS onboard AQUA or TERRA
#sensor <- "TERRA"	  # Select MODIS onboard AQUA or TERRA

collection <- 6			# Calibration collection

cat("\n Working on MODIS onboard",sensor,"collection",collection,"...\n");
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define geophysical variable worked on (ie, SST or SST4), algorithm type, etc. ----
# --- and other relevant quantities.

geophys.var <- "SST"						# Geophysical variable
sst.algo <- "latband1"					# Type of SST algorithm

algo.coeffs.version <- "6.3"		# Version of algorithm coefficients

# -- Define matchups version and input format (old or new)

matchup.version  <- paste("collection_",collection, sep="")	  # Version of matchups

matchup.format <- "OLD"
#matchup.format <- "NEW"
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
# --- Prepare objects related to zonal bands ----

latband.names <- levels(orig$latband)
n.latbands <- length(latband.names)
lat.boundaries <- c(-90, -40, -20, 0, 20, 40, 90) 	# Fixed boundaries for latitude bands

if (n.latbands != length(lat.boundaries) - 1) {
	stop("ERROR: Check definition of latitude bands and their boundaries...\n")	
}

# --- Prepare object holding boundaries for each latitude band.
# --- It will be used when exporting coefficients file.

lat.boundaries2 <- matrix(data=NA, nrow=n.latbands, ncol=3)
for (i in 1:n.latbands) {
	lat.boundaries2[i,] <- c(i, lat.boundaries[i],lat.boundaries[i+1])
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create matrices to store results from coeff estimation ----

bb1 <- list(month.abb, latband.names)														# Names of x and y dimensions

mad <- matrix(NA, ncol=n.latbands, nrow=12)											# MAD of LTS residuals

coef1 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef2 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef3 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef4 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef5 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)     # Coeffs from weighted regression for additional term
coef6 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)     # Coeffs from weighted regression for additional term
coef7 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)     # Coeffs from weighted regression for additional term

xnumber <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)		# No. of records
xsigma <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)		# Std. dev of residuals
xrsq <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# R-squared
sig.red.model <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)  		# significance of reduced model
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create objects to store GAM estimates of SST and SST residuals ----

SST.latband1.gam <- rep(NA, times=nrow(orig))

SST.latband1.gam.res <- rep(NA, times=nrow(orig))
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Assemble a data frame with variables to be used in regressions ----
# --- DO NOT filter by training records at this point.

reg.input <- data.frame(
  bsst    = orig$buoy.sst,                      # Buoy SST
  ch11000 = orig$cen.11000,                     # Channel 31 brightness temperature
  BTdiff  = (orig$cen.11000 - orig$cen.12000),  # BT difference MODIS channels 31 and 32
  satz    = orig$satz,                          # Satellite zenith angle
  train   = orig$train.sst,                     # Training set for SST?
  latband = orig$latband,                       # Latitude band
  mon     = orig$sat.mon,                       # Month
  mirror  = factor(orig$mirror,
    levels=c(1,2), labels=c("side1", "side2"))) # Mirror side (factor)
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Iterate through the data to re-estimate preliminary SST algorithm coefficients ----

for (i in 1:12) {		# Begin looping through months of the year with matchups in them
	
  ###  i <- 8; j <- 5  # for debugging purposes
  
	# --- First, select the 5 months that will be used
	# --- to estimate coefficients for each given month
	
  if (i == 1) {
    use.months <- c(11,12,1:3)  # For January use Nov,Dec,Jan,Feb,Mar
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
    use.months <- c(10:12,1,2)  # For December use Oct,Nov,Dec,Jan,Feb
  } else {
    stop("ERROR: Check month in satellite dates! \n")
  }

	# --- Start iterating over latitude bands

	for (j in 1:n.latbands) {  # Begin looping through atmospheric regimes or latitude bands
    	
		cat("\nCoefficients being estimated for", month.abb[i], "...\n")
		cat("Coefficients being estimated for latitude band", latband.names[j], "...\n")
		
		# Select matchups for estimating coefficients that are
    # (a) in training set, and
    # (b) within month window and latband being worked on and
    # (c) with abs of preliminary SST residuals < 1 deg C. 
    
    use <- orig$use.4.coeffs.SST == TRUE &
      (orig$SST.latband1.prelim2.res + 0.17) >= -1.0 &
	    (orig$SST.latband1.prelim2.res + 0.17) <= 1.0 &
      orig$SST.tree.class == "P-Good"
		in.month.window <- reg.input$mon %in% use.months       
    in.latband <- as.numeric(reg.input$latband) == j
    in.training.set <- reg.input$train == "Train"
  
    use.est <- use & in.training.set & in.month.window & in.latband 
    
    # Select matchups to be used for GAM SST calculation that are 
    # (a) within the central month, and
    # (b) within the latband being worked on.
    
    in.month <- reg.input$mon == i
    use.calc <- in.month & in.latband
    
    # Check if there are any matchups within month and latitude band

		if (any(use.est)) {	

			# --- Build second data frame with variables
      # --- Select those training records within month and latband
            
      reg.input.2 <- data.frame(
        bsst    = reg.input$bsst[use.est],
        ch11000 = reg.input$ch11000[use.est],
        BTdiff  = reg.input$BTdiff[use.est],
        satz    = reg.input$satz[use.est],
        btd.sst = (reg.input$BTdiff[use.est] * reg.input$bsst[use.est]),
        secterm = (secant.deg(reg.input$satz[use.est]) - 1) * reg.input$BTdiff[use.est],
        mirror  = reg.input$mirror[use.est],
        mon     = reg.input$mon[use.est],
        latband = reg.input$latband[use.est] )
    
			# --- Define weights for each record
			# --- (to be used in weighted regression)
		
			# TODO: Decide if we use 0.6 or 0.5 for months 1 and 5 in window
		
			wts <- c(0.6, 0.8, 1.0, 0.8, 0.6)
  		xwt <- wts[match(reg.input.2$mon, use.months)]
			
			table(xwt, reg.input.2$mon)

			# --- Perform a first iteration using a robust regression
				
			xreg.rob.MM <- lmRob(bsst ~ ch11000 + btd.sst + secterm +
        mirror + satz + I(satz ^ 2),
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
			
			xwtf <- xwt * xwt2		# Robustness and month weights
			rm(bb2,bb3,bb4)

      # plot((xreg.rob.MM$residuals), xwt2)	# MAD weights vs. residuals
			# plot((xreg.rob.MM$residuals), xwtf)	# Final weights vs. residuals

			# -------------------------------------------------------------------#
			# --- Now, do a standard regression using weights determined above.
			# --- The weights are the product of the robustness and time weights.
			# --- Only use records with weights above a certain threshold
			# --- (excluded using the subset option).
			# -------------------------------------------------------------------#
						
			weight.threshold <- 0.0		# Do not use records with weights below this value
      
      #contrasts(mirror) <- contr.treatment(levels(mirror), base=1)
			 
			# Build a parsimonious model starting from a full model and going backward	
      
      full.model <- lm(bsst ~ ch11000 + btd.sst + secterm +
        mirror + satz + I(satz^2),
        data = reg.input.2,
        weights = xwtf,
        subset = (xwtf >= weight.threshold),
        na.action = na.omit)
      
      bckwd.model <- step(full.model,
        direction = "backward",
        scope = (~ ch11000 + btd.sst + secterm +
            mirror + satz + I(satz ^ 2)),
        weights = xwtf,
        subset=(xwtf >= weight.threshold))
      
      uuu <- anova(bckwd.model, full.model)
			uu1 <- uuu[ ,"Pr(>F)"]
      uu2 <- tail(uu1,1)  # Significance of backward parsimonious model
			sig.red.model[i,j] <- uu2
			  
			uu3 <- attributes(bckwd.model$terms)$term.labels      
      
			# Extract coefficient values
      
      coef1[i, j] <- bckwd.model$coef[1]    # Intercept
			
      # For the rest of the coefficients, write value if term appears in reduced model, else write 0
	    
			coef2[i, j] <- ifelse ("ch11000" %in% uu3, bckwd.model$coef["ch11000"], 0.00)
			coef3[i, j] <- ifelse ("btd.sst" %in% uu3, bckwd.model$coef["btd.sst"], 0.00)
			coef4[i, j] <- ifelse ("secterm" %in% uu3, bckwd.model$coef["secterm"], 0.00)
			coef5[i, j] <- ifelse ("mirror" %in% uu3, bckwd.model$coef["mirrorside2"], 0.00)
			coef6[i, j] <- ifelse ("satz" %in% uu3, bckwd.model$coef["satz"], 0.00)
			coef7[i, j] <- ifelse ("I(satz^2)" %in% uu3, bckwd.model$coef["I(satz^2)"], 0.00)
			
      # Extract number of matchups in bin and other statistics
      
      xnumber[i,j] <- length(xwtf > weight.threshold)   # number inside window      	
			qqq <- summary(bckwd.model)
			xrsq[i,j] <- qqq$adj.r.squared		  # Overall adjusted r-squared
			xsigma[i,j] <- qqq$sigma            # Std. error of residuals
      
      # Diagnostics of performance against satz
      
      # u1 <- loess(full.model$residuals ~ reg.input.2$satz, span=0.40)
      # u2 <- predict(u1, newdata=seq(-60,60,0.5))      
      # smoothScatter(reg.input.2$satz, full.model$residuals, nbin=100, ylim=c(-2,2))
      # lines(seq(-60,60,0.5), u2, lwd=3, col="orange")    
      
      # -------------------------------------------------------------------#
  		# --- While still in the loop, fit a GAM with smooth terms
      # --- for BT difference and satellite zenith angle.
      # -------------------------------------------------------------------#
      
      weight.threshold <- 0.0		# Do not use records with weights below this value
      
      xreg.gam <- gam(bsst ~ ch11000 + s(BTdiff) + s(satz) + mirror,
        data = reg.input.2,
        method = "REML",
        weights = xwtf,
        na.action = na.omit,
        subset = (xwtf > weight.threshold) )
      
      predvals <- predict(xreg.gam, newdata=reg.input[use.calc,], type="response")
      predvals.2 <- predict(xreg.gam, newdata=reg.input[use.calc,], type="terms")
      
      # **** REinsert here the diagnostics removed ****
  
      # Store estimated SST values and SST residuals
      # Remove 0.17 deg C to estimates to make "Skin" SSTs

      indices.use.calc <- which(use.calc)

      SST.latband1.gam[indices.use.calc] <- (predvals - 0.17)
      SST.latband1.gam.res[indices.use.calc] <- (predvals - 0.17) -
        orig$buoy.sst[indices.use.calc]
      
      #gam.coeffs[indices.use.calc,"mon"] <- rep(i, times=nrow(reg.input[use.calc,]))
      #gam.coeffs[indices.use.calc,"latband"] <- rep(j, times=nrow(reg.input[use.calc,]))

      #gam.coeffs[indices.use.calc,"intcp"] <- rep(xreg.gam$coefficients[1],
      #  times=nrow(reg.input[use.calc,]))
      #gam.coeffs[indices.use.calc,"ch11000"] <- rep(xreg.gam$coefficients[2],
      #  times=nrow(reg.input[use.calc,]))
      #gam.coeffs[indices.use.calc,"mirror"] <- rep(xreg.gam$coefficients[3],
      #  times=nrow(reg.input[use.calc,]))
      #gam.coeffs[indices.use.calc,"btd"] <- predvals.2[,"s(BTdiff)"] / reg.input[use.calc,"BTdiff"]
      #gam.coeffs[indices.use.calc,"satz"] <- predvals.2[,"s(satz)"] / reg.input[use.calc,"satz"]

      # plot(reg.input[use.calc, "satz"], predvals.2[ ,"s(satz)"], type="p", pch=16)
      # plot(reg.input[use.calc, "BTdiff"], predvals.2[ ,"s(BTdiff)"], type="p", pch=16,
      #   xlim=c(-2,5), ylim=c(-5,15))

      #ks.test(xreg.rob.MM$residuals, xreg.gam$residuals)
      #qqplot(xreg.rob.MM$residuals, xreg.gam$residuals)
      #abline(0,1)
      # Ecdf(xreg.gam$residuals)
      # Ecdf(xreg.rob.MM$residuals, add=TRUE, col="tomato")

      # smoothScatter(xreg.rob.MM$residuals, xreg.gam$residuals)
      #abline(0, 1)
  
     
		}	# End of test for availability of matchups within month and latitude band	
	}		# End of looping through latitude bands
}			# End of looping through months
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Build a data frame with coefficients for each month and latitude band. ----
# --- **** NOTE: We subtract 0.17 degC from the constant (coeff1)
# --- **** so we effectively compute a "skin" SST.

mm1 <- rep(seq(1, length(month.abb), 1), each=n.latbands)    
mm2 <- rep(seq(1,n.latbands,1), times=length(month.abb))   
mm3 <- as.vector(t(coef1)) - 0.17		# *** Here we apply the "skin" correction *** #
mm4 <- as.vector(t(coef2))
mm5 <- as.vector(t(coef3))
mm6 <- as.vector(t(coef4))
mm7 <- as.vector(t(coef5))
mm8 <- as.vector(t(coef6))
mm9 <- as.vector(t(coef7))

mm10 <- as.vector(t(xnumber))
mm11 <- as.vector(t(xrsq))
mm12 <- as.vector(t(sig.red.model))

coeffs.SST.latband1.df <- data.frame(mon=mm1, lat=mm2,
	coeff1=mm3, coeff2=mm4, coeff3=mm5, coeff4=mm6, coeff5=mm7,
  coeff6=mm8, coeff7=mm9,
  N=mm10, rsq=mm11, sig.red=mm12) 

rm(mm1,mm2,mm3,mm4,mm5,mm6,mm7)
rm(xreg, xsigma, xwt, xwt2, xwtf, mad, xnumber,xrsq)
rm(wts,x1,x2,x3,x4,x5,x6)
rm(bb1,coef1,coef2,coef3,coef4, weight.threshold,xreg.rob.MM)
rm(training,validation)
rm(in.latband, in.month, qqq, use,i,j); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Store GAM-derived SST estimates in dataframe "orig" ----

orig$SST.latband1.gam <- SST.latband1.gam

orig$SST.latband1.gam.res <- SST.latband1.gam.res
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Write a text file with coefficients ----
# --- to be used in calculation of global SST fields
# --- NOTE that the constant (coef1) already has the skin temperature correction incorporated.

mm1 <- rep(sensor, times=(n.latbands * length(month.abb)))
mm2 <- rep(seq(1,length(month.abb),1), each=n.latbands)
mm3 <- rep(seq(1,n.latbands,1), times=length(month.abb)) 
mm4 <- lat.boundaries2[match(mm3, lat.boundaries2[,1]), 2]
mm5 <- lat.boundaries2[match(mm3, lat.boundaries2[,1]), 3]

coeffs.export.frame <- data.frame(sensor=mm1, mon=mm2, lat.s=mm4, lat.n=mm5,
  coeffs.SST.latband1.df[,-c(1,2)])

outfile <- paste(coeff.outdir, sensor,
	"_", matchup.version,"_",geophys.var,
	"_",sst.algo,
	"_coeffs_v",algo.coeffs.version,"_final.txt", sep="")

# --- Write a brief header for the coefficient file

cat("# Filename:", outfile,"\n", file=outfile, append=FALSE)
cat("# Coefficients estimated:",
  format(now(),"%Y-%m-%d %H:%M:%S\n"),
  file=outfile, append=TRUE)
cat("# Algorithm version 6.3 FINAL COEFFS\n",file=outfile, append=TRUE)
cat("# Model: bsst ~ ch11000 + btd.sst + secterm + mirror.side + satz + satz^2\n",
  file=outfile, append=TRUE)

write.table(format(coeffs.export.frame, digits = 9),
	file=outfile, append=TRUE, quote=FALSE,
	col.names=FALSE, row.names=FALSE, sep=" ")

# --- Write out object "coeffs.SST.latband1.df"
# --- to a text file that can be easily reconstructed

outfile2 <- paste(coeff.outdir, sensor,
	"_", matchup.version,"_",geophys.var,
	"_",sst.algo,
	"_coeffs_v",algo.coeffs.version,
	"_dput_final.txt", sep="")

dput(coeffs.SST.latband1.df, file=outfile2)

#   coeffs.SST.latband1.df <- dget(file=outfile2)

rm(coeffs.export.frame)
rm(mm1,mm2,mm3,mm4,mm5,outfile, outfile2)
rm(lat.boundaries, lat.boundaries2,latband,bsst,n.latbands)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Compute latband1 SSTs for all matchups ----
# --- Fetch the SST coefficients corresponding to each month and latitude band
# --- Note: Many of the steps in this section are necessary because
# --- function merge() seems to get rows out of order, and satellite
# --- quantities and coefficients do not align.

orig2 <- orig 	# Create new working object "orig2"

# --- Add numeric values for mon and latband to "orig2"

orig2 <- data.frame(orig2, mon=orig2$sat.mon, lat=as.numeric(orig2$latband))

# --- Merge the orig2 data frame with the coefficients data frame
# --- Note we are using the join() command un the plyr library.
# --- Unlike merge(), join() does not change the order of the first data.frame toe be merged.
# --- Nevertheless, because of the way we do things, the result should be cooorect.
# --- For this reason, there shpould not be a need to use

library(plyr)
aa2 <- join(orig2, coeffs.SST.latband1.df, by=c("mon","lat"), type="left", match="all")

# --- Verify that we have a single coefficient value for each month/latband combination

ttt <- tapply(aa2$coeff1, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per month and latband")

ttt <- tapply(aa2$coeff2, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per month and latband")

ttt <- tapply(aa2$coeff3, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per month and latband")

ttt <- tapply(aa2$coeff4, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per month and latband")

ttt <- tapply(aa2$coeff5, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per month and latband")

ttt <- tapply(aa2$coeff6, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per month and latband")

ttt <- tapply(aa2$coeff7, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  stop("ERROR: there is more than one coefficient value per month and latband")

rm(ttt); gc()

# --- Extract vectors of coefficients

coef1 <- aa2$coeff1
coef2 <- aa2$coeff2
coef3 <- aa2$coeff3
coef4 <- aa2$coeff4
coef5 <- aa2$coeff5
coef6 <- aa2$coeff6
coef7 <- aa2$coeff7

# --- Extract the independent variable used in computing SST

x1 <- aa2$cen.11000											  # BT31 (brigthness temperature for channel 31)
x2 <- aa2$cen.11000 - aa2$cen.12000			  # BT31 - BT32
x3 <- aa2$buoy.sst											  # Buoy SST
x4 <- aa2$satz											      # satellite zenith angle (no sign)
x5 <- x2 * x3														  # BT31-BT32 * buoy SST
x6 <- (secant.deg(aa2$satz) - 1) * x2     # Secant times BT31-BT32
x7 <- ifelse(aa2$mirror == 1, 0, 1)       # Dummy mirror variable 0 for side1, 1 for side2

# --- Compute "latband1" SSTs and residuals for ALL the data.

SST.latband1 <- coef1 + (coef2 * x1) + (coef3 * x5) + (coef4 * x6) +
  (coef5 * x7) + (coef6 * x4) + (coef7 * (x4 ^ 2))

# --- Compute "latband1" SST residuals as (satellite minus buoy).
# --- NOTE: Be careful to use object "aa2",
# --- otherwise records may be misaligned with "orig".

SST.latband1.res <- SST.latband1 - aa2$buoy.sst		# SST residuals

# --- Replace previously-estimated latband SST and residuals in data frame "aa2".

aa2$SST.latband1 <- SST.latband1 						# Replace preliminary SST values
aa2$SST.latband1.res <- SST.latband1.res 		# Replace preliminary residual values

# --- Eliminate unwanted columns from object "aa2"

tt1 <- names(aa2)
tt2 <- c("mon","lat","coeff1","coeff2","coeff3","coeff4","coeff5",
  "coeff6","coeff7","N","rsq","sig.red") 	# Columns to eliminate
tt3 <- which(!(tt1 %in% tt2))		# Numbers of columns in aa2 to KEEP in "orig"

# --- Save wanted columns in NEW object "orig"

orig <- aa2[,tt3]		# Original object with SST latband1 and residuals added


# --- Plots of SST and SST residuals vs. buoy

use <- orig$use.4.coeffs.SST == TRUE &
  (orig$SST.latband1.res + 0.17) >= -1.0 &
  (orig$SST.latband1.res + 0.17) <= 1.0 &
  orig$SST.tree.class == "P-Good"

smoothScatter(orig$buoy.sst[use],
  orig$SST.latband1[use],
  xlab="Buoy SST",
  ylab="Satellite SST",
  main=paste0(sensor," - Final SST"),
  nbin=60)
abline(-0.17, 1)

smoothScatter(orig$buoy.sst[use],
  orig$SST.latband1.res[use],
  xlab="Buoy SST",
  ylab="Satellite SST Residuals",
  main=paste0(sensor," - Final SST residuals"),
  nbin=60)
abline(h=-0.17)

rm(x1,x2,x3,x4,x5,x6,coef1,coef2,coef3,coef4,coef5,tt1,tt2,tt3)
rm(SST.latband1, SST.latband1.res); gc()
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

rm(out.data.file, out.data.file2); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Clean up all objects EXCEPT those with names ----
# --- equal to "orig" or starting with string "AQUA" or "TERRA

tt1 <- objects()
tt2a <- str_detect(tt1, "^orig$")
tt2b <- str_detect(tt1, "^AQUA")
tt2c <- str_detect(tt1, "^TERRA")
tt2d <- str_detect(tt1, "^config")
tt3 <- tt2a | tt2b | tt2c | tt2d
tt4 <- tt1[!tt3]
rm(list=tt4)

rm(tt1,tt2a,tt2b,tt2c,tt2d,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------




# ---------------------------------------------------------------------------------------#
# --- Statistics for re-estimated SST latband1 residuals

rms <- function(x) {
  x <- x[!is.na(x)]
  x2 <- x * x
  result <- sqrt( sum(x2) / length(x) )
  return(result)
}

tt0 <- orig$SST.latband1.res

use <- orig$use.4.coeffs.SST == TRUE &
  orig$train.sst == "Train" &
  (orig$SST.latband1.res + 0.17) >= -1.0 &
  (orig$SST.latband1.res + 0.17) <= 1.0 &
  orig$SST.tree.class == "P-Good"

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
  (orig$SST.latband1.res + 0.17) >= -1.0 &
  (orig$SST.latband1.res + 0.17) <= 1.0 &
  orig$SST.tree.class == "P-Good"

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

# --- Now try more realistic selection, as could be used with images

use <- orig$solz > 90 &
  abs(orig$satz) <= 60 &
  abs(orig$buoy.lat) < 60 &
  orig$SST.tree.prob.GOOD > 0.90 &
  abs((orig$SST.latband1 + 0.17) - orig$ref.type.1.SST) <= 2

tt1 <- round(min(tt0[use], na.rm=TRUE), 3)
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

tt11













# --- Statistics for GAM SST residuals

tt0 <- orig$SST.latband1.gam.res

use <- orig$use.4.coeffs.SST == TRUE &
  orig$train.sst == "Train" &
  abs(tt0 + 0.17) < 1.5

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
  abs(tt0 + 0.17) < 1.5

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

rm(use,tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,tt11); gc()
# ----------------------------------------------------------------------------------------


tt0 <- orig$SST.latband1.res

use <- orig$use.4.coeffs.SST == TRUE &
  abs(tt0 + 0.17) < 1.5

smoothScatter(orig$buoy.sst[use], orig$SST.latband1[use],
  xlab="Buoy SST",
  ylab="Satellite SST (LR)",
  main=paste(sensor,"- Re-estimated SST (LR)"))
abline(-0.17, 1, col="grey80", lwd=3)

smoothScatter(orig$buoy.sst[use], orig$SST.latband1.res[use],
  xlab="Buoy SST",
  ylab="SST LR Residuals",
  ylim=c(-2,2),
  main=paste(sensor,"- Re-estimated SST (LR)"))
abline(h=-0.17, col="grey80", lwd=3)


tt0 <- orig$SST.latband1.gam.res

use <- orig$use.4.coeffs.SST == TRUE &
  abs(tt0 + 0.17) < 1.5

smoothScatter(orig$buoy.sst[use], orig$SST.latband1.gam[use],
  xlab="Buoy SST",
  ylab="Satellite SST (GAM)",
  main=paste(sensor,"- Re-estimated SST (GAM)"))
abline(-0.17, 1, col="grey80", lwd=3)

smoothScatter(orig$buoy.sst[use], orig$SST.latband1.gam.res[use],
  xlab="Buoy SST",
  ylab="SST GAM Residuals",
  ylim=c(-2,2),
  main=paste(sensor,"- Re-estimated SST (GAM)"))
abline(h=-0.17, col="grey80", lwd=3)



# Diagnostics to be reinserted

      
      #median(full.model$residuals)
      #median(xreg.gam$residuals, na.rm=TRUE)
      #mad(full.model$residuals)
      #mad(xreg.gam$residuals, na.rm=TRUE)
      #hist(xreg.gam$residuals)    
      
      #u1 <- loess(xreg.gam$residuals ~ reg.input.2$satz, span=0.40)
      #u2 <- predict(u1, newdata=seq(-60,60,0.5))
      #smoothScatter(reg.input.2$satz, xreg.gam$residuals, nbin=60, ylim=c(-2,2),
      #  nrpoints=300)
      #lines(seq(-60,60,0.5), u2, lwd=3, col="orange")
      
         
      g1 <- (reg.input[use.calc, "BTdiff"])
      g2 <- order(g1)
      g3 <- g1[g2]
      g4 <- predvals.2[, "s(BTdiff)"]
      g5 <- g4[g2]
      plot(g3, g5, type="l", xlab="BT31 - BT32", ylab="smooth(BT diff)",
        ylim=c(-8, 28),
        lwd=3, col="tomato")

      g1 <- (reg.input[use.calc, "satz"])
      g2 <- order(g1)
      g3 <- g1[g2]
      g4 <- predvals.2[ ,"s(satz)"]
      g5 <- g4[g2]
      plot(g3, g5, type="l", xlab="Satellite zenith angle", ylab="smooth(satz)",
        ylim=c(-0.6, 1.4),
        lwd=3, col="tomato")
    
      
      plot(reg.input[use.calc, "satz"], predvals.2[ ,"s(satz)"], type="p", pch=16)
      plot(reg.input[use.calc, "BTdiff"], predvals.2[ ,"s(BTdiff)"], type="p", pch=16,
         xlim=c(-2,5), ylim=c(-5,15))

      
      
