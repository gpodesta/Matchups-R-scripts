# ---------------------------------------------------------------------------------------------
# --- Script to re-estimate "latband1" SST4 algorithm coefficients
# --- for MODIS matchups AFTER fitting night quality trees.
# ---------------------------------------------------------------------------------------------

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
if (!require(zoo)) {install.packages("zoo"); require(zoo)}
if (!require(xts)) {install.packages("xts"); require(xts)}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define sensor and calibration collection on which we will be working ----

sensor <- "AQUA"        # Select MODIS onboard AQUA or TERRA
#sensor <- "TERRA"				# Select MODIS onboard AQUA or TERRA

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

# ---------------------------------------------------------------------------------------------
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
# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------
# --- Create matrices to store results from coeff estimation ----

bb1 <- list(month.abb, latband.names)														# Names of x and y dimensions

mad <- matrix(NA, ncol=n.latbands, nrow=12)											# MAD of LTS residuals

coef1 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef2 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef3 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef4 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# Coeffs from weighted regression
coef5 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)  		# Coeffs from weighted regression
coef6 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)  		# Coeffs from weighted regression
coef7 <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)  		# Coeffs from weighted regression

xnumber <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)		# No. of records
xsigma <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)		# Std. dev of residuals
xrsq <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)			# R-squared
sig.red.model <- matrix(NA, ncol=n.latbands, nrow=12, dimnames=bb1)  # significance of reduced model
# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create objects to store GAM estimates of SST4 and SST4 residuals ----

SST4.latband1.gam.prelim2 <- rep(NA, times=nrow(orig))

SST4.latband1.gam.prelim2.res <- rep(NA, times=nrow(orig))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Assemble a data frame with variables to be used in SST4 regressions ----
# --- DO NOT filter by training records at this point.

reg.input <- data.frame(
  bsst       = orig$buoy.sst,                     # Buoy SST
  ch3959     = orig$cen.3959,                     # BT for MODIS channel 22
  BTdiff     = (orig$cen.3959 - orig$cen.4050),   # BT difference of channels 22 and 23
  satz       = orig$satz,                         # Satellite zenith angle
  train.sst4 = orig$train.sst4,                   # Training set for SST4
  latband    = orig$latband,                      # Latitude band
  mon        = orig$sat.mon,                      # Month
  mirror     = factor(orig$mirror,
    levels=c(1,2), labels=c("side1", "side2")))   # Mirror side (factor)

# --- Check rows that have one or more missing values

#tt1 <- apply(reg.input, MARGIN=1, FUN=function(x) {any(is.na(x))})
#table(tt1)

#reg.input <- na.omit(reg.input)   	# Select rows without missing values
#rm(tt1); gc()
# ----------------------------------------------------------------------------------------




# ---------------------------------------------------------------------------------------#
# --- Iterate through the data to re-estimate preliminary SST4 algorithm coefficients ----

for (i in 1:12) {		# Begin looping through months of the year with matchups in them
	  
  ###  i <- 3; j <- 5  for debugging purposes

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

	for (j in 1:n.latbands) {     # Begin looping through atmospheric regimes or latitude bands
    	
		cat("\nSST4 Coefficients being estimated for", month.abb[i], "...\n")
  	cat("SST4 Coefficients being estimated for latitude band", latband.names[j], "...\n")
		
		# Select matchups for estimating SST4 coefficients that are
    # (a) in training set, and
    # (b) within month window and latband being worked on and
    # (c) with abs of preliminary SST residuals < 1.5 deg C. 
    
    use <- orig$use.4.coeffs.SST4 == TRUE &
      (orig$SST4.latband1.prelim.res + 0.17) > -1.5 &
	    (orig$SST4.latband1.prelim.res + 0.17) < 1.5
    
		in.month.window <- reg.input$mon %in% use.months       
    in.latband <- as.numeric(reg.input$latband) == j
    in.training.set <- reg.input$train.sst4 == "Train"
    after.2003 <- orig$sat.timedate >= ymd("2003-01-01", tz="GMT")
    
    if (sensor == "TERRA") {
      use.est <- use & in.month.window & in.latband & in.training.set & after.2003
    } else if (sensor == "AQUA") {
      use.est <- use & in.month.window & in.latband & in.training.set
    } else {
      cat("Sensor name is incorrect...\n")
    }
  
		# Select matchups to be used for GAM SST4 calculation that are 
    # (a) within the central month, and
    # (b) within the latband being worked on.
    
    in.month <- reg.input$mon == i
    use.calc <- in.month & in.latband
     
    if (any(use.est)) {	# Check if there are any matchups within month and latitude band

      # --- Build second data frame with variables
      # --- Select those training records within month and latband
            
      reg.input.2 <- data.frame(
        bsst    = reg.input$bsst[use.est],
        ch3959  = reg.input$ch3959[use.est],
        BTdiff  = reg.input$BTdiff[use.est],
        satz    = reg.input$satz[use.est],
        secterm = (secant.deg(reg.input$satz[use.est]) - 1),
        mirror  = reg.input$mirror[use.est],
        mon     = reg.input$mon[use.est],
        latband = reg.input$latband[use.est] )

      # --- Define weights for each record
			# --- (to be used in weighted regression)
				
			wts <- c(0.6, 0.8, 1.0, 0.8, 0.6)
  		xwt <- wts[match(reg.input.2$mon, use.months)]
			
			table(xwt, reg.input.2$mon)

			# --- Perform a first iteration using a robust regression
				
			xreg.rob.MM <- lmRob(bsst ~ ch3959 + BTdiff + secterm +
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
			
			#plot((xreg.rob.MM$residuals), xwt2)	# MAD weights vs. residuals
			#plot((xreg.rob.MM$residuals), xwtf)		# Final weights vs. residuals

			# -------------------------------------------------------------------#
  		# --- Now, do a standard regression using weights determined above.
			# --- The weights are the product of the robustness and time weights.
			# --- Only use records with weights above a certain threshold
			# --- (excluded using the subset option).
			# -------------------------------------------------------------------#
						
			weight.threshold <- 0.0		# Do not use records with weights below this value
      
      #contrasts(mirror) <- contr.treatment(levels(mirror), base=1)
			 
			# Build a parsimonious model starting from a full model and going backward	
      
      full.model <- lm(bsst ~ ch3959 + BTdiff + secterm +
        mirror + satz + I(satz^2),
        data = reg.input.2,
        weights = xwtf,
        subset = (xwtf >= weight.threshold),
        na.action = na.omit)
      
      bckwd.model <- step(full.model,
        direction = "backward",
        scope = (~ ch3959 + BTdiff + secterm +
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
	    
			coef2[i, j] <- ifelse ("ch3959" %in% uu3, bckwd.model$coef["ch3959"], 0.00)
			coef3[i, j] <- ifelse ("BTdiff" %in% uu3, bckwd.model$coef["BTdiff"], 0.00)
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
      
      xreg.gam <- gam(bsst ~ ch3959 + s(BTdiff) + s(satz) + mirror,
        data = reg.input.2,
        method = "REML",
        weights = xwtf,
        na.action = na.omit,
        subset = (xwtf > weight.threshold) )   
      
      predvals <- predict(xreg.gam, newdata=reg.input[use.calc,], type="response")
      predvals.2 <- predict(xreg.gam, newdata=reg.input[use.calc,], type="terms")
    
      # Store estimated GAM SST4 values and SST4 residuals.
      # Remove 0.17 deg C to compute "pseudo-skin" SST4

      SST4.latband1.gam.prelim2[which(use.calc)] <- (predvals - 0.17)
      SST4.latband1.gam.prelim2.res[which(use.calc)] <- (predvals - 0.17) -
          orig$buoy.sst[which(use.calc)]
      
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
        
      #gam.coeffs[indices.use.calc,"mon"] <- rep(i, times=nrow(reg.input[use.calc,]))
      #gam.coeffs[indices.use.calc,"latband"] <- rep(j, times=nrow(reg.input[use.calc,]))

      #gam.coeffs[indices.use.calc,"intcp"] <- rep(xreg.gam$coefficients[1],
      #  times=nrow(reg.input[use.calc,]))
      #gam.coeffs[indices.use.calc,"cen3959"] <- rep(xreg.gam$coefficients[2],
      #  times=nrow(reg.input[use.calc,]))
      #gam.coeffs[indices.use.calc,"mirror"] <- rep(xreg.gam$coefficients[3],
      #  times=nrow(reg.input[use.calc,]))
      #gam.coeffs[indices.use.calc,"BTdiff"] <- predvals.2[,"s(BTdiff)"] / reg.input[use.calc,"BTdiff"]
      #gam.coeffs[indices.use.calc,"satz"] <- predvals.2[,"s(satz)"] / reg.input[use.calc,"satz"]

      # plot(reg.input[use.calc, "satz"], predvals.2[ ,"s(satz)"], type="p", pch=16)
      # plot(reg.input[use.calc, "BTdiff"], predvals.2[ ,"s(BTdiff)"], type="p", pch=16)
      
			
		}	# End of test for availability of matchups within month and latitude band 		
	}		# End of looping through latitude bands
}			# End of looping through months
# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Write a data frame with SST4 coefficients for each month and latitude band.----
# --- **** NOTE: We subtract 0.17 deg C from the constant (coeff1)
# --- **** so we effectively compute a "skin" SST.

mm1 <- rep(seq(1, length(month.abb), 1), each=n.latbands)    
mm2 <- rep(seq(1,n.latbands,1), times=length(month.abb))   
mm3 <- as.vector(t(coef1)) - 0.17  	# *** Here we apply the "skin" correction *** #
mm4 <- as.vector(t(coef2))
mm5 <- as.vector(t(coef3))
mm6 <- as.vector(t(coef4))
mm7 <- as.vector(t(coef5))
mm8 <- as.vector(t(coef6))
mm9 <- as.vector(t(coef7))

mm10 <- as.vector(t(xnumber))
mm11 <- as.vector(t(xrsq))
mm12 <- as.vector(t(sig.red.model))

coeffs.SST4.latband1.df <- data.frame(mon=mm1, lat=mm2,
	coeff1=mm3, coeff2=mm4, coeff3=mm5, coeff4=mm6, coeff5=mm7,
  coeff6=mm8, coeff7=mm9,
  N=mm10, rsq=mm11, sig.red=mm12) 

table(coeffs.SST4.latband1.df$sig.red, useNA="always")

table(coeffs.SST4.latband1.df$coeff7, useNA="always")

hist(coeffs.SST4.latband1.df$coeff7)


rm(mm1,mm2,mm3,mm4,mm5,mm6,mm7,mm8,mm9,mm10,mm11,mm12)
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
mm2 <- rep(seq(1, length(month.abb), 1), each=n.latbands)
mm3 <- rep(seq(1, n.latbands, 1), times=length(month.abb)) 
mm4 <- lat.boundaries2[match(mm3, lat.boundaries2[,1]), 2]
mm5 <- lat.boundaries2[match(mm3, lat.boundaries2[,1]), 3]

coeffs.export.frame <- data.frame(sensor=mm1, mon=mm2, lat.bot=mm4,
  lat.top=mm5, coeffs.SST4.latband1.df[,-c(1,2)])

outfile <- paste(coeff.outdir, sensor,
	"_", matchup.version,"_",geophys.var,
	"_", sst4.algo,
	"_coeffs_v",algo.coeffs.version,"_prelim2.txt", sep="")

# --- Write a brief header for the coefficient file

cat("# Filename:", outfile,"\n", file=outfile, append=FALSE)
cat("# Coefficients estimated:",
  format(now(),"%Y-%m-%d %H:%M:%S\n"),
  file=outfile, append=TRUE)
cat("# Algorithm version 6.3 PRELIMINARY\n",file=outfile, append=TRUE)
cat("# Model: bsst ~ ch3959 + btdiff + sec(satz) + mirror + satz + satz^2\n",
  file=outfile, append=TRUE)

write.table(format(coeffs.export.frame, digits = 9),
	file=outfile, append=TRUE, quote=FALSE,
	col.names=FALSE, row.names=FALSE, sep=" ")

# --- Write out object "coeffs.SST.latband1.df"
# --- to a text file that can be easily reconstructed

outfile2 <- paste(coeff.outdir, sensor,
	"_", matchup.version,"_",geophys.var,
	"_", sst4.algo,
	"_coeffs_v",algo.coeffs.version,
	"_dput_prelim2.txt", sep="")

dput(coeffs.SST4.latband1.df, file=outfile2)

#   coeffs.SST.latband1.df <- dget(file=outfile2)

rm(coeffs.export.frame)
rm(mm1,mm2,mm3,mm4,mm5,outfile, outfile2)
rm(lat.boundaries, lat.boundaries2,latband,bsst,n.latbands)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Store GAM-derived SST estimates in dataframe "orig" ----
# --- SST4 is only computed for night time (solar zenith angle > 90 degrees).
# --- Set GAM SST4 daytime estimates to NA.

nite <- orig$solz > 90

orig$SST4.latband1.gam.prelim2 <- ifelse(nite, SST4.latband1.gam.prelim2, NA)
  
orig$SST4.latband1.gam.prelim2.res <- ifelse(nite, SST4.latband1.gam.prelim2.res, NA)

rm(nite); gc()
# ----------------------------------------------------------------------------------------

 ----------------------------------------------------------------------------------------
# --- COMPUTE LATBAND1 SST4s FOR ALL MATCHUPS ----
# --- Fetch the SST4 coefficients corresponding to each month and latitude band
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
aa2 <- join(orig2, coeffs.SST4.latband1.df, by=c("mon","lat"), type="left", match="all")

# --- Verify that we have a single SST4 coefficient value for each month/latband combination

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

# --- Extract vectors of SST4coefficients

coef1 <- aa2$coeff1
coef2 <- aa2$coeff2
coef3 <- aa2$coeff3
coef4 <- aa2$coeff4
coef5 <- aa2$coeff5
coef6 <- aa2$coeff6
coef7 <- aa2$coeff7

# --- Extract the independent variable used in computing SST

x1 <- aa2$cen.3959  										  # BT31 (brigthness temperature for channel 31)
x2 <- aa2$cen.3959 - aa2$cen.4050			    # BT31 - BT32
x3 <- aa2$buoy.sst											  # Buoy SST
x4 <- aa2$satz										        # satellite zenith angle
x6 <- (secant.deg(aa2$satz) - 1)	        # Secant [*** NOT times BT31-BT32]
x7 <- ifelse(aa2$mirror == 1, 0, 1)       # Dummy mirror variable 0 for side1, 1 for side2

# --- Compute "latband1" SSTs and residuals for ALL the data.

SST4.latband1.prelim2 <- coef1 + (coef2 * x1) + (coef3 * x2) + 
  (coef4 * x6) + (coef5 * x7) + (coef6 * x4) + (coef7 * (x4 ^ 2))

# --- Compute "latband1" SST4 residuals as (satellite SST4 minus buoy SST).
# --- NOTE: Be careful to use object "aa2" to subtract buoy SST,
# --- otherwise records may be misaligned with "orig".

SST4.latband1.prelim2.res <- SST4.latband1.prelim2 - aa2$buoy.sst  	# SST4 residuals

# --- Replace previously-estimated latband SST4 and residuals in data frame "aa2".

aa2$SST4.latband1.prelim2 <- SST4.latband1.prelim2   				# Add preliminary 2 SST4 values
aa2$SST4.latband1.prelim2.res <- SST4.latband1.prelim2.res 	# Add preliminary 2 SST4 residual values

# --- SST4 values are good only at night (solar zenith angle > 90)
# --- Set non-night SST4 and SST4 residual values to NA

aa2$SST4.latband1.prelim2 <- ifelse(aa2$solz > 90, aa2$SST4.latband1.prelim2, NA)
aa2$SST4.latband1.prelim2.res <- ifelse(aa2$solz > 90, aa2$SST4.latband1.prelim2.res, NA)

# --- Eliminate unwanted columns from object "aa2"

tt1 <- names(aa2)
tt2 <- c("mon","lat","coeff1","coeff2","coeff3","coeff4","coeff5",
  "coeff6","coeff7","rsq","sig.red","N","P") 	# Columns to eliminate
tt3 <- which(!(tt1 %in% tt2))		# Numbers of columns in aa2 to KEEP in "orig"

# --- Save wanted columns in NEW object "orig"

orig <- aa2[,tt3]		# Original object with SST4 latband1 and SST4 residuals added

rm(x1,x2,x3,x4,x6,x7,coef1,coef2,coef3,coef4,coef5,coef6,coef7,tt1,tt2,tt3)
rm(SST4.latband1, SST4.latband1.res, aa2); gc()
# ----------------------------------------------------------------------------------------




# ---------------------------------------------------------------------------------------#
# --- Now, fix the shift in SST4 values at thebeginning of the record due to electronics.
# --- Compute median of preliminary 2 SST4 residuals by month and year.

# --- Create a sequence of months
# --- It will be used to plot monthly series of statistics

min.date <- floor_date(min(orig$sat.timedate, na.rm=TRUE), "month") 
max.date <- floor_date(max(orig$sat.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rm(min.date, max.date)

# --- Compute monthly medians and number of LR SST4 residuals
# --- We use only matchups that can be used for coefficient estimation AND
# --- with absolute value of (residuals + 0.17) < 1.5 deg C.

use <- orig$use.4.coeffs.SST4 &
  abs(orig$SST4.latband1.prelim2.res + 0.17) < 1.5

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt4 <- tapply(X=orig$SST4.latband1.prelim2.res[use],
  INDEX=tt0[use],
  FUN=median, na.rm=TRUE, simplify=TRUE)      # Calculate median by month

tt4 <- xts(tt4, order.by=seq.months)

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

tt3 <- tapply(X=orig$SST4.latband1.prelim2.res[orig$use.4.coeffs.SST4],
  INDEX=tt2[orig$use.4.coeffs.SST4],
  FUN=median, simplify=TRUE)           

round(tt3, 4)

# --- Plot of time series of SST4 residuals, including the levels for each epoch

plot.zoo(tt4,
  type="l", lwd=2, col="steelblue",
  ylim=c(-0.75, 0),
  main=paste(sensor,"- SST4 Final Residuals"),
  xlab="Time", las=1, cex.axis=0.7,
  ylab="Median of SST4 Residuals")
points(tt4, pch=16, col="tomato")
abline(h=tt3, col="grey50")


# --- These values can be used to correct SST4 values, as the residuals seem to be
# --- fairly stable around the offset value for each epoch.    
# --- The difference between the median after July 2001 and the previous
# --- two epochs is about 0.44  and 0.25 degC, respectively. This value needs to be ADDED to
# --- SST4 values prior to 2001-07-01 in order to correct them. 
# --- *** NOTE The following code is only run for Terra:

if (sensor == "TERRA") {

  # --- The median of SST residuals for each epoch identified is shown below.
  # Preliminary 1 residuals
  #       P1         P2         P3 
  # -0.6174036 -0.4258075 -0.1723426 
  # Preliminary 2 residuals
  #       P1         P2         P3 
  #-0.6164536 -0.4305070 -0.1710983 
  
  
  correct1 <- tt3["P3"] - tt3["P1"]     # Prelim 1: 0.445061 Prelim 2: 0.4453553 
  correct2 <- tt3["P3"] - tt3["P2"]     # Prelim 1: 0.253465 Prelim 2: 0.2594086
  
  SST4.latband1.prelim2 <- rep(NA, times=length(orig$SST4.latband1.prelim2))
  SST4.latband1.prelim2.res <- rep(NA, times=length(orig$SST4.latband1.prelim2.res))

  # --- Correct for period 1: Feb 2000 to 30 October 2000
  uu1 <- which(tt2 == "P1")
  SST4.latband1.prelim2[uu1] <- orig$SST4.latband1.prelim2[uu1] + correct1
  SST4.latband1.prelim2.res[uu1] <- orig$SST4.latband1.prelim2.res[uu1] + correct1

  # --- Correct for period 1: 30 October 2000 to 2 July 2001
  uu2 <- which(tt2 == "P2")
  SST4.latband1.prelim2[uu2] <- orig$SST4.latband1.prelim2[uu2] + correct2
  SST4.latband1.prelim2.res[uu2] <- orig$SST4.latband1.prelim2.res[uu2] + correct2
  
  # --- Correct for period 3: 2 July 2001 to end of record (no correction!)
  uu3 <- which(tt2 == "P3")
  SST4.latband1.prelim2[uu3] <- orig$SST4.latband1.prelim2[uu3] + 0.0
  SST4.latband1.prelim2.res[uu3] <- orig$SST4.latband1.prelim2.res[uu3] + 0.0
   
  # --- Put corrected SST4 values in "orig"
  
  orig$SST4.latband1.prelim2 <- SST4.latband1.prelim2
  orig$SST4.latband1.prelim2.res <- SST4.latband1.prelim2.res

  rm(SST4.latband1.prelim2, SST4.latband1.prelim2.res, uu1,uu2,uu3); gc()

} # End of SST4 corrections for TERRA only


# --- Compute AGAIN monthly medians of SST4 residuals AFTER the correction.

use <- orig$use.4.coeffs.SST4

tt0 <- cut(orig$sat.timedate, breaks="month") # Cut dates by months

tt4 <- tapply(X=orig$SST4.latband1.prelim2.res[use],
  INDEX=tt0[use],
  FUN=median, na.rm=TRUE, simplify=TRUE)      # Calculate median by month

tt4 <- xts(tt4, order.by=seq.months)

plot.zoo(tt4,
  type="l", lwd=2, col="steelblue",
  ylim=c(-0.75, 0),
  main=paste(sensor,"- SST4 Residuals"),
  xlab="Time", las=1, cex.axis=0.7,
  ylab="Median of SST4 Residuals")
points(tt4, pch=16, col="tomato")
abline(h=median(tt4, na.rm=TRUE), col="grey50")

rm(use,tt0,tt1,tt2,tt3,tt4); gc()
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
tt3 <- tt2a | tt2b | tt2c
tt4 <- tt1[!tt3]
rm(list=tt4)

rm(tt1,tt2a,tt2b,tt2c,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# --- Compute statistics of re-estimated (preliminary 2) SST4 latband 1 residuals

rms <- function(x) {
	x <- x[!is.na(x)];
	x2 <- x * x;
	result <- sqrt( sum(x2) / length(x) );
	return(result);
}

tt0 <- orig$SST4.latband1.prelim2.res

use.tr <- orig$use.4.coeffs.SST == TRUE &
  (orig$SST4.latband1.prelim2.res + 0.17) >= -1.5 &
  (orig$SST4.latband1.prelim2.res + 0.17) <= 1.5 &
  orig$train.sst4 == "Train"

tt1 <- round(min(tt0[use.tr], na.rm=TRUE), 3)
tt2 <- round(quantile(tt0[use.tr], probs=0.25, na.rm=TRUE), 3)
tt3 <- round(median(tt0[use.tr], na.rm=TRUE), 3)
tt4 <- round(mean(tt0[use.tr], na.rm=TRUE), 3)
tt5 <- round(quantile(tt0[use.tr], probs=0.75, na.rm=TRUE), 3)
tt6 <- round(max(tt0[use.tr], na.rm=TRUE), 3)
tt7 <- round(rms(tt0[use.tr]), 3)
tt8 <- round(sd(tt0[use.tr], na.rm=TRUE), 3)
tt9 <- round(mad(tt0[use.tr], na.rm=TRUE), 3)
tt10 <- length(tt0[use.tr]) 

tt11 <- data.frame(min=tt1, q1=tt2, median=tt3, mean=tt4,
	q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10, row.names=NULL)


use.tr <- orig$use.4.coeffs.SST == TRUE &
  (orig$SST4.latband1.prelim2.res + 0.17) >= -1.5 &
  (orig$SST4.latband1.prelim2.res + 0.17) <= 1.5 &
  orig$train.sst4 == "Validate"

tt1 <- round(min(tt0[use.tr], na.rm=TRUE), 3)
tt2 <- round(quantile(tt0[use.tr], probs=0.25, na.rm=TRUE), 3)
tt3 <- round(median(tt0[use.tr], na.rm=TRUE), 3)
tt4 <- round(mean(tt0[use.tr], na.rm=TRUE), 3)
tt5 <- round(quantile(tt0[use.tr], probs=0.75, na.rm=TRUE), 3)
tt6 <- round(max(tt0[use.tr], na.rm=TRUE), 3)
tt7 <- round(rms(tt0[use.tr]), 3)
tt8 <- round(sd(tt0[use.tr], na.rm=TRUE), 3)
tt9 <- round(mad(tt0[use.tr], na.rm=TRUE), 3)
tt10 <- length(tt0[use.tr]) 

tt11 <- data.frame(min=tt1, q1=tt2, median=tt3, mean=tt4,
	q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10, row.names=NULL)

rm(use.tr,tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10);



tt0 <- orig$SST4.latband1.gam.res

use.tr <- orig$use.4.coeffs.SST == TRUE &
  (orig$SST4.latband1.gam.res + 0.17) >= -1.5 &
  (orig$SST4.latband1.gam.res + 0.17) <= 1.5 &
  orig$train.sst4 == "Train"

tt1 <- round(min(tt0[use.tr], na.rm=TRUE), 3)
tt2 <- round(quantile(tt0[use.tr], probs=0.25, na.rm=TRUE), 3)
tt3 <- round(median(tt0[use.tr], na.rm=TRUE), 3)
tt4 <- round(mean(tt0[use.tr], na.rm=TRUE), 3)
tt5 <- round(quantile(tt0[use.tr], probs=0.75, na.rm=TRUE), 3)
tt6 <- round(max(tt0[use.tr], na.rm=TRUE), 3)
tt7 <- round(rms(tt0[use.tr]), 3)
tt8 <- round(sd(tt0[use.tr], na.rm=TRUE), 3)
tt9 <- round(mad(tt0[use.tr], na.rm=TRUE), 3)
tt10 <- length(tt0[use.tr]) 

tt11 <- data.frame(min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10, row.names=NULL)


use.tr <- orig$use.4.coeffs.SST == TRUE &
  (orig$SST4.latband1.gam.res + 0.17) >= -1.5 &
  (orig$SST4.latband1.gam.res + 0.17) <= 1.5 &
  orig$train.sst4 == "Validate"

tt1 <- round(min(tt0[use.tr], na.rm=TRUE), 3)
tt2 <- round(quantile(tt0[use.tr], probs=0.25, na.rm=TRUE), 3)
tt3 <- round(median(tt0[use.tr], na.rm=TRUE), 3)
tt4 <- round(mean(tt0[use.tr], na.rm=TRUE), 3)
tt5 <- round(quantile(tt0[use.tr], probs=0.75, na.rm=TRUE), 3)
tt6 <- round(max(tt0[use.tr], na.rm=TRUE), 3)
tt7 <- round(rms(tt0[use.tr]), 3)
tt8 <- round(sd(tt0[use.tr], na.rm=TRUE), 3)
tt9 <- round(mad(tt0[use.tr], na.rm=TRUE), 3)
tt10 <- length(tt0[use.tr]) 

tt11 <- data.frame(min=tt1, q1=tt2, median=tt3, mean=tt4,
	q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10, row.names=NULL)


