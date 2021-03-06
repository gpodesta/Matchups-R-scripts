

# ---------------------------------------------------------------------------------------#
# --- Load required packages ----

if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(lubridate)) install.packages("lubridate")
if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(xts)) install.packages("xts")
if (!require(caret)) install.packages("caret")
if (!require(plotmo)) install.packages("plotmo")
if (!require(stringr)) install.packages("stringr")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define sensor and calibration collection on which we will be working ----

sensor <- "AQUA"        # Select MODIS onboard AQUA or TERRA
# sensor <- "TERRA"			# Select MODIS onboard AQUA or TERRA

collection <- 6				  # Calibration collection

cat("\n Working on MODIS onboard",sensor,"collection",collection,"...\n")
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
# ----------------------------------------------------------------------------------------
# --- PART 1. Growing a NIGHTTIME cloud-contamination decision tree ----
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Select nightime data ----
# --- N.B. The solar zenith angle used as day-nite boundary is 90 degrees for MODIS
# --- A value of 80 is used for AVHRR.

nite <- orig$solz > 90 & !is.na(orig$cen.3750) & !is.na(orig$cen.4050)

table(nite, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define variables to be used in nightime tree estimation ----

d3 <- (orig$max.3959[nite] - orig$min.3959[nite])		  # max minus min BT ch22 inside box
d4 <- (orig$max.4050[nite] - orig$min.4050[nite])  	  # max minus min BT ch23 inside box
d11 <- (orig$max.11000[nite] - orig$min.11000[nite])	# max minus min BT ch31 inside box
d12 <- (orig$max.12000[nite] - orig$min.12000[nite])	# max minus min BT ch32 inside box

d34 <- orig$cen.39.40.ref.new[nite]   # residuals from standardized difference between BT22 and BT23

d311 <- orig$cen.3959[nite] - orig$cen.11000[nite]    # BT22 - BT31
d312 <- orig$cen.3959[nite] - orig$cen.12000[nite]    # BT22 - BT32
d411 <- orig$cen.4050[nite] - orig$cen.11000[nite]    # BT23 - BT31
d412 <- orig$cen.4050[nite] - orig$cen.12000[nite]    # BT23 - BT32
d1112 <- orig$cen.11000[nite] - orig$cen.12000[nite]  # BT31 - BT32

satz <- abs(orig$satz[nite])			# satellite zenith angle (absolute value)
satzint <- cut(abs(orig$satz[nite]),
  breaks=c(0, 30, 40, 50, max(abs(orig$satz[nite])) + 0.01),
  include.lowest = TRUE)

lat <- orig$sat.lat[nite]
latband <- orig$latband[nite]

delta.sst <- abs(orig$buoy.sst[nite] - orig$ref.type.1.SST[nite])		# Absolute difference between buoy and reference SST
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define "good" SST residuals ---- 
# --- Note we grow the tree using preliminary SST estimates

# --- Add 0.17 deg to the residuals so they are centered around zero 
# --- and the deviation tests are symmetrical.

sst.res.use <- orig$SST.latband1.prelim2.res[nite] + 0.17

# --- Define a threshold to consider residuals "bad" or "good"

resid.threshold <- 0.75  	# Threshold to consider residuals good or bad

# --- Create a factor for the SST residuals

res.class <- ifelse((sst.res.use < -1 * resid.threshold) |
  (sst.res.use > resid.threshold),
  "Bad", "Good")

res.class <- ordered(res.class,
  levels=c("Bad","Good"),
  labels=c("Bad","Good"))

table(res.class, useNA="always")

hist(sst.res.use, main="Aqua Nite SST residuals"); box()

library(Hmisc)
Ecdf(sst.res.use, what="F", xlim=c(-1.5, 1.5),
  xlab="AQUA SST latband1 prelim 2 residuals", add=FALSE)

abline(v=c(-1.0, 1.0))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define a data frame to be used as input for tree estimation ----

nite.input.data <- data.frame(res.class, sst.res.use,
  d3,d4,d11,d12,
  d34,d311,d312,d411,d412,d1112,
  satz, lat, latband, delta.sst)

# --- Eliminate rows with any missing variables

nite.input.data <- na.omit(nite.input.data)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Divide available data into training and validation sets ----
# --- First, count the names of "good" and "bad" records

n.bad <- nrow(subset(nite.input.data, res.class == "Bad"))
n.good <- nrow(subset(nite.input.data, res.class == "Good"))

if ((n.bad + n.good) != nrow(nite.input.data))
  stop("Problem with values of column RES.CLASS in nite.input.data ...\n")

# --- Decide proportion of matchups used in fitting the tree

prop.training <- 0.60																	# Proportion of matchups in the training set
n.training.bad <- round((n.bad * prop.training), 0)		# No. of "bad" matchups to be used for training
n.training.good <- round((n.good * prop.training), 0)	# No. of "good" matchups to be used for training

# --- Sample matchups proportionally to the number of good and bad SSTs

set.seed(284)

which.bad <- which(nite.input.data$res.class == "Bad")  # Which records are "bad"
which.good <- which(nite.input.data$res.class == "Good") # Which records are "good"

samp.bad <- sample(which.bad, size=n.training.bad, replace=F)
samp.good <- sample(which.good, size=n.training.good, replace=F)
yy1 <- sort(c(samp.bad, samp.good))

set.type <- rep("Validation", length=nrow(nite.input.data))
set.type <- replace(set.type, yy1, "Training")
table(set.type)

# --- Add variable defining if matchup is used for tree "Training" or "Validation" 

nite.input.data <- data.frame(nite.input.data, set.type=set.type)

table(nite.input.data[,"res.class"], nite.input.data[,"set.type"])

addmargins(xtabs(~ res.class + set.type, data=nite.input.data))   # add row/col summary (default is sum)
addmargins(prop.table(xtabs(~ res.class + set.type, data=nite.input.data)))   # show counts as proportions of total

rm(n.good, n.bad, prop.training, n.training.good, n.training.bad)
rm(which.good, which.bad, samp.good, samp.bad, yy1, set.type); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Grow the nighttime tree ----

# We use a loss matrix because error sof type I and type II are not equally important.
# WARNING: rpart orders factors according to alphabetic order, unless they
# have been pre-ordered. Just in case, We ordered them so "BAD" is before "GOOD".
# see http://www.louisaslett.com/Courses/Data_Mining_09-10/ST4003-Lab4-New_Tree_Data_Set_and_Loss_Matrices.pdf

# the error matrix is constructed as (with POSITIVE being BAD):
#                   predicted BAD(+)    predicted GOOD(-)
# actually BAD(+)        TP              FN
# actually GOOD(-)       FP              TN

# --- An asymmetric cost or loss matrix can be specified.
# --- Loss matrix is specified as:
# ---  ** row1:      0         loss_FN
# ---  ** row2:  Loss_FP           0
# --- LOSS_FN: loss value for "False Negatives" (positives predicted as negatives)
# --- LOSS_FP: loss value for "False Positives" (negatives predicted as positives)

# example loss.matrix <- matrix(c(0, 4, 1, 0), byrow=TRUE, nrow=2)
#       [,1] [,2]
#   [1,]    0    4
#   [2,]    1    0
# Here the cost of False Negatives is 4 time higher than that of False Positives

#loss.matrix <- matrix(c(0, 1, 1, 0), byrow=TRUE, nrow=2)
#loss.matrix <- matrix(c(0, 3, 1, 0), byrow=TRUE, nrow=2)
#loss.matrix <- matrix(c(0, 4, 1, 0), byrow=TRUE, nrow=2)
loss.matrix <- matrix(c(0, 5, 1, 0), byrow=TRUE, nrow=2)
#loss.matrix <- matrix(c(0, 6, 1, 0), byrow=TRUE, nrow=2)
#loss.matrix <- matrix(c(0, 7, 1, 0), byrow=TRUE, nrow=2)
#loss.matrix <- matrix(c(0, 9, 1, 0), byrow=TRUE, nrow=2)


library(rpart)
library(rpart.plot)

nite.tree <- rpart(res.class ~ 
  d3 + d4 + d11 + d12 +
  d34 + d311 + d312 + d411 + d412 + d1112 +
  satz + delta.sst,
  data=nite.input.data,
  subset=(set.type == "Training"),
  method = "class",
  x=TRUE, y=TRUE,							 
  na.action=na.rpart,
  parms=list(loss=loss.matrix),
  control=rpart.control(maxsurrogate=0, cp=0.005,
  minbucket=50, xval=10) )

rpart.plot(x=nite.tree, type=1, extra=1,
  under=TRUE,uniform=TRUE, digits=6,
  main=paste(sensor, "-", geophys.var, "- Night tree"))

# --- Alternative tree-plotting...

plot(nite.tree, uniform=TRUE)
text(nite.tree, cex=0.6)
title(paste(sensor, " - ",geophys.var,"Night tree"))

# --- Check predicted classes for night TRAINING sample

rm(pred.test, pred.vals, pred.class)
newdata <- subset(nite.input.data, set.type=="Training")

# --- Get actual class for data set used in prediction

actual.class <- ordered(newdata$res.class,
  levels=c("Bad","Good"), labels=c("Bad","Good"))  		# Actual (true) class

table(actual.class, useNA="always")

# --- Now use fitted tree to predict class for new data set

pred.test <- predict(nite.tree, newdata=newdata, type="matrix")

pred.class <- ordered(pred.test[,1], levels=c(1, 2), labels=c("P-Bad","P-Good"))
names(pred.class) <- NULL

prob.bad <- round(pred.test[ ,4], 4)
prob.good <- round(pred.test[ ,5], 4)

pred.vals <- data.frame(actual.class=actual.class, pred.class=pred.class,
  prob.bad=prob.bad, prob.good=prob.good)

# --- Build a confusion matrix
# --- This assumes the table has predicted values in the rows
# --- and actual (observed) values in the columns.

# --- function confusionMatrix in package "caret" requires that
# --- predicted and actual classes have the same values.
# --- We convert "P-Good" into "good", and "P-Bad" into "Bad".

pred.class2 <- ifelse(pred.class == "P-Bad", "Bad", "Good")
pred.class2 <- ordered(pred.class2, levels=c("Bad","Good"), labels=c("Bad","Good"))

tt1 <- confusionMatrix(data=pred.class2, reference=actual.class, positive="Bad")
tt1

ttt <- table(pred.vals[,"pred.class"], pred.vals[,"actual.class"], exclude=NA)
ttt

# --- Specify levels for actual and predicted positive & negative values
# --- in the 2x2 confusion matrix

actual.pos <- "Bad"
actual.neg <- "Good"
pred.pos <-"P-Bad" 
pred.neg <- "P-Good"

# True positive: positive correctly predicted as positive
# False positive: positive incorrectly predicted as negative
# True negative: negative correctly predicted as negative
# False negative: negative incorrectly predicted as positive

TP <- ttt[pred.pos, actual.pos]		# True positive
FP <- ttt[pred.pos, actual.neg]   # False positive (Type I error)
TN <- ttt[pred.neg, actual.neg]   # True negative
FN <- ttt[pred.neg, actual.pos]   # False negative (Type II error)         

# --- Calculate metrics for confusion matrix

# Sensitivity: it is the proportion of true positives that are correctly identified by the test.
# Sensitivity relates to the test's ability to identify positive results.
# If a test has high sensitivity then a negative result would suggest the absence of positive
# Specificity: it is the proportion of true negatives that are correctly identified by the test.
# Specificity relates to the ability of the test to identify negative results.
# If a test has high specificity, a positive predicted result means a high probability of positive
# Positive predictive value (PPV) is the proportion of cases with predicted positive values that are actually positive.
# Negative predictive value (NPV) is the proportion of cases with predicted negative values that are actually negative.

sensitivity <- TP / (TP + FN); # or True Positive Rate

specificity <- TN / (TN + FP)	# or True Negative Rate

misclassification.rate <- (FP + FN) / (TP + FN + FP + TN)

prevalence <- (TP + FN) / (TP + FN + FP + TN)

PPV <- (sensitivity * prevalence) / ((sensitivity * prevalence) +
  ((1 - specificity) * (1 - prevalence)))   #  TN /(FN + TN)

NPV <-  (specificity * (1 - prevalence)) / (((1 - sensitivity) * prevalence) +
  ((specificity) * (1 - prevalence)))

detection.rate <- TP / (TP + FN + FP + TN)

detection.prevalence <- (TP + FP) / (TP + FN + FP + TN)

likelihood.ratio.positive <- sensitivity  / (1 - specificity);

likelihood.ratio.negative <- (1 - sensitivity) / specificity;

accuracy <- (TP + TN) / (TP + FN + FP + TN)

confusion.matrix.results <- round(data.frame(sensitivity, specificity,
  misclassification.rate,PPV,NPV,detection.rate,detection.prevalence,
  likelihood.ratio.positive,likelihood.ratio.negative, accuracy), 3)

ttt

confusion.matrix.results[,c(3,1,2,4,5)]


rm(actual.pos, actual.neg, false.pos, false.neg)
rm(TP,FP,TN,FN)
rm(specificity, sensitivity, misclassification.rate,PPV,NPV,likelihood.ratio.positive,likelihood.ratio.negative);
rm(prevalence, detection.rate, detection.prevalence)
rm(pred.test,pred.class,prob.good,prob.bad, ttt);
# --------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------
# --- Predict values for night VALIDATION set using RPART nightime tree ----
# --- to get independent estimate of performance.
# --- Note we must use the predict.rpart() function.

rm(newdata, pred.test, pred.vals, pred.class);gc()
rm(pred.test, pred.vals, pred.class)

newdata <- subset(nite.input.data, set.type == "Validation")

# --- Get actual class for data set used in prediction

actual.class <- ordered(newdata$res.class,
  levels=c("Bad","Good"), labels=c("Bad","Good"))    	# Actual (true) class

table(actual.class, useNA="always")

# --- Now use fitted tree to predict class for the VALIDATION data set

pred.test <- predict(nite.tree, newdata=newdata, type="matrix")

pred.class <- ordered(pred.test[,1], levels=c(1, 2), labels=c("P-Bad","P-Good"))
names(pred.class) <- NULL

prob.bad <- round(pred.test[ ,4], 4)
prob.good <- round(pred.test[ ,5], 4)

pred.vals <- data.frame(actual.class=actual.class, pred.class=pred.class,
  prob.bad=prob.bad, prob.good=prob.good)

# --- Build a confusion matrix
# --- This assumes the table has predicted values in the rows
# --- and actual (observed) values in the columns.

# --- function confusionMatrix in package "caret" requires that
# --- predicted and actual classes have the same values.
# --- We convert "P-Good" into "good", and "P-Bad" into "Bad".

pred.class2 <- ifelse(pred.class == "P-Bad", "Bad", "Good")
pred.class2 <- ordered(pred.class2, levels=c("Bad","Good"), labels=c("Bad","Good"))

tt1 <- confusionMatrix(data=pred.class2, reference=actual.class, positive="Bad")
tt1

ttt <- table(pred.vals[,"pred.class"], pred.vals[,"actual.class"], exclude=NA)
ttt

# --- Specify levels for actual and predicted positive & negative values
# --- in the 2x2 confusion matrix

actual.pos <- "Bad"
actual.neg <- "Good"
pred.pos <-"P-Bad" 
pred.neg <- "P-Good"

# True positive: positive correctly predicted as positive
# False positive: positive incorrectly predicted as negative
# True negative: negative correctly predicted as negative
# False negative: negative incorrectly predicted as positive

TP <- ttt[pred.pos, actual.pos]		# True positive
FP <- ttt[pred.pos, actual.neg]   # False positive (Type I error)
TN <- ttt[pred.neg, actual.neg]   # True negative
FN <- ttt[pred.neg, actual.pos]   # False negative (Type II error)         

# --- Calculate metrics for confusion matrix

# Sensitivity: it is the proportion of true positives that are correctly identified by the test.
# Sensitivity relates to the test's ability to identify positive results.
# If a test has high sensitivity then a negative result would suggest the absence of positive
# Specificity: it is the proportion of true negatives that are correctly identified by the test.
# Specificity relates to the ability of the test to identify negative results.
# If a test has high specificity, a positive predicted result means a high probability of positive
# Positive predictive value (PPV) is the proportion of cases with predicted positive values that are actually positive.
# Negative predictive value (NPV) is the proportion of cases with predicted negative values that are actually negative.

sensitivity <- TP / (TP + FN); # or True Positive Rate

specificity <- TN / (TN + FP)	# or True Negative Rate

misclassification.rate <- (FP + FN) / (TP + FN + FP + TN)

prevalence <- (TP + FN) / (TP + FN + FP + TN)

PPV <- (sensitivity * prevalence) / ((sensitivity * prevalence) +
  ((1 - specificity) * (1 - prevalence)))   #  TN /(FN + TN)

NPV <-  (specificity * (1 - prevalence)) / (((1 - sensitivity) * prevalence) +
  ((specificity) * (1 - prevalence)))

detection.rate <- TP / (TP + FN + FP + TN)

detection.prevalence <- (TP + FP) / (TP + FN + FP + TN)

likelihood.ratio.positive <- sensitivity  / (1 - specificity);

likelihood.ratio.negative <- (1 - sensitivity) / specificity;

accuracy <- (TP + TN) / (TP + FN + FP + TN)

confusion.matrix.results <- round(data.frame(sensitivity, specificity,
  misclassification.rate,PPV,NPV,detection.rate,detection.prevalence,
  likelihood.ratio.positive,likelihood.ratio.negative, accuracy), 3)

ttt

confusion.matrix.results[,c(3,1,2,4,5)]

# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save results from nighttime tree in object "predicted.categories" ----
# -- Use nightime tree to predict results for ALL nite matchups (training & validation)

pred.test <- as.data.frame(predict(nite.tree, newdata=nite.input.data, type="matrix"))

actual.class <- nite.input.data$res.class
pred.class <- factor(pred.test[,"V1"], levels=c(1, 2), labels=c("P-Bad", "P-Good"))
prob.bad <- round(pred.test[ ,"V4"], 4)
prob.good <- round(pred.test[ ,"V5"], 4)

pred.vals.nite <- data.frame(SST.actual.class=actual.class,
  SST.pred.class=pred.class,
  SST.prob.bad=prob.bad,
  SST.prob.good=prob.good)

# --- Create object where both night and daytime tree results will be stored

predicted.categories <- as.data.frame(matrix(data=NA,
  nrow=nrow(orig), ncol=ncol(pred.vals.nite),
  dimnames=list(NULL, c("SST.actual.class","SST.tree.class",
  "SST.tree.prob.BAD", "SST.tree.prob.GOOD"))))		# Store predicted categories here

# --- Store results in object "predicted.categories"

predicted.categories[nite, ] <- pred.vals.nite

table(predicted.categories$SST.actual.class, useNA="always")
table(predicted.categories$SST.tree.class, useNA="always")

# --- Plot probabilities of being good
save.plots <- TRUE

if (save.plots) {
  #wmf.graph(paste(graph.outdir,sensor,"-nite.tree.barplot.wmf", sep=""))
  #pdf.graph(paste(graph.outdir,sensor,"-nite.tree.barplot.pdf", sep=""))
  
  png(filename=paste(graph.outdir,sensor,"-nite.tree.barplot.wmf", sep=""),
      width = 480, height = 480, units = "px")
  
  tt1 <- table(pred.vals.nite[,"SST.prob.good"])
  tt2 <- as.character(round(as.numeric(names(tt1)),3))
  barplot(tt1, names=tt2,
    xlab="Probability of being GOOD",
    ylab="Number of matchups",
    main=paste(sensor, "Nighttime Tree"),
    col="goldenrod", las=2)  
  box()
  
  
  dev.off()
  rm(tt1,tt2)
}	

hist(pred.vals.nite[,"SST.prob.good"],
  breaks=seq(0,1,0.1),
  xlim=c(0,1),
  xlab="Probability of GOOD",
  main=paste(sensor,"Night tree - P(good)"),
  col="tomato", freq=FALSE)
box()

rm(newdata,pred.test,pred.class,prob.good,prob.bad, pred.vals, nite)
rm(loss.matrix, nite.input.data)
rm(d11,d1112,d12,d3,d311,d312,d34,d4,d411,d412,delta.sst,satz,satzint,sst.res.use,tt1)
rm(loss.matrix,confusion.matrix.results, lat,latband, nite, nite.tree)
rm(actual.pos, actual.neg, false.pos, false.neg, actual.class, res.class)
rm(pred.neg,pred.pos)
rm(TP,FP,TN,FN)
rm(specificity, sensitivity, misclassification.rate,PPV,NPV,
   likelihood.ratio.positive,likelihood.ratio.negative, accuracy);
rm(prevalence, detection.rate, detection.prevalence)
rm(pred.test,pred.class,pred.class2,prob.good,prob.bad,pred.vals,newdata,
   nite.input.data, ttt); gc()
# ----------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------#
# ----------------------------------------------------------------------------------------
# --- PART 2. Growing a DAYTIME cloud-contamination decision tree ----
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Select DAYTIME data ----
# --- N.B. The solar zenith angle used as day-nite boundary is 90 degrees for MODIS
# --- A value of 80 is used for AVHRR.

day <- orig$solz <= 90

table(day, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define variables to be used in daytime tree estimation ----

c6 <- orig$cen.678[day]     # MODIS Channel 14
d6 <- (orig$max.678[day] - orig$min.678[day])     # max minus min channel 14
# sd6 <- orig$sd.678[day]                         # NOT USED

c13 <- orig$cen.1380[day]   # MODIS channel 26
d13 <- (orig$max.1380[day] - orig$min.1380[day])  # max minus min channel 26
# sd13 <- orig$sd.1380[day]                       # NOT USED

d11 <- (orig$max.11000[day] - orig$min.11000[day])      # max minus min BT 31
d12 <- (orig$max.12000[day] - orig$min.12000[day])	    # max minus min BT 32
d1112 <- (orig$cen.11000[day] - orig$cen.12000[day])    # BT31 - BT32

# --- We check if we are in a sun glint region

if (matchup.format == "OLD") {
  in.glint <- ifelse(orig$glint[day] > 0, TRUE, FALSE)     # Are we in a sunglint region?
} else if (matchup.format == "NEW") {
  in.glint <- ifelse(orig$glint[day] > 0.005, TRUE, FALSE) # Are we in a sunglint region
} else {
  stop("ERROR: Matchup format is not defined\n")
}  

solz <- orig$solz[day]

satz <- abs(orig$satz[day])

satzint <- cut(abs(orig$satz[day]),
  breaks=c(0, 30, 40, 50, max(abs(orig$satz[day])) + 0.01),
  include.lowest = TRUE)

latband <- orig$latband[day]

# --- Absolute difference between buoy and reference SST
delta.sst <- abs(orig$buoy.sst[day] - orig$ref.type.1.SST[day])
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define "good" residuals as those with abs(resid) < threshold deg C ----

sst.res.use <- orig$SST.latband1.prelim2.res[day] + 0.17

# --- Define a threshold to consider residuals bad or good

resid.threshold <- 0.75    # Threshold to consider residuals good or bad

# --- Create a factor for the SST residuals

res.class <- ifelse((sst.res.use < -1 * resid.threshold) |
  (sst.res.use > resid.threshold),
  "Bad", "Good")

res.class <- ordered(res.class,
  levels=c("Bad","Good"),
  labels=c("Bad","Good"))

addmargins(table(res.class, useNA="always"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define a data frame to be used as input for tree estimation ----

day.input.data <- data.frame(res.class, sst.res.use,
  c6, d6, c13, d13, d11, d12, d1112,
  in.glint, solz, satz, latband, delta.sst)

# --- Eliminate rows with any missing variables

#day.input.data <- na.omit(day.input.data)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Divide available daytime data into training and validation sets ----
# --- First, count the names of "good" and "bad" records

n.bad <- nrow(subset(day.input.data, res.class == "Bad"))
n.good <- nrow(subset(day.input.data, res.class == "Good"))

if ((n.bad + n.good) != nrow(day.input.data))
  stop("Problem with values of column RES.CLASS in day.input.data ...\n")

# --- Decide proportion of matchups used in fitting the tree

prop.training <- 0.60  																# Proportion of matchups in the training set
n.training.bad <- round((n.bad * prop.training), 0)		# No. of "bad" matchups to be used for training
n.training.good <- round((n.good * prop.training), 0)	# No. of "good" matchups to be used for training

# --- Sample matchups proportionally to the number of good and bad SSTs

set.seed(284)

which.bad <- which(day.input.data$res.class == "Bad")  # Which records are "bad"
which.good <- which(day.input.data$res.class == "Good") # Which records are "good"

samp.bad <- sample(which.bad, size=n.training.bad, replace=F)
samp.good <- sample(which.good, size=n.training.good, replace=F)
yy1 <- sort(c(samp.bad, samp.good))

set.type <- rep("Validation", length=nrow(day.input.data))
set.type <- replace(set.type, yy1, "Training")
table(set.type)

# --- Add variable defining if matchup is used for tree "Training" or "Validation" 

day.input.data <- data.frame(day.input.data, set.type=set.type)

table(day.input.data[,"res.class"], day.input.data[,"set.type"])

addmargins(xtabs(~ res.class + set.type, data=day.input.data))   # add row/col summary (default is sum)
addmargins(prop.table(xtabs(~ res.class + set.type, data=day.input.data)))   # show counts as proportions of total

rm(n.good, n.bad, prop.training, n.training.good, n.training.bad)
rm(which.good, which.bad, samp.good, samp.bad, yy1, set.type); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Grow the daytime tree ----

# We use a loss matrix because error sof type I and type II are not equally important.
# WARNING: rpart orders factors according to alphabetic order, unless they
# have been pre-ordered. Just in case, We ordered them so "BAD" is before "GOOD".
# see http://www.louisaslett.com/Courses/Data_Mining_09-10/ST4003-Lab4-New_Tree_Data_Set_and_Loss_Matrices.pdf

# the error matrix is constructed as (with POSITIVE being BAD):
#                   predicted BAD(+)    predicted GOOD(-)
# actually BAD(+)        TP              FN
# actually GOOD(-)       FP              TN

# --- An asymmetric cost or loss matrix can be specified.
# --- Loss matrix is specified as:
# ---  ** row1:      0         loss_FN
# ---  ** row2:  Loss_FP           0
# --- LOSS_FN: loss value for "False Negatives" (positives predicted as negatives)
# --- LOSS_FP: loss value for "False Positives" (negatives predicted as positives)

# example loss.matrix <- matrix(c(0, 4, 1, 0), byrow=TRUE, nrow=2)
#       [,1] [,2]
#   [1,]    0    4
#   [2,]    1    0
# Here the cost of False Negatives is 4 time higher than that of False Positives

#loss.matrix <- matrix(c(0, 1, 1, 0), byrow=TRUE, nrow=2)
loss.matrix <- matrix(c(0, 3, 1, 0), byrow=TRUE, nrow=2)
#loss.matrix <- matrix(c(0, 4, 1, 0), byrow=TRUE, nrow=2)
#loss.matrix <- matrix(c(0, 5, 1, 0), byrow=TRUE, nrow=2)
#loss.matrix <- matrix(c(0, 6, 1, 0), byrow=TRUE, nrow=2)

day.tree <- rpart(res.class ~ 
  c6 + d6 + c13 + d13 + d11 + d12 + d1112 +
  in.glint + solz + satz + latband + delta.sst,
  data=day.input.data,
  subset=(set.type == "Training"),
  method = "class",
  x=TRUE, y=TRUE,  						 
  na.action=na.rpart,
  parms=list(loss=loss.matrix),
  control=rpart.control(maxsurrogate=0, cp=0.005,
  minbucket=50, xval=10) )

rpart.plot(x=day.tree, type=1, extra=1,
  under=TRUE,uniform=TRUE, digits=6,
  main=paste(sensor, "-", geophys.var, "- Daytime tree"))

# --- Alternative tree-plotting...

plot(day.tree, uniform=TRUE)
text(day.tree, cex=0.6)
title(paste(sensor, " - ", geophys.var,"Daytime tree"))

# --- Check predicted classes for daytime TRAINING sample

rm(pred.test, pred.vals, pred.class)
newdata <- subset(day.input.data, set.type == "Training")

# --- Get actual class for data set used in prediction

actual.class <- ordered(newdata$res.class,
  levels=c("Bad","Good"), labels=c("Bad","Good"))  # Actual (true) class

table(actual.class, useNA="always")

# --- Now use fitted tree to predict class for new data set

pred.test <- predict(day.tree, newdata=newdata, type="matrix")

pred.class <- ordered(pred.test[,1], levels=c(1, 2), labels=c("P-Bad","P-Good"))
names(pred.class) <- NULL

prob.bad <- round(pred.test[ ,4], 4)
prob.good <- round(pred.test[ ,5], 4)

pred.vals <- data.frame(actual.class=actual.class, pred.class=pred.class,
  prob.bad=prob.bad, prob.good=prob.good)

# --- Build a confusion matrix
# --- This assumes the table has predicted values in the rows
# --- and actual (observed) values in the columns.

# --- function confusionMatrix in package "caret" requires that
# --- predicted and actual classes have the same values.
# --- We convert "P-Good" into "good", and "P-Bad" into "Bad".

pred.class2 <- ifelse(pred.class == "P-Bad", "Bad", "Good")
pred.class2 <- ordered(pred.class2, levels=c("Bad","Good"), labels=c("Bad","Good"))

tt1 <- confusionMatrix(data=pred.class2, reference=actual.class, positive="Bad")
tt1

ttt <- table(pred.vals[,"pred.class"], pred.vals[,"actual.class"], exclude=NA)
ttt

# --- Specify levels for actual and predicted positive & negative values
# --- in the 2x2 confusion matrix

actual.pos <- "Bad"
actual.neg <- "Good"
pred.pos <-"P-Bad" 
pred.neg <- "P-Good"

# True positive: positive correctly predicted as positive
# False positive: positive incorrectly predicted as negative
# True negative: negative correctly predicted as negative
# False negative: negative incorrectly predicted as positive

TP <- ttt[pred.pos, actual.pos]		# True positive
FP <- ttt[pred.pos, actual.neg]   # False positive (Type I error)
TN <- ttt[pred.neg, actual.neg]   # True negative
FN <- ttt[pred.neg, actual.pos]   # False negative (Type II error)         

# --- Calculate metrics for confusion matrix

sensitivity <- TP / (TP + FN); # or True Positive Rate

specificity <- TN / (TN + FP)	# or True Negative Rate

misclassification.rate <- (FP + FN) / (TP + FN + FP + TN)

prevalence <- (TP + FN) / (TP + FN + FP + TN)

PPV <- (sensitivity * prevalence) / ((sensitivity * prevalence) +
  ((1 - specificity) * (1 - prevalence)))   #  TN /(FN + TN)

NPV <-  (specificity * (1 - prevalence)) / (((1 - sensitivity) * prevalence) +
  ((specificity) * (1 - prevalence)))

detection.rate <- TP / (TP + FN + FP + TN)

detection.prevalence <- (TP + FP) / (TP + FN + FP + TN)

likelihood.ratio.positive <- sensitivity  / (1 - specificity)

likelihood.ratio.negative <- (1 - sensitivity) / specificity

accuracy <- (TP + TN) / (TP + FN + FP + TN)

confusion.matrix.results <- round(data.frame(sensitivity, specificity,
  misclassification.rate,PPV,NPV,detection.rate,detection.prevalence,
  likelihood.ratio.positive,likelihood.ratio.negative, accuracy), 3)

ttt

confusion.matrix.results[,c(3,1,2,4,5)]


rm(actual.pos, actual.neg, false.pos, false.neg)
rm(TP,FP,TN,FN)
rm(specificity, sensitivity, misclassification.rate,PPV,NPV,
  likelihood.ratio.positive,likelihood.ratio.negative)
rm(prevalence, detection.rate, detection.prevalence)
rm(pred.test,pred.class,prob.good,prob.bad,ttt);gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Predict values for VALIDATION set using RPART daytime tree ----
# --- to get independent estimate of performance.
# --- Note we must use the predict.rpart() function.

rm(newdata, pred.test, pred.vals, pred.class);gc()

newdata <- subset(day.input.data, set.type == "Validation")

# --- Get actual class for data set used in prediction

actual.class <- ordered(newdata$res.class,
  levels=c("Bad","Good"), labels=c("Bad","Good"))    	# Actual (true) class

table(actual.class, useNA="always")

# --- Now use fitted tree to predict class for the VALIDATION data set

pred.test <- predict(day.tree, newdata=newdata, type="matrix")

pred.class <- ordered(pred.test[,1], levels=c(1, 2), labels=c("P-Bad","P-Good"))
names(pred.class) <- NULL

prob.bad <- round(pred.test[ ,4], 4)
prob.good <- round(pred.test[ ,5], 4)

pred.vals <- data.frame(actual.class=actual.class, pred.class=pred.class,
  prob.bad=prob.bad, prob.good=prob.good)

# --- Build a confusion matrix
# --- This assumes the table has predicted values in the rows
# --- and actual (observed) values in the columns.

# --- function confusionMatrix in package "caret" requires that
# --- predicted and actual classes have the same values.
# --- We convert "P-Good" into "good", and "P-Bad" into "Bad".

pred.class2 <- ifelse(pred.class == "P-Bad", "Bad", "Good")
pred.class2 <- ordered(pred.class2, levels=c("Bad","Good"), labels=c("Bad","Good"))

tt1 <- confusionMatrix(data=pred.class2, reference=actual.class, positive="Bad")
tt1

ttt <- table(pred.vals[,"pred.class"], pred.vals[,"actual.class"], exclude=NA)
ttt

# --- Specify levels for actual and predicted positive & negative values
# --- in the 2x2 confusion matrix

actual.pos <- "Bad"
actual.neg <- "Good"
pred.pos <-"P-Bad" 
pred.neg <- "P-Good"

# True positive: positive correctly predicted as positive
# False positive: positive incorrectly predicted as negative
# True negative: negative correctly predicted as negative
# False negative: negative incorrectly predicted as positive

TP <- ttt[pred.pos, actual.pos]		# True positive
FP <- ttt[pred.pos, actual.neg]   # False positive (Type I error)
TN <- ttt[pred.neg, actual.neg]   # True negative
FN <- ttt[pred.neg, actual.pos]   # False negative (Type II error)         

# --- Calculate metrics for confusion matrix

sensitivity <- TP / (TP + FN); # or True Positive Rate

specificity <- TN / (TN + FP)	# or True Negative Rate

misclassification.rate <- (FP + FN) / (TP + FN + FP + TN)

prevalence <- (TP + FN) / (TP + FN + FP + TN)

PPV <- (sensitivity * prevalence) / ((sensitivity * prevalence) +
  ((1 - specificity) * (1 - prevalence)))   #  TN /(FN + TN)

NPV <-  (specificity * (1 - prevalence)) / (((1 - sensitivity) * prevalence) +
  ((specificity) * (1 - prevalence)))

detection.rate <- TP / (TP + FN + FP + TN)

detection.prevalence <- (TP + FP) / (TP + FN + FP + TN)

likelihood.ratio.positive <- sensitivity  / (1 - specificity);

likelihood.ratio.negative <- (1 - sensitivity) / specificity;

accuracy <- (TP + TN) / (TP + FN + FP + TN)

confusion.matrix.results <- round(data.frame(sensitivity, specificity,
  misclassification.rate,PPV,NPV,detection.rate,detection.prevalence,
  likelihood.ratio.positive,likelihood.ratio.negative, accuracy), 3)

ttt

confusion.matrix.results[,c(3,1,2,4,5)]

# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save results from daytime tree in object "predicted.categories" ----
# -- Use daytime tree to predict results for ALL day matchups (training & validation)

pred.test <- as.data.frame(predict(day.tree, newdata=day.input.data, type="matrix"))

actual.class <- day.input.data$res.class

pred.class <- ordered(pred.test[,"V1"], levels=c(1, 2), labels=c("P-Bad", "P-Good"))
prob.bad <- round(pred.test[ ,"V4"], 4)
prob.good <- round(pred.test[ ,"V5"], 4)

pred.vals.day <- data.frame(SST.actual.class=actual.class,
  SST.pred.class=pred.class,
  SST.prob.bad=prob.bad,
  SST.prob.good=prob.good)

# --- Store daytime results in object "predicted.categories"
# --- that had been filled out for nightime records previously.

predicted.categories[day, ] <- pred.vals.day

table(predicted.categories$SST.actual.class, useNA="always")
table(predicted.categories$SST.tree.class, useNA="always")

xtabs(~ SST.tree.class + SST.actual.class, data=predicted.categories)   # add row/col summary (default is sum)
addmargins(xtabs(~ SST.tree.class + SST.actual.class, data=predicted.categories))  # show counts as proportions of total

addmargins(prop.table(xtabs(~ SST.tree.class + SST.actual.class, data=predicted.categories)))   # show counts as proportions of total
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot probabilities of daytime matchups being good
save.plots <- TRUE

if (save.plots) {
  #wmf.graph(paste(graph.outdir,sensor,"-day.tree.barplot.wmf", sep=""))
  #pdf.graph(paste(graph.outdir,sensor,"-day.tree.barplot.pdf", sep=""))
  
  png(filename=paste(graph.outdir,sensor,"-day.tree.barplot.wmf", sep=""),
      width = 480, height = 480, units = "px")
  tt1 <- table(pred.vals.day[,"SST.prob.good"])
  tt2 <- as.character(round(as.numeric(names(tt1)),3))
  barplot(tt1, names=tt2,
    xlab="Probability of being GOOD",
    ylab="Number of matchups",
    main=paste(sensor, "Daytime Tree"),
    col="goldenrod", las=2)
  box()
  dev.off()
  rm(tt1,tt2)
}	

hist(pred.vals.day[,"SST.prob.good"],
  breaks=seq(0,1,0.1),
  xlim=c(0,1),
  xlab="Probability of GOOD",
  main=paste(sensor,"Daytime tree - P(good)"),
  col="tomato", freq=FALSE)
box()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Histogram of the probability of matchups being good Nite + Day

hist(predicted.categories[,"SST.tree.prob.GOOD"],
     breaks=seq(0,1,0.1),
     xlim=c(0,1),
     xlab="Probability of GOOD",
     main=paste(sensor,"Day + Night - P(good)"),
     col="tomato", freq=FALSE)
box()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Add cloud tree classifications and probabilities  to data frame "orig" ----
# --- Define factor levels for tree actual and predicted categories.

orig <- data.frame(orig, predicted.categories)

orig$SST.actual.class <- ordered(orig$SST.actual.class,
  levels=c(1, 2), labels=c("Bad", "Good"))

orig$SST.tree.class <- ordered(orig$SST.tree.class,
  levels=c(1, 2), labels=c("P-Bad", "P-Good"))

addmargins(xtabs(~ SST.tree.class + SST.actual.class, data=orig))   # add row/col summary (default is sum)

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

