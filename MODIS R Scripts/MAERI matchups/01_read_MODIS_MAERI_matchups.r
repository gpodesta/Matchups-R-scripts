
# ---------------------------------------------------------------------------------------#
# --- Script to read MODIS matchups in OLDER format. ----
# --- The script will be generic for both MODIS sensors (TERRA and AQUA).
# ---------------------------------------------------------------------------------------#

Sys.setenv(TZ = "UTC") # Define local time to be UTC

# ---------------------------------------------------------------------------------------#
# --- Define input directory from where MAERI matchup files will be read ----

indir <- config$matchups.indir

# --- Verify if input directory exists

tt0 <- file.info(indir)
if(!tt0$isdir)
  stop("Directory", indir,"does not exist...\n")
rm(tt0); gc()

filename <- paste0(config$sensor,"_MAERI_matchups.txt")

infile <- paste0(indir, filename)

rm(indir, filename); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Build a string with the column names of the MODIS matchups to be read ----
# --- as the daily matchups do not have a header record.

if (config$matchups$format == "OLD") {
  
  cat("Defining header for OLD matchup format...\n")  
  
  header <- c("sat.date","sat.time","sat.pftime", "sat.lat", "sat.lon", 
    "solz", "satz", "azimuth", "aoi", "detector", "mirror",
    "l2flg", "comflg","qual", "qsst", "qsst4","glint",
    "cen.sst","cen.sst4",
    "cen.3750","cen.3959", "cen.4050","cen.11000", "cen.12000",
    "cen.1380", "cen.6715", "cen.7325", "cen.8550", 
    "cen.39.40.ref","cen.678", "cen.748","ref.type.1.SST",
    "med.sst","med.sst4",
    "med.3750","med.3959", "med.4050",  "med.11000", "med.12000", 
    "med.1380","med.6715", "med.7325", "med.8550",
    "med.39.40.ref", "med.678", "med.748","med.reysst",         
    "min.sst","min.sst4",
    "min.3750","min.3959", "min.4050",  "min.11000", "min.12000", 
    "min.1380","min.6715", "min.7325", "min.8550",
    "min.39.40.ref", "min.678", "min.748","min.reysst",        
    "max.sst","max.sst4",
    "max.3750","max.3959", "max.4050",  "max.11000", "max.12000", 
    "max.1380","max.6715", "max.7325", "max.8550",
    "max.39.40.ref", "max.678", "max.748","max.reysst", 
    "avg.sst","avg.sst4",
    "avg.3750","avg.3959", "avg.4050",  "avg.11000", "avg.12000", 
    "avg.1380","avg.6715", "avg.7325", "avg.8550",
    "avg.39.40.ref", "avg.678", "avg.748","avg.reysst",
    "sd.sst","sd.sst4",
    "sd.3750","sd.3959", "sd.4050",  "sd.11000", "sd.12000", 
    "sd.1380","sd.6715", "sd.7325", "sd.8550",
    "sd.39.40.ref", "sd.678", "sd.748","sd.reysst",        
    "buoy.pftime","buoy.lat","buoy.lon","buoy.id","buoy.sst",
    "ref.type.1.SST")

  # --- Check that length of header names is equal to 113 
  # --- (the correct number of fields for the OLD natchup format)
  
  if (length(header) != 113)  # Check length of header vector - should be 113 for OLD format
    stop("Error in number of variables listed for header... should be 113 fields...\n")	
  
} else if (config$matchups$format == "NEW") {
  
  cat("Defining header for NEW matchup format...\n")
  
  header <- c("buoy.date", "buoy.time", "buoy.pftime", "buoy.lon", "buoy.lat",
    "buoy.type", "buoy.id", "buoy.sst", "buoy.qual", "buoy.wspd", "buoy.wdir", "buoy.airtmp",
    "sat.id", "sat.date", "sat.time", "sat.pftime", "sat.lon", "sat.lat",
    "solz", "satz", "azimuth", "aoi", "detector", "mirror", "sstflg", "sst4flg",
    "l2flg", "qsst", "qsst4", "sunside", "glint",
    "pixel", "line", "boxsizx", "boxsizy",
    "ray.ch1", "ray.ch2", "aer.ch1", "aer.ch2", 
    "prt.avg", "prt.1", "prt.2", "prt.3", "prt.4", 
    "emis.ch3", "emis.ch4", "emis.ch5", "slope.ch3", "slope.ch4", "slope.ch5",
    "intcp.ch3", "intcp.ch4", "intcp.ch5", 
    "cen.sst", "cen.sst4", 
    "cen.630", "cen.678", "cen.748", "cen.850", "cen.1380", "cen.1610", "cen.3750",
    "cen.3959", "cen.4050", "cen.6715", "cen.7325", "cen.8550", "cen.11000", "cen.12000", 
    "cen.39.40.ref",
    "med.sst", "med.sst4", 
    "med.630", "med.678", "med.748", "med.850", "med.1380", "med.1610", "med.3750",
    "med.3959", "med.4050", "med.6715", "med.7325", "med.8550", "med.11000", "med.12000", 
    "med.39.40.ref",
    "min.sst", "min.sst4", 
    "min.630", "min.678", "min.748", "min.850", "min.1380", "min.1610", "min.3750",
    "min.3959", "min.4050", "min.6715", "min.7325", "min.8550", "min.11000", "min.12000", 
    "min.39.40.ref",
    "max.sst", "max.sst4", 
    "max.630", "max.678", "max.748", "max.850", "max.1380", "max.1610", "max.3750",
    "max.3959", "max.4050", "max.6715", "max.7325", "max.8550", "max.11000", "max.12000", 
    "max.39.40.ref",
    "avg.sst", "avg.sst4", 
    "avg.630", "avg.678", "avg.748", "avg.850", "avg.1380", "avg.1610", "avg.3750",
    "avg.3959", "avg.4050", "avg.6715", "avg.7325", "avg.8550", "avg.11000", "avg.12000", 
    "avg.39.40.ref",
    "sd.sst", "sd.sst4", 
    "sd.630", "sd.678", "sd.748", "sd.850", "sd.1380", "sd.1610", "sd.3750",
    "sd.3959", "sd.4050", "sd.6715", "sd.7325", "sd.8550", "sd.11000", "sd.12000", 
    "sd.39.40.ref",
    "anc.type", "anc.wspd","anc.wdir","anc.wv", "matchup.version",
    "ref.type.1", "ref.type.1.SST",
    "ref.type.2", "ref.type.2.SST")

  # --- Check that length of header names is equal to 164 
  # --- (the correct number of fields for the NEW natchup format)
  
  if (length(header) != 164)	# Check length of header vector - should be 164 for NEW format
    stop("Error in number of variables listed for header... should be 164 fields...\n")	
  
} else {
  stop("ERROR: Matchup format is not defined\n")
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Decide if ALL variables in the input matchup records will be kept ----
# --- or, alternatively, only SOME variables (used often, or corresponding
# --- to channels for THIS sensor) are kept.

config$keep.all.vars <- FALSE    # if FALSE, select only SOME variables

if (config$keep.all.vars) {
	# All variables are kept
	vars.to.keep.MODIS <- header
} else {
	
	if (config$matchups$format == "OLD") {
		# --- 100 variables are kept for the old format
  
  	# vars.to.keep.MODIS <- c("sat.date","sat.time","sat.pftime", "sat.lat", "sat.lon", 
  	#  "solz", "satz", "mirror","qsst", "qsst4","glint", 
  	#  "cen.sst","cen.sst4","cen.678","cen.748","cen.1380","cen.3750","cen.3959","cen.4050",
  	#  "cen.6715","cen.7325","cen.8550","cen.11000","cen.12000","cen.39.40.ref",
  	#  "med.sst","med.sst4","med.678","med.748","med.1380","med.3750","med.3959","med.4050",
  	#  "med.6715","med.8550","med.11000","med.12000",
  	#  "min.sst","min.sst4","min.678","min.748","min.1380","min.3750","min.3959","min.4050",
  	#  "min.6715","min.7325","min.8550","min.11000","min.12000",
  	#  "max.sst","max.sst4","max.678","max.748","max.1380","max.3750","max.3959","max.4050",
  	#  "max.6715","max.7325","max.8550","max.11000","max.12000",
  	#  "avg.sst","avg.sst4","avg.678","avg.748","avg.1380","avg.3750","avg.3959","avg.4050",
  	#  "avg.6715","avg.7325","avg.8550","avg.11000","avg.12000",
  	#  "sd.sst","sd.sst4","sd.678","sd.748","sd.1380","sd.3750","sd.3959","sd.4050",
  	#  "sd.6715","sd.7325","sd.8550","sd.11000","sd.12000",
  	#  "buoy.pftime","buoy.lat","buoy.lon","buoy.id","buoy.sst",
  	#  "ref.type.1.SST")

  	# --- Build an even shorter list of variables to keep...
  
  	#vars.to.keep.MODIS <- c("sat.pftime", "sat.lat", "sat.lon", 
    #	"solz", "satz", "mirror","qsst", "qsst4","glint", 
    #	"cen.sst","cen.sst4","cen.678","cen.748","cen.1380","cen.3750","cen.3959","cen.4050",
    #	"cen.6715","cen.7325","cen.8550","cen.11000","cen.12000","cen.39.40.ref",
    #	"min.sst","min.sst4","min.678","min.748","min.1380","min.3750","min.3959","min.4050",
    #	"min.6715","min.7325","min.8550","min.11000","min.12000",
    #	"max.sst","max.sst4","max.678","max.748","max.1380","max.3750","max.3959","max.4050",
    #	"max.6715","max.7325","max.8550","max.11000","max.12000",
    #	"buoy.pftime","buoy.lat","buoy.lon","buoy.id","buoy.sst",
    #	"ref.type.1.SST")
    
    # --- And even shorter so we can ingest ALL matchups...
    
  	vars.to.keep.MODIS <- c("sat.pftime", "sat.lat", "sat.lon", 
  	  "solz", "satz", "mirror","qsst", "qsst4","glint", 
  	  "cen.sst","cen.sst4","cen.678","cen.748","cen.1380","cen.3750","cen.3959","cen.4050",
  	  "cen.11000","cen.12000","cen.39.40.ref",
  	  "min.sst","min.sst4","min.678","min.748","min.1380","min.3750","min.3959","min.4050",
  	  "min.11000","min.12000",
  	  "max.sst","max.sst4","max.678","max.748","max.1380","max.3750","max.3959","max.4050",
  	  "max.11000","max.12000",
  	  "buoy.pftime","buoy.lat","buoy.lon","buoy.id","buoy.sst",
  	  "ref.type.1.SST")
  	
	
	} else if (config$matchups$format == "NEW") {
		
		# --- 124 variables are kept for the new format
  
  	vars.to.keep.MODIS <- c("buoy.date","buoy.time","buoy.pftime","buoy.lon","buoy.lat",
    	"buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd","buoy.wdir","buoy.airtmp",
    	"sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
    	"solz","satz",  "mirror","sstflg","sst4flg",
    	"l2flg","qsst","qsst4","sunside","glint",
    	"pixel","line","boxsizx","boxsizy",
    	"cen.sst","cen.sst4","cen.678","cen.748","cen.1380","cen.3750","cen.3959","cen.4050",
    	"cen.6715","cen.7325","cen.8550","cen.11000","cen.12000","cen.39.40.ref",
    	"med.sst","med.sst4","med.678","med.748","med.1380","med.3750","med.3959","med.4050",
    	"med.6715","med.8550","med.11000","med.12000","med.39.40.ref",
    	"min.sst","min.sst4","min.678","min.748","min.1380","min.3750","min.3959","min.4050",
    	"min.6715","min.7325","min.8550","min.11000","min.12000","min.39.40.ref",
    	"max.sst","max.sst4","max.678","max.748","max.1380","max.3750","max.3959","max.4050",
   	 	"max.6715","max.7325","max.8550","max.11000","max.12000","max.39.40.ref",
    	"avg.sst","avg.sst4","avg.678","avg.748","avg.1380","avg.3750","avg.3959","avg.4050",
    	"avg.6715","avg.7325","avg.8550","avg.11000","avg.12000","avg.39.40.ref",
    	"sd.sst","sd.sst4","sd.678","sd.748","sd.1380","sd.3750","sd.3959","sd.4050",
    	"sd.6715","sd.7325","sd.8550","sd.11000","sd.12000","sd.39.40.ref",
    	"ref.type.1","ref.type.1.SST",
    	"ref.type.2","ref.type.2.SST",
    	"anc.type","anc.wspd","anc.wdir","anc.wv","matchup.version")                    

	} else {
  	stop("ERROR: Matchup format is not defined\n")
	}
}		# End of check for keeping all variables in input matchups
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Read in the FIRST matchup file in file.list  ----
# --- IMPORTANT: We assume that each file has ONE line of headers
# --- (although not used for variable naming) and no information other than matchups.
# --- That is, there are no lines with metadata, as there may be in some
# ---  matchup files coming directly from extraction programs.

# --- Read the first matchups file.
# --- Put matchups in dataframe "orig".

cat("\nReading FIRST file in the input list...\n")
cat("Reading file", infile,"...\n")

orig <- read.table(file = infile, sep="\t", skip = 1,
  header = FALSE, col.names = header,
  stringsAsFactors = FALSE, na.strings="NA")

# --- Check that the number of fields in all rows of the input file 
# --- coincides with the number of variables specified in the "header" object...
# --- [There should be 164 columns in the new, generic matchup format]
# --- [There should be 113 columns in the OLD matchup format]

hh1 <- count.fields(infile, sep="\t")		# No. of fields in input file	

if (config$matchups$format == "OLD") {
  hh2 <- hh1 == 113		# Number of fields in old matchup format						
} else if (config$matchups$format == "NEW") {
  hh2 <- hh1 == 164  	# Number of fields in new matchup format						
} else {
  stop("ERROR: Matchup format is not defined...\n")
}  

if (!all(hh2)) {
  stop("Error in number of columns in file", file=infile, "\n")	
}

rm(hh1, hh2); gc()

# --- DO NOT perform any initial tests, BUT... 
# --- If config$keep.all.vars is TRUE, keep only SOME of the variables.

if (!config$keep.all.vars) {
  # Sample matchups, but keep only SOME variables
  orig <- subset(orig, select = vars.to.keep.MODIS)
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- NOW THAT WE HAVE FINISHED READING ALL INDIVIDUAL MATCHUP FILES ----
# --- PROCESS THE RESULTING DATA FRAME.
# ---------------------------------------------------------------------------------------#
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Change a few variables into character vectors ----
# --- to ensure they get set to this type for all matchup files.

orig$buoy.id <- as.character(orig$buoy.id)    		# Buoy ID	

# --- Do these variables only for new matchups

if (config$matchups$format == "NEW") {
  orig$sat.id <- as.character(orig$sat.id)  				# Satellite ID
  orig$sunside <- as.character(orig$sunside)  			# Sun side of the scan
  orig$anc.type <- as.character(orig$anc.type)  		# Ancillary data type description
  orig$ref.type.1 <- as.character(orig$ref.type.1)	# Reference SST, type 1
  orig$ref.type.2 <- as.character(orig$ref.type.2)	# Reference SST, type 2
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Sort final file with all matchups and find any duplicates ----
# --- NOTE: we sort by SST quality because if there are duplicated records we will keep
# --- the record with the highest quality (lowest value), which will be sorted first.
# --- If duplicate records have the same quality SST, THEN pick the one with the smallest 
# --- satellite zenith angle.

tt1 <- order(orig$buoy.pftime, orig$buoy.id, orig$buoy.lat, orig$buoy.lon,
  orig$sat.pftime, orig$sat.lat, orig$sat.lon, orig$qsst, orig$satz,
  decreasing = FALSE)

orig <- orig[tt1, ]		# Data frame with sorted records
gc()

# --- Check for duplicate records.
# --- These are records that have the same buoy data, lat and lon, and the same satellite time.

vars <- c("buoy.pftime","buoy.id", "buoy.lat","buoy.lon","sat.pftime", "sat.lat", "sat.lon")

lines.duplicated <- duplicated(orig[, vars])

if (any(lines.duplicated)) {
  table(lines.duplicated)
  warning("There are duplicate lines in matchups...\n")
  # --- Create object "ttt" indicating which records are duplicated
  ttt <-  which(lines.duplicated)
  # --- Write out data without duplicates to object "orig"
  warning("Duplicate lines are being eliminated...\n")
  warning("Number of duplicate lines is ", length(ttt),"...\n")
  orig <- orig[!lines.duplicated, ]
} else {
  cat("No duplicate lines detected...\n");
}

rm(lines.duplicated, vars, tt1, ttt); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Check if matchups are in a sun glint region ----

# --- Check if in a sun glint region; thresholds differ among matchup formats

if (config$matchups$format == "OLD") {
	in.glint <- ifelse(orig$glint > 0, TRUE, FALSE)   		# Are we in a sunglint region?
} else if (config$matchups$format == "NEW") {
	in.glint <- ifelse(orig$glint > 0.005, TRUE, FALSE)   # Are we in a sunglint region
} else {
	stop("ERROR: Matchup format is not defined\n")
}

# --- Add "in.glint" variable to data frame "orig"

orig <- data.frame(orig, in.glint = in.glint)

# --- We check for situations in which the effect of sunglint might potentially
# --- influence values of both visible and near-IR channels. 

is.day <- ifelse(orig$solz < 90, TRUE, FALSE)    # Are we in daytime?

addmargins(xtabs(~ is.day + orig$in.glint))   # add row/col summary (default is sum)
prop.table(xtabs(~ is.day + orig$in.glint))   # show counts as proportions of total

# --- The tables above suggest that there are some "weird" matchups that are
# --- nightime (solar zenith > 90 degrees) but have a positive glint index.
# --- We plot the lcoation of these records.

figura <- paste(graph.outdir, "weird_nite_glint.png", sep="")  

CairoPNG(filename = figura,
	width = 640, height = 480, units = "px", pointsize = 12,
	bg = "white", res = NA)

map(database = "world", regions = ".",
	exact = FALSE, boundary = FALSE,
	interior = FALSE, fill=TRUE, col="grey80") 
map.axes()
title(paste(config$sensor, " - Night records with glint"))

points(orig$buoy.lon[!is.day & in.glint],
	orig$buoy.lat[!is.day & orig$in.glint], pch=16, cex=0.5, col="tomato")

dev.off()
rm(figura); gc()

# --- Check the range of solar zenith angles for glint/nite matchups

uuu <- orig$solz >= 90 & orig$in.glint == TRUE
summary(orig$solz[uuu])

rm(is.day, uuu); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Build  POSIX date objects with buoy (in situ) and satellite times ----
# --- Original times are expressed in elapsed seconds since 1 Jan 1981 00:00:00 UTC.
# --- The output formats selected conforms to the International Standard ISO 8601 
# --- which specifies numeric representations of date and time.
# --- For more info see: http://www.cl.cam.ac.uk/~mgk25/iso-time.html

# --- WARNING FOR OLD MATCHUP FORMAT: Even though the header of MODIS matchups indicates
# --- that variables "sat.date" and "sat.time" should correspond to the satellite time,
# --- when we compute date/time POSIXct objects below using the continuous Pathfinder time
# --- we find that they actually coincide with the buoy times. 

buoy.timedate <- as.POSIXlt(orig$buoy.pftime, origin="1981-01-01", tz="UTC")
range(buoy.timedate, na.rm = TRUE)
gc()

sat.timedate <- as.POSIXlt(orig$sat.pftime, origin="1981-01-01", tz="UTC")
range(sat.timedate, na.rm = TRUE)
gc()

# --- Compute some useful time-related variables that will be added
# --- to the large dataframe.

mon <- as.numeric(month(sat.timedate))  # Month of the year: 1,2,3,....
yr <- as.numeric(year(sat.timedate))		# Year: 2005, 2006,...

xx1 <- format(sat.timedate, "%b-%Y")
xx2 <- unique(xx1)
mo.yr <- ordered(xx1, levels = xx2)

rm(xx1,xx2); gc()

orig <- data.frame(orig,
  buoy.timedate = buoy.timedate,
  sat.timedate = sat.timedate,
  sat.mon = mon,
  sat.yr = yr,
  sat.moyr = mo.yr)

rm(buoy.timedate, sat.timedate);gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Assign records to latitude bands ----
# --- NOTE: If definition of latband changes, change boundaries.
# --- A latband includes the uper limit; eg the band between 40S+ and 20S
# --- includes values > 40S and <= 20S.

lat.boundaries <- c(-90, -40, -20, 0, 20, 40, 90)

latband <- ordered(cut(orig$buoy.lat,
  breaks=lat.boundaries, right=TRUE,
  labels=c("<=40S","40S+ to 20S","20S+ to Eq",
  "Eq+ to 20N", "20N+ to 40N",">40N")))

n.latbands <- dim(table(latband))			# No. of latitude bands
latband.names <- levels(latband)			# Labels for latband intervals

# --- Check definition of latitude bands

check <- tapply(orig$buoy.lat,
  INDEX=as.numeric(latband), FUN=range, simplify=T)

rm(check, lat.boundaries); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Assign MODIS matchups to a BTdiff regime (difference between BT31 and BT32) ----
# --- First, calculate difference between brightness temperatures (BTs)
# --- for MODIS channels 31 and 32.

BTdiff <- (orig$cen.11000 - orig$cen.12000)   # ch31 minus ch32 difference

# --- Value of (BT31 - BT32) that defines low or high BTdiff regimes.
# --- It is read from the configuration file
# --- If two categories are used, the current boundary is 0.7 deg C.
# --- If more categories are desired, more boundaris must be defined.

threshold <- config$MODIS.BTdiff.threshold 		

BTdiff.bound <- c(min(BTdiff, na.rm = TRUE) - 0.01,
	threshold,     # <--- Add more boundaries in this line, if desired....
  max(BTdiff, na.rm = TRUE) + 0.01)
n.BTdiff.regimes <- length(BTdiff.bound) - 1		# Number of BTdiff regimes	

# --- Finally, cut values of BTdiff into the number of categories desired...
# --- Currently the threshold 0.7 is included in the low interval.

if (length(BTdiff.bound) == 3) {
  BTdiff.regime <- cut(BTdiff, breaks=BTdiff.bound, include.lowest=FALSE,
    labels=c("lowBTdiff", "highBTdiff"), ordered_result=TRUE)
} else if (length(BTdiff.bound) == 4) {
  BTdiff.regime <- cut(BTdiff, breaks=BTdiff.bound, include.lowest=FALSE,
    labels=c("lowBTdiff", "medBTdiff", "highBTdiff"), ordered_result=TRUE)
} else {
  BTdiff.regime <- cut(BTdiff, breaks=BTdiff.bound, include.lowest=FALSE,
    ordered_result=TRUE)
}	

# --- Check that values are OK

check <- tapply(BTdiff, INDEX = BTdiff.regime, FUN = range, simplify = T)
table(BTdiff.regime, useNA="always")  # Table with values of BT31 - BT32 difference

rm(BTdiff.bound, BTdiff, n.BTdiff.regimes, check, threshold); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Add variables created so far to "orig" data frame ----

orig <- data.frame(orig,
  BTdiff.regime = BTdiff.regime,
  latband = latband)

if (ncol(orig) != 54) {
  stop("Number of columns in \"orig\" appears to be wrong...\n")
}

rm(mon,yr,mo.yr, BTdiff.regime, latband); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Decode the "NEW" buoy IDs ----
# --- The old IDs used to have just a number with 5 digits (buoy numbers) or
# --- a string (eg, with a ship name).

# --- Identify which records have "new" buoy IDs.
# --- They are those which contain an underscore ("_")

ttt <- orig$buoy.id

is.new.buoy.ID <- str_detect(ttt, "_")

# --- For those records that do not have an underscore (ie, old buoy ID format),
# --- insert the string "NV_". That is we assume that all IDs with the old format
# ---- come from the Navy data.

ttt[!is.new.buoy.ID] <- paste("NV_", ttt[!is.new.buoy.ID], sep="")

# --- Create objects to store source of in situ data and platform type.
# --- The source can be a database (e.g., "NV" is "Navy", "IQ" is IQUAM data)
# --- or a particular type of sensor (e.g., "MA" for MAERI).
# --- If no prefix indicating source is presnet, we assume data come from the Navy,
# --- so by default the source is "NV".

in.situ.source <- rep("NV", length.out=length(ttt)) 
in.situ.platform <- rep(NA, length.out=length(ttt))

# --- Use the underscore to split the buoy ID in two parts
# --- (to the left and right of the "_").
# --- NOTE: This command takes some time to execute.

tt1 <- str_split_fixed(ttt, "_", n=2)

# --- Fill in values for in.situ.source

in.situ.source <- tt1[, 1]  # in situ source for buoys with new IDs

table(in.situ.source, useNA="always")

# --- Now work with the part of buoy IDs to the right of the underscore

tt2 <- tt1[, 2]  # in situ platform
table(tt2, useNA="always")

tt2[which(tt2 == "C1")] <- "M1"
tt2[which(tt2 == "PRT")] <- "M1"

in.situ.platform <- toupper(tt2)

addmargins(xtabs(~ in.situ.source + in.situ.platform))   # add row/col summary (default is sum)
round(prop.table(xtabs(~ in.situ.source + in.situ.platform)),3)  # show counts as proportions of total

rm(ttt,is.new.buoy.ID,tt1,tt2); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Add variables created above to "orig" data frame ----

orig <- data.frame(orig,
  in.situ.source = in.situ.source,          
  in.situ.platform = in.situ.platform)

if (ncol(orig) != 56) {
	stop("Number of columns in \"orig\" appears to be wrong...\n")
}

rm(in.situ.source,in.situ.platform); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Exclude data for certain dates when the MODIS-TERRA instrument had problems. ----
# --- ONLY FOR TERRA:
# --- See tables "Terra MODIS MSCN in Y2000" and "Terra MODIS MSCN in Y2001-2002"
# --- provided by Bob and Kay, Jan 2005. 

# The MODIS instrument experienced a Power Supply 2 (PS2 = electronics
# side - B) shutdown anomaly and did not take science data during the period
# from June 15, 2001 (day 2001166) to July 2, 2001 (day 2001183). 
# The cause of the failure is consistent with an over - voltage shutdown most likely
# initiated by a high - energy radiation event that caused the Metal - oxide Semiconductor
# Field Effect Transistor (MOSFET) within the down - regulator of PS2 to fail.

if (config$sensor == "Terra") {
  
  # Extract year and day of the year from satellite time
  yyy <- year(orig$sat.timedate)   # year
  doy <- yday(orig$sat.timedate)   # day of year
  
  # Define BAD data days for TERRA
  
  bad.days.2000 <- c(218, 231, 238, 245, 253, 260, 267, 273:287, 294, 304)
  bad.days.2002 <- c(90, 91:121, 147:141, 148:226, 236:254)
  bad.dates <- ((yyy == 2000) & (doy %in% bad.days.2000)) |
    ((yyy == 2002) & (doy %in% bad.days.2002))
  table(bad.dates)
  
  cat("Excluding BAD DAYS for MODIS TERRA...\n")
  cat("Excluding", length(which(bad.dates)),"records for MODIS TERRA bad dates...\n")
  orig <- subset(orig, subset = !bad.dates) 
  rm(yyy, doy, bad.days.2000, bad.days.2002, bad.dates); gc()

} else {
  cat("No BAD DATES need to be deleted...\n")   # For AQUA
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Add cell numbers for 1-degree and 5-degree grids ----
# --- These numbers may help perform statistics for the matchups
# --- (e.g., number of matchups per cell, etc.).

# --- Create a raster object with 5-degree pixels

grid5deg <- raster(ncol = 72, nrow = 36,
  xmn = -180, xmx = 180,
  ymn = -90, ymx = 90,
  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

# --- Create a raster object with 1-degree pixels

grid1deg <- raster(ncol = 360, nrow = 180,
  xmn = -180, xmx =180,
  ymn = -90, ymx = 90,
  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

# --- Write cell numbers as value for the grids.
# --- cell 1 is the upper left corner and numbers go
# --- across the top line through its end, then
# --- start again in the second line, and so forth.

values(grid5deg) <- 1:ncell(grid5deg)

values(grid1deg) <- 1:ncell(grid1deg)

# --- Build a SpatialPoints object with lons and lats of matchups

pts <- SpatialPoints(coords=data.frame(lon=orig$sat.lon, lat=orig$sat.lat),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# --- Overlay the SpatialPoints object with matchup locations 
# --- over the 5-degree raster grid.
# --- The result is an object that lists the cell number in which a matchup is located. 

tt1 <- extract(x=grid5deg, y=pts, method="simple")

tt2 <- extract(x=grid1deg, y=pts, method="simple")

orig$cell5deg <- tt1  # Store cell number in 5-degree grid in dataframe orig
orig$cell1deg <- tt2  # Store cell number in 1-degree grid in dataframe orig

rm(grid1deg, grid5deg, pts, tt1, tt2); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Add geographic information (eg., ocean basin) to matchup locations ----
# --- The information is from the Natural Earth web site.

tt0 <- file.info(config$geo.dir)     	# Get info about input directory
if (!tt0$isdir)               				# Is it a directory?
  stop("ERROR: Specified input directory does not exist... verify name...\n")
rm(tt0); gc()

file <- "ne_10m_geography_marine_polys.shp"  	# Name of file with geographic info
infile <- paste(config$geo.dir, file, sep="") # Build complete (long) input file name

if (!exists("infile")) {
  stop("ERROR: Specified input file  does not exist...\n") }

geo.info <- readShapeSpatial(infile, repair=TRUE,force_ring=TRUE,
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# --- Build a SpatialPoints object with lons and lats of matchups

pts <- SpatialPoints(coords=data.frame(lon=orig$sat.lon, lat=orig$sat.lat),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# --- Overlay the matchups over the polygons with geographic information.
# --- The result is an object that lists the basin in which matchups are located. 

tt1 <- over(pts, geo.info)    # Overlay matchups over polygons with geo info
geo.location <- tt1$name      # Extract basin information

orig$geo.location <- geo.location   # Store in dataframe orig

# Table of ocean basins with matchups, in decreasing order of frequency

tt2 <- table(geo.location)
tt3 <- tt2[order(tt2, decreasing = TRUE)]
tt4 <- tt3[tt3 > 0]

rm(file,infile,geo.info,pts,tt1,tt2,tt3,tt4,geo.location); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Add result of initial quality tests ----
# --- but do not eliminate any records...
# --- Values equal to TRUE mean that the records PASS tests

# --- Usual space-time coincidence criteria

tt0 <- (abs(orig$buoy.pftime - orig$sat.pftime) <= 1800) &
  (abs(orig$buoy.lat - orig$sat.lat) <= 0.1) &
  (abs(orig$buoy.lat - orig$sat.lat) <= 0.1)

# --- Buoy SSTs not missing and within reasonable range

tt1 <- !is.na(orig$buoy.sst) & (orig$buoy.sst >= - 2) & (orig$buoy.sst <= 45)

# --- Non-missing values for far IR BTs

tt2 <- !is.na(orig$cen.11000) &
  !is.na(orig$max.11000) &
  !is.na(orig$min.11000) &
  !is.na(orig$cen.12000) &
  !is.na(orig$max.12000) &
  !is.na(orig$min.12000) &
  abs(orig$cen.11000) < 999 &
  abs(orig$max.11000) < 999 &
  abs(orig$min.11000) < 999 &
  abs(orig$cen.12000) < 999 &
  abs(orig$max.12000) < 999 &
  abs(orig$min.12000) < 999

# --- Nite range test: pass if 11-12 and 3-4um bands are within valid range 

tt3 <- (orig$solz >= 90) & 
  (orig$cen.3750 >= -4.0) & (orig$cen.3750 <= 37.0) & 
  (orig$cen.3959 >= -4.0) & (orig$cen.3959 <= 37.0) & 
  (orig$cen.4050 >= -4.0) & (orig$cen.4050 <= 35.0) & 
  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) & 
  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) 

# --- Day range test outside of glint
# --- eg. low glint value test true if the 11,12um bands are within range and
# --- 3-4um band are not cold 

tt4 <- ((orig$solz < 90 & orig$glint < 0.005) & 
  (orig$cen.3750 >= -4.0) & 
  (orig$cen.3959 >= -4.0) & 
  (orig$cen.4050 >= -4.0)) & 
  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) & 
  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) 

# --- Day range test in high glint region where we can't do any test on the 3-4um values.

tt5 <- (orig$solz < 90 & orig$glint > 0.005) & 
  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) & 
  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) 

# --- Spatial homogeneity (max minus min) for near and far IR BTs

tt6 <- (orig$max.3959 - orig$min.3959 <= 1.5) &
  (orig$max.4050 - orig$min.4050 <= 1.5) &
  (orig$max.11000 - orig$min.11000 <= 1.5) &
  (orig$max.12000 - orig$min.12000 <= 1.5)

pass.initial.filters <- tt0 & tt1 & tt2 & (tt3 | tt4 | tt5) & tt6
table(pass.initial.filters, useNA="always")

# --- Add to "orig" data frame...

orig[, "pass.initial.filters"] <- pass.initial.filters

rm(tt0,tt1,tt2,tt3,tt4,tt5,tt6,pass.initial.filters); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- FINALLY, SAVE INPUT DATA FRAME "ORIG" to a backup data frame and abinary file ----
# ---------------------------------------------------------------------------------------#
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save "orig" data frame into a data frame for the sensor in question... ----

if (config$sensor == "Terra") {
	TERRA <- orig 			# Store object "orig" in object "TERRA"
} else if (config$sensor == "Aqua") {
	AQUA <- orig 				# Store object "orig" in object "AQUA"
}	
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Clean up most objects   ----
# --- EXCEPT those objects equal to "orig" or starting with string "AQUA" or "TERRA"

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

# END of script 01_read_MODIS_matchups.r



