
# ---------------------------------------------------------------------------------------#
#  --- MODIS V7 master script
#
# --- Master script used to process MODIS matchups and produce V7 SST algorithms.
# --- This script will call other scripts that will perform specific functions.

Sys.setenv(TZ = "UTC") # Define local time to be UTC

# ---------------------------------------------------------------------------------------#
# --- STEP 0: Setup
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- Clean up ALL objects before starting ----

rm(list = ls()); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Start measuring time required by setup stage ----

ptm <- proc.time()
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
if (!require(mgcv)) {install.packages("mgcv"); library(mgcv)}
if (!require(yaml)) {install.packages("yaml"); library(yaml)}
if (!require(Cairo)) {install.packages("Cairo"); library(Cairo)}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Read YML configuration file ----

# --- Read a configuration file in YML format defining
# --- some characteristtics of the analyses in these scripts.

config.file <- "D:/Matchups/MODIS/MODIS R Scripts/MODIS_V7/config_file_MODIS.yml"

if (!file.exists(config.file)) {
	stop("ERROR: Specified configuration file does not exist...\n")
} else {
	cat("Reading configuration parameters from", config.file,"...\n")
}

# --- Read configuration file, put into data.frame "config" using package "yaml"

config <- yaml.load_file(config.file)

cat("Working on MODIS onboard", config$sensor,"collection", config$collection,"...\n")
cat("Working on variable", config$geophys.var,"...\n\n")

rm(config.file); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Find out which operating system we are using ----

op.sys <- .Platform$OS.type  											# Get operating system

if (regexpr("^[Ww]in*", op.sys) == 1) {
	op.sys <- "Windows"															# Windows
} else if (regexpr("^[LlIiNnUuXx]*", op.sys) == 1) {
	op.sys <- "Linux"																# Linux
}
cat(paste("We are running on a", op.sys, "system...   \n"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Build names of directories for outputs (coefficients, figures) ----

if (op.sys == "Windows") {
	
	# --- Directory where algorithm coefficients will be placed
	
	coeff.outdir <- paste0(config$results.dir, config$algorithm$version.name, "/", config$sensor,
												 "/coeffs/")
	# --- Verify that directory exists, if not create it	
	tt0 <- file.info(coeff.outdir)
	if(is.na(tt0$isdir)) {
		dir.create(coeff.outdir, showWarnings=TRUE)  # Create directory if it does not exist
	} else if (!tt0$isdir) {
		stop(paste0(coeff.outdir, " is NOT a directory..."))
	}
	
	# --- Directory where output graphics will be placed
	
	graph.outdir <- paste0(config$results.dir, config$algorithm$version.name, "/", config$sensor,
												 "/figs/")
	# --- Verify that directory exists, if not create it	
	tt0 <- file.info(graph.outdir)
	if(is.na(tt0$isdir)) {
		dir.create(graph.outdir, showWarnings=TRUE)  # Create directory if it does not exist
	} else if (!tt0$isdir) {
		stop(paste0(graph.outdir, " is NOT a directory..."))
	}
	
	# --- Directory where quality hypercubes will be placed
	
	cube.outdir <- paste0(config$results.dir, config$algorithm$version.name, "/", config$sensor,
												"/hypercubes/")
	# --- Verify that directory exists, if not create it	
	tt0 <- file.info(cube.outdir)
	if(is.na(tt0$isdir)) {
		dir.create(cube.outdir, showWarnings=TRUE)  # Create directory if it does not exist
	} else if (!tt0$isdir) {
		stop(paste0(cube.outdir, " is NOT a directory..."))
	}
	
	# --- Directory where big data frame with partial results will be placed
	R.objects.outdir <- paste0(config$results.dir, config$algorithm$version.name,
		"/", config$sensor, "/R_objects/")
	# --- Verify that directory exists, if not create it	
	tt0 <- file.info(R.objects.outdir)
	if(is.na(tt0$isdir)) {
		dir.create(R.objects.outdir, showWarnings=TRUE)  # Create directory if it does not exist
	} else if (!tt0$isdir) {
		stop(paste0(R.objects.outdir, " is NOT a directory..."))
	}
	
}	# End of checking if operating system is Windows

rm(tt0); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Measure time required by setup stage ----

setup.time <- proc.time() - ptm
cat("Setup stage involved", setup.time[3],"seconds...\n")

rm(ptm)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 1: Read text matchup files for a given MODIS sensor ----

ptm <- proc.time()

source("01_read_MODIS_matchups.r", echo=FALSE)

step.1.time <- proc.time() - ptm
cat("Step 1 (reading matchups) involved", step.1.time[3],"seconds...\n")

rm(ptm); gc()
# ----------------------------------------------------------------------------------------






# ---------------------------------------------------------------------------------------#
# STEPS TO BE IMPLEMENTED:
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- STEP 2:
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 3:
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 3:
# ----------------------------------------------------------------------------------------










