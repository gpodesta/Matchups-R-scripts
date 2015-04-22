
# ---------------------------------------------------------------------------------------#
#  --- MODIS V6b master script
#
# --- Master script used to process MODIS matchups and produce V6 SST algorithms.
# --- This script will call other scripts that perform specific functions.
# ---------------------------------------------------------------------------------------#

Sys.setenv(TZ = "UTC") # Define local time to be UTC

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- STEP 0: Setup
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- Clean up most objects   ----
# --- EXCEPT those objects starting with string "AQUA" or "TERRA"

tt1 <- objects()
tt2a <- stringr::str_detect(tt1, "^AQUA$")
tt2b <- stringr::str_detect(tt1, "^TERRA$")
tt3 <- tt2a | tt2b
tt4 <- tt1[!tt3]
rm(list = tt4)

rm(tt1,tt2a,tt2b,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Install required packages ----

if (!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(maps)) {install.packages("maps"); library(maps)}
if (!require(maptools)) {install.packages("maptools"); library(maptools)}
if (!require(robust)) {install.packages("robust"); library(robust)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(sp)) {install.packages("sp"); library(sp)}
if (!require(raster)) {install.packages("raster"); library(raster)}
if (!require(mgcv)) {install.packages("mgcv"); library(mgcv)}
if (!require(yaml)) {install.packages("yaml"); library(yaml)}
if (!require(Cairo)) {install.packages("Cairo"); library(Cairo)}
if (!require(circular)) {install.packages("circular"); library(circular)}
if (!require(futile.logger)) {install.packages("futile.logger"); library(futile.logger)}
if (!require(xts)) {install.packages("xts"); library(xts)}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Read YML configuration file ----

# --- Read a configuration file in YML format defining
# --- some characteristtics of the analyses in these scripts.

config.file <- "D:/Matchups/MODIS/MODIS R Scripts/MODIS_V6b/config_file_MODIS_v6b.yml"

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
flog.info(paste("We are running on a", op.sys, "system..."), name = 'ml')
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Build names of directories for input and outputs (coefficients, figures) ----

if (op.sys == "Windows") {
   
  # --- INPUT directory from where matchup files will be read 
  indir <- paste0(config$matchups.indir, config$sensor,
    "/collection_", config$collection, "/")
  # --- Verify if input directory exists
  tt0 <- file.info(indir)
  if(!tt0$isdir)
    flog.error(paste("Directory", indir,"does not exist..."), name = 'ml')
  rm(tt0); gc()

  # --- Directory where algorithm coefficients will be placed  
  coeff.outdir <- paste0("D:/Matchups/MODIS/results/",
    "version_", config$matchups$version, "/",
    config$sensor, "/collection_",
    config$collection,
    "/coeffs/")
  dir.create(coeff.outdir, showWarnings = FALSE)
  
  # --- Directory where cloud trees will be saved  
  trees.outdir <- paste0("D:/Matchups/MODIS/results/",
    "version_", config$matchups$version, "/",
    config$sensor, "/collection_",
    config$collection,
    "/trees/")
  dir.create(trees.outdir, showWarnings = FALSE)  
      
  # --- Directory where output graphics will be placed
  graph.outdir <- paste0("D:/Matchups/MODIS/results/",
    "version_", config$matchups$version, "/",
    config$sensor, "/collection_",
    config$collection,
    "/figs/")
  dir.create(graph.outdir, showWarnings = FALSE)  
      
  # --- Directory where quality hypercubes will be placed 
  cube.outdir <- paste0("D:/Matchups/MODIS/results/",
    "version_", config$matchups$version, "/",
    config$sensor, "/collection_",
    config$collection,
    "/hypercubes/")
  dir.create(cube.outdir, showWarnings = FALSE)  
     
  # --- Directory where processed data.frame will be placed
  results.outdir <- paste0("D:/Matchups/MODIS/results/",
    "version_", config$matchups$version, "/",
    config$sensor, "/collection_",
    config$collection,
    "/results/") 
  dir.create(results.outdir, showWarnings = FALSE)  
  
}  # End of checking if operating system is Windows
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define logging level and log file location ----

log.file <- paste(results.outdir, "log_file.txt")

flog.appender(appender.file(log.file), name='ml')

layout <- layout.format('[~l] [~t] ~m')
flog.layout(layout)

flog.threshold(config$log.level)

flog.info("*** START processing MODIS matchups ***", name = 'ml')
# ----------------------------------------------------------------------------------------





# ---------------------------------------------------------------------------------------#
# --- STEP 1: Read text matchup files for a given MODIS sensor ----

ptm <- proc.time()

flog.info("*** START reading matchups ***", name = 'ml')

source("01_read_MODIS_matchups.r", echo=FALSE)

step.1.time <- proc.time() - ptm

flog.info(paste("Step 1 (reading matchups) took", step.1.time[3],"seconds"), name = 'ml')
flog.info("*** FINISH reading matchups ***", name = 'ml')

rm(ptm, step.1.time); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 2: Compute SST and SST4 (and residuals) ----

ptm <- proc.time()

flog.info("*** START computing SST and SST4 ***", name = 'ml')

source("06a_compute_SST_latband1.R", echo=FALSE)
source("06b_compute_SST4_latband1.R", echo=FALSE)

step.2.time <- proc.time() - ptm

flog.info(paste("Step 2 (computing SST and SST4]) required", step.2.time[3],"seconds"),
  name = 'ml')
flog.info("*** FINISH computing SST and SST4 ***", name = 'ml')

rm(ptm, step.2.time); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 3: Apply image quality trees ----

ptm <- proc.time()

flog.info("*** START applying quality trees ***", name = 'ml')

source("07_apply_img_trees_SST_SST4.R", echo=FALSE)

step.3.time <- proc.time() - ptm

flog.info(paste("Step 3 (applying image quality trees) required", step.3.time[3],"seconds"),
  name = 'ml')
flog.info("*** FINISH applying quality trees ***", name = 'ml')

rm(ptm, step.3.time); gc()
# ----------------------------------------------------------------------------------------








# ---------------------------------------------------------------------------------------#
# STEPS TO BE IMPLEMENTED:
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- STEP 3:
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 3:
# ----------------------------------------------------------------------------------------










