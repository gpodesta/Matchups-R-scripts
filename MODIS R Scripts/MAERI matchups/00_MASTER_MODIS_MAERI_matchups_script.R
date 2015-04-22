
# ---------------------------------------------------------------------------------------#
#  --- MODIS MAERI master script
#
# --- Master script used to process MODIS matchups and produce V6 SST algorithms.
# --- This script will call other scripts that will perform specific functions.

Sys.setenv(TZ = "UTC") # Define local time to be UTC

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- STEP 0: Setup
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- Clean up ALL objects before starting ----

rm(list = ls()); gc()
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
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(sp)) {install.packages("sp"); library(sp)}
if (!require(raster)) {install.packages("raster"); library(raster)}
if (!require(mgcv)) {install.packages("mgcv"); library(mgcv)}
if (!require(yaml)) {install.packages("yaml"); library(yaml)}
if (!require(Cairo)) {install.packages("Cairo"); library(Cairo)}
if (!require(circular)) {install.packages("circular"); library(circular)}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Read YML configuration file ----

# --- Read a configuration file in YML format defining
# --- some characteristtics of the analyses in these scripts.

config.file <- "D:/Matchups/MODIS/MODIS R Scripts/MAERI matchups/config_file_MODIS_MAERI.yml"

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
# --- STEP 1: Read text matchup files for a given MODIS sensor ----

ptm <- proc.time()

source("01_read_MODIS_MAERI_matchups.r", echo=FALSE)

step.1.time <- proc.time() - ptm
cat("Step 1 (reading matchups) required", step.1.time[3],"seconds...\n")

rm(ptm); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 2: Compute SST and SST4 (and residuals) ----

ptm <- proc.time()

source("06a_compute_SST_latband1.R", echo=FALSE)
source("06b_compute_SST4_latband1.R", echo=FALSE)

step.2.time <- proc.time() - ptm
cat("Step 2 (comp[uting SST and SST4]) required", step.2.time[3],"seconds...\n")

rm(ptm); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 3: Apply image quality trees ----

ptm <- proc.time()

source("07_apply_img_trees_SST_SST4.R", echo=FALSE)

step.3.time <- proc.time() - ptm
cat("Step 3 (applying image quality trees) required", step.3.time[3],"seconds...\n")

rm(ptm); gc()
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










