

# ---------------------------------------------------------------------------------------#
# --- Install required packages ----

if (!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(maps)) {install.packages("maps"); library(maps)}
if (!require(maptools)) {install.packages("maptools"); library(maptools)}
if (!require(robust)) {install.packages("robust"); library(robust)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(sp)) {install.packages("sp"); library(sp)}
if (!require(raster)) {install.packages("raster"); library(raster)}
if (!require(mgcv)) {install.packages("mgcv"); library(mgcv)}
if (!require(yaml)) {install.packages("yaml"); library(yaml)}
if (!require(Cairo)) {install.packages("Cairo"); library(Cairo)}
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
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
flog.info(paste("We are running on a", op.sys, "system...   \n"), name = 'ml')
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
# --- Define logging level and log file location ----

flog.appender(appender.file(config$log.file), name='ml')

layout <- layout.format('[~l] [~t] ~m')
flog.layout(layout)

flog.threshold(config$log.level)

flog.warn("test of logger warning", name = 'ml')
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Check if object 'orig' exists -----

if (!exists('orig')) {
  if (config$sensor == "Aqua") {
    orig <- AQUA
  } else if (config$sensor == "Terra") {
    orig <- TERRA
  } else {
    flog.error("Sensor to process not specified in configuration file...", name = 'ml')
  }
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Extract a few variables from "orig" ----

orig2 <- dplyr::select(orig,
  sat.lat, sat.lon, solz, satz, sat.timedate, cen.11000, cen.12000,
  latband, in.situ.source,
  in.situ.platform, buoy.sst, ref.type.1.SST,
  use.4.coeffs.SST, use.4.coeffs.SST4, SST.latband1, SST4.latband1,
  img.SST.tree.class, img.SST4.tree.class, qsst.new, qsst4.new)

orig2$cen.11000 <- ifelse(orig2$cen.11000 < -900, NA, orig2$cen.11000)
orig2$cen.12000 <- ifelse(orig2$cen.12000 < -900, NA, orig2$cen.12000)



orig2$btd <- orig$cen.11000 - orig2$cen.12000


# ----------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------#
# --- Scatterplots of "continuous" hypercube dimensions



plot(orig2$satz, orig2$btd, pch = ".")






