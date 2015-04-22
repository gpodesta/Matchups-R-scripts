
orig <- AQUA

# ---------------------------------------------------------------------------------------#
# --- First, recalculate standardized difference between MODIS channels 22 and 23 ----

# --- Read curve fit as part of version 6.3 processing

infile <- paste0(results.outdir, "AQUA_d2223_fit.txt")
if (!exists("infile")) {
  stop("Input file does not exist...\n")
}

fit <- read.table(infile, header = TRUE)

gg1 <- smooth.spline(fit[, "sza"], fit[, "d.22.23"])
gg2 <- predict(gg1, orig$satz)

# --- Store standardized difference in "orig"

orig$cen.39.40.ref.new <- (orig$cen.3959 - orig$cen.4050) - gg2$y

rm(infile, fit, gg1, gg2); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- AQUA SST image day and night trees ----

# --- Create object to hold results of Aqua image sst tree

aqua.img.SST.tree.class <- rep(NA, times = nrow(orig))

# ----------------------------------#
# --- AQUA SST image NIGHT tree
# ----------------------------------#

# --- Define variables to be used in image SST nightime tree 

nite <- orig$solz > 90
d34 <- orig$cen.39.40.ref.new[nite]   # residuals from std diff between BT22 and BT23
d1112 <- orig$cen.11000[nite] - orig$cen.12000[nite]  # BT31 - BT32

# --- Implement AQUA SST image NIGHT tree (in document V4)

bb1 <- ifelse((d34 < -0.7563128) |
  ((d34 >= -0.7563128) & (d1112 < 0.22815)),
  "P-Bad", "P-Good")

# --- Store results from SST image night tree

aqua.img.SST.tree.class[which(nite)] <- bb1

table(aqua.img.SST.tree.class, useNA = "always")

# ----------------------------------#
# --- AQUA SST image DAY tree
# ----------------------------------#

# --- Define variables to be used in image SST daytime tree 

day <- !nite
c6 <- orig$cen.678[day]                               # MODIS Channel 14
c13 <- orig$cen.1380[day]                             # MODIS channel 26
d11 <- (orig$max.11000[day] - orig$min.11000[day])    # max minus min BT 31
d1112 <- (orig$cen.11000[day] - orig$cen.12000[day])  # BT31 - BT32

# --- Implement AQUA SST image DAYTIME tree (in document V4)

bb2 <- ifelse(
  (((c6 >= 0.17245) & (d1112 < 0.6646)) |
  ((c6 >= 0.17245) & (d1112 >= 0.6646) & (d11 >= 0.43735)) |
  ((c6 < 0.17245) & (c13 >= 0.00685))),
  "P-Bad", "P-Good")

# --- Store results from SST image daytime tree

aqua.img.SST.tree.class[which(day)] <- bb2

table(aqua.img.SST.tree.class, useNA = "always")

# --- Store tree results in "orig"

orig$img.SST.tree.class <- aqua.img.SST.tree.class
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- AQUA SST4 image night tree ----

# --- Create object to hold results of Aqua image SST4 tree

aqua.img.SST4.tree.class <- rep(NA, times = nrow(orig))

# ----------------------------------#
# --- AQUA SST4 image NIGHT tree
# ----------------------------------#

# --- Define variables to be used in image SST4 nightime tree 

nite <- orig$solz > 90
d34 <- orig$cen.39.40.ref.new[nite]   # residuals from std diff between BT22 and BT23
d411 <- orig$cen.4050[nite] - orig$cen.11000[nite]    # BT23 - BT31
d1112 <- orig$cen.11000[nite] - orig$cen.12000[nite]  # BT31 - BT32

# --- Implement AQUA SST4 image NIGHT tree (in document V4)

bb3 <- ifelse(
  (d34 < -0.763815) |
  ((d34 >= -0.763815) & (d411 >= 4.03815)) |
  ((d34 >= -0.763815) & (d411 < 4.03815)& (d1112 < 0.2252)),
  "P-Bad", "P-Good")

# --- Store results from SST4 image night tree

aqua.img.SST4.tree.class[which(nite)] <- bb3

table(aqua.img.SST4.tree.class, useNA = "always")

# --- Store tree results in "orig"

orig$img.SST4.tree.class <- aqua.img.SST4.tree.class
# ----------------------------------------------------------------------------------------

AQUA <- orig

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# --- Work with TERRA

orig <- TERRA

# ---------------------------------------------------------------------------------------#
# --- First, recalculate standardized difference between MODIS channels 22 and 23 ----

# --- Read curve fit as part of version 6.3 processing

infile <- paste0(results.outdir, "TERRA_d2223_fit.txt")
if (!exists("infile")) {
  stop("Input file does not exist...\n")
}

fit <- read.table(infile, header = TRUE)

gg1 <- smooth.spline(fit[, "sza"], fit[, "d.22.23"])
gg2 <- predict(gg1, orig$satz)

# --- Store standardized difference in "orig"

orig$cen.39.40.ref.new <- (orig$cen.3959 - orig$cen.4050) - gg2$y

rm(infile, fit, gg1, gg2); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- TERRA SST image day and night trees ----

# --- Create object to hold results of TERRA image sst tree

terra.img.SST.tree.class <- rep(NA, times = nrow(orig))

# ----------------------------------#
# --- TERRA SST image NIGHT tree
# ----------------------------------#

# --- Define variables to be used in image SST nightime tree 

nite <- orig$solz > 90
d34 <- orig$cen.39.40.ref.new[nite]   # residuals from std diff between BT22 and BT23

# --- Implement TERRA SST image NIGHT tree (in document V4)

bb1 <- ifelse((d34 < -0.6953903), "P-Bad", "P-Good")

# --- Store results from SST image night tree

terra.img.SST.tree.class[which(nite)] <- bb1

table(terra.img.SST.tree.class, useNA = "always")

# ----------------------------------#
# --- TERRA SST image DAY tree
# ----------------------------------#

# --- Define variables to be used in image SST daytime tree 

day <- !nite
c6 <- orig$cen.678[day]                               # MODIS Channel 14
c13 <- orig$cen.1380[day]                             # MODIS channel 26
d11 <- (orig$max.11000[day] - orig$min.11000[day])    # max minus min BT 31
d1112 <- (orig$cen.11000[day] - orig$cen.12000[day])  # BT31 - BT32

# --- Implement TERRA SST image DAYTIME tree (in document V4)

bb2 <- ifelse(
  ((c6 >= 0.16885) & (d1112 < 0.55605)) |
  ((c6 >= 0.16885) & (d1112 >= 0.55605) & (d11 >= 0.4042)) |
  ((c6 < 0.16885) & (c13 >= 0.00595) & (d1112 >= 0.57)),  
  "P-Bad", "P-Good")
  
# --- Store results from SST image daytime tree

terra.img.SST.tree.class[which(day)] <- bb2

table(terra.img.SST.tree.class, useNA = "always")

# --- Store tree results in "orig"

orig$img.SST.tree.class <- terra.img.SST.tree.class
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- TERRA SST4 image night tree ----

# --- Create object to hold results of Aqua image SST4 tree

terra.img.SST4.tree.class <- rep(NA, times = nrow(orig))

# ----------------------------------#
# --- TERRA SST4 image NIGHT tree
# ----------------------------------#

# --- Define variables to be used in image SST4 nightime tree 

nite <- orig$solz > 90
d3 <- (orig$max.3959[nite] - orig$min.3959[nite])       # max minus min BT ch22 inside box
d11 <- (orig$max.11000[nite] - orig$min.11000[nite])    # max minus min BT ch31 inside box
d34 <- orig$cen.39.40.ref.new[nite]   # residuals from std diff between BT22 and BT23
d411 <- orig$cen.4050[nite] - orig$cen.11000[nite]      # BT23 - BT31

# --- Implement TERRA SST4 image NIGHT tree (**NOTE: Here use version in document V3)

bb3 <- ifelse(
  (d34 < -0.7002907) |
  ((d34 >= -0.7002907) & (d11 >= 0.37905) & (d411 >= 1.65375)) |
  ((d34 >= -0.7002907) & (d11 >= 0.37905) & (d411 < 1.65375) & (d34 < -0.05674069)) |
  ((d34 >= -0.7002907) & (d11 < 0.37905) & (d3 >= 0.4318) & (d3 >= 0.6903)), 
  "P-Bad", "P-Good")

# --- Store results from SST4 image night tree

terra.img.SST4.tree.class[which(nite)] <- bb3

table(terra.img.SST4.tree.class, useNA = "always")

# --- Store tree results in "orig"

orig$img.SST4.tree.class <- terra.img.SST4.tree.class
# ----------------------------------------------------------------------------------------

TERRA <- orig





