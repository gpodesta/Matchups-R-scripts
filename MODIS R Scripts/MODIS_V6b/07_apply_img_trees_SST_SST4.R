

# ---------------------------------------------------------------------------------------#
# --- Apply SST and SST4 trees for day and night for the sensor being analyzed ----

if (config$sensor == "Aqua") {
 
  #---  WORK WITH AQUA FIRST
  
  # --- Recalculate standardized difference between MODIS channels 22 and 23
  # --- Read AQUA curve fit as part of version 6.3 processing
  
  infile <- paste0(results.outdir, "AQUA_d2223_fit.txt")
  if (!exists("infile")) {
    flog.error("Input file does not exist...", name = 'ml')
  }
  
  fit <- read.table(infile, header = TRUE)
  
  gg1 <- smooth.spline(fit[, "sza"], fit[, "d.22.23"])
  gg2 <- predict(gg1, orig$satz)
  
  # --- Store standardized difference in "orig"
  
  orig$cen.39.40.ref.new <- (orig$cen.3959 - orig$cen.4050) - gg2$y
  
  rm(infile, fit, gg1, gg2); gc()
  
  # --- AQUA SST image day and night trees
  
  # --- Create object to hold results of Aqua image sst tree
  
  aqua.img.SST.tree.class <- rep(NA, times = nrow(orig))
  
  # ----------------------------------#
  # --- AQUA SST image NIGHT tree
  # ----------------------------------#
  
  # --- Define variables to be used in image SST nightime tree 
  
  nite <- orig$solz >= 90
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
  glint <- (orig$glint[day])                            # Glint
  
  # --- Implement AQUA SST image DAYTIME tree (in document V4) page 82
  # --- NOTE: modified to not apply vis test in glint so pixels in glint
  # --- always pass tree - KAK dec 2013
  
  bb2 <- ifelse( ((glint == 0 & c6 >= 0.17245 &  d1112 < 0.6646) |
    (glint == 0 & c6 >= 0.17245 & d1112 >= 0.6646 & d11 >= 0.43735 )|
    (glint == 0 & c6 < 0.17245 & c13 >= 0.00685 & d1112 >= 0.57935)) ,
    "P-Bad", "P-Good")
  
  # --- Store results from SST image daytime tree
  
  aqua.img.SST.tree.class[which(day)] <- bb2
  
  table(aqua.img.SST.tree.class, useNA = "always")
  
  # --- Store tree results in "orig"
  
  orig$img.SST.tree.class <- aqua.img.SST.tree.class
  
  # ----------------------------------#
  # --- AQUA SST4 image NIGHT tree
  # ----------------------------------#
  
  # --- Create object to hold results of Aqua image SST4 tree
  
  aqua.img.SST4.tree.class <- rep(NA, times = nrow(orig))
    
  # --- Define variables to be used in image SST4 nightime tree 
  
  nite <- orig$solz >= 90
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

  # End of working with AQUA


} else if (config$sensor == "Terra") {
  
  #---  WORK WITH TERRA
  
  # --- Recalculate standardized difference between MODIS channels 22 and 23
  # --- Read TERRA curve fit as part of version 6.3 processing
  
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
   
  # --- TERRA SST image day and night trees
  
  # --- Create object to hold results of TERRA image sst tree
  
  terra.img.SST.tree.class <- rep(NA, times = nrow(orig))
  
  # ----------------------------------#
  # --- TERRA SST image NIGHT tree
  # ----------------------------------#
  
  # --- Define variables to be used in image SST nightime tree 
  
  nite <- orig$solz >= 90
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
  glint <- orig$glint[day]                              # Sun glint
  
  # --- Implement TERRA SST image DAYTIME tree (in document V4)
  #implement tree from terra_collection6_v4 document page 83
  #as this is what l2gen does- KAK dec 2013
    
  bb2 <- ifelse( (glint == 0 & c6 >= 0.12605 & d1112 < 0.5582) |
    (glint == 0 & c6 >= 0.12605 & d1112 >= 0.5582 & d11 >= 0.36735) |
    (glint == 0 & c6 >= 0.12605 & d1112 >=  0.5582 & d11 < 0.36735 & c6 >= 24.00315) |
    (glint == 0 & c6 < 0.12605 & c13 >= 0.00535 & d1112 >= 0.47445),
    "P-Bad", "P-Good")
  
  # commenting out these lines have no idea where this tree came from
  # bb2 <- ifelse(
  #  ((c6 >= 0.16885) & (d1112 < 0.55605)) |
  #  ((c6 >= 0.16885) & (d1112 >= 0.55605) & (d11 >= 0.4042)) |
  #  ((c6 < 0.16885) & (c13 >= 0.00595) & (d1112 >= 0.57)),  
  #  "P-Bad", "P-Good")
   
  # --- Store results from SST image daytime tree
  
  terra.img.SST.tree.class[which(day)] <- bb2
  table(terra.img.SST.tree.class, useNA = "always")
  
  # --- Store tree results in "orig"
  
  orig$img.SST.tree.class <- terra.img.SST.tree.class
  
  # ----------------------------------#
  # --- TERRA SST4 image NIGHT tree
  # ----------------------------------#
  
  # --- Create object to hold results of Aqua image SST4 tree
  
  terra.img.SST4.tree.class <- rep(NA, times = nrow(orig))
  
  # --- Define variables to be used in image SST4 nightime tree 
  
  nite <- orig$solz >= 90
  d3 <- (orig$max.3959[nite] - orig$min.3959[nite])       # max minus min BT ch22 inside box
  d11 <- (orig$max.11000[nite] - orig$min.11000[nite])    # max minus min BT ch31 inside box
  d34 <- orig$cen.39.40.ref.new[nite]   # residuals from std diff between BT22 and BT23
  d411 <- orig$cen.4050[nite] - orig$cen.11000[nite]      # BT23 - BT31
  
  # --- Implement TERRA SST4 image NIGHT tree (**NOTE: Here we use version in document V3)
  
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
  
}  # End of working with TERRA
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save "orig" data frame into a data frame for the sensor in question... ----

if (config$sensor == "Terra") {
  TERRA <- orig     	# Store object "orig" in object "TERRA"
} else if (config$sensor == "Aqua") {
  AQUA <- orig 				# Store object "orig" in object "AQUA"
}	
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Export a text file with processed data frame ----
# --- Includes all variables for a sensor.

# --- Directory where processed data.frame will be placed

results.outdir <- paste0("D:/Matchups/MODIS/results/",
  "version_", config$matchups$version, "/",
  config$sensor, "/collection_",
  config$collection,
  "/results/") 

# --- First, convert buoy and satellite dates/times to character vectors
# --- so that they are written out surrounded by quotes and thus can be read in
# --- as a single string.

orig.write <- orig   	# Create bogus object "orig.write" that will be written out

orig.write$buoy.timedate <- as.character(orig.write$buoy.timedate)
orig.write$sat.timedate <- as.character(orig.write$sat.timedate)

out.data.file <- paste(results.outdir,
  config$sensor, "_matchups_",
  config$matchups$version,
  "_dataframe.txt", sep="")

write.table(orig.write, file=out.data.file, append=FALSE,
  sep=" ", na="NA", row.names=FALSE, col.names=TRUE, quote=TRUE)
gc()

# --- Also write out a text file using dput() that
# --- can be easily regenerated

out.data.file2 <- paste(results.outdir,
  config$sensor, "_matchups_",
  config$matchups$version,
  "_dputoutput.txt", sep="")

dput(orig, file=out.data.file2)
gc()

# uuu <- dget(out.data.file2)

rm(out.data.file, out.data.file2, orig.write); gc()
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

table(orig$img.SST.tree.class, useNA = "always")
table(orig$img.SST4.tree.class, useNA = "always")

