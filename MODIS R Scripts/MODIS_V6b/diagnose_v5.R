Sys.setenv(TZ = "UTC") # Define local time to be UTC

# ---------------------------------------------------------------------------------------#
# --- Plot RVS effects for SST4 -----

# --- Adjust a line to preliminary SST4 residuals as a function of satellite zenith angle.
# --- Fit is made for satellite zenith angles between -60 and +60 degrees.

AQUA.sst4.resids <- AQUA$cen.sst4 - AQUA$buoy.sst

# ***** USE THESE FILTERS - DO NOT SEPARATE BY MIRROR
uuu <- abs(AQUA$satz) < 55 & AQUA$qsst4 == 0
uu2 <- abs(AQUA$satz) >= 55 & AQUA$qsst4 <= 1
use <- (uuu | uu2)

gg4 <- loess(AQUA.sst4.resids[use] ~ AQUA$satz[use], span=0.35)  # Fit trend
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))  							# predict value for regular sequence
aqua.rvs <- data.frame(sza = seq(-60.0, 60.0, 0.5), SST4res = gg5)	# make data frame

TERRA.sst4.resids <- TERRA$cen.sst4 - TERRA$buoy.sst

uuu <- abs(TERRA$satz) < 55 & TERRA$qsst4 == 0
uu2 <- abs(TERRA$satz) >= 55 & TERRA$qsst4 <= 1
use <- (uuu | uu2)
  
gg4 <- loess(TERRA.sst4.resids[use] ~ TERRA$satz[use], span=0.35)  # Fit trend
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))    						# predict value for regular sequence
TERRA.rvs <- data.frame(sza = seq(-60.0, 60.0, 0.5), SST4res = gg5)	# make data frame

# --- Grafico 

figura <- paste(config$results.dir,"RVS_sst4.pdf", sep="")  

CairoPDF(file = figura,
         width = 6, height = 5,
         bg = "transparent", pointsize = 14, family="Helvetica")

plot(aqua.rvs$sza, aqua.rvs$SST4res, type="l",
     xlab="Satellite zenith angle",
     ylab="SST4 residuals (K)",
     lwd=3, col="black",
     ylim = c(-0.6, -0.05))

lines(TERRA.rvs$sza, TERRA.rvs$SST4res, col = 'grey60', lwd=3)
abline(h=-0.17, col = "grey70")

legend("bottom",
       legend = c("AQUA", "TERRA"),
       lwd = 2,
       col = c("black","grey60"), bty="n")

dev.off()

rm(use,gg4,gg5,gg6);
rm(sst4.resids, use, AQUA.sst4.resids, TERRA.sst4.resids, figura,
   aqua.rvs, TERRA.rvs); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot RVS effects for SST -----

# --- Adjust a line to preliminary SST residuals as a function of satellite zenith angle.
# --- Fit is made for satellite zenith angles between -60 and +60 degrees.

AQUA.sst.resids <- AQUA$cen.sst - AQUA$buoy.sst

# ***** USE THESE FILTERS - DO NOT SEPARATE BY MIRROR
uuu <- abs(AQUA$satz) < 55 & AQUA$qsst == 0 & AQUA$solz > 90
uu2 <- abs(AQUA$satz) >= 55 & AQUA$qsst <= 1 & AQUA$solz > 90
use <- (uuu | uu2)
  
gg4 <- loess(AQUA.sst.resids[use] ~ AQUA$satz[use], span=0.35)  # Fit trend
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))    						# predict value for regular sequence
aqua.rvs <- data.frame(sza = seq(-60.0, 60.0, 0.5), sstres = gg5)	# make data frame

TERRA.sst.resids <- TERRA$cen.sst - TERRA$buoy.sst

uuu <- abs(TERRA$satz) < 55 & TERRA$qsst == 0 & TERRA$solz > 90
uu2 <- abs(TERRA$satz) >= 55 & TERRA$qsst <= 1 & TERRA$solz > 90
use <- (uuu | uu2)

gg4 <- loess(TERRA.sst.resids[use] ~ TERRA$satz[use], span=0.35)  # Fit trend
gg5 <- predict(gg4, seq(-60.0, 60.0, 0.5))    						# predict value for regular sequence
TERRA.rvs <- data.frame(sza = seq(-60.0, 60.0, 0.5), sstres = gg5)	# make data frame

# --- Grafico 

figura <- paste(config$results.dir,"RVS_sst.pdf", sep="")  

Cairo::CairoPDF(file = figura,
         width = 6, height = 5,
         bg = "transparent", pointsize = 14, family="Arial")

plot(aqua.rvs$sza, aqua.rvs$sstres, type="l",
     xlab="Satellite zenith angle",
     ylab="SST residuals (K)",
     lwd=3, col="black",
     ylim = c(-0.6, -0.05))

lines(TERRA.rvs$sza, TERRA.rvs$sstres, col = 'grey60', lwd=3)
abline(h=-0.17, col = "grey70")

legend("bottom",
       legend = c("AQUA", "TERRA"),
       lwd = 2,
       col = c("black","grey60"), bty="n")

dev.off()

rm(use,gg4,gg5,gg6, figura,aqua.mirror1,aqua.rvs,
   TERRA.rvs, AQUA.sst.resids, TERRA.sst.resids); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot AQUA and TERRA SST residuals corrected by mirror side ----

library(xts, dplyr, Cairo, lubridate)

AQUA2 <- dplyr::select(AQUA, cen.sst, cen.sst4, buoy.sst, mirror,
                       sat.moyr, qsst, solz) %>%
  dplyr::filter(.,  qsst == 0 & solz > 90) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst)

# --- Time series of AQUA SST residuals, mirror side 1

AQUA3.m1 <- dplyr::filter(AQUA2, mirror == 1) %>%
  dplyr::group_by(., sat.moyr) %>%
  dplyr::summarize(., med.sst.resids = median(sst.resids.v5, na.rm = TRUE))

ff1 <- as.POSIXlt(paste0("15-", as.character(AQUA3.m1$sat.moyr)),
                  tz = "UTC", format = "%d-%b-%Y")
AQUA4.m1 <- xts(AQUA3.m1[,-1], order.by = ff1)

# --- Time series of AQUA SST residuals, mirror side 1

AQUA3.m2 <- dplyr::filter(AQUA2, mirror == 2) %>%
  dplyr::group_by(., sat.moyr) %>%
  dplyr::summarize(., med.sst.resids = median(sst.resids.v5, na.rm = TRUE))

ff1 <- as.POSIXlt(paste0("15-", as.character(AQUA3.m2$sat.moyr)),
                  tz = "UTC", format = "%d-%b-%Y")
AQUA4.m2 <- xts(AQUA3.m2[,-1], order.by = ff1)

# --- Plot time series of median SST by mirror

figura <- paste(config$results.dir,"AQUA_sstresid_by_mirror.pdf", sep="")  

#setEPS()
#postscript(figura, width = 6, height = 5, pointsize = 14)

Cairo::CairoPDF(file = figura,
                width = 6, height = 5,
                bg = "transparent", pointsize = 14)

plot.zoo(AQUA4.m1,
         screens = 1, plot.type = "multiple",
         xlab = "Year", ylab = "Median SST residuals (K)",
         ylim = c(-0.5, 0.0),xlim = c(ymd("2000-09-01"), ymd("2014-01-01")),
         lwd=2, col="black")

lines(AQUA4.m2, lwd=2, col="grey60")
abline(h = -0.17, col = "grey70")

legend("bottomright", title = "AQUA",
       legend = c("Mirror side 1", "Mirror side 2"),
       lwd = 2,
       col = c("black","grey60"), bty="n")

dev.off()

# --- TERRA SST residuals by mirror side

TERRA2 <- dplyr::select(TERRA, cen.sst, cen.sst4, buoy.sst, mirror,
                       sat.moyr, qsst, solz) %>%
  dplyr::filter(.,  qsst == 0 & solz > 90) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst)

# --- Time series of TERRA SST residuals, mirror side 1

TERRA3.m1 <- dplyr::filter(TERRA2, mirror == 1) %>%
  dplyr::group_by(., sat.moyr) %>%
  dplyr::summarize(., med.sst.resids = median(sst.resids.v5, na.rm = TRUE))

ff1 <- as.POSIXlt(paste0("15-", as.character(TERRA3.m1$sat.moyr)),
                  tz = "UTC", format = "%d-%b-%Y")

TERRA4.m1 <- xts(TERRA3.m1[,-1], order.by = ff1)

# --- Time series of TERRA SST residuals, mirror side 1

TERRA3.m2 <- dplyr::filter(TERRA2, mirror == 2) %>%
  dplyr::group_by(., sat.moyr) %>%
  dplyr::summarize(., med.sst.resids = median(sst.resids.v5, na.rm = TRUE))

ff1 <- as.POSIXlt(paste0("15-", as.character(TERRA3.m2$sat.moyr)),
                  tz = "UTC", format = "%d-%b-%Y")

TERRA4.m2 <- xts(TERRA3.m2[,-1], order.by = ff1)

# --- Plot time series of median SST by mirror

figura <- paste(config$results.dir,"TERRA_sstresid_by_mirror.PDF", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 5,
                bg = "transparent", pointsize = 14)

plot.zoo(TERRA4.m1,
         screens = 1, plot.type = "multiple",
         xlab = "Year", ylab = "Median SST residuals (K)",
         ylim = c(-0.5, 0.0), xlim = c(ymd("2000-09-01"), ymd("2013-01-01")),
         lwd=2, col="black")

lines(TERRA4.m2, lwd=2, col="grey60")

legend("bottomright", title = "TERRA",
       legend = c("Mirror side 1", "Mirror side 2"),
       lwd = 2,
       col = c("black","grey60"), bty="n")

dev.off()

rm(AQUA2,AQUA3.m1,AQUA3.m2,AQUA4.m1,AQUA4.m2,ff1,figura,TERRA2,
   TERRA3.m1, TERRA3.m2, TERRA4.m1, TERRA4.m2, uu2, uuu); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot TERRA SST residuals NOT corrected by mirror side ----

TERRA2 <- dplyr::select(TERRA, cen.sst, cen.sst4, buoy.sst, mirror,
                        sat.timedate, sat.moyr, qsst, solz) %>%
  dplyr::filter(.,  qsst == 0 & solz > 90) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst) %>%
  dplyr::arrange(., sat.timedate)

# --- Remove correction for mirror side effects.
# --- Mirror side 2 is left untouched; corrections are made to side 1.

uu1 <- cut(TERRA2$sat.timedate,
  breaks = ymd(c("2000-01-01","2000-02-01","2000-03-01","2000-04-01","2000-05-01",
  "2000-06-01","2000-07-01","2000-08-01","2000-09-01","2000-10-01",
  "2000-11-01","2000-12-01",
  "2001-01-01","2001-02-01","2001-03-01","2001-04-01","2001-05-01",
  "2001-06-01","2001-07-01","2001-08-01","2001-09-01","2001-10-01",
  "2001-11-01","2001-12-01",
  "2002-01-01","2002-02-01","2002-03-01","2002-04-01","2002-05-01",
  "2002-06-01","2003-06-01","2014-12-31")),
  include.lowest = TRUE)

correction <- c(-0.224, -0.212, -0.200, -0.188, -0.176, -0.164, -0.152, -0.140, -0.128,
                -0.116, -0.104, -0.092, -0.080, -0.068, -0.056, -0.044, -0.032, -0.020,
                -0.008, 0.004, 0.016, 0.028, 0.040, 0.053, 0.064, 0.076, 0.088, 0.100,
                0.120, 0.120, 0.120)

correction2 <- correction[as.numeric(uu1)]
correction2 <- ifelse(TERRA2$mirror == 1, correction2, 0)
  
TERRA2$sst.resids.v5 <- TERRA2$sst.resids.v5 - correction2

# --- Time series of TERRA SST residuals, mirror side 1

TERRA3.m1 <- dplyr::filter(TERRA2, mirror == 1) %>%
  dplyr::group_by(., sat.moyr) %>%
  dplyr::summarize(., med.sst.resids = median(sst.resids.v5, na.rm = TRUE))

ff1 <- as.POSIXlt(paste0("15-", as.character(TERRA3.m1$sat.moyr)),
                  tz = "UTC", format = "%d-%b-%Y")

TERRA4.m1 <- xts(TERRA3.m1[,-1], order.by = ff1)

# --- Time series of TERRA SST residuals, mirror side 2

TERRA3.m2 <- dplyr::filter(TERRA2, mirror == 2) %>%
  dplyr::group_by(., sat.moyr) %>%
  dplyr::summarize(., med.sst.resids = median(sst.resids.v5, na.rm = TRUE))

ff1 <- as.POSIXlt(paste0("15-", as.character(TERRA3.m2$sat.moyr)),
                  tz = "UTC", format = "%d-%b-%Y")

TERRA4.m2 <- xts(TERRA3.m2[,-1], order.by = ff1)

figura <- paste(config$results.dir,"TERRA_sstresid_NOTCORRECTED_by_mirror.PDF", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 5,
                bg = "transparent", pointsize = 14)

plot.zoo(TERRA4.m1,
         screens = 1, plot.type = "multiple",
         xlab = "Year", ylab = "Median SST residuals (K)",
         ylim = c(-0.5, 0.0), xlim = c(ymd("2000-09-01"), ymd("2013-01-01")),
         lwd=2, col="black")

lines(TERRA4.m2, lwd=2, col="grey60")

legend("bottom", title = "TERRA",
       legend = c("Mirror side 1", "Mirror side 2"),
       lwd = 2,
       col = c("black","grey60"), bty="n")

dev.off()

rm(ff1,figura,TERRA2, TERRA3.m1, TERRA3.m2, TERRA4.m1, TERRA4.m2,
   uu1, uu2, correction, correction2); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Time series of number of matchups per month and latitude band ----

library(xts, dplyr, Cairo, zoo, lattice)

# --- AQUA number of matchups

AQUA2 <- dplyr::select(AQUA, cen.sst, cen.sst4, qsst, qsst4,
                       buoy.sst, latband, solz,
                       sat.moyr, ref.type.1.SST) %>%
  dplyr::filter(., qsst == 0 & abs(ref.type.1.SST - buoy.sst) < 2 & solz > 90) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., n.matchups = length(sst.resids.v5))

AQUA2$sat.moyr <- as.Date(paste0("15-", as.character(AQUA2$sat.moyr)),
                             tz = "UTC", format = "%d-%b-%Y")

AQUA2 <- AQUA2[order(AQUA2$sat.moyr), ]

figura <- paste(config$results.dir,"AQUA_n_matchups.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 6,
                bg = "transparent", pointsize = 14, family="Helvetica")

xyplot(n.matchups ~ sat.moyr | latband, data = AQUA2,
       layout = c(2,3),
       main = "Aqua",
       xlab = "Year", 
       xlim = c(as.Date("2000-09-01"), as.Date("2014-01-10")),
       ylab = "Number of matchups",
       panel = function(x, y, ...) { 
         panel.xyplot(x, y, type="l", lwd = 2, col="steelblue")
         panel.abline(h=100, col = "grey70")
       })

dev.off()

# --- TERRA number of matchups

TERRA2 <- dplyr::select(TERRA, cen.sst, cen.sst4, qsst, qsst4,
                       buoy.sst, latband, solz,
                       sat.moyr, ref.type.1.SST) %>%
  dplyr::filter(., qsst == 0 & abs(ref.type.1.SST - buoy.sst) < 2 & solz > 90) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., n.matchups = length(sst.resids.v5))

TERRA2$sat.moyr <- as.Date(paste0("15-", as.character(TERRA2$sat.moyr)),
                          tz = "UTC", format = "%d-%b-%Y")

TERRA2 <- TERRA2[order(TERRA2$sat.moyr), ]

figura <- paste(config$results.dir,"TERRA_n_matchups.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 6,
                bg = "transparent", pointsize = 14, family="Arial")

xyplot(n.matchups ~ sat.moyr | latband, data = TERRA2,
       layout = c(2,3),
       main = "Terra",
       xlab = "Year", 
       xlim = c(as.Date("2000-09-01"), as.Date("2014-01-10")),
       ylab = "Number of matchups",
       panel = function(x, y,...) { 
         panel.xyplot(x, y, type="l", lwd = 2, col="steelblue")
         panel.abline(h=100, col = "grey70")
       })

dev.off()

rm(AQUA2, TERRA2)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot time series of median, MAD and robust SD of SST residuals ----

rob.sd <- function(x) {(IQR(x, na.rm = TRUE)) / 1.348}

AQUA2 <- dplyr::select(AQUA, cen.sst, cen.sst4, qsst, qsst4,
                       buoy.sst, latband,
                       sat.moyr, ref.type.1.SST) %>%
  dplyr::filter(., qsst == 0 & abs(ref.type.1.SST - buoy.sst) < 2) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., med = median(sst.resids.v5, na.rm = TRUE),
                   MAD = mad(sst.resids.v5, na.rm = TRUE),
                   rsd = rob.sd(sst.resids.v5))

tt1 <- reshape2::melt(AQUA2, id.vars=c("sat.moyr", "latband"))
tt1b <- data.frame(sat = "AQUA", tt1)

TERRA2 <- dplyr::select(TERRA, cen.sst, cen.sst4, qsst, qsst4,
                       buoy.sst, latband,
                       sat.moyr, ref.type.1.SST) %>%
  dplyr::filter(., qsst == 0 & abs(ref.type.1.SST - buoy.sst) < 2) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., med = median(sst.resids.v5, na.rm = TRUE),
                   MAD = mad(sst.resids.v5, na.rm = TRUE),
                   rsd = rob.sd(sst.resids.v5))

tt2 <- reshape2::melt(TERRA2, id.vars=c("sat.moyr", "latband"))
tt2b <- data.frame(sat = "TERRA", tt2)

tt3 <- rbind(tt1b, tt2b)

tt3$sat.moyr <- as.Date(paste0("15-", as.character(tt3$sat.moyr)),
                        tz = "UTC", format = "%d-%b-%Y")

# --- Plot median of SST residuals

tt4 <- dplyr::filter(tt3, variable == "med") %>%
  dplyr::arrange(., sat.moyr, latband, sat) %>%
  dplyr::select(., sat.moyr, latband, sat, value)

figura <- paste(config$results.dir,"median_SST_resids.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt4,
       type = "l",
       ylim=c(-0.8, 0.3),     
       main=paste("Median of SST residuals"),
       xlab="Time",
       ylab="Median of SST residuals (K)",
       col=c("black","grey60"), lwd=c(2, 2),
       groups = sat, 
       panel = panel.superpose, 
       panel.groups = function(x, y, ...) { 
         panel.abline(h=-0.17, col = "steelblue")
         panel.xyplot(x, y, ...)},
       layout=c(1,6),
       strip = strip.custom(par.strip.text = list(cex = 0.9)),
       par.settings = list(layout.heights=list(strip=0.7)))
       

dev.off()

# --- Plot MAD of SST residuals

tt5 <- dplyr::filter(tt3, variable == "MAD") %>%
  dplyr::arrange(., sat.moyr, latband, sat) %>%
  dplyr::select(., sat.moyr, latband, sat, value)

figura <- paste(config$results.dir,"mad_SST_resids.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt5,
                type = "l",
                ylim=c(0, 0.85),     
                main=paste("MAD of SST residuals"),
                xlab="Time",
                ylab="MAD of SST residuals (K)",
                col=c("black","grey60"), lwd=c(2, 2),
                groups = sat, 
                panel = panel.superpose, 
                panel.groups = function(x, y, ...) { 
                  panel.xyplot(x, y, ...)},
                layout=c(1,6),
                strip = strip.custom(par.strip.text = list(cex = 0.9)),
                par.settings = list(layout.heights=list(strip=0.7)))

dev.off()

# --- Plot robust standard deviation of SST residuals

tt5 <- dplyr::filter(tt3, variable == "rsd") %>%
  dplyr::arrange(., sat.moyr, latband, sat) %>%
  dplyr::select(., sat.moyr, latband, sat, value)

figura <- paste(config$results.dir,"rsd_SST_resids.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt5,
                type = "l",
                ylim=c(0, 0.85),     
                main=paste("Robust SD of SST residuals"),
                xlab="Time",
                ylab="Robust SD of SST residuals (K)",
                col=c("black","grey60"), lwd=c(2, 2),
                groups = sat, 
                panel = panel.superpose, 
                panel.groups = function(x, y, ...) { 
                  panel.xyplot(x, y, ...)},
                layout=c(1,6),
                strip = strip.custom(par.strip.text = list(cex = 0.8)),
                par.settings = list(layout.heights=list(strip=0.7)))

dev.off()

rm(AQUA2, TERRA2, tt1, tt1b,tt2, tt2b, tt3, tt4, tt5,figura); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Plot time series of median, MAD and RSD of SST4 residuals ----

# --- AQUA number of matchups

AQUA2 <- dplyr::select(AQUA, cen.sst, cen.sst4, qsst, qsst4,
                       buoy.sst, latband,
                       sat.moyr, ref.type.1.SST) %>%
  dplyr::filter(., qsst4 == 0 & abs(ref.type.1.SST - buoy.sst) < 2) %>%
  dplyr::mutate(., sst4.resids.v5 = cen.sst4 - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., med = median(sst4.resids.v5, na.rm = TRUE),
                  MAD = mad(sst4.resids.v5, na.rm = TRUE),
                  rsd = rob.sd(sst4.resids.v5))

tt1 <- reshape2::melt(AQUA2, id.vars=c("sat.moyr", "latband"))
tt1b <- data.frame(sat = "AQUA", tt1)

TERRA2 <- dplyr::select(TERRA, cen.sst, cen.sst4, qsst, qsst4,
                        buoy.sst, latband,
                        sat.moyr, ref.type.1.SST) %>%
  dplyr::filter(., qsst4 == 0 & abs(ref.type.1.SST - buoy.sst) < 2) %>%
  dplyr::mutate(., sst4.resids.v5 = cen.sst4 - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., med = median(sst4.resids.v5, na.rm = TRUE),
                   MAD = mad(sst4.resids.v5, na.rm = TRUE),
                  rsd = rob.sd(sst4.resids.v5))

tt2 <- reshape2::melt(TERRA2, id.vars=c("sat.moyr", "latband"))
tt2b <- data.frame(sat = "TERRA", tt2)

tt3 <- rbind(tt1b, tt2b)

tt3$sat.moyr <- as.Date(paste0("15-", as.character(tt3$sat.moyr)),
                        tz = "UTC", format = "%d-%b-%Y")

# --- Plot median of SST4 residuals

tt4 <- dplyr::filter(tt3, variable == "med") %>%
  dplyr::arrange(., sat.moyr, latband, sat) %>%
  dplyr::select(., sat.moyr, latband, sat, value)

figura <- paste(config$results.dir,"median_SST4_resids.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt4,
                type = "l",
                ylim=c(-0.8, 0.3),     
                main=paste("Median of SST4 residuals"),
                xlab="Time",
                ylab="Median of SST4 residuals (K)",
                col=c("black","grey60"), lwd=c(2, 2),
                groups = sat, 
                panel = panel.superpose, 
                panel.groups = function(x, y, ...) { 
                  panel.abline(h=-0.17, col = "steelblue")
                  panel.xyplot(x, y, ...)},
                layout=c(1,6),
                strip = strip.custom(par.strip.text = list(cex = 0.9)),
                par.settings = list(layout.heights=list(strip=0.7)))


dev.off()

# --- Plot MAD of SST4 residuals

tt5 <- dplyr::filter(tt3, variable == "MAD") %>%
  dplyr::arrange(., sat.moyr, latband, sat) %>%
  dplyr::select(., sat.moyr, latband, sat, value)

figura <- paste(config$results.dir,"mad_SST4_resids.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt5,
                type = "l",
                ylim=c(0, 0.85),     
                main=paste("MAD of SST4 residuals"),
                xlab="Time",
                ylab="MAD of SST4 residuals (K)",
                col=c("black","grey60"), lwd=c(2, 2),
                groups = sat, 
                panel = panel.superpose, 
                panel.groups = function(x, y, ...) { 
                  panel.xyplot(x, y, ...)},
                layout=c(1,6),
                strip = strip.custom(par.strip.text = list(cex = 0.9)),
                par.settings = list(layout.heights=list(strip=0.7)))

dev.off()

# --- Plot robust standard deviation of SST4 residuals

tt5 <- dplyr::filter(tt3, variable == "rsd") %>%
  dplyr::arrange(., sat.moyr, latband, sat) %>%
  dplyr::select(., sat.moyr, latband, sat, value)

figura <- paste(config$results.dir,"rsd_SST4_resids.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt5,
                type = "l",
                ylim=c(0, 0.85),     
                main=paste("Robust SD of SST4 residuals"),
                xlab="Time",
                ylab="Robust SD of SST4 residuals (K)",
                col=c("black","grey60"), lwd=c(2, 2),
                groups = sat, 
                panel = panel.superpose, 
                panel.groups = function(x, y, ...) { 
                  panel.xyplot(x, y, ...)},
                layout=c(1,6),
                strip = strip.custom(par.strip.text = list(cex = 0.8)),
                par.settings = list(layout.heights=list(strip=0.7)))

dev.off()



rm(AQUA2, TERRA2, tt1, tt1b,tt2, tt2b, tt3, tt4, tt5,figura); gc()
# ----------------------------------------------------------------------------------------








# ---------------------------------------------------------------------------------------#
# --- EXPERIMENT: Use coeffs for 20-40N to compute global SST ----

# --- Read v6 coefficients

config$sensor <- "Terra"
config$sensor_uc <- "TERRA"

if (config$sensor == "Aqua") {
  coeffs.SST.latband1.df <- dget("D:/Matchups/MODIS/results/version_6/Aqua/collection_6/coeffs/AQUA_collection_6_SST_latband1_coeffs_v6.3_dput_final.txt")
} else if (config$sensor == "Terra") {
  coeffs.SST.latband1.df <- dget("D:/Matchups/MODIS/results/version_6/Terra/collection_6/coeffs/TERRA_collection_6_SST_latband1_coeffs_v6.3_dput_final.txt")
}

coeffs.20.40 <- dplyr::filter(coeffs.SST.latband1.df, lat == 5) # Coefficients for 20-40N latband

if (config$sensor_uc == "AQUA") {
  orig2 <- AQUA   # Create new working object "orig2" with AQUA data
} else if (config$sensor_uc == "TERRA") {
  orig2 <- TERRA   # Create new working object "orig2" with TERRA data
} else {
  stop("Sensor name is incorrect. Check configuraytion file...\n")
}


# --- Add numeric values for mon and latband to "orig2"

orig2 <- data.frame(orig2, mon=orig2$sat.mon, lat=as.numeric(orig2$latband))

# --- Merge the orig2 data frame with the coefficients data frame
# --- Note we are using the join() command un the dplyr library.
# --- Unlike merge(), join() does not change the order of the first data.frame toe be merged.
# --- Nevertheless, because of the way we do things, the result should be cooorect.
# --- For this reason, there shpould not be a need to use

aa2 <- dplyr::inner_join(x = orig2, y = coeffs.20.40, by = "mon")

# --- Verify that we have a single SST coefficient value for each month/latband combination

ttt <- tapply(aa2$coeff1, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff2, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff3, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff4, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff5, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff6, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff7, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

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

library(circular)
secant.deg <- function(x) {1 / (cos(rad(x)))}

x1 <- aa2$cen.11000    									  # BT31 (brigthness temperature for channel 31)
x2 <- aa2$cen.11000 - aa2$cen.12000			  # BT31 - BT32
x3 <- aa2$buoy.sst											  # Buoy SST
x4 <- aa2$satz											      # satellite zenith angle (no sign)
x5 <- x2 * x3														  # BT31-BT32 * buoy SST
x6 <- (secant.deg(aa2$satz) - 1) * x2     # Secant times BT31-BT32
x7 <- ifelse(aa2$mirror == 1, 0, 1)       # Dummy mirror variable 0 for side1, 1 for side2

# --- Compute "latband1" SSTs and residuals for ALL the data.

SST.20.40 <- coef1 + (coef2 * x1) + (coef3 * x5) + (coef4 * x6) +
  (coef5 * x7) + (coef6 * x4) + (coef7 * (x4 ^ 2))

# --- Compute "latband1" SST residuals as (satellite minus buoy).
# --- NOTE: Be careful to use object "aa2",
# --- otherwise records may be misaligned with "orig".

SST.20.40.res <- SST.20.40 - aa2$buoy.sst		# SST residuals

orig2$SST.20.40 <- SST.20.40
orig2$SST.20.40.res <- SST.20.40.res


if (config$sensor_uc == "AQUA") {
  AQUA2 <- orig2
} else if (config$sensor_uc == "TERRA") {
  TERRA2 <- orig2
} else {
  stop("Sensor name is incorrect. Check configuration file...\n")
}



TERRA3 <- dplyr::select(TERRA2, cen.sst, cen.sst4, qsst, qsst4,
  buoy.sst, latband,
  sat.moyr, ref.type.1.SST, SST.20.40, SST.20.40.res) %>%
  dplyr::filter(., qsst == 0 & abs(ref.type.1.SST - buoy.sst) < 2) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., med1 = median(SST.20.40.res, na.rm = TRUE),
    med2 = median(sst.resids.v5, na.rm = TRUE))

tt2 <- reshape2::melt(TERRA3, id.vars=c("sat.moyr", "latband"))
tt2b <- data.frame(sat = "TERRA", tt2)
tt2b$sat.moyr <- as.Date(paste0("15-", as.character(tt2b$sat.moyr)),
  tz = "UTC", format = "%d-%b-%Y")

tt3 <- dplyr::arrange(tt2b, sat, sat.moyr, latband, variable) %>%
  dplyr::select(., sat.moyr, latband, variable, value)


figura <- paste(config$results.dir,"TERRA_SST_resids_20_40.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt3,
                type = "l", col = c("steelblue","tomato"), lwd = c(2,2),
                ylim=c(-0.8, 1.0),     
                main=paste("TERRA - Median of SST residuals"),
                xlab="Time",
                ylab="Median of SST residuals (K)",
                groups = as.character(variable), 
                panel = panel.superpose, 
                panel.groups = function(x, y,...) { 
                  panel.abline(h=-0.17, col = "steelblue")
                  panel.xyplot(x, y,...)},
                layout=c(1,6),
                strip = strip.custom(par.strip.text = list(cex = 0.9)),
                par.settings = list(layout.heights=list(strip=0.7)))

dev.off()









AQUA3 <- dplyr::select(AQUA2, cen.sst, cen.sst4, qsst, qsst4,
                       buoy.sst, latband,
                       sat.moyr, ref.type.1.SST, SST.20.40, SST.20.40.res) %>%
  dplyr::filter(., qsst == 0 & abs(ref.type.1.SST - buoy.sst) < 2) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., med1 = median(SST.20.40.res, na.rm = TRUE),
                   MAD1 = mad(SST.20.40.res, na.rm = TRUE),
                   med2 = median(sst.resids.v5, na.rm = TRUE),
                   MAD2 = mad(sst.resids.v5, na.rm = TRUE))

tt1 <- reshape2::melt(AQUA3, id.vars=c("sat.moyr", "latband"))
tt1b <- data.frame(sat = "AQUA", tt1)


tt3 <- rbind(tt1b, tt2b)

tt3$sat.moyr <- as.Date(paste0("15-", as.character(tt3$sat.moyr)),
                        tz = "UTC", format = "%d-%b-%Y")

# --- Plot median of SST residuals

tt4 <- dplyr::filter(tt3, variable == "med1" | variable == "med2") %>%
  dplyr::arrange(., sat.moyr, latband, sat) %>%
  dplyr::select(., sat.moyr, latband, sat, value)

figura <- paste(config$results.dir,"median_SST_resids.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt4,
                type = "l",
                ylim=c(-0.8, 0.3),     
                main=paste("Median of SST residuals"),
                xlab="Time",
                ylab="Median of SST residuals (K)",
                col=c("black","grey60"), lwd=c(2, 2),
                groups = sat, 
                panel = panel.superpose, 
                panel.groups = function(x, y, ...) { 
                  panel.abline(h=-0.17, col = "steelblue")
                  panel.xyplot(x, y, ...)},
                layout=c(1,6),
                strip = strip.custom(par.strip.text = list(cex = 0.9)),
                par.settings = list(layout.heights=list(strip=0.7)))


dev.off()





# --- Plot MAD of SST residuals

tt5 <- dplyr::filter(tt3, variable == "MAD") %>%
  dplyr::arrange(., sat.moyr, latband, sat) %>%
  dplyr::select(., sat.moyr, latband, sat, value)

figura <- paste(config$results.dir,"mad_SST_resids.pdf", sep="")  

Cairo::CairoPDF(file = figura,
                width = 6, height = 7,
                bg = "transparent", pointsize = 14, family="Helvetica")

lattice::xyplot(value ~ sat.moyr | latband, data = tt5,
                type = "l",
                ylim=c(0, 0.85),     
                main=paste("MAD of SST residuals"),
                xlab="Time",
                ylab="MAD of SST residuals (K)",
                col=c("black","grey60"), lwd=c(2, 2),
                groups = sat, 
                panel = panel.superpose, 
                panel.groups = function(x, y, ...) { 
                  panel.xyplot(x, y, ...)},
                layout=c(1,6),
                strip = strip.custom(par.strip.text = list(cex = 0.9)),
                par.settings = list(layout.heights=list(strip=0.7)))

dev.off()

rm(AQUA2, TERRA2, tt1, tt1b,tt2, tt2b, tt3, tt4, tt5,figura); gc()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------


AQUA2 <- dplyr::select(AQUA,
                       SST.latband1.res, qsst.new,
                       sat.moyr, ref.type.1.SST) %>%
  dplyr::filter(., qsst == 0 & abs(ref.type.1.SST - buoy.sst) < 2 & solz > 90) %>%
  dplyr::mutate(., sst.resids.v5 = cen.sst - buoy.sst) %>%
  dplyr::group_by(., sat.moyr, latband) %>%
  dplyr::summarize(., n.matchups = length(sst.resids.v5))


