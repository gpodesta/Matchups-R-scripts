

Sys.setenv(TZ = "UTC") # Define local time to be UTC


library(dplyr)

# --- Calculate statistics for V5 MAERI MODIS matchups

# --- Calculate V5 SST and SST4 residuals

MAERI.SST.resids <- orig$cen.sst - orig$buoy.sst

MAERI.SST4.resids <- orig$cen.sst4 - orig$buoy.sst

ttt <- as.data.frame(cbind(MAERI.SST.resids, MAERI.SST4.resids, solz=orig$solz,
  year = orig$sat.yr, qsst=orig$qsst, qsst4=orig$qsst4))

ttt$day <- ifelse(ttt$solz >= 90, "Nite", "Day")

tt2a <- dplyr::group_by(ttt, day, qsst)

tt2b <- dplyr::group_by(ttt, day, year, qsst)

tt3a <- dplyr::summarise(tt2a,
  mean = round(mean(MAERI.SST.resids, na.rm = TRUE),3),
  median = round(median(MAERI.SST.resids, na.rm = TRUE),3),
  sd = round(sd(MAERI.SST.resids, na.rm = TRUE),3),
  mad = round(mad(MAERI.SST.resids, na.rm = TRUE),3),
  IQR = round(IQR(MAERI.SST.resids, na.rm = TRUE),3),
  N = length(MAERI.SST.resids))

tt3b <- dplyr::summarise(tt2b,
  mean = round(mean(MAERI.SST.resids, na.rm = TRUE),3),
  median = round(median(MAERI.SST.resids, na.rm = TRUE),3),
  sd = round(sd(MAERI.SST.resids, na.rm = TRUE),3),
  mad = round(mad(MAERI.SST.resids, na.rm = TRUE),3),
  IQR = round(IQR(MAERI.SST.resids, na.rm = TRUE),3),
  N = length(MAERI.SST.resids))






# --- Work with SST4 residuals

tt4 <- dplyr::filter(ttt, day == "Nite")

tt5a <- dplyr::group_by(tt4, day, qsst4)
tt5b <- dplyr::group_by(tt4, day, year, qsst4)

tt6a <- dplyr::summarise(tt5a,
  mean = mean(MAERI.SST4.resids, na.rm = TRUE),
  median = median(MAERI.SST4.resids, na.rm = TRUE),
  sd = sd(MAERI.SST4.resids, na.rm = TRUE),
  mad = mad(MAERI.SST4.resids, na.rm = TRUE),
  IQR = IQR(MAERI.SST4.resids, na.rm = TRUE),
  N = length(MAERI.SST4.resids))


tt6b <- dplyr::summarise(tt5b,
  mean = mean(MAERI.SST4.resids, na.rm = TRUE),
  median = median(MAERI.SST4.resids, na.rm = TRUE),
  sd = sd(MAERI.SST4.resids, na.rm = TRUE),
  mad = mad(MAERI.SST4.resids, na.rm = TRUE),
  IQR = IQR(MAERI.SST4.resids, na.rm = TRUE),
  N = length(MAERI.SST4.resids))




# ----------------------------------------------------------------------------------------
# --- Calculate V6 SST and SST4 residuals

ttt <- select(orig, SST.latband1.res, SST4.latband1.res, solz, sat.yr, qsst.new, qsst4.new)

ttt$day <- ifelse(ttt$solz >= 90, "Nite", "Day")

tt2a <- group_by(ttt, day, qsst.new)

tt2b <- group_by(ttt, day, sat.yr, qsst.new)

tt3a <- summarise(tt2a,
  mean = round(mean(SST.latband1.res, na.rm = TRUE),3),
  median = round(median(SST.latband1.res, na.rm = TRUE),3),
  sd = round(sd(SST.latband1.res, na.rm = TRUE),3),
  mad = round(mad(SST.latband1.res, na.rm = TRUE),3),
  IQR = round(IQR(SST.latband1.res, na.rm = TRUE),3),
  N = length(SST.latband1.res))

tt3b <- summarise(tt2b,
  mean = round(mean(SST.latband1.res, na.rm = TRUE),3),
  median = round(median(SST.latband1.res, na.rm = TRUE),3),
  sd = round(sd(SST.latband1.res, na.rm = TRUE),3),
  mad = round(mad(SST.latband1.res, na.rm = TRUE),3),
  IQR = round(IQR(SST.latband1.res, na.rm = TRUE),3),
  N = length(SST.latband1.res))


####    SST4 

tt2a <- group_by(ttt, day, qsst4.new)

tt2b <- group_by(ttt, day, sat.yr, qsst4.new)

tt3a <- summarise(tt2a,
  mean = round(mean(SST4.latband1.res, na.rm = TRUE),3),
  median = round(median(SST4.latband1.res, na.rm = TRUE),3),
  sd = round(sd(SST4.latband1.res, na.rm = TRUE),3),
  mad = round(mad(SST4.latband1.res, na.rm = TRUE),3),
  QR = round(IQR(SST4.latband1.res, na.rm = TRUE),3),
  N = length(SST4.latband1.res))

tt3b <- summarise(tt2b,
  mean = round(mean(SST4.latband1.res, na.rm = TRUE),3),
  median = round(median(SST4.latband1.res, na.rm = TRUE),3),
  sd = round(sd(SST4.latband1.res, na.rm = TRUE),3),
  mad = round(mad(SST4.latband1.res, na.rm = TRUE),3),
  IQR = round(IQR(SST4.latband1.res, na.rm = TRUE),3),
  N = length(SST4.latband1.res))











                 



