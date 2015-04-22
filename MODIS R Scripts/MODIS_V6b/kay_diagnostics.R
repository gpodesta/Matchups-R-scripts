

if (!require(lubridate)) {install.packages("lubridate"); require(lubridate)}
if (!require(xts)) {install.packages("xts"); require(xts)}
if (!require(zoo)) {install.packages("zoo"); require(zoo)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)}
if (!require(reshape2)) {install.packages("reshape2"); require(reshape2)}
if (!require(stringr)) {install.packages("stringr"); require(stringr)}
if (!require(Cairo)) {install.packages("Cairo"); require(Cairo)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(lattice)) {install.packages("lattice"); require(lattice)}
# -------------------------------------------------------------------------------

Sys.setenv(TZ = "UTC") 


rms <- function(x) {
  x <- x[!is.na(x)]
  x2 <- x * x
  result <- sqrt( sum(x2) / length(x) )
  return(result)
}

# Select a few fields for V5 SST and SST4 statistics

orig3 <- subset(AQUA, select = c(solz, qsst, qsst4, cen.sst, cen.sst4, buoy.sst))

orig3$sst.res <- orig3$cen.sst - orig3$buoy.sst
orig3$sst4.res <- orig3$cen.sst4 - orig3$buoy.sst

# --- V5 SST Statistics for day + night

tt1 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=min, na.rm=TRUE, simplify=TRUE),3)
tt2 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=quantile, probs=0.25, na.rm=TRUE, simplify=TRUE),3)
tt3 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=median, na.rm=TRUE, simplify=TRUE),3)
tt4 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=mean, na.rm=TRUE, simplify=TRUE),3)
tt5 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3);
tt6 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=max, na.rm=TRUE, simplify=TRUE),3)
tt7 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=rms, simplify=TRUE),3)
tt8 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=sd, na.rm=TRUE, simplify=TRUE),3)
tt9 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=mad, na.rm=TRUE, simplify=TRUE),3)
tt10 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=IQR, na.rm=TRUE, simplify=TRUE),3)
tt11 <- round(tapply(orig3$sst.res, INDEX=orig3$qsst, FUN=length, simplify=TRUE),3)

tt12 <- data.frame(quality=names(table(orig3$qsst)), min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, IQR=tt10, N=tt11);


# --- V5 SST Statistics for night only

orig4 <- subset(orig3, solz >= 90)

tt1 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=min, na.rm=TRUE, simplify=TRUE),3)
tt2 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=quantile, probs=0.25, na.rm=TRUE, simplify=TRUE),3)
tt3 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=median, na.rm=TRUE, simplify=TRUE),3)
tt4 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=mean, na.rm=TRUE, simplify=TRUE),3)
tt5 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3);
tt6 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=max, na.rm=TRUE, simplify=TRUE),3)
tt7 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=rms, simplify=TRUE),3)
tt8 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=sd, na.rm=TRUE, simplify=TRUE),3)
tt9 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=mad, na.rm=TRUE, simplify=TRUE),3)
tt10 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=IQR, na.rm=TRUE, simplify=TRUE),3)
tt11 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=length, simplify=TRUE),3)

tt12 <- data.frame(quality=names(table(orig4$qsst)), min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, IQR=tt10, N=tt11)


# --- SST Statistics for day only

orig4 <- subset(orig3, solz < 90)

tt1 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=min, na.rm=TRUE, simplify=TRUE),3)
tt2 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=quantile, probs=0.25, na.rm=TRUE, simplify=TRUE),3)
tt3 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=median, na.rm=TRUE, simplify=TRUE),3)
tt4 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=mean, na.rm=TRUE, simplify=TRUE),3)
tt5 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3);
tt6 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=max, na.rm=TRUE, simplify=TRUE),3)
tt7 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=rms, simplify=TRUE),3)
tt8 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=sd, na.rm=TRUE, simplify=TRUE),3)
tt9 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=mad, na.rm=TRUE, simplify=TRUE),3)
tt10 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=IQR, na.rm=TRUE, simplify=TRUE),3)
tt11 <- round(tapply(orig4$sst.res, INDEX=orig4$qsst, FUN=length, simplify=TRUE),3)

tt12 <- data.frame(quality=names(table(orig4$qsst)), min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, IQR=tt10, N=tt11)



# --- V5 SST4 Statistics for night only

orig4 <- subset(orig3, solz >= 90)

tt1 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=min, na.rm=TRUE, simplify=TRUE),3)
tt2 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=quantile, probs=0.25, na.rm=TRUE, simplify=TRUE),3)
tt3 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=median, na.rm=TRUE, simplify=TRUE),3)
tt4 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=mean, na.rm=TRUE, simplify=TRUE),3)
tt5 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3);
tt6 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=max, na.rm=TRUE, simplify=TRUE),3)
tt7 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=rms, simplify=TRUE),3)
tt8 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=sd, na.rm=TRUE, simplify=TRUE),3)
tt9 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=mad, na.rm=TRUE, simplify=TRUE),3)
tt10 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=IQR, na.rm=TRUE, simplify=TRUE),3)
tt11 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=length, simplify=TRUE),3)

tt12 <- data.frame(quality=names(table(orig4$qsst4)), min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, IQR=tt10, N=tt11)
# ----------------------------------------------------------------------------------------



# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# PLOTS OF STATISTICS 
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#

# --- Work with AQUA SST

# -----------------------------------------------------------------------------#
# --- Median of SST residuals (V5 and V6) using quality = 0 ----

sst_v5 <- subset(AQUA, subset = (qsst == 0),
  select = c(buoy.timedate, solz, cen.3750, cen.4050, buoy.sst, cen.sst, latband))

nite <- sst_v5$solz >= 90
sst_v5$nite <- ifelse(nite, "Night", "Day")

sst_v5$sst_v5.res <- sst_v5$cen.sst - sst_v5$buoy.sst 

min.date <- floor_date(min(sst_v5$buoy.timedate, na.rm=TRUE), "month")
max.date <- ceiling_date(max(sst_v5$buoy.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

tt0 <- cut(sst_v5$buoy.timedate, breaks = "months")

tt1 <- tapply(X = sst_v5$sst_v5.res, INDEX = list(tt0, sst_v5$latband),
  FUN = median, na.rm = TRUE, simplify=TRUE)

tt2 <- data.frame(time=seq.months[1:nrow(tt1)], tt1)
colnames(tt2) <- c("date", levels(sst_v5$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable.name="latband", value.name = "v5res")

tt4 <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("v5", times=length(tt3$date)),
  SST.res=tt3$v5res)
colnames(tt4) <- c("date","latband","SST.algo","SST.res")


# --- Statistics for V6 calculated by month/year and quality level for v6

sst_v6 <- subset(AQUA, subset = (qsst.new == 0),
  select = c(buoy.timedate, solz, cen.3750, cen.4050, buoy.sst, SST.latband1, latband))

nite <- sst_v6$solz >= 90
sst_v6$nite <- ifelse(nite, "Night", "Day")

sst_v6$sst_v6.res <- sst_v6$SST.latband1 - sst_v6$buoy.sst

min.date <- floor_date(min(sst_v6$buoy.timedate, na.rm=TRUE), "month")
max.date <- ceiling_date(max(sst_v6$buoy.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

tt0b <- cut(sst_v6$buoy.timedate, breaks = "months")

tt1b <- tapply(X = sst_v6$sst_v6.res, INDEX = list(tt0b, sst_v6$latband),
  FUN = median, na.rm = TRUE, simplify=TRUE)

tt2b <- data.frame(time=seq.months[1:nrow(tt1b)], tt1b)
colnames(tt2b) <- c("date", levels(sst_v6$latband))
rownames(tt2b) <- NULL

tt3b <- melt(tt2b, id.vars=c("date"), variable.name="latband", value.name = "v6res")

tt4b <- data.frame(tt3b$date,
  tt3b$latband,
  SST.algo=rep("v6", times=length(tt3b$date)),
  SST.res=tt3b$v6res)
colnames(tt4b) <- c("date","latband","SST.algo","SST.res")

# --- Put together residuals for v5 and v6

tt5 <- rbind(tt4, tt4b)

xyplot(SST.res ~ date | latband, data = tt5,
  type = "l",
  ylim=c(-0.75, 0.35),
  main=paste("AQUA - Median of SST residuals"),
  xlab="Time",
  ylab="SST residuals (deg C)",
  col=c("tomato","steelblue"), lwd=c(2, 2),
  groups = SST.algo, 
  panel = panel.superpose, 
  panel.groups = function(x, y, ...) { 
    panel.abline(h=-0.17, col="grey", lwd=1)
    panel.xyplot(x, y, ...) },
  layout=c(1,6))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Std dev of SST residuals using quality = 0 ----

sst_v5 <- subset(AQUA, subset = (qsst == 0),
  select = c(buoy.timedate, solz, cen.3750, cen.4050, buoy.sst, cen.sst, latband))

nite <- sst_v5$solz >= 90
sst_v5$nite <- ifelse(nite, "Night", "Day")

sst_v5$sst_v5.res <- sst_v5$cen.sst - sst_v5$buoy.sst 

min.date <- floor_date(min(sst_v5$buoy.timedate, na.rm=TRUE), "month")
max.date <- ceiling_date(max(sst_v5$buoy.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

tt0 <- cut(sst_v5$buoy.timedate, breaks = "months")

tt1 <- tapply(X = sst_v5$sst_v5.res, INDEX = list(tt0, sst_v5$latband),
  FUN = sd, na.rm = TRUE, simplify=TRUE)

tt2 <- data.frame(t=seq.months[1:nrow(tt1)], tt1)
colnames(tt2) <- c("date", levels(sst_v5$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable.name="latband", value.name = "v5res")

tt4 <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("v5", times=length(tt3$date)),
  SST.res=tt3$v5res)
colnames(tt4) <- c("date","latband","SST.algo","SST.res")

# --- Statistics for V6 calculated by month/year and quality level for v6

sst_v6 <- subset(AQUA, subset = (qsst.new == 0),
  select = c(buoy.timedate, solz, cen.3750, cen.4050, buoy.sst, SST.latband1, latband))

nite <- sst_v6$solz >= 90
sst_v6$nite <- ifelse(nite, "Night", "Day")

sst_v6$sst_v6.res <- sst_v6$SST.latband1 - sst_v6$buoy.sst

min.date <- floor_date(min(sst_v6$buoy.timedate, na.rm=TRUE), "month")
max.date <- ceiling_date(max(sst_v6$buoy.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

tt0b <- cut(sst_v6$buoy.timedate, breaks = "months")

tt1b <- tapply(X = sst_v6$sst_v6.res, INDEX = list(tt0b, sst_v6$latband),
  FUN = sd, na.rm = TRUE, simplify=TRUE)

tt2b <- data.frame(time=seq.months[1:nrow(tt1b)], tt1b)
colnames(tt2b) <- c("date", levels(sst_v6$latband))
rownames(tt2b) <- NULL

tt3b <- melt(tt2b, id.vars=c("date"), variable.name="latband", value.name = "v6res")

tt4b <- data.frame(tt3b$date,
  tt3b$latband,
  SST.algo=rep("v6", times=length(tt3b$date)),
  SST.res=tt3b$v6res)
colnames(tt4b) <- c("date","latband","SST.algo","SST.res")

# --- Put together residuals for v5 and v6

tt5 <- rbind(tt4, tt4b)

xyplot(SST.res ~ date | latband, data = tt5,
  type = "l",
  ylim=c(0, 1.2),     
  main=paste("AQUA - Std deviation of SST residuals"),
  xlab="Time",
  ylab="SD of SST residuals (deg C)",
  col=c("tomato","steelblue"), lwd=c(2, 2),
  groups = SST.algo, 
  panel = panel.superpose, 
  panel.groups = function(x, y, ...) { 
    panel.abline(h=-0.17, col="grey", lwd=1)
    panel.xyplot(x, y, ...) },
  layout=c(1,6))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- MAD of SST residuals using quality = 0 ----

sst_v5 <- subset(AQUA, subset = (qsst == 0),
  select = c(buoy.timedate, solz, cen.3750, cen.4050, buoy.sst, cen.sst, latband))

nite <- sst_v5$solz >= 90
sst_v5$nite <- ifelse(nite, "Night", "Day")

sst_v5$sst_v5.res <-sst_v5$cen.sst - sst_v5$buoy.sst 

min.date <- floor_date(min(sst_v5$buoy.timedate, na.rm=TRUE), "month")
max.date <- ceiling_date(max(sst_v5$buoy.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

tt0 <- cut(sst_v5$buoy.timedate, breaks = "months")

tt1 <- tapply(X = sst_v5$sst_v5.res, INDEX = list(tt0, sst_v5$latband),
  FUN = mad, na.rm = TRUE, simplify=TRUE)

tt2 <- data.frame(time=seq.months[1:nrow(tt1)], tt1)
colnames(tt2) <- c("date", levels(sst_v5$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable.name="latband", value.name = "v5res")

tt4 <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("v5", times=length(tt3$date)),
  SST.res=tt3$v5res)
colnames(tt4) <- c("date","latband","SST.algo","SST.res")

# --- Statistics for V6 calculated by month/year and quality level for v6

sst_v6 <- subset(AQUA, subset = (qsst.new == 0),
  select = c(buoy.timedate, solz, cen.3750, cen.4050, buoy.sst, SST.latband1, latband))

nite <- sst_v6$solz >= 90
sst_v6$nite <- ifelse(nite, "Night", "Day")

sst_v6$sst_v6.res <- sst_v6$SST.latband1 - sst_v6$buoy.sst

min.date <- floor_date(min(sst_v6$buoy.timedate, na.rm=TRUE), "month")
max.date <- ceiling_date(max(sst_v6$buoy.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

tt0b <- cut(sst_v6$buoy.timedate, breaks = "months")

tt1b <- tapply(X = sst_v6$sst_v6.res, INDEX = list(tt0b, sst_v6$latband),
  FUN = mad, na.rm = TRUE, simplify=TRUE)

tt2b <- data.frame(time=seq.months[1:nrow(tt1b)], tt1b)
colnames(tt2b) <- c("date", levels(sst_v6$latband))
rownames(tt2b) <- NULL

tt3b <- melt(tt2b, id.vars=c("date"), variable.name="latband", value.name = "v6res")

tt4b <- data.frame(tt3b$date,
  tt3b$latband,
  SST.algo=rep("v6", times=length(tt3b$date)),
  SST.res=tt3b$v6res)
colnames(tt4b) <- c("date","latband","SST.algo","SST.res")

# --- Put together residuals for v5 and v6

tt5 <- rbind(tt4, tt4b)

xyplot(SST.res ~ date | latband, data = tt5,
  type = "l",
  ylim=c(0.1, 0.6),     
  main=paste("AQUA - MAD of SST residuals"),
  xlab="Time",
  ylab="MAD of SST residuals (deg C)",
  col=c("tomato","steelblue"), lwd=c(2, 2),
  groups = SST.algo, 
  panel = panel.superpose, 
  panel.groups = function(x, y, ...) { 
    panel.xyplot(x, y, ...) },
  layout=c(1,6))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Robust SD of SST residuals using quality = 0 ----

sst_v5 <- subset(AQUA, subset = (qsst == 0),
                 select = c(buoy.timedate, solz, cen.3750, cen.4050,
                            buoy.sst, cen.sst, latband))

nite <- sst_v5$solz >= 90
sst_v5$nite <- ifelse(nite, "Night", "Day")

sst_v5$sst_v5.res <-sst_v5$cen.sst - sst_v5$buoy.sst 

min.date <- lubridate::floor_date(min(sst_v5$buoy.timedate, na.rm=TRUE), "month")
max.date <- lubridate::ceiling_date(max(sst_v5$buoy.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

rob.sd <- function(x) {(IQR(x, na.rm = TRUE)) / 1.348}

tt0 <- cut(sst_v5$buoy.timedate, breaks = "months")

tt1 <- tapply(X = sst_v5$sst_v5.res, INDEX = list(tt0, sst_v5$latband),
              FUN = rob.sd, simplify=TRUE)

tt2 <- data.frame(time=seq.months[1:nrow(tt1)], tt1)
colnames(tt2) <- c("date", levels(sst_v5$latband))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable.name="latband", value.name = "v5res")

tt4 <- data.frame(tt3$date,
                  tt3$latband,
                  SST.algo=rep("v5", times=length(tt3$date)),
                  SST.res=tt3$v5res)
colnames(tt4) <- c("date","latband","SST.algo","SST.res")

# --- Statistics for V6 calculated by month/year and quality level for v6

sst_v6 <- subset(AQUA, subset = (qsst.new == 0),
                 select = c(buoy.timedate, solz, cen.3750, cen.4050, buoy.sst,
                            SST.latband1, latband))

nite <- sst_v6$solz >= 90
sst_v6$nite <- ifelse(nite, "Night", "Day")

sst_v6$sst_v6.res <- sst_v6$SST.latband1 - sst_v6$buoy.sst

min.date <- lubridate::floor_date(min(sst_v6$buoy.timedate, na.rm=TRUE), "month")
max.date <- lubridate::ceiling_date(max(sst_v6$buoy.timedate, na.rm=TRUE), "month")
seq.months <- seq(from=min.date, to=max.date, by="months")

tt0b <- cut(sst_v6$buoy.timedate, breaks = "months")

tt1b <- tapply(X = sst_v6$sst_v6.res, INDEX = list(tt0b, sst_v6$latband),
               FUN = rob.sd, simplify=TRUE)

tt2b <- data.frame(time=seq.months[1:nrow(tt1b)], tt1b)
colnames(tt2b) <- c("date", levels(sst_v6$latband))
rownames(tt2b) <- NULL

tt3b <- melt(tt2b, id.vars=c("date"), variable.name="latband", value.name = "v6res")

tt4b <- data.frame(tt3b$date,
                   tt3b$latband,
                   SST.algo=rep("v6", times=length(tt3b$date)),
                   SST.res=tt3b$v6res)
colnames(tt4b) <- c("date","latband","SST.algo","SST.res")

# --- Put together residuals for v5 and v6

tt5 <- rbind(tt4, tt4b)

xyplot(SST.res ~ date | latband, data = tt5,
       type = "l",
       ylim=c(0.1, 0.6),     
       main=paste("AQUA - Robust SD of SST residuals"),
       xlab="Time",
       ylab="Robust SD of SST residuals (deg C)",
       col=c("tomato","steelblue"), lwd=c(2, 2),
       groups = SST.algo, 
       panel = panel.superpose, 
       panel.groups = function(x, y, ...) { 
         panel.xyplot(x, y, ...) },
       layout=c(1,6))
# ------------------------------------------------------------------------------











# -----------------------------------------------------------------------------#
# --- Visualization of bias and dispersion of SST residuals -----
# --- as a function of continuous "hypercube" coordinates 

# --- AQUA 

#dplyr::filter(., qsst.new == 0 & abs(ref.type.1.SST - buoy.sst) < 3) %>%

AQUA2 <- dplyr::select(AQUA, SST.latband1, SST.latband1.res,
                       sat.mon, solz, latband, satz,
                       qsst.new, cen.11000, cen.12000,
                       buoy.sst, ref.type.1.SST) %>%
  dplyr::filter(., solz > 90 & sat.mon >= 4 & sat.mon <= 6 &
                  as.numeric(latband) == 5 &
                  qsst.new <= 1) %>%
  dplyr::mutate(., SST.latband1.res = SST.latband1.res + 0.17,
                satz = abs(satz),
                ch.diff = cen.11000 - cen.12000)


plot(AQUA2$satz, AQUA2$SST.latband1.res,
     main = "AQUA - SST Residuals vs. scan angle",
     xlab = "Satellite zenith angle",
     ylab = "SST residuals",
     pch = ".", col = "grey60")

plot(AQUA2$ch.diff, AQUA2$SST.latband1.res,
     main = "AQUA - SST Residuals vs. channel difference",
     xlab = "BT31 - BT32",
     ylab = "SST residuals",
     pch = ".", col = "grey60")

plot(AQUA2$buoy.sst, AQUA2$SST.latband1.res,
     main = "AQUA - SST Residuals vs. channel difference",
     xlab = "Buoy SST",
     ylab = "SST residuals",
     pch = ".", col = "grey60")


p <- ggplot(AQUA2, aes(satz, SST.latband1.res))
h3 <- p + stat_bin2d(bins=50) + scale_fill_gradientn(colours=rff, trans="log")
h3

# --- Combinations of continuous coordinates in hypercube

hexbinplot(buoy.sst ~ satz , data = AQUA2, colramp = rf,
           trans=log, inv=exp, xbins = 25, aspect = 1,
           xlab = "Satellite zenith angle",
           ylab = "Buoy SST",
           main = "AQUA")

hexbinplot(buoy.sst ~ ch.diff , data = AQUA2, colramp = rf,
           trans=log, inv=exp, xbins = 25, aspect = 1,
           xlab = "BT Difference",
           ylab = "Buoy SST",
           main = "AQUA")

hexbinplot(satz ~ ch.diff , data = AQUA2, colramp = rf,
           trans=log, inv=exp, xbins = 25, aspect = 1,
           xlab = "BT Difference",
           ylab = "Satellite zenith angle",
           main = "AQUA")

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
uu1 <- data.frame(satz = AQUA2$satz, bsst = AQUA2$buoy.sst)

uu1.x <- seq(0, 69, 1)        # satz
uu1.y <- seq(0, 34.5, 0.5)    # buoy.sst
uu1.pts <- cbind(uu1.x, uu1.y)

uu2 <- sm::sm.density(uu1, eval.points = uu1.pts)
uu3 <- sort(uu2$estimate, decreasing = TRUE)
uu4 <- cumsum(uu3) / sum(uu3)

uu5 <- loess(uu3 ~ uu4, span = 0.20)
plot(uu4, uu3)
lines(uu5$x, uu5$fitted, col = 'tomato')

uu6 <- predict(uu5, newdata = c(0.5, 0.75, 0.90, 0.95, 0.99, 0.995))

image(x = uu2$eval.points[, "xgrid"],
      y = uu2$eval.points[, "ygrid"],
      z = uu2$estimate,
      xlab = "Satellite zenith angle",
      ylab = "Buoy SST",
      main="AQUA - Buoy SST vs. scan angle",
      xlim = c(-2, 70),
      breaks = sort(c(0, uu6, 1)),
      col = c('white', RColorBrewer::brewer.pal(6, 'YlOrRd')))
box()
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
uu1 <- data.frame(satz = AQUA2$satz, btdiff = AQUA2$ch.diff)

uu1.x <- seq(0, 69, 1)      # satz
uu1.y <- seq(-0.45, 3.00, 0.05)  # BT difference
uu1.pts <- cbind(uu1.x, uu1.y)

uu2 <- sm::sm.density(uu1, eval.points = uu1.pts)
uu3 <- sort(uu2$estimate, decreasing = TRUE)
uu4 <- cumsum(uu3) / sum(uu3)

uu5 <- loess(uu3 ~ uu4, span = 0.20)
plot(uu4, uu3)
lines(uu5$x, uu5$fitted, col = 'tomato')

uu6 <- predict(uu5, newdata = c(0.5, 0.75, 0.90, 0.95, 0.99, 0.995))

image(x = uu2$eval.points[, "xgrid"],
      y = uu2$eval.points[, "ygrid"],
      z = uu2$estimate,
      xlab = "Satellite zenith angle",
      ylab = "BT difference",
      main="AQUA - BT difference vs. scan angle",
      xlim = c(-2, 70),
      breaks = sort(c(0, uu6, 1)),
      col = c('white', RColorBrewer::brewer.pal(6, 'YlOrRd')))
box()
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
uu1 <- data.frame(btdiff = AQUA2$ch.diff, bsst = AQUA2$buoy.sst)

uu1.x <- seq(-0.45, 3.00, 0.05)   # BT difference
uu1.y <- seq(0, 34.5, 0.5)        # buoy.sst
uu1.pts <- cbind(uu1.x, uu1.y)

uu2 <- sm::sm.density(uu1, eval.points = uu1.pts)
uu3 <- sort(uu2$estimate, decreasing = TRUE)
uu4 <- cumsum(uu3) / sum(uu3)

uu5 <- loess(uu3 ~ uu4, span = 0.2)
plot(uu4, uu3)
lines(uu5$x, uu5$fitted, col = 'tomato')

uu6 <- predict(uu5, newdata = c(0.5, 0.75, 0.90, 0.95, 0.99, 0.995))

image(x = uu2$eval.points[, "xgrid"],
      y = uu2$eval.points[, "ygrid"],
      z = uu2$estimate,
      xlab = "BT Difference",
      ylab = "Buoy SST",
      main="AQUA - Buoy SST vs. BT Difference",
      breaks = sort(c(0, uu6, 1)),
      col = c('white', RColorBrewer::brewer.pal(6, 'YlOrRd')),
      xlim = c(-0.45, 3.00),
      ylim = c(0, 34.5))
box()

filled.contour(uu2$estimate,
               xlab = "BT Difference",
               ylab = "Buoy SST",
               main="AQUA - Buoy SST vs. BT Difference",
               x = uu2$eval.points[,"xgrid"],
               y = uu2$eval.points[,"ygrid"],
               levels=sort(c(0, uu6, 1)),
               col = c('white', RColorBrewer::brewer.pal(6, 'YlOrRd')))
# ------------------------------------------------------------------------------


hexbinplot(buoy.sst ~ ch.diff , data = AQUA2, colramp = rf,
           trans=log, inv=exp, xbins = 25, aspect = 1,
           xlab = "BT Difference",
           ylab = "Buoy SST",
           main = "AQUA - Buoy SST vs. BT Difference",
           xlim = c(-0.45, 3.00),
           ylim = c(0, 34.5))


uu1 <- hexbin(x = AQUA2$ch.diff, y = AQUA2$buoy.sst,
       xbins = 25, IDs = TRUE)

uu2 <- hexbin::hcell2xy(uu1)

uu3 <- data.frame(ch.diff = uu2$x, buoy.sst = uu2$y, cell = uu1@cell, n = uu1@count)

uu4 <- loess(SST.latband1.res ~ buoy.sst + ch.diff, data = AQUA2, span = 0.4)


uu1.x <- seq(-0.45, 3.00, 0.05)   # BT difference
uu1.y <- seq(0, 34.5, 0.5)        # buoy.sst
uu1.pts <- expand.grid(ch.diff = uu1.x, buoy.sst = uu1.y)

uu5 <- predict(uu4, newdata = uu1.pts)


image(x = uu1.x, y = uu1.y, z = uu5,
      xlab = "BT Difference",
      ylab = "Buoy SST",
      main = "AQUA - Buoy SST vs. BT Difference",
      xlim = c(-0.45, 3.00),
      ylim = c(0, 34.5))
box()

contour(x = uu1.x, y = uu1.y, z = uu5,
      xlab = "BT Difference",
      ylab = "Buoy SST",
      main = "AQUA - Buoy SST vs. BT Difference",
      xlim = c(-0.45, 3.00),
      ylim = c(0, 34.5),
      levels = seq(-1, 1, 0.1))



library(ggplot2)
library(hexbin)
library(RColorBrewer)

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
#rf <- colorRampPalette(brewer.pal(9,'YlOrRd'))
rff <- rf(32)

library(hexbin)

figura <- paste("AQUA_hexbin1.png")  

CairoPNG(filename = figura,
         width = 640, height = 480, units = "px", pointsize = 12,
         bg = "white", res = NA)

hexbinplot(SST.latband1.res ~ satz , data = AQUA2, colramp = rf,
           trans=log, inv=exp, xbins = 25, aspect = 1,
           xlab = "Satellite zenith angle",
           ylab = "SST residuals",
           main = "AQUA")
dev.off()


figura <- paste("AQUA_hexbin2.png")  

CairoPNG(filename = figura,
         width = 640, height = 480, units = "px", pointsize = 12,
         bg = "white", res = NA)

hexbinplot(SST.latband1.res ~ ch.diff , data = AQUA2, colramp = rf,
           trans=log, inv=exp, xbins = 25, aspect = 1,
           xlab = "BT31 - BT32",
           ylab = "SST residuals",
           main = "AQUA")
dev.off()

figura <- paste("AQUA_hexbin3.png")  

CairoPNG(filename = figura,
         width = 640, height = 480, units = "px", pointsize = 12,
         bg = "white", res = NA)

hexbinplot(SST.latband1.res ~ buoy.sst , data = AQUA2, colramp = rf,
           trans=log, inv=exp, xbins = 25, aspect = 1,
           xlab = "Buoy SST",
           ylab = "SST residuals",
           main = "AQUA")

dev.off()


uu1 <- data.frame(satz = AQUA2$satz, res = AQUA2$SST.latband1.res)

uu1.x <- pretty(range(uu1$satz), n = 80)
uu1.y <- pretty(range(uu1$res), n = 80)
uu1.pts <- expand.grid(x = uu1.x, y = uu1.y)
uu2 <- sm::sm.density(uu1, eval.points = uu1.pts)
uu3 <- sort(uu2$estimate, decreasing = TRUE)
uu4 <- cumsum(uu3) / sum(uu3)

uu5 <- loess(uu3 ~ uu4, span = 0.20)
plot(uu4, uu3)
lines(uu5$x, uu5$fitted, col = 'tomato')

uu6 <- predict(uu5, newdata = c(0.5, 0.75, 0.90, 0.95, 0.99, 0.995))

image(x = uu2$eval.points[, "xgrid"],
      y = uu2$eval.points[, "ygrid"],
      z = uu2$estimate,
      xlab = "Satellite zenith angle",
      ylab = "SST residuals",
      main="AQUA - Residuals vs. scan angle",
      xlim = c(-2, 58),
      breaks = sort(c(0, uu6, 1)),
      col = c('white', RColorBrewer::brewer.pal(6, 'YlOrRd')))
box()










# 95% density
100 * sum(uu2$estimate[uu2$estimate > 0.001095941]) / sum(uu2$estimate)

# 99% density
100 * sum(uu2$estimate[uu2$estimate > 0.00022]) / sum(uu2$estimate)






filled.contour(uu2$estimate,
               xlab = "Satellite zenith angle",
               ylab = "SST residuals",
               main="AQUA - Residuals vs. scan angle",
               x = uu2$eval.points[,"xgrid"],
               y = uu2$eval.points[,"ygrid"],
               levels=c(0,0.00022, 0.0011, 1),
               col=c('white', '#ffeda0', '#feb24c'))

points(uu1$satz[uu2$estimate < 0.00022], uu1$res[uu2$estimate < 0.00022])

[uu2$estimate < 0.00022]

h.df <- hexbin(AQUA2)


# Setting max and mins
hexbinplot(y~x, data=df, colramp=rf, mincnt=2, maxcnt=60)






