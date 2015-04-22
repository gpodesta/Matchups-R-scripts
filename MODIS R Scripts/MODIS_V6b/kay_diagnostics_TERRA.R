

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

orig3 <- subset(TERRA, select = c(solz, qsst, qsst4, cen.sst, cen.sst4, buoy.sst))

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
tt5 <- round(tapply(orig4$sst4.res, INDEX=orig4$qsst4, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3)
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

# --- Work with TERRA SST

# -----------------------------------------------------------------------------#
# --- Median of SST residuals (V5 and V6) using quality = 0 ----

sst_v5 <- subset(TERRA, subset = (qsst == 0),
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
colnames(tt2) <- c("date", levels(sst_v5$latband),
  paste0("N",1:length(levels(sst_v5$latband))))
rownames(tt2) <- NULL

tt3 <- melt(tt2, id.vars=c("date"), variable.name=c"latband", value.name = "v5res")

tt4 <- data.frame(tt3$date,
  tt3$latband,
  SST.algo=rep("v5", times=length(tt3$date)),
  SST.res=tt3$v5res)
colnames(tt4) <- c("date","latband","SST.algo","SST.res")


# --- Statistics for V6 calculated by month/year and quality level for v6

sst_v6 <- subset(TERRA, subset = (qsst.new == 0),
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

tt5 <- rbind(tt4, tt4b, yy3)

xyplot(SST.res ~ date | latband, data = tt5,
  type = "l",
  ylim=c(-0.75, 0.35),
  main=paste("TERRA - Median of SST residuals"),
  xlab="Time",
  ylab="SST residuals (deg C)",
  col=c("tomato","steelblue"), lwd=c(2, 2),
  groups = SST.algo, 
  panel = panel.superpose, 
  panel.groups = function(x, y, ...) { 
    panel.abline(h=-0.17, col="grey", lwd=1)
    panel.xyplot(x, y, ...) },
  layout=c(1,6))



# --- Count number of matchups

yyy <- tapply(X = sst_v6$sst_v6.res, INDEX = list(tt0b, sst_v6$latband),
  FUN = function(x){length(x[!is.na(x)])}, simplify=TRUE)

yy2 <- data.frame(time=seq.months[1:nrow(yyy)], yyy)
colnames(yy2) <- c("date", levels(sst_v6$latband))
rownames(yy2) <- NULL

yy3 <- melt(yy2, id.vars=c("date"), variable.name="latband", value.name = "N")

yy3$NN <- ifelse(yy3$N < 100, 1, 0)

xyplot(N ~ date | latband, data = yy3,
       type = "h",
       ylim=c(0, 100),
       main=paste("TERRA - Insufficient matchups"),
       xlab="Time",
       ylab="SST residuals (deg C)",
       col=c("tomato"), lwd=c(2),   
       layout=c(1,6))


ylim=c(-0.75, 0.35)

groups = SST.algo, 
panel = panel.superpose, 
panel.groups = function(x, y, ...) { 
  panel.abline(h=-0.17, col="grey", lwd=1)
  panel.xyplot(x, y, ...) },
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Std dev of SST residuals using quality = 0 ----

sst_v5 <- subset(TERRA, subset = (qsst == 0),
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

sst_v6 <- subset(TERRA, subset = (qsst.new == 0),
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
  main=paste("TERRA - Std deviation of SST residuals"),
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

sst_v5 <- subset(TERRA, subset = (qsst == 0),
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

sst_v6 <- subset(TERRA, subset = (qsst.new == 0),
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
  main=paste("TERRA - MAD of SST residuals"),
  xlab="Time",
  ylab="MAD of SST residuals (deg C)",
  col=c("tomato","steelblue"), lwd=c(2, 2),
  groups = SST.algo, 
  panel = panel.superpose, 
  panel.groups = function(x, y, ...) { 
    panel.xyplot(x, y, ...) },
  layout=c(1,6))
# ------------------------------------------------------------------------------

