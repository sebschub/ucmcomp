## This file reads BUBBLE measurement data into the data frame
## bubble. It includes the following columns:
## t: time of measurement
## site: measurement site
## type: "measurement" to distingush from model output later
## subtype: distingush different sensors at same height
## height: height of measurement
## wv: wind velocity
## at: atmoshperic temperature
## fh: sensible heat flux
## fl: latent heat flux
## ld: longwave radiation down
## lu: longwave radiation up
## sd: shortwave radiation down
## su: shortwave radiation up
## al: albedo
## rt: roof temperature
## tg: ground temperature
## cc: cloud cover
## pt: potential temperature
## wd: wind direction

library(reshape2)
library(dplyr)

readBUBBLE <- function(file, site, varname, heights, sep = ",", na.strings = "NA") {
    d <- read.table(file, sep=sep, comment.char="#", header=FALSE, na.strings=na.strings)
    d2 <- melt(d)
    levels(d2$variable) <- heights
    subtype <- vector(mode="character", len=2)
    for (i in 1:length(heights)) {
        subtype=paste("sensor",sum(heights[1:i]==heights[i]), sep="")
    }
    df <- data.frame(t=strptime(d2[,1], format="%d.%m.%Y %H:%M", tz="UTC+1"),
                     site=site,
                     type="measurement",
                     subtype=subtype,
                     height=d2$variable,
                     value=d2$value)
    colnames(df)[6] <- varname
    return(df)
}

readBUBBLEmult <- function(file, cols, sites, varname, heights, sep=",", na.strings = "NA") {
    d <- read.table(file, sep=sep, comment.char="#", header=FALSE, na.strings = na.strings)[,c(1,cols)]
    d2 <- melt(d)
    df <- data.frame(t=strptime(d2[,1], format="%d.%m.%Y %H:%M", tz="UTC+1"),
                     site=d2$variable,
                     type="measurement",
                     subtype="sensor1", # CHECK if several measurements per height
                     height=d2$variable,
                     value=d2$value)
    levels(df$height) <- heights
    levels(df$site) <- sites
    colnames(df)[6] <- varname
    return(df)
}

at <- rbind(
    readBUBBLE("BUBBLE/at_bspr", "BSPR", "at",
               c( 2.60, 13.90, 17.50, 21.50, 25.50, 31.20, 26.00)),
    readBUBBLE("BUBBLE/at_bspa", "BSPA", "at",
               c(3.00, 15.80, 3.00, 15.80, 22.90, 27.80, 32.90, 33.00)),
    readBUBBLEmult("BUBBLE/at", c(6,7,8,9,10), c("BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                   "at", c(29.8, 15.00, 2.00, 1.5, 2.0))
    )

wv <- readBUBBLEmult("BUBBLE/wv", c(2,3,4,5,6,7,8),
                     c("BSPR", "BSPA", "BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "wv", c(31.70, 37.60, 29.20, 15.80, 2.00, 28.00, 10.00))

fh <- rbind(
    readBUBBLE("BUBBLE/fh_bspr", "BSPR", "fh",
               c(3.60, 11.30, 14.70, 17.90, 22.40, 31.70)),
    readBUBBLE("BUBBLE/fh_bspa", "BSPA", "fh",
               c(5.60, 13.90, 16.60, 21.80, 29.90, 37.60)),
    readBUBBLE("BUBBLE/fh_alls", "ALLS", "fh",
               c(8.30, 12.10, 15.80)),
    readBUBBLEmult("BUBBLE/fh", c(4,5,6,7), c("BMES", "ALLS", "VLNF", "GRNZ"),
                   "fh", c(29.2,15.80, 3.30, 28.0)))

fl <- readBUBBLEmult("BUBBLE/fl", c(2,3,4,5,6,7), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ"),
                     "fl", c(31.70, 29.90, 29.20, 15.80, 3.30, 28.0))

ld <- readBUBBLEmult("BUBBLE/ld", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "ld", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))

lu <- readBUBBLEmult("BUBBLE/lu", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "lu", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))

sd <- readBUBBLEmult("BUBBLE/sd", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "sd", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))

su <- readBUBBLEmult("BUBBLE/su", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "su", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))

al <- readBUBBLEmult("BUBBLE/al", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "al", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))

rt <- readBUBBLE("BUBBLE/rt_bspr", "BSPR", "rt", c(16.00,15.50,16.00))

tg <- readBUBBLEmult("BUBBLE/tg", c(2,3,4), c("BLER", "GRNZ", "VLNF"),
                     "tg", c(-0.02, -0.02, -0.02), sep="\t")

cc <- readBUBBLE("BUBBLE/cc_bbin", "BBIN", "cc", NaN, sep="\t")

pt <- readBUBBLE("BUBBLE/pt_bklh", "BKLH", "pt",
                 c(500.0, 480.0, 460.0, 440.0, 420.0, 200.0, 180.0, 160.0, 140.0, 120.0, 100.0, 80.0, 60.0, 40.0, 400.0, 380.0, 360.0, 340.0, 320.0, 300.0, 280.0, 260.0, 240.0, 220.0),
                 sep="\t", na.strings = "-9999.0")

wv2 <- readBUBBLE("BUBBLE/wv_bklh", "BKLH", "wv",
                 c(500.0, 480.0, 460.0, 440.0, 420.0, 200.0, 180.0, 160.0, 140.0, 120.0, 100.0, 80.0, 60.0, 40.0, 400.0, 380.0, 360.0, 340.0, 320.0, 300.0, 280.0, 260.0, 240.0, 220.0),
                 sep="\t", na.strings = "-9999.0")

wd <- readBUBBLE("BUBBLE/wd_bklh", "BKLH", "wd",
                 c(500.0, 480.0, 460.0, 440.0, 420.0, 200.0, 180.0, 160.0, 140.0, 120.0, 100.0, 80.0, 60.0, 40.0, 400.0, 380.0, 360.0, 340.0, 320.0, 300.0, 280.0, 260.0, 240.0, 220.0),
                 sep="\t", na.strings = "-9999.0")


bubble <- Reduce(function(...) merge(..., all=TRUE),
                 list(at,fh,fl,ld,lu,sd,su,wv,al,rt,tg,cc,pt,wv2,wd))
