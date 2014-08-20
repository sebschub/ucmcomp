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
    ## round heights to ensure grouping by height is possible
    heights <- round(heights, digits=2)
    
    ## subtype to differentiate between sensors at equal height
    if (length(heights)==1) {
        subtype <- "sensor1"
    } else {
        subtype <- vector(mode="character", length=length(heights))
        for (i in 1:length(heights)) {
            subtype[i] <- paste("sensor",sum(heights[1:i]==heights[i]), sep="")
        }
    }

    ## read data, melt columns and add columns
    d <- read.table(file, sep=sep, comment.char="#", header=FALSE, na.strings=na.strings) %>%
            melt()
    d <- data.frame(
        ## convert time strings, convention is opposite to common usage,
        ## just "GMT-1" is wrong
        time=as.POSIXct(strptime(d$V1, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")),
        site=site,
        type="measurement",
        subtype=subtype[d$variable],
        height=heights[d$variable],
        variable=varname,
        value=d$value)
    
    ## NaNs for measuring errors, not NA
    if (hasArg(na.strings)) {
        d$value[which(is.na(d$value))] <- NaN
    }
    
    return(d)
}

readBUBBLEmult <- function(file, cols, sites, varname, heights, sep=",", na.strings = "NA") {
    ## round heights to ensure grouping by height is possible
    heights <- round(heights, digits=2)

    d <- read.table(file, sep=sep, comment.char="#",
                    header=FALSE, na.strings = na.strings)[,c(1,cols)] %>%
                        melt()
    ## convert time strings, convention is opposite to common usage,
    ## just "GMT-1" is wrong
    d <- data.frame(time=as.POSIXct(strptime(d$V1, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")),
                    site=sites[d$variable],
                    type="measurement",
                    subtype="sensor1", # CHECK if several measurements per height
                    height=heights[d$variable],
                    variable=varname,
                    value=d$value)
    return(d)
}

## return hourly average; apply shift to time before to take averaging
## into account
calcAverage <- function(df, shift=0) {
    dfav <- df %>%
        group_by(site, type, subtype, height, variable,
                 datehour=cut(as.POSIXlt(time+shift), breaks="hour")) %>%
                     summarize(time=max(time),value=mean(value)) %>%
                         select(-datehour)
    return(dfav)
}

selectFullHours <- function(df) {
    dfsel <- df %>%
        mutate(minute=as.numeric(format(time, "%M"))) %>%
            filter(minute==0) %>%
                select(-minute)
    return(dfsel)
}


## average air temperature every 10 min, time indicates end of averaging period
at <- rbind(
    readBUBBLE("BUBBLE/at_bspr", "BSPR", "at",
               c( 2.60, 13.90, 17.50, 21.50, 25.50, 31.20, 26.00)),
    readBUBBLE("BUBBLE/at_bspa", "BSPA", "at",
               c(3.00, 15.80, 3.00, 15.80, 22.90, 27.80, 32.90, 33.00)),
    readBUBBLEmult("BUBBLE/at", c(6,7,8,9,10), c("BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                   "at", c(29.8, 15.00, 2.00, 1.5, 2.0))
    )
at <- selectFullHours(at)


## average wind velocity every 10 min, time indicates end of averaging period
wv <- readBUBBLEmult("BUBBLE/wv", c(2,3,4,5,6,7,8),
                     c("BSPR", "BSPA", "BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "wv", c(31.70, 37.60, 29.20, 15.80, 2.00, 28.00, 10.00))
wv <- calcAverage(wv, -10*60)


## average sensible heat flux every 10 min, time indicates end of averaging period
fh <- rbind(
    readBUBBLE("BUBBLE/fh_bspr", "BSPR", "fh",
               c(3.60, 11.30, 14.70, 17.90, 22.40, 31.70)),
    readBUBBLE("BUBBLE/fh_bspa", "BSPA", "fh",
               c(5.60, 13.90, 16.60, 21.80, 29.90, 37.60)),
    readBUBBLE("BUBBLE/fh_alls", "ALLS", "fh",
               c(8.30, 12.10, 15.80)),
    readBUBBLEmult("BUBBLE/fh", c(4,5,6,7), c("BMES", "ALLS", "VLNF", "GRNZ"),
                   "fh", c(29.2,15.80, 3.30, 28.0)))
fh <- calcAverage(fh, -10*60)


## average latent heat flux every 10 min, time indicates end of averaging period
fl <- readBUBBLEmult("BUBBLE/fl", c(2,3,4,5,6,7), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ"),
                     "fl", c(31.70, 29.90, 29.20, 15.80, 3.30, 28.0))
fl <- calcAverage(fl, -10*60)

## soil heat flux
fg <- readBUBBLEmult("BUBBLE/fg", c(2,3,4), c("BLER", "GRNZ", "VLNF"),
                     "fg", c(0,0,0), sep="\t", na.strings = "-9999.0")
fg <- mutate(fg, value=-value)

## average incoming longwave radiation every 10 min, time indicates end of averaging period
ld <- readBUBBLEmult("BUBBLE/ld", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "ld", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))
ld <- calcAverage(ld, -10*60)


## average emitted and reflected longwave radiation every 10 min, time
## indicates end of averaging period
lu <- readBUBBLEmult("BUBBLE/lu", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "lu", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))
lu <- calcAverage(lu, -10*60)


## average incoming shortwave radiation every 10 min, time indicates end of averaging period
sd <- readBUBBLEmult("BUBBLE/sd", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "sd", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))
sd <- calcAverage(sd, -10*60)


## average reflected shortwave radiation every 10 min, time indicates end of averaging period
su <- readBUBBLEmult("BUBBLE/su", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "su", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))
su <- calcAverage(su, -10*60)


## average albedo every 10 min, time indicates end of averaging period
al <- readBUBBLEmult("BUBBLE/al", c(2,3,4,5,6,7,8), c("BSPR","BSPA","BMES", "ALLS", "VLNF", "GRNZ", "BLER"),
                     "al", c(31.50, 32.90, 29.20, 15.10, 2.00, 1.4, 2.0))
al <- calcAverage(al, -10*60)


## average roof temperature every 10 min, time indicates end of averaging period
rt <- readBUBBLE("BUBBLE/rt_bspr", "BSPR", "rt", c(16.00,15.50,16.00))
rt <- selectFullHours(rt)


## average soil temperature every 10 min, time indicates end of averaging period
tg <- readBUBBLEmult("BUBBLE/tg", c(2,3,4), c("BLER", "GRNZ", "VLNF"),
                     "tg", c(-0.02, -0.02, -0.02), sep="\t")
tg <- selectFullHours(tg)


## cloud cover every 60 min
cc <- readBUBBLE("BUBBLE/cc_bbin", "BBIN", "cc", NaN, sep="\t")
## convert to range [0,1]
cc <- mutate(cc, value=value/10)

## average potential temparature profile every 10 min, time indicates end of aggregation period
pt <- readBUBBLE("BUBBLE/pt_bklh", "BKLH", "pt",
                 c(500.0, 480.0, 460.0, 440.0, 420.0, 200.0, 180.0, 160.0, 140.0, 120.0, 100.0, 80.0, 60.0, 40.0, 400.0, 380.0, 360.0, 340.0, 320.0, 300.0, 280.0, 260.0, 240.0, 220.0),
                 sep="\t", na.strings = "-9999.0")
pt <- selectFullHours(pt)


## average wind velocity profile every 10 min, time indicates end of aggregation period
wv2 <- readBUBBLE("BUBBLE/wv_bklh", "BKLH", "wv",
                 c(500.0, 480.0, 460.0, 440.0, 420.0, 200.0, 180.0, 160.0, 140.0, 120.0, 100.0, 80.0, 60.0, 40.0, 400.0, 380.0, 360.0, 340.0, 320.0, 300.0, 280.0, 260.0, 240.0, 220.0),
                 sep="\t", na.strings = "-9999.0")
wv2 <- selectFullHours(wv2)


## average wind direction profile every 10 min, time indicates end of aggregation period
wd <- readBUBBLE("BUBBLE/wd_bklh", "BKLH", "wd",
                 c(500.0, 480.0, 460.0, 440.0, 420.0, 200.0, 180.0, 160.0, 140.0, 120.0, 100.0, 80.0, 60.0, 40.0, 400.0, 380.0, 360.0, 340.0, 320.0, 300.0, 280.0, 260.0, 240.0, 220.0),
                 sep="\t", na.strings = "-9999.0")
wd <- selectFullHours(wd)


bubble <- rbind(at,fh,fl,fg,ld,lu,sd,su,wv,al,rt,tg,cc,pt,wv2,wd)

save(file="measurements.Rdata", bubble)
