source("netcdffunc.R")
source("sitecoord.R")
library(dplyr)

startdate <- strptime("2002-06-10 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT")
nts <- 30*24


## calculate site indices
icoord <- findlonlatindex(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out01/lffd2002050100.nc",
    coord)


## bulk simulation
nurb <- read.folder.netcdf(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out02/",
    startdate, nts,
    c("T_2M", "U_10M_AV", "V_10M_AV", "ASHFL_S", "ALHFL_S", "ALWD_S", "ALWU_S", "ASWDIR_S", "ASWDIFD_S", "ASWDIFU_S", "CLCT", "T_G"),
    c("at", "wvu", "wvv", "fh", "fl", "ld", "lu", "sd_dir", "sd_diff", "su", "cc", "tg"),
    icoord)

## add simulation type
nurb <- mutate(nurb, type="simulation", subtype="bulk")


## DCEP simulation
urb <- read.folder.netcdf(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009_urb/out02/",
    startdate, nts,
    c("T_2M", "U_10M_AV", "V_10M_AV", "ASHFL_S", "ALHFL_S", "ALWD_S", "ALWU_S", "ASWDIR_S", "ASWDIFD_S", "ASWDIFU_S", "CLCT", "T_G"),
    c("at", "wvu", "wvv", "fh", "fl", "ld", "lu", "sd_dir", "sd_diff", "su", "cc", "tg"),
    icoord)

## add simulation type
urb <- mutate(urb, type="simulation", subtype="DCEP")

simS <- rbind(nurb,urb) %>%             # merge data
    mutate(at=at-273.15,                # use degree celsius
           wv=sqrt(wvu^2+wvv^2),        # calculate total wind velocity
           sd=sd_dir+sd_diff,           # calculate total incoming shortwave radiation
           al=su/sd) %>%                # calculate albedo
    select(-c(wvu,wvv,sd_dir,sd_diff))  # remove some fields

save(file="DCEPbulk.Rdata", simS)
