source("netcdffunc.R")
source("sitecoord.R")
source("CCLMcalc.R")

startdate <- strptime("2002-06-09 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT")
nts <- 30*24+1


## calculate site indices
icoord <- findlonlatindex(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out01/lffd2002050100.nc",
    coord)

## calculate athmospheric heights
heights <- calcatmoheights("/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out02//lffd2002050100c.nc")

## read 2d fields in out02
read2d <- function(folder) {
    read.folder.netcdf(
        folder,
        startdate, nts,
        c("T_2M", "U_10M_AV", "V_10M_AV", "ASHFL_S", "ALHFL_S", "ALWD_S", "ALWU_S", "ASWDIR_S", "ASWDIFD_S", "ASWDIFU_S", "CLCT", "T_G", "P", "AUMFL_S", "AVMFL_S"),
        c("at", "wvu", "wvv", "fh", "fl", "ld", "lu", "sd_dir", "sd_diff", "su", "cc", "tg", "ap", "fu", "fv"),
        icoord,
        maxheight=500,
        levels = heights)
}

## read 3d fields in out01
read3d <- function(folder) {
    read.folder.netcdf(
        folder,
        startdate, nts,
        c("T", "U", "V"),
        c("at2", "wvu2", "wvv2"),
        icoord,
        maxheight=500,
        levels = heights)
}

## DCEP simulation
urb <- read2d("/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009_urb/out02/")
## add simulation type
urb <- lapply(urb, function(x) cbind(x, data.frame(type="TERRAurb")))

urb3d <- read3d("/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009_urb/out01/")
## add simulation type
urb3d <- lapply(urb3d, function(x) cbind(x, data.frame(type="TERRAurb")))

urb <- c(urb,urb3d)

simH <- calcderived(urb)

save(file="TERRAurb.Rdata", simH)
