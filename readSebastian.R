source("netcdffunc.R")
source("sitecoord.R")

startdate <- strptime("2002-06-10 01:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT")
nts <- 30*24+1

## calculate site indices
icoord <- findlonlatindex(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out01/lffd2002050100.nc",
    coord)

## calculate athmospheric heights
heights <- calcatmoheights("/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out02//lffd2002050100c.nc")

## bulk simulation
nurb <- read.folder.netcdf(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out02/",
    startdate, nts,
    c("T_2M", "U_10M_AV", "V_10M_AV", "ASHFL_S", "ALHFL_S", "ALWD_S", "ALWU_S", "ASWDIR_S", "ASWDIFD_S", "ASWDIFU_S", "CLCT", "T_G", "P"),
    c("at", "wvu", "wvv", "fh", "fl", "ld", "lu", "sd_dir", "sd_diff", "su", "cc", "tg", "ap"),
    icoord,
    maxheight=500,
    levels = heights)
## add simulation type
nurb <- lapply(nurb, function(x) cbind(x, data.frame(type="bulk")))

nurb3d <- read.folder.netcdf(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out01/",
    startdate, nts,
    c("T", "U", "V"),
    c("at2", "wvu2", "wvv2"),
    icoord,
    maxheight=500,
    levels = heights)
## add simulation type
nurb3d <- lapply(nurb3d, function(x) cbind(x, data.frame(type="bulk")))

## put both lists together
nurb <- c(nurb, nurb3d)


## DCEP simulation
urb <- read.folder.netcdf(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009_urb/out02/",
    startdate, nts,
    c("T_2M", "U_10M_AV", "V_10M_AV", "ASHFL_S", "ALHFL_S", "ALWD_S", "ALWU_S", "ASWDIR_S", "ASWDIFD_S", "ASWDIFU_S", "CLCT", "T_G", "P"),
    c("at", "wvu", "wvv", "fh", "fl", "ld", "lu", "sd_dir", "sd_diff", "su", "cc", "tg", "ap"),
    icoord,
    maxheight=500,
    levels = heights)
## add simulation type
urb <- lapply(urb, function(x) cbind(x, data.frame(type="DCEP")))

urb3d <- read.folder.netcdf(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009_urb/out01/",
    startdate, nts,
    c("T", "U", "V"),
    c("at2", "wvu2", "wvv2"),
    icoord,
    maxheight=500,
    levels = heights)
## add simulation type
urb3d <- lapply(urb3d, function(x) cbind(x, data.frame(type="DCEP")))

urb <- c(urb,urb3d)

## combine both and ensure that fitting list elements are put
## together, all variables in nurb have to be present in urb
sim <- list()
for (vn in names(nurb)) {
    sim[[vn]] <- rbind(nurb[[vn]], urb[[vn]])
}


simS <- calcderived(sim)

save(file="DCEPbulk.Rdata", simS)
