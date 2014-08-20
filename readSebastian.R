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

## bulk simulation
bulk <- read2d("/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out02/")
## add simulation type
bulk <- lapply(bulk, function(x) cbind(x, data.frame(type="bulk")))

bulk3d <- read3d("/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009/out01/")
## add simulation type
bulk3d <- lapply(bulk3d, function(x) cbind(x, data.frame(type="bulk")))

## put both lists together
bulk <- c(bulk, bulk3d)


## bulk5 simulation
bulk5 <- read2d("/scratch/01/schubert/clm/output/urban_comparison/basel0.009/out02/")
## add simulation type
bulk5 <- lapply(bulk5, function(x) cbind(x, data.frame(type="bulk5")))

bulk53d <- read3d("/scratch/01/schubert/clm/output/urban_comparison/basel0.009/out01/")
## add simulation type
bulk53d <- lapply(bulk53d, function(x) cbind(x, data.frame(type="bulk5")))

## put both lists together
bulk5 <- c(bulk5, bulk53d)


## DCEP simulation
urb <- read2d("/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009_urb/out02/")
## add simulation type
urb <- lapply(urb, function(x) cbind(x, data.frame(type="DCEP")))

urb3d <- read3d("/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009_urb/out01/")
## add simulation type
urb3d <- lapply(urb3d, function(x) cbind(x, data.frame(type="DCEP")))

urb <- c(urb,urb3d)

## DCEP simulation new
urb_new <- read2d("/scratch/01/schubert/clm/output/urban_comparison/basel0.009_urb/out02/")
## add simulation type
urb_new <- lapply(urb_new, function(x) cbind(x, data.frame(type="DCEPnew")))

urb3d_new <- read3d("/scratch/01/schubert/clm/output/urban_comparison/basel0.009_urb/out01/")
## add simulation type
urb3d_new <- lapply(urb3d_new, function(x) cbind(x, data.frame(type="DCEPnew")))

urb_new <- c(urb_new,urb3d_new)


## combine both and ensure that fitting list elements are put
## together, all variables in bulk have to be present in urb
sim <- list()
for (vn in names(bulk)) {
    sim[[vn]] <- rbind(bulk[[vn]], bulk5[[vn]], urb[[vn]], urb_new[[vn]])
}

save(file="sim.Rdata", sim)
simS <- calcderived(sim)

save(file="DCEPbulk.Rdata", simS)
