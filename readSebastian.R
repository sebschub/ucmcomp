source("netcdffunc.R")
source("sitecoord.R")

## calculate site indices
icoord <- findlonlatindex(
    "/scratch/01/schubert/clm/output/urban_comparison/basel0.009/out01/lffd2002050100.nc",
    coord)

startdate <- strptime("2002-06-15 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT")

nurb <- read.folder.netcdf(
    "/scratch/01/schubert/clm/output/urban_comparison/basel0.009/out02/",
    startdate, 20,
    c("T_2M", "U_10M_AV", "V_10M_AV", "ASHFL_S", "ALHFL_S", "ALWD_S", "ALWU_S", "ASWDIR_S", "ASWDIFD_S", "ASWDIFU_S", "CLCT", "T_G"),
    c("at", "wvu", "wvv", "fh", "fl", "ld", "lu", "sd_dir", "sd_diff", "su", "cc", "tg"),
    icoord)
