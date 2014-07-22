source("netcdffunc.R")
source("sitecoord.R")
library(dplyr)
library(reshape2)

startdate <- strptime("2002-06-10 01:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT")
nts <- 30*24+1


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
nurb <- lapply(nurb, function(x) cbind(x, data.frame(type="bulk")))


## DCEP simulation
urb <- read.folder.netcdf(
    "/iplex/01/clme/schubert/clm/output/urban_comparison/basel0.009_urb/out02/",
    startdate, nts,
    c("T_2M", "U_10M_AV", "V_10M_AV", "ASHFL_S", "ALHFL_S", "ALWD_S", "ALWU_S", "ASWDIR_S", "ASWDIFD_S", "ASWDIFU_S", "CLCT", "T_G"),
    c("at", "wvu", "wvv", "fh", "fl", "ld", "lu", "sd_dir", "sd_diff", "su", "cc", "tg"),
    icoord)

## add simulation type
urb <- lapply(urb, function(x) cbind(x, data.frame(type="DCEP")))


## combine both and ensure that fitting list elements are put
## together, all variables in nurb have to be present in urb
sim <- list()
for (vn in names(nurb)) {
    sim[[vn]] <- rbind(nurb[[vn]], urb[[vn]])
}


## use degree celsius
sim$at <- mutate(sim$at, at=at-273.15)
sim$tg <- mutate(sim$tg, tg=tg-273.15)

## change convention of fluxes
sim$fh <- mutate(sim$fh, fh=-fh)
sim$fl <- mutate(sim$fl, fl=-fl)


## calculate total wind velocity
sim[["wv"]] <- merge(sim[["wvu"]], sim[["wvv"]]) %>%
    mutate(wv = sqrt(wvu^2 + wvv^2)) %>%
    select(-c(wvu, wvv))

## calculate total incoming solar radiation
sim[["sd"]] <- merge(sim[["sd_dir"]], sim[["sd_diff"]]) %>%
    mutate(sd = sd_dir + sd_diff) %>%
    select(-c(sd_dir, sd_diff))

## calculate albedo
sim[["al"]] <- merge(sim[["sd"]], sim[["su"]]) %>%
    mutate(al = su/sd) %>%
    select(-c(sd, su))

## remove these temporary field
for (toremove in c("wvu", "wvv", "sd_dir", "sd_diff")) {
    sim[[toremove]] <- NULL
}

simS <- lapply(sim,
               function(x) melt(x, id.vars=c("time", "site", "type", "height"))) %>%
    do.call(what="rbind")


save(file="DCEPbulk.Rdata", simS)
