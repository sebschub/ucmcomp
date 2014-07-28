library(dplyr)
library(reshape2)


calcderived <- function(sim) {
    ## use degree celsius
    sim$at <- mutate(sim$at, at=at-273.15)
    sim$tg <- mutate(sim$tg, tg=tg-273.15)
    
    ## change convention of fluxes
    sim$fh <- mutate(sim$fh, fh=-fh)
    sim$fl <- mutate(sim$fl, fl=-fl)
    
    ## combine different wind velocity
    sim[["wvu"]] <- rbind(sim[["wvu"]], sim[["wvu2"]])
    sim[["wvu"]] <- rbind(sim[["wvv"]], sim[["wvv2"]])
    
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
    
    ## calculate potential temperature
    sim[["pt"]] <- merge(sim[["ap"]], sim[["at2"]]) %>%
        mutate(pt = at2 / ((1.0e-5 * ap)^(287.05/1005.0)) - 273.15) %>%
            select(-c(ap, at2))
    
    ## remove these temporary field
    for (toremove in c("wvu", "wvv", "sd_dir", "sd_diff", "ap", "at2")) {
        sim[[toremove]] <- NULL
    }

    return(lapply(sim,
                  function(x) melt(x, id.vars=c("time", "site", "type", "height"))) %>%
           do.call(what="rbind"))
    
}
