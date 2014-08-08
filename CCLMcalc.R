library(dplyr)
library(reshape2)


calcderived <- function(sim) {
    
    ## calculate 2m potential temperature with lowest pressure,
    ## other hights use correct heights
    lowestap <- sim$ap %>%
        arrange(time, site, type, height) %>%
            group_by(time, site, type) %>%
                summarise(ap=first(ap)) %>%
                    mutate(height=2)

    names(sim$at2)[names(sim$at2)=="at2"] <- "at"
    
    sim$pt <- rbind(
        merge(sim$at, lowestap),
        merge(sim$at2, sim$ap)
        ) %>%
            mutate(pt = at / ((1.0e-5 * ap)^(287.05/1005.0)) - 273.15) %>%
                select(-c(ap, at))

    ## use degree celsius
    sim$at <- mutate(sim$at, at=at-273.15)
    sim$tg <- mutate(sim$tg, tg=tg-273.15)
    
    ## change convention of fluxes
    sim$fh <- mutate(sim$fh, fh=-fh)
    sim$fl <- mutate(sim$fl, fl=-fl)
    
    ## combine different wind velocity
    names(sim$wvu2)[names(sim$wvu2)=="wvu2"] <- "wvu"
    names(sim$wvv2)[names(sim$wvv2)=="wvv2"] <- "wvv"
    sim$wvu <- rbind(sim$wvu, sim$wvu2)
    sim$wvv <- rbind(sim$wvv, sim$wvv2)
    
    ## calculate total wind velocity
    sim$wv <- merge(sim$wvu, sim$wvv) %>%
        mutate(wv = sqrt(wvu^2 + wvv^2)) %>%
            select(-c(wvu, wvv))
    
    ## calculate total incoming solar radiation
    sim$sd <- merge(sim$sd_dir, sim$sd_diff) %>%
        mutate(sd = sd_dir + sd_diff) %>%
            select(-c(sd_dir, sd_diff))
    
    ## calculate albedo
    sim$al <- merge(sim$sd, sim$su) %>%
        mutate(al = su/sd) %>%
            select(-c(sd, su))
    
    
    ## remove these temporary field
    for (toremove in c("sd_dir", "sd_diff", "ap", "at2")) {
        sim[[toremove]] <- NULL
    }

    return(lapply(sim,
                  function(x) melt(x, id.vars=c("time", "site", "type", "height"))) %>%
           do.call(what="rbind"))
    
}
