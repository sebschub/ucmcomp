#### Preparation

## packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

main_sites <- c("BSPR", "BSPA", "ALLS", "GRNZ", "VLNF", "BLER")
## plot symbols smaller if more than nafc are NaN or NA
nafc <- 0.3

## MBE and RMSE of one site and one variable
mbe_rmse <- function(df) {
    ## return nothing if not measured
    if (all(df$type!="measurement"))
        return(data.frame())
    df %>%
        dcast(time~type, value.var = "value") %>%
            summarise_each(funs(
                 mbe=     mean( measurement- .   , na.rm=TRUE) ,
                rmse=sqrt(mean((measurement- .)^2, na.rm=TRUE))
                ),
                           vars=-c(time,measurement))
}

## calculate MBE and RMSE
calc_rmse_mbe <- function(df) {
    df %>% 
        group_by(site, variable) %>%
            do(mbe_rmse(.))
}

## average diurnal cycle
avdiurnal <- function(df) {
    ## add hour of day, name "time" reusage of 
    df2 <- ungroup(df) %>%  mutate(time=as.numeric(format(time, "%H")))
    ## if height is present, group by it
    if ("height" %in% names(df)) {
        df3 <- group_by(df2, time, site, type, height, variable)
    } else {
        df3 <- group_by(df2, time, site, type,         variable)
    }
    df3 %>% summarize(naf=mean(is.na(value)),
                      value=mean(value, na.rm=TRUE))
}

## get the highest measurement/simulation
get_highest <- function(df) {
    df %>%
        arrange(time, site, type, variable, desc(height)) %>%
            group_by(time, site, type, variable) %>%
                summarise(height=first(height), value=first(value))
}

## get the measurement/simulation nearest to target height (th)
get_th <- function(df, th) {
    df %>%
        mutate(hth=abs(height-th)) %>%
            arrange(time, site, type, variable, hth) %>%
                group_by(time, site, type, variable) %>%
                    summarise(height=first(height), value=first(value))
}

## get the simulation at height nearest to the measurement height
get_near_measurement <- function(df) {
    df %>%
        group_by(time, site, variable) %>%
            mutate(hd =
                   abs(height-ifelse(length(height[type=="measurement"])==1, height[type=="measurement"], NaN )) ) %>%
                       arrange(time, site, type, variable, hd) %>%
                           group_by(time, site, type, variable) %>%
                               summarise(height=first(height), value=first(value))
}

## filter only fluxes
filter_fluxes <- function(df) {
    df %>%
        filter(variable %in% c("al", "fl", "fh", "sd", "su", "ld", "lu"),
               site %in% main_sites) %>%
            droplevels() %>%
                get_highest()
}

## filter only cloud cover
filter_ccBBIN <- function(df) {
    df %>%
        filter(variable %in% c("cc"), site=="BBIN") %>%
            droplevels() %>%
                select(-height)
}

## filter wind velocity near measurement height
filter_wv <- function(df) {
    df %>%
        filter(variable=="wv", site %in% main_sites) %>%
            droplevels() %>%
                get_near_measurement()
}


## filter 2m air temperature
filter_at_2m <- function(df) {
    df %>%
        filter(variable=="at", site %in% main_sites) %>%
            droplevels() %>%
                get_th(2)
}

## filter and calculate momentum flux
filter_fm <- function(df) {
    df %>%
        filter(variable %in% c("fu", "fv")) %>%
            spread(variable, value) %>%
                mutate(fm=fu^2+fv^2) %>%
                    gather(variable, value, fm, fu, fv)
}

filter_pt <- function(df) {
    df %>%
        filter(variable=="pt", site=="BKLH" | site=="VLNF" | site=="BSPR") %>%
            droplevels()
}

filter_wv_profile<- function(df) {
    df %>%
        filter(variable=="wv", site=="BKLH" | site=="VLNF" | site=="BSPR") %>%
            droplevels()
}


filter_tg <- function(df) {
    df %>%
        filter(variable=="tg", site %in% main_sites )%>%
            droplevels()
}


## calculate net radiation and storage flux
calc_nr_fg<- function(df) {
    df  %>%
        select(-height) %>%
            spread(variable, value) %>%
                mutate(nr=sd+ld-su-lu, fg=nr-fl-fh) %>%
                    gather(variable, value, -c(time, site, type))
}

flux <- function(df) {
    df1 <- df %>%
        filter_fluxes() %>%
            calc_nr_fg()
    ## get also measured storage flux
    df2 <- df %>%
        filter(variable=="fg", type=="measurement") %>%
            get_highest() %>%
                ungroup() %>%
                    mutate(type="measdir") %>%
                        select(-height)
    rbind(df1, df2)
}

plotit <- function(df, annotation=NULL) {
    p <- ggplot(df, aes(x=time, y=value, color=type)) +
        geom_point(aes(size=naf<nafc)) +
            geom_line() +
                facet_grid(variable~site, scales="free_y") +
                    scale_size_manual(values=c(2,3))
    if (!missing(annotation)) {
        p <- p + geom_text(data=annotation, aes(label=height, x=x, y=y))
    }
    return(p)
}

plotprofile <- function(df) {
    ggplot( df, aes(x=value, y=height, color=type)) +
        geom_point(aes(size=naf<nafc)) +
            facet_grid(time~site) +
                scale_size_manual(values=c(2,3))
}

## load data, ADD YOUR DATA HERE
load("measurements.Rdata")
load("DCEPbulk.Rdata")
load("TERRAurb.Rdata")
simH2 <- simH
load("TERRAurb_noahf.Rdata")

## measurements: average over different sensors at same height and
## remove `subtype`; combine with simulation; ADD YOUR DATA HERE
df <- bubble %>%
    group_by(time, site, type, height, variable) %>% 
    summarize(value=mean(value)) %>%
    rbind(simS) %>%
    rbind(simH) %>%
    rbind(simH2) %>%
    ungroup()

## remove old DCEP simulation and CCLM5 Simulation
df <- df %>%
    filter(!(type %in% c("bulk5", "DCEP"))) %>%
    droplevels()


#### Fluxes

## select flux variables, take highest measurements
df.flux <- flux(df)

## calculate RMSE and MBE
rm.flux <- calc_rmse_mbe(df.flux)

## plot fluxes
p.flux <- plotit( avdiurnal(df.flux))


#### Cloud Cover
df.ccBBIN <- df %>%
    filter_ccBBIN()

#df.ccBBIN %>% calc_rmse_mbe()
p.cc <- plotit( avdiurnal(df.ccBBIN))


#### Air temperature
df.at <- df %>%
    filter_at_2m()

rm.at <- calc_rmse_mbe(df.at)

p.at <- plotit( avdiurnal(df.at) )
p.atVLNF <- plotit( avdiurnal(df.at%>%filter(site=="VLNF") ))
p.atVLNFts <- plotit( filter(df.at, site=="VLNF"))


#### Wind velocity
df.wv <- df %>%
    filter_wv()

rm.wv <- calc_rmse_mbe(df.wv)

## show also the measurement heights
height.wv <- filter_wv(df.wv) %>% group_by(type, site) %>% summarise(height=first(height))
## manual position
ntype <- n_distinct(levels(height.wv$type))
nsite <- n_distinct(levels(height.wv$site))
height.wv <- cbind(height.wv, x=12, y=rep( seq(0.5, by=0.25, length.out=ntype), rep(nsite, ntype)))

p.wv <- plotit( avdiurnal(df.wv), height.wv)


#### Potential temperature
df.pt <- df %>%
    filter_pt()

p.pt <- plotprofile( avdiurnal(df.pt) )


#### Wind profile
df.wv_profile <- df %>%
    filter_wv_profile()

p.wv_profile <- plotprofile( avdiurnal(df.wv_profile) )


#### Momentum flux
df.fm <- df %>%
    filter_fm()

p.fm <- plotit( avdiurnal(df.fm) )


#### Surface temperature
df.tg <- df %>%
    filter_tg()

p.tg <- plotit( avdiurnal(df.tg))


## show plots on screen:
# p.flux

## save plot to file:
# ggsave(filename="fluxes.pdf", p.flux, width=30, height=30, limitsize=FALSE)
# ggsave(filename="pt.pdf", p.pt, width=10, height=30, limitsize=FALSE)

## show mbe or rmse:
# rm.flux
## or
# View(rm.flux)
