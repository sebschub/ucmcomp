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
avdiurnl <- function(df) {
    ## add hour of day, name "time" reusage of 
    df2 <- ungroup(df) %>%  mutate(time=as.numeric(format(time, "%H")))
    ## if height is present, group by it
    if ("height" %in% names(df.flux)) {
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
        filter(variable=="at") %>%
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

filter_tg <- function(df) {
    df %>%
        filter(variable %in% c("tg"))%>%
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
    df %>%
        filter_fluxes() %>%
            calc_nr_fg()
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


## load data
load("measurements.Rdata")
load("DCEPbulk.Rdata")

## measurements: average over different sensors at same height and
## remove `subtype`; combine with simulation
df <- bubble %>%
    group_by(time, site, type, height, variable) %>% 
    summarize(value=mean(value)) %>%
    rbind(simS) %>%
    ungroup()
    
#### FLUXES

## select flux variables, take highest measurements
df.flux <- flux(df)

## calculate RMSE and MBE
rm.flux <- calc_rmse_mbe(df.flux)

## plot fluxes
p.flux <- plotit( avdiurnl(df.flux))

#### CLOUD COVER

df.ccBBIN <- df %>%
    filter_ccBBIN()

#df.ccBBIN %>% calc_rmse_mbe()
p.cc <- plotit( avdiurnl(df.ccBBIN))


#### Air temperature

df.at <- df %>%
    filter_at_2m()

rm.at <- calc_rmse_mbe(df.at)

p.at <- plotit( avdiurnl(df.at) )
p.atVLNF <- plotit( avdiurnl(df.at%>%filter(site=="VLNF") ))
p.atVLNFts <- plotit( filter(df.at, site=="VLNF"))

#### Wind velocity
df.wv <- df %>%
    filter_wv()

rm.wv <- calc_rmse_mbe(df.wv)

height.wv <- filter_wv(df.wv) %>% group_by(type, site) %>% summarise(height=first(height))
## manual position
height.wv <- cbind(height.wv, x=12, y=c(rep(1.5,6), rep(1.,6), rep(0.5,6)))

p.wv <- plotit( avdiurnl(df.wv), height.wv)


#### Potential temperature

p.pt <- ggplot( dfav %>% filter(variable=="pt", site=="BKLH" | site=="VLNF" | site=="BSPR"), aes(x=value, y=height, color=type)) +  geom_point() + facet_grid(time~site)

## one day
pdate <- strptime("2002-06-17 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT-1")

p.ptday <- ggplot( df %>% filter(variable=="pt", time>=pdate, time<pdate+3600*6, site=="BKLH" | site=="VLNF" | site=="BSPR"), aes(x=value, y=height, color=type)) +  geom_point() + facet_grid(time~site)


#### Momentum flux
p.fm <- plotit( df %>% filter_fm() %>% avdiurnl)

#### Surface temperature
p.tg <- ggplot( filter_tg(dfav), aes(x=time, y=value, color=type)) + geom_point() + geom_line() +  facet_grid(~site)

p.tgat <- ggplot(rbind(filter_tg(dfav) %>% select(-nac), filter_at_2m(dfav)), aes(x=time, y=value, color=type, linetype=variable, shape=variable)) + geom_point() + geom_line() +  facet_grid(~site)
