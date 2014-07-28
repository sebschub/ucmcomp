#### Preparation

## packages
library(ggplot2)
library(reshape2)
library(dplyr)

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

## get the highest measurement/simulation
get_highest <- function(df) {
    df %>%
        arrange(time, site, type, variable, desc(height)) %>%
            group_by(time, site, type, variable) %>%
                summarise(value=first(value))
}

## get the measurement/simulation nearest to target height (th)
get_th <- function(df, th) {
    df %>%
        mutate(hth=abs(height-th)) %>%
            arrange(time, site, type, variable, hth) %>%
                group_by(time, site, type, variable) %>%
                    summarise(height=first(height), value=first(value))
}

## filter only fluxes
filter_fluxes <- function(df) {
    df %>%
        filter(variable %in% c("fl", "fh", "sd", "su", "ld", "lu")) %>%
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

## filter 2m air temperature
filter_at_2m <- function(df) {
    df %>%
        filter(variable=="at") %>%
            droplevels() %>%
                get_th(2)
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
    
## average diurnal cycle
dfav <- df %>%
    mutate(time=as.numeric(format(time, "%H"))) %>%   # add hour of day, name "time" reusage of code
    group_by(time, site, type, height, variable) %>%  # for theses groups:
    summarize(value=mean(value, na.rm=TRUE), nac=sum(is.na(value))) # average and na

#### FLUXES

## select flux variables, take highest measurements
df.flux <- df %>%
    filter_fluxes()

## calculate RMSE and MBE
rm.flux <- df.flux %>% calc_rmse_mbe()

## plot fluxes
p.flux <- ggplot( filter_fluxes(dfav), aes(x=time, y=value, color=type)) + geom_point() + geom_line() + facet_grid(variable~site, scales="free_y")

#### CLOUD COVER

df.ccBBIN <- df %>%
    filter_ccBBIN()

#df.ccBBIN %>% calc_rmse_mbe()
p.cc <- ggplot( filter_ccBBIN(dfav), aes(x=time, y=value, color=type)) + geom_point() + geom_line() + facet_grid(variable~site, scales="free_y")


#### Air temperature

df.at <- df %>%
    filter_at_2m()

rm.at <- calc_rmse_mbe(df.at)

p.at <- ggplot( filter_at_2m(dfav), aes(x=time, y=value, color=type)) + geom_point() + geom_line() + facet_grid(variable~site, scales="free_y")

p.atVLNF <- ggplot( filter_at_2m(dfav)%>%filter(site=="VLNF"), aes(x=time, y=value, color=type)) + geom_point() + geom_line() + facet_grid(variable~site, scales="free_y")

p.atVLNFts <- ggplot( df.at %>%filter(site=="VLNF"), aes(x=time, y=value, color=type)) + geom_point() + geom_line() + facet_grid(variable~site, scales="free_y")
