##' Read values of NetCDFs from a folder at certain sites
##'
##' Data is read from NetCDF files in folder following a regular
##' structure defined by the date. Data output in a data frame at the
##' sites given as argument.
##' @param folder Folder that includes NetCDF
##' @param startdate Date to start reading file
##' @param ninc Number of files to open
##' @param var Variables to read
##' @param varname Output variable names
##' @param sitelli Data frame with \code{site}, \code{ilon} and
##' \code{ilat} column
##' @param maxheight Maximum height up to which read variable (used if
##' NetCDF includes height variable)
##' @param levels Heights of lowest level to read (from bottom to
##' top). Used if NetCDF does not include height variable)
##' @param hinc Hour increase step
##' @param fpre NetCDF name prefix
##' @param average Number of grid cells to average in every direction
##' @return Data frame with read variables
##' @author Sebastian Schubert
read.folder.netcdf <- function(folder, startdate, ninc, var, varname, sitelli, maxheight=50, levels=NA, hinc=1, fpre="lffd", average=0) {
    stopifnot(nrow(sitelli)>0, length(var)>0, ninc>=0, average>=0)
    
    require(RNetCDF)

    ## check heights
    nc <- open.nc(paste(folder, "/",fpre, format(startdate, "%Y%m%d%H"), ".nc", sep=""))
    heightids <- list()
    heights <- list()
    for (v in var) {
        dim <- var.inq.nc(nc,v)$dimids
        ## for now, assume dim[1] and dim[2] are rlon and rlat, dim[lenght(dim)] time
        if (length(dim)==3) {
            heights[[v]] <- NaN
        } else if (length(dim)==4) {
            tryCatch({
                ## dimension variable available
                h <- var.get.nc(nc,dim.inq.nc(nc,dim[3])$name)
                heightids[[v]] <- which(h<=maxheight)
                heights[[v]] <- h[heightids[[v]]]
            }, error=function(e) {
                ## dimension variable not available, use argument values
                heightids[[v]] <-
                    seq(from=dim.inq.nc(nc,dim[3])$length, by=-1, length.out=length(levels))
                heights[[v]] <- levels
                
            }
                     )
        } else {
            stop(paste("Number of dimensions of",v,"currently not treated."))
        }
        
    }
    close.nc(nc)
    dates <- seq.POSIXt(from=startdate, by=hinc*3600, length.out=ninc)

    ## initial empty data frames
    tempdf <- list()
    for (iv in 1:length(var)) {
        tempdf[[var[iv]]] <- data.frame(t=as.POSIXlt(character()),
                                        site=character(),
                                        height=numeric(),
                                        value=numeric())
    }
    
    ## loop over all files
    for (id in 1:length(dates)) {
        print(paste("Reading", paste(folder, "/",fpre, format(dates[id], "%Y%m%d%H"), ".nc", sep="")))
        nc <- open.nc(paste(folder, "/",fpre, format(dates[id], "%Y%m%d%H"), ".nc", sep=""))
        
        for (v in var) {
            tempfield <- var.get.nc(nc,v, collapse=FALSE)
            for (s in 1:nrow(sitelli)) {

                ilon <- sitelli$ilon[s]
                ilat <- sitelli$ilat[s]
                site <- sitelli$site
                
                switch(length(dim(tempfield))-2,
                       values <- mean(
                           tempfield[(ilon-average):(ilon+average),
                                     (ilat-average):(ilat+average),
                                     1])
                       ,
                       values <- apply(
                           tempfield[(ilon-average):(ilon+average),
                                     (ilat-average):(ilat+average),
                                     heightids[[v]],
                                     1, drop=FALSE],
                           c(3),
                           mean
                           )
                       )
                tempdf[[v]] <- rbind(
                    tempdf[[v]],
                    data.frame(
                        t=dates[id],
                        site=sitelli$site[s],
                        height=heights[[v]],
                        value=values)
                       
                    )
            }
            
            rm(tempfield, values)
            gc(verbose=FALSE)
        }
        
        close.nc(nc)
    }

    ## apply correct variable name
    for (iv in 1:length(var)) {
        names(tempdf[[var[iv]]])[[4]] <- varname[iv]
    }

    output <- Reduce(function(...) merge(..., all=TRUE),
                     tempdf)
    
    return(output)
}

##' Find index of grid cells which include site
##'
##' This routine finds the coordinate indices which correspond the the
##' grid cell that includes certain sites. A NetCDF is opened to read
##' the rotated coordinate system fields, which are compared to the
##' sites' coordinates.
##' @param llnc NetCDF file that includes the fields \code{rlon} and
##' \code{rlat} of the simulation region files
##' @param rlrldf Data frame with columns \code{site} including the
##' site's names as well as \code{lon} and \code{lat} in rotated
##' coordinate system
##' @return toll
##' @author Sebastian Schubert
findlonlatindex <- function(llnc, rlrldf) {
    require(RNetCDF)
    require(dplyr)

    nc <- open.nc(llnc)
    rlon <- var.get.nc(nc, "rlon")
    rlat <- var.get.nc(nc, "rlat")
    close.nc(nc)

    return (coord %>%
            group_by(site) %>%
            summarise(ilon=which.min(abs(rlon-lon)), ilat=which.min(abs(rlat-lat)))
            )
}
