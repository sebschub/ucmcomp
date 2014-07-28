##' Read values of NetCDFs from a folder at certain sites
##'
##' Data is read from NetCDF files in folder following a regular
##' structure defined by the date. Data output in a data frame at the
##' sites given as argument.
##' @param folder Folder that includes NetCDF
##' @param startdate Date to start reading file
##' @param ninc Number of files to open
##' @param var Variables to read in NetCDF files
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
    stopifnot(nrow(sitelli)>0, length(var)>0, ninc>=0, average>=0, length(var)==length(varname))
    
    require(RNetCDF)

    ## check heights
    nc <- open.nc(paste(folder, "/",fpre, format(startdate, "%Y%m%d%H"), ".nc", sep=""))
    heightids <- list()
    heights <- list()
    for (v in var) {
        dim <- var.inq.nc(nc,v)$dimids
        ## for now, assume dim[1] and dim[2] are rlon and rlat, dim[lenght(dim)] time
        if (length(dim)==3) {
            for (s in 1:nrow(sitelli)) {
                heights[[v]][[s]] <- NaN
            }
        } else if (length(dim)==4) {
            h <- var.get.nc(nc,dim.inq.nc(nc,dim[3])$name)
            if (!is.null(h)) {
                for (s in 1:nrow(sitelli)) {
                    heightids[[v]][[s]] <- which(h<=maxheight)
                    heights[[v]][[s]] <- h[ heightids[[v]][[s]] ]
                }
            } else {
                heightids[[v]] <- list()
                for (s in 1:nrow(sitelli)) {
                    heightids[[v]][[s]] <- which( levels[sitelli$ilon[s], sitelli$ilat[s],] <= maxheight)
                    heights[[v]][[s]] <- levels[ sitelli$ilon[s], sitelli$ilat[s], heightids[[v]][[s]] ]
                }
            }
        } else {
            stop(paste("Number of dimensions of",v,"currently not treated."))
        }
        
    }

    close.nc(nc)
    dates <- seq.POSIXt(from=startdate, by=hinc*3600, length.out=ninc)

    ## initial empty data frames
    tempdf <- list()
    for (vn in varname) {
        tempdf[[vn]] <- data.frame(time=as.POSIXct(character()),
                                   site=character(),
                                   height=numeric(),
                                   value=numeric())
    }
    
    ## loop over all files
    for (id in 1:length(dates)) {
        print(paste("Reading", paste(folder, "/",fpre, format(dates[id], "%Y%m%d%H"), ".nc", sep="")))
        nc <- open.nc(paste(folder, "/",fpre, format(dates[id], "%Y%m%d%H"), ".nc", sep=""))
        
        for (iv in 1:length(var)) {
            v <- var[iv]
            vn <- varname[iv]
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
                                     heightids[[v]][[s]],
                                     1, drop=FALSE],
                           c(3),
                           mean
                           )
                       )
                for (h in 1:length(heights[[v]][[s]])) {
                    tempdf[[vn]] <- rbind(
                        tempdf[[vn]],
                        data.frame(
                            time=dates[id],
                            site=sitelli$site[s],
                            height=heights[[v]][[s]][h],
                            value=values[h])
                        )
                }
            }
            
            rm(tempfield, values)
            gc(verbose=FALSE)
        }
        
        close.nc(nc)
    }

    ## use correct column names
    for (vn in varname) {
        names(tempdf[[vn]])[4] <- vn
    }
    
    ## round heights to ensure grouping by height is possible
    tempdf <- lapply(tempdf, function (df) {df$height <- round(df$height, digits=2); return(df)})
    
    return(tempdf)
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

calcatmoheights <- function(ncconst) {
    require(RNetCDF)

    nc <- open.nc(ncconst)
    hsurf <- var.get.nc(nc, "HSURF")
    hhl <- var.get.nc(nc, "HHL")

    height <- array(0, dim=(dim(hhl)-c(0,0,1)))
    for (i in 1:(dim(hhl)[3]-1)) {
        height[,,i] <- 1/2*(hhl[,,i] + hhl[,,i+1]) - hsurf
    }
    return(height)
    
}
