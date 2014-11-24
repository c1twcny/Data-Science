corr <- function(directory, threshold=0) {

## 'directory' is a character vector of length 1 indicating the location
## of the CSV files
    pollutant <- "nitrate" 
    id <- 1:332
## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    datadir <- mydatadir(directory, id) # return list of data files w/ full path

    mydata <- mycleandata(datadir)
    
    nobs <- sapply(mydata, function(tdata) {
        tmpdata  <- as.data.frame(tdata)
        length(tmpdata[[pollutant]])
         
    } )
    tmp <- cbind(id, nobs) ## output a better format
    tmp2 <- subset(tmp, nobs > threshold) ## extract nobs >= threshold
    #as.data.frame(tmp2)
    #sprintf("%03d", tmp2[, 1]) 
    mynewdatadir <- mydatadir(directory, tmp2[, 1])

    #print(mydatadir(directory, tmp2[, 1]))

    mynewdata <- mycleandata(mynewdatadir)
    sapply(mynewdata, function(tdata) {
        tmpdata <- as.data.frame(tdata)
        mycor   <- cor(tmpdata[[2]], tmpdata[[3]])
    })

}

## function.mydatadir 
## Input: data directory, file id 
## Output:full path for a data file    
mydatadir <- function(xdir, xid) {
    rootdir <- paste(getwd(), "/", sep="")
    datadir <- paste(rootdir, xdir, "/", sep="")
    dataid  <- sprintf("%03d", xid)
    newfile <- lapply(dataid, function(tid) paste(tid, '.csv', sep=""))
    datadir <- lapply(newfile, function(tid) paste(datadir, tid, sep=""))
}

## function.mycleandata
## Input: full path of a data file -> derived from mydatadir function
## Output:new data file with NA entry removed   
mycleandata <- function(xdir) {
    mydata <- lapply(xdir, function(xid) {
              rawdata <- lapply(xid, function(x) read.csv(x))
              cleandata <- lapply(rawdata, function(y) na.exclude(y))
    })

}

