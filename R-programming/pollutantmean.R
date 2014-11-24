pollutantmean <- function(directory, pollutant, id = 1:332) {

## 'directory' is a character vector of length 1 indicating the location
## of the CSV files
    rootdir <- paste(getwd(), "/", sep="")
    datadir <- paste(rootdir, directory, "/", sep="")
    #print(pollutant)
    

## 'pollutant' is a character vector of length 1 indicating the name of
## the pollutant for which we will calculate the mean; either "sulfate"
## or "nitrate".


## 'id' is an integer vector indicating the monitor ID numbers to be used
    dataid <- sprintf("%03d", id)
    newfile <- lapply(dataid, function(tid) paste(tid, '.csv', sep=""))
    datadir <- lapply(newfile, function(tid) paste(datadir,tid,sep=""))

## Return the mean of the pollutant across all monitors list in the 'id'
## vector (ignoring NA values)
    #print(datadir)
    mydata <- lapply(datadir, function(tid) {
        rawdata <- lapply(tid, function(x) read.csv(x))
        cleandata <- lapply(rawdata, function(y) na.exclude(y))
    }) 
    
    mymean <- sapply(mydata, function(tdata) {
        tmpdata  <- as.data.frame(tdata)
        sum(tmpdata[[pollutant]])
        length(tmpdata[[pollutant]])
        rbind(sum(tmpdata[[pollutant]]), length(tmpdata[[pollutant]]))
    } )
    #(sum(mymean[1, ]) / sum(mymean[2, ])) - 0.026979 
    sum(mymean[1, ]) / sum(mymean[2, ]) 
}


