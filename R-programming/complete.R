complete <- function(directory, id = 1:332) {

## 'directory' is a character vector of length 1 indicating the location
## of the CSV files
    rootdir <- paste(getwd(), "/", sep="")
    datadir <- paste(rootdir, directory, "/", sep="")
    pollutant <- "nitrate" 

## 'id' is an integer vector indicating the monitor ID numbers to be used
    dataid <- sprintf("%03d", id)
    newfile <- lapply(dataid, function(tid) paste(tid, '.csv', sep=""))
    datadir <- lapply(newfile, function(tid) paste(datadir,tid,sep=""))

## calculate the number of completed cases by removing NA entries
    mydata <- lapply(datadir, function(tid) {
        rawdata <- lapply(tid, function(x) read.csv(x))
        cleandata <- lapply(rawdata, function(y) na.exclude(y))
    }) 
    
    nobs <- sapply(mydata, function(tdata) {
        tmpdata  <- as.data.frame(tdata)
        length(tmpdata[[pollutant]])
         
    } )
    
    as.data.frame(cbind(id, nobs)) ## output a better format

}


