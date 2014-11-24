best <- function(state="AK", outcome="heart attack") {

##
## best() will take two inputs (state & outcome), search through a database
## "outcome-of-care-measures.csv", and return the best hospital in a given
## state with lowest mortality rate on certain disease
##

## Load necessary package(s)
    library(data.table)

## Read input data from "outcome-of-care-measures.csv" file
## Make sure the file is in the working directory
    my.data    <- read.csv("outcome-of-care-measures.csv", 
                           colClasses="character")
    my.state   <- state
    my.outcome <- outcome

## Extract subset of my.data
    heart.attack   <- suppressWarnings(as.numeric(my.data[, 11]))
    heart.failure  <- suppressWarnings(as.numeric(my.data[, 17]))
    pneumonia      <- suppressWarnings(as.numeric(my.data[, 23]))
    hospital.name  <- my.data$Hospital.Name
    state.full     <- as.character(my.data$State)
    state.list     <- as.data.frame(table(my.data$State))
    mortality.case <- c("heart attack", "heart failure", "pneumonia")

    if (!any(toupper(my.state) %in% state.list$Var1)) {
        stop("invalid state")
    } else if (!any(tolower(my.outcome) %in% mortality.case)) {
        stop("invalid outcome")
    }

    df <- data.frame(state.full, heart.attack, heart.failure, pneumonia,
                     hospital.name)
    DT <- as.data.table(df)









} # End of best() block
