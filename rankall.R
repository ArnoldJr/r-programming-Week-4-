rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Data to retrieve
        ## Hospital.Name, 
        ## State,
        ## Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, (function depening)
        ## Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, (function depening)
        ## Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia (function depening)
        ##
        
        
        ## switch for creating a subset depended on the outcome
        if(outcome == "heart attack") {
                data <- subset(data, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        } else if(outcome == "heart failure") {
                data <- subset(data, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        } else if(outcome == "pneumonia") {
                data <- subset(data, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        } else {
                stop("invalid outcome")
        }
        
        ## Convert the factor collom to numeric, so that the sort function works correctly
        data[, 3] <- suppressWarnings( as.numeric(data[, 3]) )
        
        ## Only work with complete cases
        data <- data[complete.cases(data), ]
        
        ## sort data ascending, first on state, second on outcome, third on the hospital name
        data <- data[order(data[, 2],data[, 3], data[, 1]),]
        
        #Split the dataframe on the states
        states <- split(data, data$State)


        ## switch for num var, and use the lapply function to get the results
        if(num == "best") {
                result <- lapply(states, function(ext) { ext[1,] })
        } else if(num == "worst") {
                result <- lapply(states, function(ext) { ext[nrow(ext),] })
        } else if (is.numeric(num)){
                result <- lapply(states, function(ext) { ext[num,] })
        }else {
                stop("invalid num")
        }        
        
        ## convert the results from the lapply function back to a dataframe        
        result <- as.data.frame(do.call(rbind, result))
        
        ## Use the rownames for the state, usefull for the NA's when a number is out of range
        result[,2] <- row.names(result)

        ## set the colnames for the result dataframe
        colnames(result) <- c("hospital","state","rate")
        
        ## message for showing the parameters
        message(c("Results for - outcome: ", outcome, ", num: ", num))
        
        ## return the result, hospitalname if available and the state
        return(result[,c(1,2)])
        
}

