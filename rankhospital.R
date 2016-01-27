rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Data to retrieve
        ## Hospital.Name, 
        ## State,
        ## Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, (function depening)
        ## Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, (function depening)
        ## Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia (function depening)
        ##
        
        
        ## Check if the state is found in the dataset 
        if (!state %in% data$State){
                stop("invalid state") 
        }
        ## subset the data for the state
        data <- subset(data, State == state)
        
        
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
        
        ## sort data ascending, first on outcome, second on the hospital name
        data <- data[order(data[, 3], data[, 1]),]
        

        ## switch for num var
        if(num == "best") {
                data <- data[1, ]       
        } else if(num == "worst") {
                data <- data[nrow(data), ]
        } else if (is.numeric(num)){
                data <- data[num, ]
        } else {
                stop("invalid num")
        }
        
        ## message for showing the parameters
        message(c("Results for - state: ", state, ", outcome: ", outcome, ", num: ", num))
        
        ## return the result(only hospitalname)
        return(data[, 1])
}

