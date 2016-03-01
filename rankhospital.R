##### Ranking hospitals by outcome in a state #####

## rankhospital function takes "state", "outcome" and "num" and returs the name of the hospital ranked by the given num in the state and outcome
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE, colClasses = "character")
  
  ## Check if state and outcome input are valid
  states <- outcomedata[ , 7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if ((state %in% states) == FALSE) { stop(print("invalid state")) }
  else if ((outcome %in% outcomes) == FALSE) { stop(print("invalid outcome")) }
  
  ## Select columns
  outcomedata <- outcomedata[,c(2,7,11,17,23)]
  
  ## Name columns
  names(outcomedata) <- c("hospital","state","heart attack","heart failure","pneumonia")
  
  ## Subset data only with an outcome specified in a function's argument
  outcomedata <- outcomedata[,c("hospital", "state", outcome)]
  
  ## Select raws only with "state"=state
  outcomedata <- outcomedata[outcomedata$state==state,]
  
  ## Remove NAs
  outcomedata <- na.omit(outcomedata)
  
  ## Coerce rate variables as numeric
  outcome_n <- as.numeric(outcomedata[,3])
  
  ## Order data by outcome then by hospital name
  sortlist <- order(outcome_n, outcomedata[,1])
  outcomedata <- outcomedata[sortlist,]
  
  ## Name rows starting from 1
  rownames(outcomedata) <- c(1:nrow(outcomedata))
  
  ## Create and combine rank variable 
  rank <- c(1:nrow(outcomedata))
  outcomedata <- cbind(outcomedata, rank)
  
  ## If num is "best" or "worst", num becomes 1 or the last number of the rank
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(rank)
    }
  }
  
  ## Check if num does not exceed the number of hospitals in the state
  if (is.numeric(num) == TRUE) {
    if (num > length(rank)) {
    return(NA)
    }
  }
  
  ## Return hospital name in that state with lowest 30-day death
  outcomedata[num,1]
}