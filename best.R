##### Finding the best hospital in a state #####

## best function takes "state" and "outcome" and returs the name of the hospital with best 30-day mortality
best <- function(state, outcome) {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE, colClasses = "character")
  
  ## Check if state and outcome input are valid
  states <- outcomedata[ , 7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if ((state %in% states) == FALSE) { stop(print("invalid state")) }
  else if ((outcome %in% outcomes) == FALSE) { stop(print("invalid outcome")) }
  
  ## Select only necessary columns for analysis
  outcomedata <- outcomedata[,c(2,7,11,17,23)]

  ## Name the columns
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
  
  ## organize rownames starting from 1
  rownames(outcomedata) <- c(1:nrow(outcomedata))
  
  ## Return hospital name in that state with lowest 30-day death
  outcomedata[1,1]
  }