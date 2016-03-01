##### Ranking hospitals in all states #####

# Set working directory
setwd("C:/Users/Hiroshi Ikeda/Desktop/Coursera/ProgrammingAssignment3")

## function test
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

## rankall function takes "outcome" and "num" and returs hospitals' names of the specified rank by num in all states
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE, colClasses = "character")
  
  ## Check if outcome input is valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if ((outcome %in% outcomes) == FALSE) { stop(print("invalid outcome")) }
  
  ## Create state variable
  state <- levels(factor(outcomedata[, 7]))
  
  ## Select columns
  outcomedata <- outcomedata[,c(2,7,11,17,23)]
  
  ## Name columns
  names(outcomedata) <- c("hospital","state","heart attack","heart failure","pneumonia")
  
  ## Subset data only with an outcome specified in a function's argument
  outcomedata <- outcomedata[,c("hospital", "state", outcome)]
  
  ## Create hospital variable and find hospitals with the specified rank in each state
  hospital<-vector()
  for (i in seq(state)) {
    statedata <- outcomedata[outcomedata$state==state[i],]
    ## Order data by outcome then by hospital name
    statedata <- statedata[order(as.numeric(statedata[,3]), as.character(statedata[,1]),decreasing=FALSE, na.last=NA), ]
    ## Name rows starting from 1
#    rownames(statedata) <- c(1:nrow(statedata))
    ## If num is "best" or "worst", num becomes 1 or the last number of the rank
    if (is.character(num) == TRUE) {
      if (num == "best") {
        num2 = 1
      }
      if (num == "worst") {
        num2 = nrow(statedata)
      }
    } else{
      num2<-num
    }
    ## return hospital name 
    hospital[i] <- statedata[num2,1]
  }
  data.frame(hospital=hospital, state=state)
}