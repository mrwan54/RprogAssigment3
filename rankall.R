# Programming Assignment 3 - Marwan Kamal


# *******************************************************
# 4) Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  if (!(outcome %in% validOutcomes)){
    stop("invalid outcome")
  }
  rm(validOutcomes)
  ## Return hospital name in that state with the given rank
  # Assigning the column of outcome's index to colIndex so we might call it later
  if (outcome=="heart attack") colIndex <- 11
  else if (outcome=="heart failure") colIndex <- 17
  else colIndex <- 23
  # making the outcome column numeric without receiving warning messages
  data[,colIndex] <- suppressWarnings(as.numeric(data[,colIndex]))
  data <- na.omit(data)
  data <- data[order(data[,7],data[,colIndex],data[,2]),]
  # sorting the data by outcome column first and hospital name second
  # return the hospital name of the best hospital in the "outcome" category
  df <- data.frame("hospital"=character(0),"state"=character(0),stringsAsFactors=FALSE)
  uniState <- unique(data$State)
  n <- num
  for (st in uniState){
    x <- data[data$State==st,]
    if(n=="best") n = 1
    else if (n=="worst") n = nrow(x)
    df <- rbind(df,c(x[n,2],st))
    n <- num
  }
  names(df) <- c("hospital","state")
  df
  ## 30-day death rate
}
