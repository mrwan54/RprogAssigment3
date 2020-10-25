# Programming Assignment 3 - Marwan Kamal


# *******************************************************
# 2) Finding the best hospital in a state
best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% data$State){
    stop("invalid state")
  }
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  if (!(outcome %in% validOutcomes)){
    stop("invalid outcome")
  }
  rm(validOutcomes)
  # Assigning the column of outcome's index to colIndex so we might call it later
  if (outcome=="heart attack") colIndex <- 11
  else if (outcome=="heart failure") colIndex <- 17
  else colIndex <- 23
  ## Return hospital name in that state with lowest 30-day death rate
  # making the outcome column numeric without receiving warning messages
  data[,colIndex] <- suppressWarnings(as.numeric(data[,colIndex]))
  # sorting the data by outcome column first and hospital name second
  data <- data[order(data[colIndex],data[,2]),]
  # keep only the current state data
  data <- data[data$State==state,]
  # return the hospital name of the best hospital in the "outcome" category
  data[1,2]
}