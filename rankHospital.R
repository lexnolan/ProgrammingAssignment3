rankHospital <- function(state, outcome, number = "best")
  ##take code from best.R and add on
{
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  stateData <- levels(outcomeData[, 7])[outcomeData[, 7]]
  validity <- FALSE
  
  for (i in 1:length(stateData)) 
  {
    if (state == stateData[i]) 
    {
      validity <- TRUE
    }
  }
  if (!validity)
  {
    stop ("invalid state")
  } 
  if (!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia")))
  {
    stop ("invalid outcome")
  }
  
  ## Return hospital name in that state with given rank 30-day death rate
  column <- if (outcome == "heart attack")
  {
    11
  } 
  else if (outcome == "heart failure")
  {
    17
  } 
  else
  {
    23
  }
  
  outcomeData[, column] <- suppressWarnings(as.numeric(levels(outcomeData[, column])[outcomeData[, column]]))
  
  outcomeData[, 2] <- as.character(outcomeData[, 2])
  
  stateOther <- outcomeData[grep(state, outcomeData$State), ]
  
  order <- stateOther[order(stateOther[, column], stateOther[, 2], na.last = NA), ]
  if(number == "best")
  {
    order[1, 2]
  }
  else if(number == "worst")
  {
    order[nrow(order), 2]
  }
  else
  {
    order[number, 2]
  }
  
}
