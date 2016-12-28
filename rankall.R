rankall <- function(outcome, number = "best")
  ##take code from best.R and add on
{
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that outcome is valid
  if (!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia")))
  {
    stop ("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
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
  
  # Generate an empty vector that will be filled later, row by row, to generate the final output
  output <- vector()
  
  stateData <- levels(outcomeData[, 7])
  
  for(i in 1:length(stateData))
  {
    stateOther <- outcomeData[grep(stateData[i], outcomeData$State), ]
    order <- stateOther[order(stateOther[, column], stateOther[, 2], na.last = NA), ]
    
    hospital <- if(number == "best")
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
    
    output <- append(output, c(hospital, stateData[i]))
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name

  output <- as.data.frame(matrix(output, length(stateData), 2, byrow = TRUE))
  colnames(output) <- c("hospital", "state")
  rownames(output) <- stateData
  
  output
}
