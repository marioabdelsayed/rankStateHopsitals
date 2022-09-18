
#read hospital statistics file
outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

stateList <- data.frame()


rankHospital <- function(state, wantedRate = "heart attack", theRank){
  #forms the name of the column to use for getting the rates 
  causeName <- causeString(wantedRate)
  #gets valid sates from original file
  validStates <- outcomeFile[['State']]
  #checks state against valid states
  checkState(state, validStates)
  #checks if the passed condition is valid 
  checkRate(wantedRate)
  #gets the death rates for the given condition
  rateColumnNum <- match(causeName,names(outcomeFile))
  #stores all rates, states, and hopsital info in a seperate data frame
  #for iteration 
  outcomeData <- data.frame(as.numeric(outcomeFile[,rateColumnNum]),
                            outcomeFile[['Hospital.Name']], outcomeFile[['State']])
  #renames columns for ease of use
  colnames(outcomeData) <- c("Outcome","Hospital", "State" )
  #removes incomplete rows
  outcomeData <- outcomeData[complete.cases(outcomeData),]
  #iterate through the data to find the hospitals
  apply(outcomeData, 1, findStateHospitals,
                          cause = 'Outcome', wantedState = state)
  colnames(stateList) <-c('Outcome','Hospital','State')
  #order the list and use alphabetical ordering of hospital names as 
  #tie breaker
  orderedList <- stateList[order(stateList$Outcome, stateList$Hospital),]
  #reset the rows to 1:nrow(orderedList)
  row.names(orderedList) <- NULL
  #evaluate the rank based on whether it's 'best','worst', or an
  #integer
  evaledRank <- evalRank(theRank, nrow(orderedList))
  return(orderedList[evaledRank,])
}

evalRank <- function(wantedRank, listSize){
  if (wantedRank == 'best'){
    return (1)
  }
  else if(wantedRank == 'worst'){
    return(listSize)
  }
  else if(wantedRank > listSize){
    stop('You indicated a rank higher than the number of hospitals.
         Retry with a different rank or use worst for the worst
         ranked hospital')
  }
  else
    return(wantedRank)
}
#logic for finding the hospitals in a state
findStateHospitals <- function(x, cause, wantedState){
  if (x[["State"]] == wantedState){
    stateList <<- rbind(stateList, x)
  }
}

#forms the name of the column condition to use for getting the 
#condition rate column from the original file
causeString <- function(cause){
  columnName <- 'Hospital.30.Day.Death..Mortality..Rates.from.'
  if (cause == 'heart attack'){
    columnName <- paste0(columnName, 'Heart.Attack')
  } else if (cause == 'heart failure'){
    columnName <- paste0(columnName, 'Heart.Failure')
  } else if (cause == 'pneumonia') {
    columnName <- paste0(columnName, 'Pneumonia')
  }
  return(columnName)
}
#checks if the provided state is a valid state
checkState <- function(stateName, statesList){
  if(!(stateName %in% statesList))
  {
    stop('Invalid State')
  }
  
}
#checks if the provided condition is a valiud condition
checkRate <- function (theRate){
  if (!(theRate == 'heart attack') && !(theRate == 'heart failure') 
      && !(theRate == 'pneumonia'))  {
    stop('Invalid outcome')
  }
}
