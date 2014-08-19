
#setwd("C:/Users/renato/Documents/MOOC/2014/course02- R programming/Assignments/Assing-03-HospQuality/")

## map of valid outcomes
getValidOutcomes<-function()
{
  if(!exists("validOutcomes"))
  {
    validOutcomes <<- list("heart attack"= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" ,
                           "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" ,
                           "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  }
  
  return(validOutcomes)
}

## List of valid states
getValidStates <-function()
{  
  if(!exists("validStates"))
  {
  validStates <<- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                   "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                   "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                   "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                   "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY");
  }
  return (validStates)
}

getFile <-function(pFileName="outcome-of-care-measures.csv")
{
  pathToFile=paste(getwd(), "/", "rprog_data_ProgAssignment3-data/", pFileName, sep="")
  return (read.csv(pathToFile, colClasses="character"))
}

#checks whether a state is valid or not
isValidState <-function(pState)
{
  return (any(getValidStates()==pState))
}

#checks whether an outcome is valid or not
isValidOutcome <-function(pOutcome)
{
  print("isValidOutcome")
  print("pOutcome: ") 
  print(pOutcome)
  print(as.character(getValidOutcomes()[pOutcome]))
  return (as.character(getValidOutcomes()[pOutcome]) != "NULL")
}

## returns a dataset with min values per state
### texting example: minDs <- getMinDataSet(x, "TX", "heart attack")
getMinDataSet <- function(pDataSet, pState, pOutcome)
{  
  colOutcome <- as.character(getValidOutcomes()[pOutcome])
  print("colOutcome")
  print(colOutcome)
  
  ## transforming the outcome column in numeric
  pDataSet[,colOutcome] <- as.numeric(pDataSet[,colOutcome])
  
  ## subset by state
  tempDs <- subset(pDataSet, pDataSet$State==pState &!is.na(pDataSet[,colOutcome]), c("Hospital.Name", "City", "State", colOutcome))
    
  #print(tempDs)
  
  #return(tempDs)
  
  ##then, the minimum from the state ordered by outcome and hospital name
  minDs <- subset(tempDs, tempDs[colOutcome]== min(tempDs[colOutcome]))[order(colOutcome, "Hospital.Name"),]
  #minDs <- subset(tempDs, tempDs[pOutcome]== min(tempDs[pOutcome]))[order(paste("-",pOutcome), "Hospital.Name"),]
  
}

best <- function(pState, pOutcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  if(!isValidState(pState))
  {
    stop("invalid state")
  }
  
  if(!isValidOutcome(pOutcome))
  {
    stop("invalid outcome")
  }
  
  colOutcome <- as.character(getValidOutcomes()[pOutcome])
  print("best")
  print(colOutcome)
  #return (subset( head( getMinDataSet(getFile(), pState, pOutcome),1), 1==1, c("Hospital.Name", colOutcome)))
  return (subset( head( getMinDataSet(getFile(), pState, pOutcome),1), 1==1, c("Hospital.Name")))
}