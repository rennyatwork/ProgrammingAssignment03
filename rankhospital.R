
#setwd("C:/Users/renato/Documents/MOOC/2014/course02- R programming/Assignments/Assing-03-HospQuality/")
# x <- getFile()
#  y <- getRankedDataSet(x, "TX", "heart failure")
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

## Contains the words "best" and "worst"
getValidNum <-function()
{
  if(!exists("validNum"))
  {
    validNum <<- c("best", "worst")
  }
  return (validNum);
}


getFile <-function(pFileName="outcome-of-care-measures.csv")
{
  pathToFile=paste(getwd(), "/", "rprog_data_ProgAssignment3-data/", pFileName, sep="")
  if (!exists("dsFromFile"))
  {
    dsFromFile <<- read.csv(pathToFile, colClasses="character")
  }
  return (dsFromFile)
}

#checks whether a state is valid or not
isValidState <-function(pState)
{
  return (any(getValidStates()==pState))
}


#checks whether an outcome is valid or not
isValidOutcome <-function(pOutcome)
{
  #print("isValidOutcome")
  #print("pOutcome: ") 
  #print(pOutcome)
  #print(as.character(getValidOutcomes()[pOutcome]))
  return (as.character(getValidOutcomes()[pOutcome]) != "NULL")
}



#checkes whether a num argument is valid or not 
isValidNum <-function(pNum)
{
  #if it's a string different from worse and best, return false
  if(!is.numeric(pNum) && !any(getValidNum()==pNum))
  {
    return (FALSE)
  }
  
  return((any(getValidNum()==pNum) || (pNum==floor(pNum)) ))
}

## returns a dataset with  values per state
### texting example: minDs <- getRankedDataSet(x, "TX", "heart attack")
getRankedDataSet <- function(pDataSet, pState, pOutcome)
{  
  colOutcome <- as.character(getValidOutcomes()[pOutcome])
  #print("colOutcome")
  #print(colOutcome)
  
  ## transforming the outcome column in numeric
  pDataSet[,colOutcome] <- as.numeric(pDataSet[,colOutcome])
  
  ## subset by state
  tempDs <- subset(pDataSet, pDataSet$State==pState &!is.na(pDataSet[,colOutcome]), c("Hospital.Name", "City", "State", colOutcome))
    
  print("pState")
  print(pState)
  #print(tempDs)
  #print("length hospital name")
  #print(length(tempDs["Hospital.Name"]))
  #return(tempDs)
  
  ##then, the minimum from the state ordered by outcome and hospital name
  #minDs <- subset(tempDs, tempDs[colOutcome]== min(tempDs[colOutcome]))[order(colOutcome, "Hospital.Name"),]
  minDs <- subset(tempDs,1==1)[order(tempDs[,colOutcome], tempDs[,"Hospital.Name"]),]
  return (minDs)
}


getHospitalName <- function(pDataset, pIndex)
{
  if(pIndex == "best")
  {
    return(as.character(head(pDataset$"Hospital.Name",1)[]))
  }
  else if (pIndex=="worst")
  {
    return(as.character(tail(pDataset$"Hospital.Name",1)[]))
  } 
  
  print("pIndex")
  print(pIndex)
  return (as.character(pDataset[pIndex, "Hospital.Name"]))
}  


rankhospital <- function(pState, pOutcome, pNum = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  if(!isValidState(pState))
  {
    stop("invalid state")
  }
  
  if(!isValidOutcome(pOutcome))
  {
    stop("invalid outcome")
  }
  if(!isValidNum(pNum))
  {
    stop("invalid num")
  }
  
  dsRankedDataSet <- getRankedDataSet(getFile(),pState, pOutcome)
  
  print("head")
  print(head(dsRankedDataSet))
  print("tail")
  print(tail(dsRankedDataSet))
  
  return(getHospitalName(dsRankedDataSet, pNum))
  
}