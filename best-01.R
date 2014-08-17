
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

## returns a dataset with min values per state
getMinDataSet <- function(pDataSet, pState, pOutcome)
{
  ## transforming the outcome column in numeric
  pDataSet[,pOutcome] <- as.numeric(pDataSet[,pOutcome])
  
  ## subset by state
  tempDs <- subset(pDataSet, pDataSet$State==pState &!is.na(pDataSet[,pOutcome]), c("Hospital.Name", "City", "State", pOutcome))
    
  #print(tempDs)
  
  #return(tempDs)
  
  ##then, the minimum from the state
  minDs <- subset(tempDs, tempDs[pOutcome]== min(tempDs[pOutcome]))[order(paste("-",pOutcome), "Hospital.Name"),]
  
}

best <- function(pState, pOutcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return (subset( head( getMinDataSet(getFile(), pState, pOutcome),1), 1==1, c("Hospital.Name", pOutcome)))
}