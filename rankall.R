
#setwd("C:/Users/renato/Documents/MOOC/2014/course02- R programming/Assignments/Assing-03-HospQuality/")
# x <- getFile()
#  y <- getRankedDataSet(x, "TX", "heart failure")

## rankedDsHeartFailure <- getRankedDataSet(getFile(), "heart failure")
## rankedDsHeartAttack <- getRankedDataSet(getFile(), "heart attack")
## rankedDsPneumonia <- getRankedDataSet(getFile(), "pneumonia")

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
    validStates <<- unique(getFile()$State)
  }
  return (sort(validStates))
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

## returns a dataset with per state
### testing example: dsRank <- getRankedDataSet(x,  "heart attack")
getRankedDataSet <- function(pDataSet,  pOutcome)
{  
  
  ## gets the name of the outcome column
  colOutcome <- as.character(getValidOutcomes()[pOutcome])
  
  
  ## transforming the outcome column in numeric
  pDataSet[,colOutcome] <- as.numeric(pDataSet[,colOutcome])
  
  ## subset by state eliminating NA
  tempDs <- subset(pDataSet, !is.na(pDataSet[,colOutcome]), c("Hospital.Name", "City", "State", colOutcome))
  
   
  
  ##then, the minimum from the state ordered by outcome and hospital name
  #minDs <- subset(tempDs, tempDs[colOutcome]== min(tempDs[colOutcome]))[order(colOutcome, "Hospital.Name"),]
  dsRank <- subset(tempDs,1==1)[order(tempDs[,"State"], tempDs[,colOutcome], tempDs[,"Hospital.Name"]),]
  return (dsRank)
}


getDfHospitalName <- function(pDataset, pIndex)
{
  dfHospital <- data.frame()
  
  for(st in getValidStates())
  {
    #print("st in getValidaStates()")
    print(st)
    
    if(pIndex == "best")
    {
      #print("best")
      #return(as.character(head(pDataset$"Hospital.Name",1)[]))
      #print(head(subset(pDataset, pDataset$State == st, c("Hospital.Name", "State")),1))
      dfH<- head(subset(pDataset, pDataset$State == st, c("Hospital.Name", "State")),1)
      if(nrow(dfH)>0)
      {
        
        dfH$State <- st
      }
      #print(rbind(dfHospital,c(dfH$"Hospital.Name", st)))
      
      #print("dfH")
      #print(dfH)
      
      dfHospital <- rbind(dfHospital,dfH)
      print("nrow")
      print(nrow(dfH))
      print(nrow(dfHospital))
    }
    else if (pIndex=="worst")
    {      
      #print("worst")
      dfH<-tail(subset(pDataset, pDataset$State == st, c("Hospital.Name", "State")),1)
      if(nrow(dfH)>0)
      {
        dfH$State <- st
      }
      dfHospital <- rbind(dfHospital,dfH)
      #print("dfH")
      #print(dfH)
      print("nrow")
      print(nrow(dfHospital))
    } 
    else
    {
      #print("pIndex")
      dfH <- subset(pDataset, pDataset$State == st, c("Hospital.Name", "State"))[pIndex,]
      if(nrow(dfH)>0)
      {
        dfH$State <- st
      }
      dfHospital <- rbind(dfHospital,dfH)
      #print("dfH")
      #print(dfH)
      print("nrow")
      print(nrow(dfHospital))
    }
  }
  
  
  
  #print("pIndex")
  #print("dfHospital")
  names(dfHospital)[1] <-"hospital"
  names(dfHospital)[2] <-"state"
  return (dfHospital)
}  


rankall <- function(pOutcome, pNum = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
    
  
  if(!isValidOutcome(pOutcome))
  {
    stop("invalid outcome")
  }
  if(!isValidNum(pNum))
  {
    stop("invalid num")
  }
  
  dsRankedDataSet <- getRankedDataSet(getFile(), pOutcome)
  
  #print("head")
  #print(head(dsRankedDataSet))
  #print("tail")
  #print(tail(dsRankedDataSet))
  
  return(getDfHospitalName(dsRankedDataSet, pNum))
  
}