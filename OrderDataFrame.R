##  A function to sort a dataframe first by State then to order the data in lowest occurrence to highest with the names of the 
##  hospitals alphabetic relationship used to determine ties.
rankhospital=function(state, outcome, num="best") {
  FileName="outcome-of-care-measures.csv"
  data=read.csv(FileName, colClasses="character", na.strings="Not Available")
  CorrectInput=c("heart attack", "heart failure", "pneumonia")
  state=toupper(state)
  outcome=tolower(outcome)
  if (!state %in% data$State) {
    stop("Invalid State")
  } 
  if (!outcome %in% CorrectInput) {
    stop("Invalid Cause")
  }
  ## Was planning on using this <match> functionality to get the <ColumnNameSub> to enter into the <order> function below but it didn't work (<order>, not <match>)
#   ColumnName=c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
#   ColumnNameSub=ColumnName[match(outcome, CorrectInput)]
  # View(ColumnNameSub)

  StateSub=data[data$State == state, ]
  # View(StateSub)
  
##  <order> does not seem to work correctly.  I've been all over StackOverflow and I've tried it without the If/Else loops, I've tried it with double brackets,
##  I've tried it with decreasing=TRUE and FALSE.  Nothing gets the <StateSub> table to re-order the way I want.
  if (outcome == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") {
    RankedData=StateSub[order(StateSub[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"], StateSub[ "Hospital.Name"], decreasing=TRUE, na.last=NA), ] 
  } else if (outcome == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"){
    RankedData=StateSub[order(StateSub[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"], StateSub[ "Hospital.Name"], decreasing=TRUE, na.last=NA), ]
  } else {
    RankedData=StateSub[order(StateSub[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"], StateSub[ "Hospital.Name"], decreasing=TRUE, na.last=NA), ]
  }
  View(RankedData)
   
  if(num == "best"){
    num=1
  }
  if (num == "worst"){
    num=nrow(RankedData)
  }
  if (is.numeric(num) & num>nrow(RankedData)){
    return(NULL)
  }
  
  Ranked=RankedData[num,"Hospital.Name"]
  return(Ranked)
}

