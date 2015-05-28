##  Function to look through at CSV table for the highest rated hosipital based on 3 categories
best=function(state, outcome) {
  state=toupper(state)
  outcome=tolower(outcome)
  FileName="outcome-of-care-measures.csv"
  data=read.csv(FileName, colClasses="character")
  
  CorrectInput=c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("Invalid State")
  } else if(!outcome %in% CorrectInput) {
    stop("Invalid Cause")
  } else {
    if(outcome == "heart attack") {
      data[, 11]=as.numeric(data[, 11])
      HospitalName=finder(data, 11, state)
    } else if(outcome == "heart failure") {
      data[, 17]=as.numeric(data[, 17])
      HospitalName=finder(data, 17, state)
    } else {
      data[, 23]=as.numeric(data[, 23])
      HospitalName=finder(data, 23, state)
    }
    result=HospitalName
    return(result)
  }

}
##  Function to use inside <best> instead of running the sorter 3 times in the If/Else loops
finder=function(data, col_num, state) {
  StateSub=data[data[, 7]==state, ]
  StateValue=StateSub[, col_num]
  min=min(StateValue, na.rm=T)
  min_index=which(StateValue == min)
  View(min_index)
  HospitalName=StateSub[min_index, 2]
  return(HospitalName)
}
