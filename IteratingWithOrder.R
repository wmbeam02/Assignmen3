##  A function to sort a dataframe first by State then to order the data in lowest occurrence to highest with the names of the 
##  hospitals alphabetic relationship used to determine ties.
rankall=function(outcome, num="best") {
  FileName="outcome-of-care-measures.csv"
  Data=read.csv(FileName, colClasses="character", na.strings="Not Available")
  CorrectInput=c("heart attack","heart failure","pneumonia")
  if (!outcome %in% CorrectInput) stop("invalid outcome")
  
# States = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", 
#                 "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", 
#                 "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
# Just testing different quotes. Found a website with single quote values below, thought it was worth a try.

  States=c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 
             'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY')

  ## convert outcome name into column name
  ColName=c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  ColumnName=ColName[match(outcome,CorrectInput)]
  
  ## For each state, find the hospital of the given rank - initializing <Hospital> to a character class
  Hospital=character(0)
  
  for (i in seq_along(States)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    DataStateSub=Data[Data$State == States[i],]
    
    # order data by outcome
    OrderDataState=DataStateSub[order(as.numeric(DataStateSub[[ColumnName]]),DataStateSub[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    #handle num input
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(OrderDataState)
    
#     if (num == "best") num=1
#     if (num == 'worst') num=nrow(OrderDataState)
#     if (is.numeric(num) & num=nrow(OrderDataState)) {
#       return(NA)}

    Hospital[i]=OrderDataState[this.num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(Hospital=Hospital,States=States, row.names=States)
}
## test
## rankall("heart failure", 10)
