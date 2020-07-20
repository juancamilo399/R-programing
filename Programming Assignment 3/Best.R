## This function will accept two arguments:
##
## 1. 2-character abbreviated name of the a state
## 2. Outcome name
##
## This will read in the outcome-of-care-measures.csv file and return a character
## vector with the name of the hospital that has the lowest 30-day
## mortality for the specified outcome in that state. Hospitals that do not have 
## data on a particular outcome should be excluded.
##
## The function sould validate the input - If any invalid state or outcome is 
## passed in, the function should throw an error via the "stop" function with 
## the exact message "invalid state" or "invalid outcome". 
best <- function(state, outcome) {
  ## Read outcome data
  
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       header = TRUE)
  
  ## Get data we're interested in
  
  rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack
                               outcomes[, 17],  # heart failure
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  
  if(!state %in% rates[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  ## Get only the hospitals in chosen state
  hRates <- rates[(rates[, "state"] == state), ]
  
  ## Convert outcome rate to numberic
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome]), ]
  
  ## Get names of hospitals with the lowest rate
  
  hNames <- hRates[hRates[, outcome] == min(hRates[,outcome]),1]
  
  ## Sort by hospital name if tie
  
  sort(hNames)[1]
}