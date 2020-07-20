## This function will take three arguments:
##
## 1. 2-character abbreviated state name. 
## 2. an outcome
## 3. ranking of the hospital in that state for that outcome (num). 
##
## The function reads the outcome-of-care-measures.csv file and returns
## a character vector with the name of the hospital that has the ranking
## specificied by the "num" argument.
##
## "num" can take values of "best", "works" or an integer indicating the 
## ranking.
##

rankhospital <- function(state, outcome, num = 'best') {
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
  
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## convert num argument to valid rank
  
  if(num == "best") {
    num <- 1 
  }
  
  if (num == "worst") {
    num <- nrow(hRates) 
  }
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
  
  ## Get names of hospital 
  
  hRates[num,1]
  
}