## This function will take two arguments:
##
## 1. An outcome
## 2. A hospital ranking (num)
##
## The function reads in the outcome-of-care-measures.csv file and returns a
## two column data frame containing the hospital in each state that has the
## ranking specified in num. 
##
## This will return a data frame containing the names of the hospitals that 
## are the best in their respective staets for 30-day heart attack deaths 
## rates. 
##
## The function should return a value for every state. The 
## first column of the data frame is named "hospital", which is the hospital
## name and the second column is named state.
##

rankall <- function(outcome, num = 'best') {
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
  
  ## Check outcome is valid
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  hRank <- data.frame()
  
  for(state in sort(unique(rates[,"state"]))){
    
    ## Get only the hospitals in this state
    hRates <- rates[(rates[, "state"] == state), ]
    
    hRates[, outcome] <- as.numeric(hRates[, outcome])
    
    hRates <- hRates[!is.na(hRates[, outcome]), ]
    
    ## convert num argument to valid rank
    
    if(num == "best") {
      rnum <- 1 
    } else if (num == "worst") {
      rnum <- nrow(hRates) 
    }
    else {rnum = num}
    
    
    ## Order by outcome rate & hospital name
    hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
    
    hName <- hRates[rnum,1]
    
    hRank <- rbind(hRank,
                   data.frame(hospital = hName,
                              state = state))
  }
  
  hRank
  
}