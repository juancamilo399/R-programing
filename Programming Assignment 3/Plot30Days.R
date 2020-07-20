outcome <- read.csv("outcome-of-care-measures.csv", TRUE, ",", , , colClasses = "character")
head(outcome)

outcome[,11] <- as.numeric(outcome[,11]) #We introduced the data as character
hist(outcome[,11])

