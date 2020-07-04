corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the number of
  ## completely observed observations (on all variables) required to compute
  ## the correlation between nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  files_full <- list.files(directory, full.names = TRUE)
  dat <- numeric()
  
  for (i in 1:length(files_full)) {
    newRead <- read.csv(files_full[i])
    csum <- sum((!is.na(newRead$sulfate)) & (!is.na(newRead$nitrate)))
    if (csum > threshold) {
      temp <- newRead[complete.cases(newRead), ]
      dat <- c(dat, cor(temp$sulfate, temp$nitrate))
    }
  }
  
  dat
}