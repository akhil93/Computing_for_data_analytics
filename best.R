best <- function(state, outcome) {  
  options("warn" = -1)
  
  ## Read outcome data  
  d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hosp <- read.csv("hospital-data.csv", colClasses = "character")  
  ocn = c("heart attack","heart failure","pneumonia")
  
  ## Check that Outcome and State are valid
  if (sum(ocn == outcome) == 0) stop("invalid outcome")
  st = unique(hosp$State)
  if (sum(st == state) == 0) stop("invalid state")  
  d <- subset(d, d$State == state, select = c(2,11,17,23))
  d[,2] <- as.numeric(d[,2])
  d[,3] <- as.numeric(d[,3])
  d[,4] <- as.numeric(d[,4])
  
  ##Return hospital name in that state with lowest 30-day death rate  
  pdx <- match(outcome, ocn) + 1
  idx <- which.min(d[,pdx])
  
  options("warn" = 0)
  
  d[idx,1]
  
}