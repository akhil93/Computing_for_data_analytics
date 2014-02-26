getmonitor <- function(id, directory, summarize = TRUE){
  filename <- paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", 
                   sep = "")
  data <- read.csv(filename)
  if(summarize == TRUE) print(summary(data))
  return(data)
}