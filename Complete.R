complete <- function(directory,id = 1:332){
  nob <- numeric(0)
  for ( cid in id){
    Did <- getmonitor(cid,directory)
    nob <- c(nob,nrow(na.omit(Did)))
  }
  data.frame(id = id, nobs = nob )
}
getmonitor <- function(id, directory, summarize = FALS){
  filename <- paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", 
                    sep = "")
  data <- read.csv(filename)
  if(summarize == TRUE) print(summary(data))
  return(data)
}