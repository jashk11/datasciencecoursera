complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  output <- data.frame()
  for(j in id)
  {
    if (j<10)
    {
      file <- paste("00",j,".csv",sep="")
    }
    else if(j<100)
    {
      file<-paste("0",j,".csv",sep="")
    }
    else
    {
      file<-paste(j,".csv",sep="")
    }
    heisenberg <- read.csv(file=file,head=TRUE,sep=",")
    valSul <- !is.na(heisenberg[,"sulfate",])  # Gets  where are the values in the sulfate col
    valNut <- !is.na(heisenberg[,"nitrate",])  # Gets where are the values in the nitrade col
    nobs <- sum(valSul & valNut)
    newRow <- data.frame(j,nobs)
    output <- rbind(output,newRow)
  }
  names(output) <- c("id","nobs")
  print(output)
}