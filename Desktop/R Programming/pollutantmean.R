pollutantmean<- function(directory,pollutant,id = 1:332) {
 sum<-c() 
  for (j in id)
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
  data_raw <- read.csv(file)
  b <- data_raw[,pollutant]
  re <- b[!is.na(b)]
  sum <- c(sum,re)
  }
  mean(sum)
}
