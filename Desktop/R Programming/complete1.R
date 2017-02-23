complete<-function(directory,id=1:332){
  
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
  comp<-file[!is.na(file[,"sulphate"])&!is.na(file[,"nitrate"])]
  nobs <- sum(comp)
  outp<- rbind(outp,data.frame(i,nobs))
  }
  
  names(outp) <- c("id","nobs")
  outp
}