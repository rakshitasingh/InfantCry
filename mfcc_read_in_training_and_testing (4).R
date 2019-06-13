


#times = no of col now 12 12
zer <- rep(0,times= 12)
dfr <- data.frame()
dfr2 <- data.frame()
for(i in 1:433)
{
 df2 <- read.csv(paste("C:\Users\RakshitaSingh\Desktop\EXCEL SHEET major\mfcc_train\mfcc_train" , as.numeric(i) , ".csv",sep=''))
 
num_row <- nrow(df2)

if(num_row < 800)
{ for(j in (num_row+1):(800))
  df2 <-rbind(df2,zer)

}
 mat <- data.matrix(df2) 
 
 
 lis <- as.list(mat)
 # dfr <- data.frame(matrix(unlist(lis), nrow=1, byrow=T),stringsAsFactors=FALSE)
 
 # 
 temp <- as.data.frame(do.call("cbind", lis), stringsAsFactors = FALSE)
 dfr <- rbind(dfr,temp)
}
write.table(dfr, file = paste("C:\Users\Rakshita Singh\Desktop\EXCEL SHEET major\training dataset\training_final.csv"),
            row.names = FALSE,  na="",col.names=TRUE, sep=",")

for(i in 1:84)
{
  df3 <- read.csv(paste("C:\Users\Rakshita Singh\Desktop\EXCEL SHEET major\mfcc_test\mfcc_test" , as.numeric(i) , ".csv",sep=''))
  
  num_row <- nrow(df3)
  
  if(num_row < 800)
  { for(j in (num_row+1):(800))
    df3 <-rbind(df3,zer)
  
  }
  mat <- data.matrix(df3) 
  
  
  lis <- as.list(mat)
  # dfr <- data.frame(matrix(unlist(lis), nrow=1, byrow=T),stringsAsFactors=FALSE)
  
  # 
  temp <- as.data.frame(do.call("cbind", lis), stringsAsFactors = FALSE)
  dfr2 <- rbind(dfr2,temp)
}

write.table(dfr2, file = paste("C:\Users\Rakshita Singh\Desktop\EXCEL SHEET major\testing dataset\testing_final.csv"),
            row.names = FALSE,  na="",col.names=TRUE, sep=",")
