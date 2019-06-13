le <- read.csv(paste("C:\\Users\\rakshita singh\\Desktop\\EXCEL SHEET major\\labels.csv"))
dfr1 <- data.frame()
dfr2 <- data.frame()

for(i in 1:560)
{

  
  dfr1 <- rbind(dfr1,le$vec[i])
}
write.table(dfr1, file = paste("C:\\Users\\rakshita singh\\Desktop\\EXCEL SHEET major\\training dataset\\training labels.csv"),
            row.names = FALSE,  na="",col.names=TRUE, sep=",")

for(i in 560:712)
{ dfr2 <- rbind(dfr2,le$vec[i])
}

write.table(dfr2, file = paste("C:\\Users\\rakshita singh\\Desktop\\EXCEL SHEET major\\testing dataset\\testing labels.csv"),
            row.names = FALSE,  na="",col.names=TRUE, sep=",")
