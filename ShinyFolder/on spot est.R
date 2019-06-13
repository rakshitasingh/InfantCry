
dat <- data.frame()
xx <- data.frame()

colnames(xx) <- c(
  "mean 1	","sd 1	","mean 2	","sd 2	","mean 3	","sd 3	","mean 4	","sd 4	","mean 5	","sd 5	","mean 6	","sd 6	","mean 7	","sd 7	","mean 8	","sd 8	","mean 9	","sd 9	","mean 10	","sd 10	","mean 11	","sd 11	","mean 12	","sd 12"
) 
file1 <- read.csv("my_recordings .csv")

i=1
#counter = 1
for( i in 1: 12)
{
 j = i
 k11 = i + 12
  me <- mean(na.omit(file1$value[j:k11]))
  
  sdd <- sd(na.omit(file1$value[j:k11]))
  
  print(paste("mean is :", me))
  print(paste("sd is : ",sdd))
  
  # print(paste("counter is :",counter))
  # counter = counter + 1
 
  xx <- rbind(xx,me)
  xx <- rbind(xx,sdd)
 
  
  
}

xx = t(xx)
write.csv(xx, file = "testing_recorded_audio.csv",row.names=FALSE)

xc <- read.csv("testing_recorded_audio.csv")

colnames(xc) <- c("mean 1","sd 1","mean 2","sd 2","mean 3","sd 3","mean 4","sd 4","mean 5","sd 5","mean 6","sd 6","mean 7","sd 7","mean 8","sd 8","mean 9","sd 9","mean 10","sd 10","mean 11","sd 11","mean 12","sd 12"
)
write.csv(xc, file = "testing_recorded_audio.csv",row.names=FALSE)
