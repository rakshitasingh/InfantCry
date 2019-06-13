#install.packages("stringr")
library(stringr)
wavPath ="C:\\Users\\sarthhak sharma\\Desktop\\donateacry-corpus-master\\donateacry-corpus-master\\donateacry-android-upload-bucket WAV"
wavFiles = list.files(wavPath, pattern=glob2rx('*.wav'), full.names=TRUE)

vec <- NULL
for(i in 1:712)
{
  
  if(str_detect( wavFiles[i],"-hu.wav")==TRUE)
    
  {
    vec <- c(vec,"hungry")
    
  } else  if(str_detect( wavFiles[i],"-bu.wav")==TRUE){
    
    vec <- c(vec,"burping")
    
  } else  if(str_detect( wavFiles[i],"-bp.wav")==TRUE) {
    
    vec <- c(vec,"belly_pain")
  } else  if(str_detect( wavFiles[i],"-dc.wav")==TRUE) {
    
    vec <- c(vec,"discomfort")
  } else  if(str_detect( wavFiles[i],"-ch.wav")==TRUE){
    
    vec <- c(vec,"cold/hot")
  } else  if(str_detect( wavFiles[i],"-lo.wav")==TRUE){
    
    vec <- c(vec,"lonely")
  } else  if(str_detect( wavFiles[i],"-sc.wav")==TRUE) {
    
    vec <- c(vec,"scared")
  } else  if(str_detect( wavFiles[i],"-dk.wav")==TRUE) {
    
    vec <- c(vec,"cant_say")
  } else  if(str_detect( wavFiles[i],"-ti.wav")==TRUE) {
    
    vec <- c(vec,"tired")
  }
  
} 
labe <- data.frame(vec)
write.table(labe, file = paste("C:\\Users\\sarthhak sharma\\Desktop\\EXCEL SHEET major\\labels.csv"),
            row.names = FALSE,  na="",col.names=TRUE, sep=",")





