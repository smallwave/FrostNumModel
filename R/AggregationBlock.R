#************************************************************************************************
# Ahthor: wavelet
# Propose : AggregationBlock  9  block 
# Date : 2017/6/17
# Update info :
#        2017/6/17/    
#************************************************************************************************

inFilePath             <-  "F:/worktemp/Permafrost(FrostModel)/Data/GST(SUB)/"
outFilePath            <-  "F:/worktemp/Permafrost(FrostModel)/Data/GST(SUB)/SUMMARY/"

#************************************************************************************************
# aggregation
#************************************************************************************************

for(i in 1:365)
{
  Day           <- str_pad(i, 3, pad = "0")
  searchStr     <- paste("*-",Day, ".csv",sep='')
  fileList      <- list.files(inFilePath,  pattern = searchStr , full.names = TRUE, recursive  = TRUE)
  if(length(fileList == 9)){
    InData <- NULL
    for(inputCvs in fileList){
      Data        <- read.csv(inputCvs, head=TRUE,sep=",")
      InData      <- rbind(InData,Data)
    }
    outName     <- paste("MY",Day,sep='-')
    fname<-paste(outFilePath,outName,".csv",sep='')
    write.csv(InData, file = fname,row.names = FALSE)
  }
}
  
#************************************************************************************************
# OUP RESULT 
#************************************************************************************************
inputCvs  <- "F:/worktemp/Permafrost(FrostModel)/MY-046.csv"
fname     <- "F:/worktemp/Permafrost(FrostModel)/MY046BU.tiff"
Data      <- read.csv(inputCvs, head=TRUE,sep=",")
names(Data) <- c("Lon","Lat","GST")
coordinates(Data) <- ~Lon + Lat
gridded(Data) <- TRUE
writeGDAL(Data,fname,drivername="GTiff", type="Float32", options=NULL)


