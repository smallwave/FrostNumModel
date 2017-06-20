#************************************************************************************************
# Ahthor: wavelet
# Propose : Calcalation DDT and DDF
# Date : 2017/6/20
# Update info :
#        2017/6/20/     
#************************************************************************************************
gstinFilePath      <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(Kri)/"
ddtddfFilePath     <- "F:/worktemp/Permafrost(FrostModel)/Data/DDTDDF/ddtddf.csv"
#************************************************************************************************
#  process  every file
#************************************************************************************************
searchStr     <- paste("*.csv",sep='')
fileList      <- list.files(gstinFilePath,  pattern = searchStr , full.names = TRUE, recursive  = TRUE)
DDTALL        <- NULL
DDFALL        <- NULL
for(fileName in fileList){
  print(fileName)
  InData        <- read.csv(fileName, head=TRUE,sep=",")
  # DDT
  if(is.null(DDTALL)){
     DDTALL <-  rep(c(0), nrow(InData))
  }
  IndexDDT      <- InData$GST > 0
  DDT           <- rep(c(0), nrow(InData))
  DDT[IndexDDT] <- InData$GST[IndexDDT]
  DDTALL        <- DDTALL + DDT
  # DDF
  if(is.null(DDFALL)){
    DDFALL <-  rep(c(0), nrow(InData))
  }
  IndexDDF      <- InData$GST < 0
  DDF           <- rep(c(0), nrow(InData))
  DDF[IndexDDF] <- InData$GST[IndexDDF]
  DDFALL        <- DDFALL + DDF
}
DDFALL  <-  abs(DDFALL)
map.Polygon <- data.frame(InData[,c("Lon","Lat")],DDTALL,DDFALL)
write.csv(map.Polygon, file = ddtddfFilePath,row.names = FALSE)


