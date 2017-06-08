#************************************************************************************************
# Ahthor: wavelet
# Date : 2017/5/13
# Update info :
#
#************************************************************************************************
library(sp)
library(maptools)
library(raster)
#************************************************************************************************
# define var name
#************************************************************************************************
shapeFileQTP <- "F:/worktemp/Permafrost(FrostModel)/Data/Envi/Bounder/Tibet_Plateau_Boundar.shp"
txtFilePath  <- "F:/worktemp/Permafrost(FrostModel)/Data/0 cm GST/InputGST"
outvar       <- "TableData"
#************************************************************************************************
# read shp
#************************************************************************************************
shapefile <- readShapeSpatial(shapeFileQTP)
#************************************************************************************************
# getFilelist
#************************************************************************************************
fileList <- list.files(txtFilePath,  full.names = TRUE)
outData <- data.frame()
for(fileName in fileList)
{
  # 1 SETP  Get orginal data GST
  data <- read.table(fileName, header = FALSE, sep = "", dec = ".")
  data <- data[,c(1:8)]
  data <- data.frame(ID=c(1:nrow(data)),data)
  names(data)<- c("ID","SID","Lat","Lon","Height","Year","Month","Day","GST")
  data$Lat    <-   floor(data$Lat/100.0) + (data$Lat/100.0 - floor(data$Lat/100.0))*100/60.0
  data$Lon    <-   floor(data$Lon/100.0) + (data$Lon/100.0 - floor(data$Lon/100.0))*100/60.0
  data$Height <-   data$Height * 0.1
  data$GST    <-   data$GST * 0.1
  
  dataSP      <-   data
  coordinates(dataSP) <- c("Lon","Lat")
  groupID     <-    over(dataSP,shapefile)
  groupID     <-    na.omit(groupID)
  groupID     <-    data.frame(rownames(groupID))
  names(groupID)<-c('QTPID')
  groupID$QTPID <- as.integer(as.character(groupID$QTPID))
  subData <- merge(data,groupID,by.x="ID",by.y="QTPID",all.y=TRUE)
  subData$ID <- NULL
  outData <- rbind(outData,subData)
}

#************************************************************************************************
# write 
#************************************************************************************************
## Write res
outPutName <- paste(outvar, ".csv",sep='')
write.csv(outData, file = outPutName)








