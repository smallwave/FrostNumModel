library(lubridate)
library (plyr)
library (sp)
library (raster)
library(sqldf)
#************************************************************************************************
# define var name
#************************************************************************************************
inputCvs <- "PointData.csv"
tifFilePath  <- "F:/worktemp/Permafrost(FrostModel)/Data/NDVI/"
outvar       <- "PointData"
#************************************************************************************************
# read data
#************************************************************************************************
InData <- read.csv(inputCvs, head=TRUE,sep=",")
InData <- data.frame(sapply(InData, `[`))
InData <- within(InData, 
                 GST[InData$GST > 1000.0] <- NA)
InData$NDVI <- NA

#************************************************************************************************
# getFilelist
#************************************************************************************************
fileList <- list.files(tifFilePath, pattern  = "\\.tif$", full.names = TRUE, recursive  = TRUE)

#************************************************************************************************
# process
#************************************************************************************************
for(fileName in fileList)
{
  #************************************************************************************************
  # 1  select data
  #************************************************************************************************
  basefilename <- gsub(pattern = "\\.tif$", "",basename(fileName))
  numTmp <- as.numeric(basefilename)
  nmonth <-  ceiling(numTmp/2.0)
  if(numTmp%%2){
    subData <- subset(InData, 
                      InData$Month == nmonth & InData$Day <= 15,
                      select = c(ID, Lon, Lat))
  }else{
    subData <- subset(InData, 
                      InData$Month == nmonth & InData$Day > 15,
                      select = c(ID, Lon, Lat))
  }
  if(nrow(subData) > 0)
  {  
    #************************************************************************************************
    # 2  extract VALUE from raster 
    #************************************************************************************************
    coordinates(subData) <- c("Lon","Lat")
    #read Raster
    rasterData    <- stack(fileName)
    ndviExtract   <- extract(rasterData,subData,sp =TRUE)
    ndviExtractDf <- ndviExtract@data
    names(ndviExtractDf) <- c("ID","NDVI")
    if(numTmp%%2){
      InData <- within(InData, 
                     NDVI[InData$Month == nmonth & InData$Day <= 15] <- ndviExtractDf$NDVI/1000.0)
    }else{     
      InData <- within(InData, 
                                NDVI[InData$Month == nmonth & InData$Day > 15] <- ndviExtractDf$NDVI/1000.0)
      
    }
  }
}

#************************************************************************************************
# write 
#************************************************************************************************
## Write res
outPutName <- paste(outvar, ".csv",sep='')
write.csv(InData, file = outPutName)

