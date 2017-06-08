library(lubridate)
library (plyr)
library (sp)
library (raster)
library(sqldf)
#************************************************************************************************
# define var name
#************************************************************************************************
inputCvs <- "TableData.csv"
tifFilePath  <- "F:/worktemp/Permafrost(FrostModel)/Data/snow depth/Tiff/"
outvar       <- "TableDataSnow"
#************************************************************************************************
# read data
#************************************************************************************************
InData <- read.csv(inputCvs, head=TRUE,sep=",")
InData <- data.frame(sapply(InData, `[`))
InData <- within(InData, 
                 GST[InData$GST > 1000.0] <- NA)
InData$SnowDepth <- NA


#************************************************************************************************
# from ndoy2ymd
#************************************************************************************************
ndoy2ymd <- function(ndoy)
{
  strDate <- strptime(paste(substr(ndoy, 1, 4),substr(ndoy, 5, 7)), format="%Y %j")
  year  <- as.numeric(format(strDate, "%Y"))
  month <- as.numeric(format(strDate, "%m"))
  day   <- as.numeric(format(strDate, "%d"))
  return(c(year,month,day))
}

#************************************************************************************************
# getFilelist
#************************************************************************************************
fileList <- list.files(tifFilePath,  full.names = TRUE, recursive  = TRUE)

#************************************************************************************************
# process
#************************************************************************************************
for(fileName in fileList)
{
  #************************************************************************************************
  # 1  select data
  #************************************************************************************************
  basefilename <- gsub(pattern = "\\.tif$", "",basename(fileName))
  ymd <- ndoy2ymd(basefilename)
  subData <- subset(InData, 
                    InData$Year == ymd[1] & InData$Month == ymd[2] & InData$Day == ymd[3],
                    select = c(ID, Lon, Lat))
  
  if(nrow(subData) > 0)
  {  
    #************************************************************************************************
    # 2  extract VALUE from raster 
    #************************************************************************************************
    coordinates(subData) <- c("Lon","Lat")
    #read Raster
    rasterData    <- stack(fileName)
    snowExtract   <- extract(rasterData,subData,sp =TRUE)
    snowExtractDf <- snowExtract@data
    names(snowExtractDf) <- c("ID","SnowDepth")
    InData <- within(InData, 
                     SnowDepth[InData$Year == ymd[1] & InData$Month == ymd[2] & InData$Day == ymd[3]] <- snowExtractDf$SnowDepth)
    
  }
}

#************************************************************************************************
# write 
#************************************************************************************************
## Write res
outPutName <- paste(outvar, ".csv",sep='')
write.csv(InData, file = outPutName)

