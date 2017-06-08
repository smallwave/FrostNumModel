#************************************************************************************************
# Ahthor: wavelet
# Date : 2017/5/25
# Update info :
#
#************************************************************************************************
library(lubridate)
library (plyr)
library (sp)
library (raster)
library(sqldf)
library(stringr)
#************************************************************************************************
# define var name
#************************************************************************************************
inputCvs <- "TableDataSnow.csv"
tifFilePath  <- "F:/worktemp/Permafrost(FrostModel)/Data/MYD2010/"
outvar       <- "TableDataSnowLST"
#************************************************************************************************
# read data
#************************************************************************************************
InData <- read.csv(inputCvs, head=TRUE,sep=",")
InData <- data.frame(sapply(InData, `[`))
InData$LST_D <- NA
InData$LST_N <- NA

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
fileList <- list.files(tifFilePath,  pattern = "\\.tif$", full.names = TRUE, recursive  = TRUE)

#************************************************************************************************
# process
#************************************************************************************************
for(fileName in fileList)
{
  #************************************************************************************************
  # 1  select data
  #************************************************************************************************
  basefilename <- gsub(pattern = "\\.tif$", "",basename(fileName))
  print(basefilename)
  strTemps <- str_split_fixed(basefilename,"_",4)
  typeLST  <- str_trim(strTemps[3])
  fileDate <- gsub(pattern = "\\.LST$", "",str_trim(strTemps[2]))
  ymd <- ndoy2ymd(fileDate)
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
    LSTExtract    <- extract(rasterData,subData,sp =TRUE)
    LSTExtractDf  <- LSTExtract@data
    names(LSTExtractDf) <- c("ID","LST")
    LSTExtractDf$LST  <- LSTExtractDf$LST*0.02
    LSTExtractDf      <- within(LSTExtractDf, 
                                LST[LSTExtractDf$LST > 500.0  | LSTExtractDf$LST < 0.1 ] <- NA )
    
    if(typeLST == "Day")
    {
       InData <- within(InData, 
                        LST_D[InData$Year == ymd[1] & InData$Month == ymd[2] & InData$Day == ymd[3]] <- LSTExtractDf$LST)
    }else {
      
       InData <- within(InData, 
                        LST_N[InData$Year == ymd[1] & InData$Month == ymd[2] & InData$Day == ymd[3]] <- LSTExtractDf$LST)
    }
  }
}

#************************************************************************************************
# write 
#************************************************************************************************
## Write res
outPutName <- paste(outvar, ".csv",sep='')
write.csv(InData, file = outPutName)

