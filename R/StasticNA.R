#************************************************************************************************
# Ahthor: wavelet
# Propose : Stastic Number of Na 
# Date : 2017/6/12
# Update info :
#        2017/6/12/    
#************************************************************************************************
library(raster)
library(maptools)
library(stringr)
library(rgdal)

#************************************************************************************************
# read point
#************************************************************************************************
shapeFile <- "F:/worktemp/Permafrost(FrostModel)/Data/Position/Point.shp"
shapefile   <- readShapeSpatial(shapeFile)
#************************************************************************************************
# set path 
#************************************************************************************************
tifInPath   <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(OUTY)/"
outNafraction <- "NafractionCos"
#************************************************************************************************
# process  RASTER
#************************************************************************************************
searchStr     <- paste("*.tif",sep='')
fileList      <- list.files(tifInPath,  pattern = searchStr , full.names = TRUE, recursive  = TRUE)
Nafraction    <- rep(c(0), length(fileList))
i<-1
for(fileName in fileList)
{
  rasterData    <- stack(fileName)
  basefilename  <- gsub(pattern = "\\.tif$", "",basename(fileName))
  print(basefilename)
  LSTExtract    <- extract(rasterData,shapefile,sp =TRUE)
  LSTExtractDf  <- LSTExtract@data
  names(LSTExtractDf) <- c("Lon","Lat","Height","GST")
  lengthAll     <- length(LSTExtractDf$GST)
  LSTExtractDf  <- na.omit(LSTExtractDf)
  lengthNo      <- length(LSTExtractDf$GST)
  Nafraction[i] <-  (lengthAll-lengthNo)/lengthAll
  i = i+1
}

#************************************************************************************************
# process  csv
#************************************************************************************************
searchStr     <- paste("*.csv",sep='')
cvsInPath     <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(SUB)/SUMMARY/" 
fileList      <- list.files(cvsInPath,  pattern = searchStr , full.names = TRUE, recursive  = TRUE)
Nafraction    <- rep(c(0), length(fileList))
i<-1
for(fileName in fileList)
{
  Data          <- read.csv(fileName, head=TRUE,sep=",")
  basefilename  <- gsub(pattern = "\\.cvs$", "",basename(fileName))
  print(basefilename)
  lengthAll     <- length(Data$GST)
  Data          <- na.omit(Data)
  lengthNo      <- length(Data$GST)
  Nafraction[i] <-  (lengthAll-lengthNo)/lengthAll
  i = i+1
}

#************************************************************************************************
# write 
#************************************************************************************************
## Write res
outPutName <- paste(outNafraction, ".csv",sep='')
write.csv(Nafraction, file = outPutName)


