#************************************************************************************************
# Ahthor: wavelet
# Propose : Time interplolation
# Date : 2017/6/13
# Update info :
#        2017/6/13/    
#        2017/6/16/  paraell computering  using  apply
#************************************************************************************************
library(raster)
library(maptools)
library(stringr)
library(rgdal)
#************************************************************************************************
# read point
#************************************************************************************************
shapeFilePath            <- "F:/worktemp/Permafrost(FrostModel)/Data/Position/Point20.shp"
shapefile                <- readShapeSpatial(shapeFilePath)
NumofKnowValues          <- 250  # Number of Know values 

#************************************************************************************************
# set path AND  process  point
#************************************************************************************************
tifInPath     <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(OUTY)/"
searchStr     <- paste("*.tif",sep='')
fileList      <- list.files(tifInPath,  pattern = searchStr , full.names = TRUE, recursive  = TRUE)
GSTValues     <- NULL
for(fileName in fileList)
{
  rasterData    <- stack(fileName)
  basefilename  <- gsub(pattern = "\\.tif$", "",basename(fileName))
  print(basefilename)
  LSTExtract    <- extract(rasterData,shapefile,sp =TRUE)
  LSTExtractDf  <- LSTExtract@data
  names(LSTExtractDf) <- c("Lon","Lat","Height","GST")
  GSTValues  <-  cbind(GSTValues,LSTExtractDf$GST)
}
GSTValues  <- as.data.frame(GSTValues)
#************************************************************************************************
# process   cell   
#************************************************************************************************
gc()
run <- function(cellValue){
  naLen                  <-  length(na.omit(cellValue))
  if(naLen >= NumofKnowValues & naLen < 365){
    cellValue              <-  data.frame(Time = seq(from =1, to = 365, by =1),cellValue)
    names(cellValue)       <-  c("Time","GST")
    cellValue$Timec        <-  cos(2*pi*cellValue$Time/365)
    cellValue$Times        <-  sin(2*pi*cellValue$Time/365)
    reslm                  <-  lm(cellValue$GST ~ cellValue$Timec  +  cellValue$Times)
    dataPre                <-  predict(reslm, cellValue, interval="prediction")
    dataPre                <-  data.frame(dataPre)
    naIndex                <-  is.na(cellValue$GST)
    cellValue$GST[naIndex] <-  dataPre$fit[naIndex]
    return(cellValue$GST)
  }
  return (cellValue)
}
#method1
Sys.time()
GSTValues <- t(apply(GSTValues,1,run))
Sys.time()

#method2
# nlen <- nrow(GSTValues) 
# for(i in 1:10)
# {
#   print(i)
#   GSTValues[i,] <- run(GSTValues[i,])
# }
# Sys.time()
#************************************************************************************************
# OUP RESULT 
#************************************************************************************************
tifOutPath   <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(SUB)/SUB1/"
for(i in 1:365)
{
  Day         <- str_pad(i, 3, pad = "0")
  outName     <- paste("MY",Day,sep='-')
  cellValue   <- as.numeric(as.character(GSTValues[,i]))
  map.Polygon <- data.frame(LSTExtractDf[,c("Lon","Lat")],cellValue)
  names(map.Polygon) <- c("Lon","Lat","GST")
  fname<-paste(tifOutPath,outName,".csv",sep='')
  write.csv(map.Polygon, file = fname,row.names = FALSE)
}

