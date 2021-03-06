#************************************************************************************************
# Ahthor: wavelet
# Date : 2017/5/31
# Update info :
#        2017/6/1/    test  the code in 2003
#************************************************************************************************
library(earth)
library(hydroGOF)
library(waveslim)
library(maptools)
library(raster)
library(stringr)
library(lubridate)
library(rgdal)
#************************************************************************************************
# define var name
#************************************************************************************************
inputCvs  <- "TableDataAll.csv"
shapeFile <- "F:/worktemp/Permafrost(FrostModel)/Data/Position/Point.shp"

#************************************************************************************************
# read data  and   train model
#************************************************************************************************
shapefile   <- readShapeSpatial(shapeFile)

InData      <- read.csv(inputCvs, head=TRUE,sep=",")
InData      <- na.omit(InData)
# var
XY          <- (InData$LST_D + InData$LST_N)/2.0

# Haar
XY.haar     <- modwt(XY, "haar",3)

inD <- data.frame(cbind(XY.haar$d1,XY.haar$d2,XY.haar$d3,XY.haar$s3))
names(inD) <- c("YD1","YD2","YD3","YS3")
InData   ˇˇ<- data.frame(InData,inD)


selVars    <- c("YD1","YD2","YD3","YS3","GST","NDVI")

dataSimulation <-  subset(InData,select = selVars)
earth.mod      <-  earth(GST ~ ., data = dataSimulation)


#************************************************************************************************
# read ndvi data
# define A(1,2...12) and B(1,2,...12)
#************************************************************************************************
tifFileNDVIPath  <- "F:/worktemp/Permafrost(FrostModel)/Data/NDVI/"
fileList <- list.files(tifFileNDVIPath, pattern  = "\\.tif$", full.names = TRUE, recursive  = TRUE)
ndviA <- NULL
ndviB <- NULL
#************************************************************************************************
# process
#************************************************************************************************
for(fileName in fileList)
{
  basefilename  <-  gsub(pattern = "\\.tif$", "",basename(fileName))
  numTmp        <-  as.numeric(basefilename)
  nmonth        <-  ceiling(numTmp/2.0)
  
  rasterData    <- stack(fileName)
  ndviExtract   <- extract(rasterData,shapefile,sp =TRUE)
  ndviExtractDf <- ndviExtract@data
  names(ndviExtractDf) <- c("Lon","Lat","Height","NDVI")
  listNDVI <- ndviExtractDf$NDVI/1000.0
  if(numTmp%%2){
    ndviA = cbind(ndviA,listNDVI)
  }else{
    ndviB = cbind(ndviB,listNDVI)
  }
}
ndviA  <- as.data.frame(ndviA)
ndviB  <- as.data.frame(ndviB)
names(ndviA) <- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12")
names(ndviB) <- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12")



#************************************************************************************************
#
#   Start here 
#
#   setfilepaths
#
#   2017.6.7
#
#************************************************************************************************
snowInPath   <- "F:/worktemp/Permafrost(FrostModel)/Data/snow depth/Tiff/2003/"
tifInPath    <- "F:/worktemp/Permafrost(FrostModel)/Data/MYD2008/"
tifOutPath   <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(OUT)/2008/"
dataSeq      <- seq(as.Date("2008/1/1"), as.Date("2010/12/31"), "days")
#************************************************************************************************
# process
#************************************************************************************************
for(i in 1:length(dataSeq))
{
  #************************************************************************************************
  # getFilelist
  #************************************************************************************************
  selDate    <- dataSeq[i]
  year       <- as.numeric(format(selDate, "%Y"))
  doy        <- strftime(selDate, format = "%j")
  yearDoy    <- paste(year,doy,sep='')
  searchStr  <- paste("*",yearDoy,"*.tif",sep='')
  fileList   <- list.files(tifInPath,  pattern =glob2rx(searchStr) , full.names = TRUE, recursive  = TRUE)
  if (length(fileList) != 2)
  {
    print(selDate)
    next
  }
  
  #define dataVerification
  dataVerification <- NULL
  #************************************************************************************************
  # Set ndvi  file   and   construct  
  #************************************************************************************************
  if(day(selDate) <= 15){
    dataVerification$NDVI  <- ndviA[,month(selDate)]
  }else{
    dataVerification$NDVI  <- ndviB[,month(selDate)]
  }
  #************************************************************************************************
  # get lst  file   and   construct  
  #************************************************************************************************
  for (fileName in fileList)
  {
      rasterData    <- stack(fileName)
      basefilename  <- gsub(pattern = "\\.tif$", "",basename(fileName))
      print(basefilename)
      strTemps      <- str_split_fixed(basefilename,"_",4)
      typeLST       <- str_trim(strTemps[3])   # DAY
      
      LSTExtract    <- extract(rasterData,shapefile,sp =TRUE)
      LSTExtractDf  <- LSTExtract@data
      names(LSTExtractDf) <- c("Lon","Lat","Height","LST")
      LSTExtractDf$LST    <- LSTExtractDf$LST*0.02
      LSTExtractDf        <- within(LSTExtractDf, 
                                    LST[LSTExtractDf$LST > 500.0  | LSTExtractDf$LST < 0.1 ] <- NA )
      if(typeLST == "Day")
      {
        dataVerification$LST_D <- LSTExtractDf$LST 
      }else {
        dataVerification$LST_N <- LSTExtractDf$LST 
      }
  }
  dataVerification           <-  data.frame(LSTExtractDf[,c("Lon","Lat","Height")],dataVerification)
  dataVerification$ID        <-  as.numeric(as.character(rownames(dataVerification)))
  
  #************************************************************************************************
  # get snow  depths  
  #************************************************************************************************
  # rasterData    <- stack("F:/worktemp/Permafrost(FrostModel)/Data/snow depth/Tiff/2003/2003001.tif")
  # LSTExtract    <- extract(rasterData,shapefile,sp =TRUE)
  # LSTExtractDf  <- LSTExtract@data
  # names(LSTExtractDf)           <- c("Lon","Lat","Height","SnowDepth")
  # dataVerification$SnowDepth    <-  LSTExtractDf[,c("SnowDepth")]
  
  #************************************************************************************************
  # construct out put
  #************************************************************************************************
  outData        <- data.frame(dataVerification[, c("ID","Lon","Lat")])
  names(outData) <- c("ID","Lon","Lat")
  #************************************************************************************************
  # wavelet  
  #************************************************************************************************
  dataVerification <- na.omit(dataVerification)

  if(length(dataVerification$Lon) == 0 ){
    print(selDate)
    next
  }
  
  
  # Haar
  XY          <-   (dataVerification$LST_D + dataVerification$LST_N)/2.0
  XY.haar     <-    modwt(XY, "haar",3)
  
  inD <- data.frame(cbind(XY.haar$d1,XY.haar$d2,XY.haar$d3,XY.haar$s3))
  names(inD) <- c("YD1","YD2","YD3","YS3")

  dataVerificationˇˇ         <- data.frame(dataVerification,inD)
  dataVerification$GST       <- predict(earth.mod, newdata = dataVerification)
  outRes                     <- merge(outData,dataVerification,by.x="ID",by.y="ID",all.x=TRUE)
  #************************************************************************************************
  # OUP RESULT 
  #************************************************************************************************
  outName <- paste(year,doy,sep='-')
  map.Polygon <- data.frame(outRes[, c("Lon.x", "Lat.x","GST")])
  names(map.Polygon) <- c("Lon","Lat","GST")
  map.Polygon$GST<-as.numeric(as.character(map.Polygon$GST))
  coordinates(map.Polygon) <- ~Lon + Lat
  gridded(map.Polygon) <- TRUE
  fname<-paste(tifOutPath,outName,".tiff",sep='')
  writeGDAL(map.Polygon,fname,drivername="GTiff", type="Float32", options=NULL)
  
}






