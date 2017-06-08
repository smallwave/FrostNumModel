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
X1          <- InData$LST_D
X2          <- InData$LST_N
XY          <- (InData$LST_D + InData$LST_N)/2.0

# Haar
X1.haar     <- modwt(X1, "haar",3)
X2.haar     <- modwt(X2, "haar",3)
XY.haar     <- modwt(XY, "haar",3)


inD <- data.frame(cbind(X1.haar$d1,X1.haar$d2,X1.haar$d3,X1.haar$s3,
                        X2.haar$d1,X2.haar$d2,X2.haar$d3,X2.haar$s3,
                        XY.haar$d1,XY.haar$d2,XY.haar$d3,XY.haar$s3))
names(inD) <- c("DD1","DD2","DD3","DS3","ND1","ND2","ND3","NS3","YD1","YD2","YD3","YS3")
InData   ¡¡<- data.frame(InData,inD)


selVars    <- c("YD1","YD2","YD3","YS3","GST","Height","NDVI")

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
# setfilepaths
#************************************************************************************************
tifInPath    <- "F:/worktemp/Permafrost(FrostModel)/Data/MYD2003/"
snowInPath   <- "F:/worktemp/Permafrost(FrostModel)/Data/snow depth/Tiff/2003/"
tifOutPath   <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(OUT)/2003/"
dataSeq      <- seq(as.Date("2003/1/1"), as.Date("2003/12/31"), "days")

#************************************************************************************************
# process
#************************************************************************************************
for(selDate in dataSeq)
{

  #************************************************************************************************
  # getFilelist
  #************************************************************************************************
  selDate    <- dataSeq[1]
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
  # Haar
  X1.haar <- modwt(dataVerification$LST_D, "haar",3)
  X2.haar <- modwt(dataVerification$LST_N, "haar",3)
  
  X  <- (dataVerification$LST_D + dataVerification$LST_N)/2.0
  X.haar <- modwt(X, "haar",3)
  
  inD <- data.frame(cbind(X1.haar$d1,X1.haar$d2,X1.haar$d3,X1.haar$s3,X2.haar$d1,X2.haar$d2,X2.haar$d3,X2.haar$s3))
  names(inD) <- c("DD1","DD2","DD3","DS3","ND1","ND2","ND3","NS3")
  
  inD <- data.frame(cbind(X.haar$d1,X.haar$d2,X.haar$d3,X.haar$s3))
  names(inD) <- c("YD1","YD2","YD3","YS3")
  
  
  dataVerification¡¡         <- data.frame(dataVerification,inD)
  dataVerification$GST       <- predict(earth.mod, newdata = dataVerification)
  outRes                     <- merge(outData,dataVerification,by.x="ID",by.y="ID",all.x=TRUE)
  #************************************************************************************************
  # OUP RESULT 
  #************************************************************************************************
  outName <- yearDoy
  map.Polygon <- data.frame(outRes[, c("Lon.x", "Lat.x","GST")])
  names(map.Polygon) <- c("Lon","Lat","GST")
  map.Polygon$GST<-as.numeric(as.character(map.Polygon$GST))
  coordinates(map.Polygon) <- ~Lon + Lat
  gridded(map.Polygon) <- TRUE
  fname<-paste(tifOutPath,outName,"MARS6",".tiff",sep='')
  writeGDAL(map.Polygon,fname,drivername="GTiff", type="Float32", options=NULL)
  
}






