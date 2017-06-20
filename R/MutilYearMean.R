#************************************************************************************************
# Ahthor: wavelet
# Propose : MutiYear to repare the LST data
# Date : 2017/6/10
# Update info :
#        2017/6/10/    
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
tifInPath   <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(OUT)/"
tifOutPath   <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(OUTY)/"

#************************************************************************************************
# process
#************************************************************************************************
for(i in 1:365)
{
   i<-190
   searchDay  <- str_pad(i, 3, pad = "0")
   searchStr  <- paste("*-",searchDay,".tif",sep='')
   fileList   <- list.files(tifInPath,  pattern = searchStr , full.names = TRUE, recursive  = TRUE)
   if (length(fileList) != 8)
   {
     print(i)
     next
   }
   
   #************************************************************************************************
   # read data
   #************************************************************************************************
   GSTValues  <- NULL
   for (fileName in fileList)
   {
     rasterData    <- stack(fileName)
     basefilename  <- gsub(pattern = "\\.tif$", "",basename(fileName))
     print(basefilename)
     LSTExtract    <- extract(rasterData,shapefile,sp =TRUE)
     LSTExtractDf  <- LSTExtract@data
     names(LSTExtractDf) <- c("Lon","Lat","Height","GST")
     LSTExtractDf <- within(LSTExtractDf, 
                      GST[LSTExtractDf$GST > 100.0] <- NA)
     GSTValues  <-  cbind(GSTValues,LSTExtractDf$GST)
   }
   GSTValues  <- as.data.frame(GSTValues)
   OutValue   <- apply(GSTValues,1,mean,na.rm = TRUE)
   OutValue   <- data.frame(LSTExtractDf[,c("Lon","Lat")],OutValue)
   
   #************************************************************************************************
   # OUP RESULT 
   #************************************************************************************************
   outName <- paste("MY",searchDay,sep='')
   names(OutValue) <- c("Lon","Lat","GST")
   coordinates(OutValue) <- ~Lon + Lat
   gridded(OutValue) <- TRUE
   fname<-paste(tifOutPath,outName,".tiff",sep='')
   writeGDAL(OutValue,fname,drivername="GTiff", type="Float32", options=NULL)

}