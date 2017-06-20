
library(raster)
library(maptools)
library(stringr)
library(rgdal)

shapeFilePath            <- "F:/worktemp/Permafrost(FrostModel)/Data/Position/Point.shp"
shapefile                <- readShapeSpatial(shapeFilePath)

tifInPath     <- "F:/worktemp/Permafrost(FrostModel)/Data/Test/M12/"
searchStr     <- paste("*.tif",sep='')
fileList      <- list.files(tifInPath,  pattern = searchStr , full.names = TRUE, recursive  = TRUE)
NanALL        <- rep(c(0), nrow(LSTExtractDf))
for(fileName in fileList)
{
  rasterData    <- stack(fileName)
  basefilename  <- gsub(pattern = "\\.tif$", "",basename(fileName))
  print(basefilename)
  LSTExtract    <- extract(rasterData,shapefile,sp =TRUE)
  LSTExtractDf  <- LSTExtract@data
  names(LSTExtractDf) <- c("Lon","Lat","Height","GST")
  Nan                 <- rep(c(0), nrow(LSTExtractDf))
  Nan[LSTExtractDf$GST == 0] <- 1
  NanALL              <- NanALL + Nan
}
length(NanALL[NanALL>7] == TRUE) / length(NanALL)



map.Polygon <- data.frame(LSTExtractDf[,c("Lon","Lat")],NanALL)
names(map.Polygon) <- c("Lon","Lat","NUM")
coordinates(map.Polygon) <- ~Lon + Lat
gridded(map.Polygon) <- TRUE
fname<-"F:/worktemp/Permafrost(FrostModel)/Data/Test/M1.tiff"
writeGDAL(map.Polygon,fname,drivername="GTiff", type="Int32", options=NULL)


