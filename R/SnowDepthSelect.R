#************************************************************************************************
# Ahthor: wavelet
# Date : 2017/5/15
# Update info :
#
#************************************************************************************************
library(sp)
library(maptools)
library(raster)
library(rgdal)

shapeFileQTP <- "F:/worktemp/Permafrost(FrostModel)/Data/Envi/Bounder/Tibet_Plateau_Boundar.shp"
txtFilePath  <- "F:/worktemp/Permafrost(FrostModel)/Data/snow depth/snowdepth-2010/"
tifFilePath  <- "F:/worktemp/Permafrost(FrostModel)/Data/snow depth/Tiff/2010/"

#************************************************************************************************
# getFilelist
#************************************************************************************************
fileList <- list.files(txtFilePath,  full.names = TRUE)

#************************************************************************************************
# read shp
#************************************************************************************************
shp <- readOGR(dsn = shapeFileQTP, layer = "Tibet_Plateau_Boundar")

#************************************************************************************************
# process clip and convertion format(*.tif)
#************************************************************************************************
for(fileName in fileList)
{
  asciiFile <- read.asciigrid(fileName, as.image = FALSE, plot.image = FALSE, colname = fileName,
                        proj4string = CRS(as.character("+proj=longlat +datum=WGS84")))
  
  rgrid <- raster(asciiFile)
  rgrid_msk <- mask(rgrid,shp)
  rgrid_sub <- crop(rgrid_msk,extent(shp))
  outfileName = paste(gsub(pattern = "\\.txt$", "",basename(fileName)),".tif",sep='')
  outfileName = paste(tifFilePath,outfileName,sep='')
  writeRaster(rgrid_sub,filename=outfileName, format="GTiff", overwrite=TRUE)
}

