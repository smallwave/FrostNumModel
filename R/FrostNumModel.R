#************************************************************************************************
# Ahthor: wavelet
# Propose : Calcalation DDT and DDF
# Date : 2017/6/20
# Update info :
#        2017/6/20/     
#************************************************************************************************
library(raster)
library(rgdal)

ddtddfFilePath     <- "F:/worktemp/Permafrost(FrostModel)/Data/DDTDDF/ddtddf.csv"

#************************************************************************************************
#  process  every file
#************************************************************************************************
frostNum <- function(val){
  if(val[3] == 0){
    return(1)
  }else if(val[4] == 0){
    return(3)
  } else  if(sqrt(val[4])/(sqrt(val[3])+sqrt(val[4])) >= 0.4){
    return(1)
  } else {
    return(2)
  }
}

InData     <- read.csv(ddtddfFilePath, head=TRUE,sep=",") #DDT,DDF
InData$P   <- apply(InData,1,frostNum)

#************************************************************************************************
# OUP RESULT 
#************************************************************************************************
OutValue   <- data.frame(InData[,c("Lon","Lat","P")])
coordinates(OutValue) <- ~Lon + Lat
gridded(OutValue) <- TRUE
fname<-paste("F:/worktemp/Permafrost(FrostModel)/Data/DDTDDF/","P0.4",".tiff",sep='')
writeGDAL(OutValue,fname,drivername="GTiff", type="Int32", options=NULL)

