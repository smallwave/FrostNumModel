#************************************************************************************************
# Ahthor: wavelet
# Date : 2017/5/10
# Update info :
#
#************************************************************************************************

library(clustMixType) # for data cleaning
library(plyr)
library(sp)
library(raster)
library(rgdal)
library(C50)
#************************************************************************************************
# define path name
#************************************************************************************************
strEnviPath <-   "../../Data/Envi/Typical/Dbf/FiveTypical.csv"
outResPathTif <- "../../Data/Envi/Typical/Class/Gaize.tif"
outResPathCsv <- "../../Data/Envi/Typical/Class/FiveTypical7.csv"
numberClass <- 7
outtype<-2
#************************************************************************************************
# 2  read data and Conver soil to factor 
#************************************************************************************************
enviVar <- read.csv(strEnviPath, head=TRUE,sep=",")
enviVar$Soil <- factor(enviVar$Soil)
enviVar$AspectType <- factor(enviVar$AspectType)
enviVar$ReliefType <- factor(enviVar$ReliefType)
#************************************************************************************************
# 2  read data and Conver soil to factor 
#************************************************************************************************
## run algorithm on x:
# part <- kmeansvar(X.quanti,X.quali,init=5,nstart=20, matsim = TRUE)
enviVarNum <- enviVar[,c(4:6,9)]
# NORMAL
enviVarNum <- apply(enviVarNum, 2, function(x)(x-min(x))/(max(x)-min(x))) 
enviVarCat <- enviVar[,c(3,7:8)]
#TO LIST
enviVarNum <- do.call(c, apply(enviVarNum, 2, list))
#CBIND
enviInput <-  cbind(enviVarCat, enviVarNum)  

lambdaest(enviInput)
res <- kproto(enviInput, numberClass)
enviVar$class = res$cluster


hv_C5 <- C5.0(formula,data = data)
map.RF <- predict(hv_C5, newdata = ov_data, type = "class")
summary(map.RF)

#************************************************************************************************
# 2  read data and Conver soil to factor 
#************************************************************************************************
enviVar.Map <- data.frame(enviVar[, c("X", "Y","class")])
enviVar.Map$class<-as.numeric(as.character(enviVar.Map$class))
# 
#************************************************************************************************
# 2  read data and Conver soil to factor 
#************************************************************************************************
if(outtype == 1){
  coordinates(enviVar.Map) <- ~X + Y
  gridded(enviVar.Map) <- TRUE
  writeGDAL(enviVar.Map,outResPathTif,drivername="GTiff", type="Int32", options=NULL)
}else {
  write.csv(enviVar.Map,file = outResPathCsv)
}




