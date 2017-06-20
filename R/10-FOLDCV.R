#************************************************************************************************
# Ahthor: wavelet
# Date : 2017/5/31
# Update info :
#        2017/6/1/    test  the code in 2003
#        2017/6/7     ALL VARABLES TEST
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
inputCvs <- "TableDataAll.csv"

#************************************************************************************************
# read data  and   train model
#************************************************************************************************
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
InData  ¡¡<- data.frame(InData,inD)

#selVars <- c("DD1","DD2","DD3","DS3","ND1","ND2","ND3","NS3","YD1","YD2","YD3","YS3","GST","Height","NDVI","Lat","SnowDepth")
selVars <- c("YD1","YD2","YD3","YS3","GST","NDVI")

#************************************************************************************************
# 1  MARS
#************************************************************************************************
nFlod        <-  5
rmseVal      <-rep(c(0), nFlod)
maeVal       <-rep(c(0), nFlod)
r2Val        <-rep(c(0), nFlod)
dataSim      <-  subset(InData,select = selVars)
dataSim$fold <-  cut(1:nrow(dataSim), breaks=nFlod, labels=F)
for (i in 1:nFlod) 
{

  dataSimulation   <- subset(dataSim, dataSim$fold != i,select = selVars)
  dataVerification <- subset(dataSim, dataSim$fold == i,select = selVars)
  earth.mod        <- earth(GST ~ ., data = dataSimulation)
  predictions      <- predict(earth.mod, newdata = dataVerification)
  simGST           <- as.vector(predictions)
  obsGST           <- as.vector(dataVerification$GST)
  rmseVal[i]       <- rmse(simGST,obsGST)
  maeVal[i]        <- mae(simGST,obsGST)
  r2Val[i]         <- (cor(simGST, obsGST, method="pearson"))^2 # r2=1
}

mean(rmseVal)
mean(maeVal)
mean(r2Val)


#************************************************************************************************
# 2 plot data struct
#************************************************************************************************

temp             <- (InData$LST_D + InData$LST_N)/2.0 - 273.15
dataSimulation   <- subset(InData,select = selVars)
earth.mod        <- earth(GST ~ ., data = dataSimulation)
predictions      <- predict(earth.mod, newdata = dataSimulation)
simGST           <- as.vector(predictions)
obsGST           <- as.vector(InData$GST)

plot(obsGST, simGST,xlab="Obseration", ylab="Model(MARS)", xlim=c(-30, 45),ylim=c(-30, 45),lwd = 10)            
abline(a = 0, b = 1)
rmseVal <- rmse(simGST,obsGST)
meVal   <- mae(simGST,obsGST)
r2Val   <- (cor(simGST, obsGST, method="pearson"))^2 # r2=1
text(-5, 40, toString(selVars),cex = 1, col =2)
text(-10, 36, paste("RMSE ", toString(rmseVal),sep=': '),cex = 1, col =2)
text(-10, 32, paste("MAE: ", toString(meVal),sep=': '),cex = 1, col =2)
text(-10, 28, paste("R2: ", toString(r2Val),sep=': '),cex = 1, col =2)
text(-18, 24, paste("N: ", toString(length(obsGST)),sep=': '),cex = 1, col =2)



plot(obsGST,temp,xlab="Obseration", ylab="(Max+min)/2.0", xlim=c(-30, 45),ylim=c(-30, 45), col = "red",lwd = 8)
abline(a = 0, b = 1)
rmseVal <- rmse(temp,obsGST)
meVal  <- me(temp,obsGST)
r2Val   <- br2(temp,obsGST)
text(-10, 36, paste("RMSE ", toString(rmseVal),sep=': '),cex = 1, col =2)
text(-10, 32, paste("MAE: ", toString(meVal),sep=': '),cex = 1, col =2)
text(-10, 28, paste("R2: ", toString(r2Val),sep=': '),cex = 1, col =2)
text(-18, 24, paste("N: ", toString(length(obsGST)),sep=': '),cex = 1, col =2)


#************************************************************************************************
# 3 time  and   station(space)
#************************************************************************************************

# time
dataSimulation   <- subset(InData, InData$Year <  2010 ,select = selVars)
dataVerification <- subset(InData, InData$Year == 2010,select = selVars)

# station
dataSimulation   <- subset(InData, InData$SID   <  56000 ,select = selVars)
dataVerification <- subset(InData, InData$SID  >=  56000,select = selVars)


earth.mod        <- earth(GST ~ ., data = dataSimulation)
predictions      <- predict(earth.mod, newdata = dataVerification)
simGST           <- as.vector(predictions)
obsGST           <- as.vector(dataVerification$GST)











