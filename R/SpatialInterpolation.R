#************************************************************************************************
# Ahthor: wavelet
# Propose : Spatial interplolation
# Date : 2017/6/18
# Update info :
#        2017/6/18/     kriging
#        2017/6/19/     using station data to kriging
#************************************************************************************************
library(sp)
library(gstat)
library(automap)
#************************************************************************************************
# read point
#************************************************************************************************
gstcvsFilePath      <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(SUB)/SUMMARY/"
gstoutFilePath      <- "F:/worktemp/Permafrost(FrostModel)/Data/GST(Kri)/"
sstation            <- "TableData.csv"
staData             <- read.csv(sstation, head=TRUE,sep=",")
#************************************************************************************************
# from ndoy2ymd
#************************************************************************************************
ndoy2ymd <- function(ndoy)
{
  strDate <- strptime(paste(substr(ndoy, 1, 4),substr(ndoy, 5, 7)), format="%Y %j")
  year  <- as.numeric(format(strDate, "%Y"))
  month <- as.numeric(format(strDate, "%m"))
  day   <- as.numeric(format(strDate, "%d"))
  return(c(year,month,day))
}
#************************************************************************************************
# set path AND  process  point
#************************************************************************************************
searchStr     <- paste("*.csv",sep='')
fileList      <- list.files(gstcvsFilePath,  pattern = searchStr , full.names = TRUE, recursive  = TRUE)
i <- 1
for(fileName in fileList)
{
  #read station data
  Day          <- str_pad(i, 3, pad = "0")
  print(Day)
  nDoy         <- paste("2010",Day,sep='')
  ymd          <- ndoy2ymd(nDoy)
  substaData   <- subset(staData, 
                         staData$Year == ymd[1] & staData$Month == ymd[2] & staData$Day == ymd[3],
                         select = c("Lon","Lat","GST"))
  substaData              <- na.omit(substaData)
  coordinates(substaData) <- ~Lon + Lat
  
  #read all data
  InData        <- read.csv(fileName, head=TRUE,sep=",")
  InData        <- data.frame(rownames(InData),InData)
  names(InData) <- c("ID","Lon","Lat","GST")
  InData$ID<-as.numeric(as.character(InData$ID))
  
  PredictD      <- subset(InData,is.na(InData$GST))
  coordinates(PredictD) <- ~Lon + Lat

  # ordinary  kriging:
  kriPrec         <- autoKrige(GST~1, substaData, PredictD)
  OutPrec         <- data.frame(PredictD$ID,kriPrec$krige_output@data$var1.pred)
  names(OutPrec)  <- c("ID","GST") 
  
  #Out result
  OutRes                 <-  merge(InData,OutPrec,by.x = "ID",by.y = "ID",all.x = TRUE)
  naIndex                <-  is.na(OutRes$GST.x)
  OutRes$GST.x[naIndex]  <-  OutRes$GST.y[naIndex]
  OutRes$GST.y           <-  NULL
  OutRes$ID              <-  NULL
  names(OutRes)  <- c("Lon","Lat","GST") 
  
  outName     <- paste("MY",Day,sep='-')
  fname<-paste(gstoutFilePath,outName,".csv",sep='')
  write.csv(OutRes, file = fname,row.names = FALSE)
  i = i+1
}


