
library(hydroGOF)
library(R.matlab)


temp <- (InData$LST_D + InData$LST_N)/2.0 - 273.15

plot(obsGST, simGST,xlab="Obseration", ylab="Model(MARS)", xlim=c(-30, 45),ylim=c(-30, 45),lwd = 10)            
abline(a = 0, b = 1)
rmseVal <- rmse(simGST,obsGST)
meVal  <- me(simGST,obsGST)
r2Val   <- br2(simGST,obsGST)
text(-5, 40, toString(selVars),cex = 1, col =2)
text(-0, 36, paste("RMSE", toString(rmseVal),sep=': '),cex = 1, col =2)
text(-0, 32, paste("ME", toString(meVal),sep=': '),cex = 1, col =2)
text(-0, 28, paste("R2", toString(r2Val),sep=': '),cex = 1, col =2)

plot(obsGST,temp,xlab="Obseration", ylab="(Max+min)/2.0", xlim=c(-30, 45),ylim=c(-30, 45), col = "red",lwd = 8)
abline(a = 0, b = 1)
rmseVal <- rmse(temp,obsGST)
meVal  <- me(temp,obsGST)
r2Val   <- br2(temp,obsGST)
text(-5, 40, toString(selVars),cex = 1, col =2)
text(-0, 36, paste("RMSE", toString(rmseVal),sep=': '),cex = 1, col =2)
text(-0, 32, paste("ME", toString(meVal),sep=': '),cex = 1, col =2)
text(-0, 28, paste("R2", toString(r2Val),sep=': '),cex = 1, col =2)



