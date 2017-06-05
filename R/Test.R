library(anfis)
library(frbs)

##Set 2 cores using global options for parallel library
require("parallel")
if(.Platform$OS.type == "windows"){
  options(mc.cores=2)
}else{
  options(mc.cores=2) ##You could use all calling detectCores()
}
##Example domain for bidimentional sinc(x,y) function
x <- seq(-10, 10, length= 11)
trainingSet <- trainSet(x,x)
Z <- matrix(trainingSet[,"z"],ncol=length(x),nrow=length(x))
persp(x,x,Z,theta = 45, phi = 15, expand = 0.8, col = "lightblue",
      ticktype="detailed",main="sinc(x)*sinc(y)")
##Training domain patterns
X <- trainingSet[,1:2]
Y <- trainingSet[,3,drop=FALSE]
##Defining the required MembershipFunctions for the ANFIS
membershipFunction<-list(
  x=c(new(Class="GaussianMF",parameters=c(mu=-10,sigma=2)),
      new(Class="GaussianMF",parameters=c(mu=-5,sigma=2)),
      new(Class="GaussianMF",parameters=c(mu=0,sigma=2)),
      new(Class="GaussianMF",parameters=c(mu=5,sigma=2)),
      new(Class="GaussianMF",parameters=c(mu=10,sigma=2))),
  y=c(new(Class="GaussianMF",parameters=c(mu=-10,sigma=2)),
      new(Class="GaussianMF",parameters=c(mu=-5,sigma=2)),
      new(Class="GaussianMF",parameters=c(mu=0,sigma=2)),
      new(Class="GaussianMF",parameters=c(mu=5,sigma=2)),
      new(Class="GaussianMF",parameters=c(mu=10,sigma=2))))
##Creating the ANFIS network with 2 inputs and 4 MembershipFunctions in
##each input
anfis3 <- new(Class="ANFIS",X,Y,membershipFunction)
anfis3
##Check for epsilon-completeness in each input
plotMFs(anfis3)
##Training the ANFIS network.
trainOutput <- trainHybridJangOffLine(anfis3, epochs=10)
##We will use instead an already trained object to reduce example time.
data(anfis3)
##How the training went. You can keep on training as the training error
##is still descending.
plot(anfis3)
##Test the fit, i. e., how the MembershipFunctions partition the input space
plotMFs(anfis3)

##Just to see if premises, consequents and errors were updated
getPremises(anfis3)[[input=1]][[mf=1]]
getConsequents(anfis3)[1:2,]
getErrors(anfis3) #Training errors
getTrainingType(anfis3)
names(coef(anfis3))
##An alternative to get premises and/or consequents ...
coef(anfis3)$premises[[input=1]][[mf=1]]
coef(anfis3)$consequents[1:2,]
##First five train pattern associated values for the training process
fitted(anfis3)[1:5,]
resid(anfis3)[1:5,]
summary(anfis3)
##Surface comparison between the original training set and the predicted
##ANFIS network
y <- predict(anfis3,X)
z <- matrix(y[,1],ncol=length(x),nrow=length(x))
par(mfrow=c(1,2))
persp(x,x,Z,theta = 45, phi = 15, expand = 0.8, col = "lightblue",
      ticktype="detailed",main="Goal")
persp(x,x,z,theta = 45, phi = 15, expand = 0.8, col = "lightblue",
      ticktype="detailed",main="Fitted training Patterns", zlim=c(min(Z),max(Z)))



m <- diamonds[1:100,-(2:4)]
m$k=m$carat*-0.1
m$l=m$x *-0.1
m$n=m$depth*2.3 

# train - test 
d.train <- m[1 : 72, ]
d.test <- m[73 : 100, 1 : 9]

# the range of my data
m.range=apply(m,2,range)
# method,type,control 
meth.type <- "ANFIS"
cont = list(num.labels = 10, max.iter = 100, step.size = 0.01, type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH", name = "dmn")
# the model 
an_md <- frbs.learn(d.train, m.range, meth.type, cont)

# predict
r.tst <- predict(an_md, d.test)





#BellMF example I
#A bell membership function with default prototype (a=1, b=1,c=0)
#The membership of x in the bell, should be 1
#The derivate of the first parameter at x, should be 0
#The derivate of the first parameter at x, should be also 0
bell <- new(Class="BellMF")
bell
evaluateMF(object=bell, x=0)
derivateMF(object=bell, x=0, i=1)
derivateMF(object=bell, x=0, i="a")
#
#BellMF example II
#A bell membership function with parameters (a=4,b=1,c=-10)
#The membership of x in the bell, should be 0.137931
#The derivate of the first parameter at x, should be 0.05945303
#The derivate on "a" at x=0, should be 0.05945303
bell2 <- new(Class="BellMF",parameters=c(a=4,b=1,c=-10))
bell2
evaluateMF(object=bell2, x=0)
derivateMF(object=bell2, x=0, i=1)
derivateMF(object=bell2, x=0, i="a")












