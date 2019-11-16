##############################################################################################################################
##R CODE FOR THE BRASS RELATIONAL LOGIT MODEL OF MORTALITY
##
##EDDIE HUNSINGER, FEBRUARY 2011 (LAST UPDATED DECEMBER 2018)
##http://www.demog.berkeley.edu/~eddieh/ 
##edyhsgr@gmail.com
##
##EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R
##
##THERE IS NO WARRANTY FOR THIS CODE
##############################################################################################################################

##############################################################################################################################
#STEP 1: Read in and plot the survivorship data (lxSample is collected data, lxSSA is Social Security Administration data, to
#be used as the standard-- so the goal is to adjust (the more reliable) lxSSA to meet the broad shape and level of lxSample)
##############################################################################################################################

lxSample<-read.table(file="https://raw.githubusercontent.com/AppliedDemogToolbox/Hunsinger_BrassRelationalLogit/master/lxSample.csv",header=TRUE,sep=",")
lxSSA<-read.table(file="https://raw.githubusercontent.com/AppliedDemogToolbox/Hunsinger_BrassRelationalLogit/master/lxSSA.csv",header=TRUE,sep=",")

plot(lxSample$X2000f,type="l",col="blue",lwd=5)
mtext(line=-14,text="Sample lx",font=2,cex=1,col="blue")
points(lxSSA$X2000f,type="l",col="red",lwd=5)
mtext(line=-10,text="SSA/Standard lx",font=2,cex=1,col="red")

##############################################################################################################################
#STEP 2: Enter the Brass Relational Logit function
##############################################################################################################################

Brass<-function(lx,lxBase){
Yx<-.5*log(lx/(1-lx))
YxBase<-.5*log(lxBase/(1-lxBase))
Estimate<-lm(Yx[2:length(lxBase)]~YxBase[2:length(lxBase)])
Alpha<-Estimate$coefficients[1]
Beta<-Estimate$coefficients[2]
Brass<- data.frame(Beta=Beta,Alpha=Alpha)
return(Brass)}

##############################################################################################################################
#STEP 3: Using the Brass function, estimate the coefficients for adjusting lxSSA
##############################################################################################################################

BrassCoefficients<-Brass(lxSample$X2000f,lxSSA$X2000f)
BrassCoefficients

##############################################################################################################################
#STEP 4: Adjust lxSSA with the estimated coefficients and plot the adjusted lxSSA
##############################################################################################################################

Yx<-NULL
for (i in 1:length(lxSSA$X2000f)){Yx[i]<-.5*log(lxSSA$X2000f[i]/(1-lxSSA$X2000f[i]))}
lxSSAAdjusted<-NULL
for (i in 1:length(lxSSA$X2000f)){lxSSAAdjusted[i]<-1/(1+exp(-2*BrassCoefficients$Alpha-2*BrassCoefficients$Beta*Yx[i]))}

points(lxSSAAdjusted,type="l",col="purple",lwd=5)
mtext(line=-12,text="SSA lx Adjusted",font=2,cex=1,col="purple")

##############################################################################################################################
#STEP 5: Plot some effects of change in Brass Alpha and Brass Beta
##############################################################################################################################

lxSSAHighAlpha<-NULL
for (i in 1:length(lxSSA$X2000f)){lxSSAHighAlpha[i]<-1/(1+exp(-2*(.5)-2*(1)*Yx[i]))}
lxSSALowAlpha<-NULL
for (i in 1:length(lxSSA$X2000f)){lxSSALowAlpha[i]<-1/(1+exp(-2*(-.5)-2*(1)*Yx[i]))}

lxSSAHighBeta<-NULL
for (i in 1:length(lxSSA$X2000f)){lxSSAHighBeta[i]<-1/(1+exp(-2*(0)-2*(1.5)*Yx[i]))}
lxSSALowBeta<-NULL
for (i in 1:length(lxSSA$X2000f)){lxSSALowBeta[i]<-1/(1+exp(-2*(0)-2*(.5)*Yx[i]))}

plot(lxSSA$X2000f,type="l",col="purple",lwd=5)
mtext(line=-12,text="SSA Standard lx",font=2,cex=1,col="purple")
points(lxSSAHighAlpha,type="l",col="red",lwd=5)
mtext(line=-10,text="SSA High (0.5) Alpha lx",font=2,cex=1,col="red")
points(lxSSALowAlpha,type="l",col="blue",lwd=5)
mtext(line=-14,text="SSA Low (-0.5) Alpha lx",font=2,cex=1,col="blue")

plot(lxSSA$X2000f,type="l",col="purple",lwd=5)
mtext(line=-12,text="SSA Standard lx",font=2,cex=1,col="purple")
points(lxSSAHighBeta,type="l",col="red",lwd=5)
mtext(line=-10,text="SSA High (1.5) Beta lx",font=2,cex=1,col="red")
points(lxSSALowBeta,type="l",col="blue",lwd=5)
mtext(line=-14,text="SSA Low (0.5) Beta lx",font=2,cex=1,col="blue")

#write.table(###, file="G:/###/###.csv", sep=",")

