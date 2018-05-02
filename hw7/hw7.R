library(ggplot2)
library("faraway", lib.loc="~/R/win-library/3.4")
require(faraway)
library("leaps", lib.loc="~/R/win-library/3.4")
require(MASS)
require(pls)
require(Amelia)
require(nlme)

rmse <- function(x,y) 
{
  sqrt(mean((x-y)^2)) 
}
seeLeverages = function(model,dataset)
{
  hats = hatvalues(model)
  states = row.names(dataset) #may not be needed
  x = halfnorm(hats,labs=states,ylab="leverage")
}
examineStudRes = function(model)
{
  studRes = rstudent(model)
  studRes[which.max(abs(studRes))]
  sort(studRes,decreasing = FALSE)
}
getCooks = function(model,dataset)
{
  states = row.names(dataset) #may not be needed
  satCook = cooks.distance(model)
  halfnorm(satCook,labs=states,ylab="Cook's distance")
  return(satCook)
}

setwd("C:/Users/BDVR/Documents/Github/regressions/hw7")
savePlot = function(name,myPlot)
{
  pdf(name)
  print(myPlot)
  dev.off()
}

seatpos  
?prcomp
set.seed(519514)
testhc=data.frame(HtShoes=181.08,Ht=178.56,Seated=91.44,Arm=35.64,Thigh=40.95,Leg=37.790)
testhcf=data.frame(Age=64.8,Weight=263.7,HtShoes=181.08,Ht=178.56,Seated=91.44,Arm=35.64,Thigh=40.95,Leg=37.790)


hold=prcomp(seatpos[,-c(9,1,2)])
print(summary(hold))
print(hold)
val1 = hold$rotation[,1]%*%c(181.08,178.56,91.44,35.64,40.95,37.790)
val2 = hold$rotation[,2]%*%c(181.08,178.56,91.44,35.64,40.95,37.790)
whatev=as.matrix(seatpos[,c(-1,-2,-9)])%*%hold$rotation[,1:2]
newStuff = data.frame(matrix(ncol = 2, nrow = 38))
colnames(newStuff)=c("PC1","PC2")
newStuff[,1]=whatev[,1]
newStuff[,2]=whatev[,2]
holdStuff = data.frame(matrix(ncol = 2, nrow = 1))
colnames(holdStuff)=c("PC1","PC2")
holdStuff[1,1]=val1
holdStuff[1,2]=val2
sppcm1=lm(seatpos$hipcenter~hold$rotation[,1]+hold$rotation[,2],data=newStuff)
names(sppcm1$coefficients)[2:3] = c('PC1','PC2')
sppcm1
plz = pcr(hipcenter~.,data=seatpos,ncomp=2)
predict(sppcm1,newdata=testhc)
sppcm1$coefficients[1] + sppcm1$coefficients[2]*-277.2875+sppcm1$coefficients[3]*14.68201

#ill hold onto the first 2 PCs
#the first one represents a strong negative correlation from height and height shoes
#the second one indcates a strong negative correlation between arm and thight while minimal in htshoes and ht. makes sense, orthogonal
hold2=prcomp(seatpos[,-c(9)])
print(summary(hold2))
print(hold2)
sppcm2=lm(seatpos$hipcenter~hold$x[,1:3])


cmonnow = pcr(hipcenter~.-Age-Weight,data=seatpos[],ncomp=2)
cmonnow
predict(cmonnow,testhcf,ncomp=2,interval="prediction")

#11.2
splsmod <- plsr(hipcenter ~ ., data=seatpos, validation="CV")
coefplot(splsmod, ncomp=4, xlab="Frequency")
plsCV <- RMSEP(splsmod, estimate="CV")
pdf("112numc.pdf")
par(mfrow=c(1,1)) # init 1 chart in 1 panel
plot(plsCV,main="")
dev.off()
#3 components looks good
hcpred = predict(splsmod,testhcf,ncomp=4)
print(hcpred)
"> print(hcpred)
, , 3 comps

hipcenter
1 -185.1849"


#11.3 use seatpos data with hipcenter as response everything else predict on ridge estimator
hcrgmod2 = lm.ridge(hipcenter~.,seatpos,lambda=seq(0,.2,len=8))
hcrgmod = lm.ridge(hipcenter~.+0,seatpos,lambda=seq(0,.2,len=8))
pdf("hcrgp.pdf")
matplot(hcrgmod$lambda,coef(hcrgmod),xlab=expression(lambda),ylab=expression(hat(beta)),type="l",col=1)
which.min(hcrgmod$GCV)
abline(v=0.05714286)
dev.off()

hcrgpred1 = cbind(1,as.matrix(testhcf[1,]))%*%coef(hcrgmod2)[8,]
hcrgpred2 = as.matrix(testhcf[1,])%*%coef(hcrgmod)[3,]

#use hcrgped1, which is -175.5, rounded

#11.4
#a
fat2=fat[-seq(1,length(fat[,1]),10),]
testfat = fat[seq(1,length(fat[,1]),10),]
oglg = lm(siri ~ . -brozek -density,fat2)
wut=predict(oglg,newdata=testfat)
rmse(wut,testfat$siri)

#b
stepwise(lm(siri ~ . -brozek -density,fat2),criterion = c("AIC"),direction=c("forward"))
splg = lm(formula = siri ~ abdom + free + weight + forearm + adipos + thigh + chest + biceps + ankle, data = fat2)
wut2=predict(splg,newdata=testfat)
rmse(wut2,testfat$siri)

#c

temp = prcomp(fat2[,-c(1,3)])
print(summary(temp))
fatpcr = pcr(siri ~ . -brozek -density,data=fat2,ncomp=15)
pcrr= predict(fatpcr,testfat,ncomp=3,interval="prediction")
rmse(pcrr,testfat$siri)

#d
par(mfrow=c(1,2)) # init 1 chart in 1 panel
fatpls<- plsr(siri ~ . -brozek -density,data=fat2, validation="CV")
par(mfrow=c(1,1)) # init 1 chart in 1 panel
pdf("114dc.pdf")
coefplot(fatpls, ncomp=5, xlab="Frequency")
plsCV <- RMSEP(fatpls, estimate="CV")
plot(plsCV,main="")
dev.off()
savePlot("11_4dnumc.pdf",plsCV)
hcpred = predict(fatpls,testfat,ncomp=4)
rmse(hcpred,testfat$siri)
print(hcpred)
#e
fatrm = lm.ridge(siri ~ .-brozek -density,data=fat2,lambda=seq(0,2,len=30))
fatrm2 = lm.ridge(siri ~ . +0 -brozek -density,data=fat2,lambda=seq(0,2,len=30))
hcrgmod = lm.ridge(hipcenter~.+0,seatpos,lambda=seq(0,.2,len=8))
pdf("fatrgp.pdf")
matplot(fatrm2$lambda,coef(fatrm2),xlab=expression(lambda),ylab=expression(hat(beta)),type="l",col=1)
which.min(fatrm2$GCV)
dev.off()

rmtestfat=testfat[,-c(1,2,3)]
hcrgpred1 = cbind(1,as.matrix(rmtestfat))%*%(coef(fatrm)[1,])
rmse(hcrgpred1,testfat$siri)
hcrgpred2 = as.matrix(testhcf[1,])%*%coef(hcrgmod)[3,] #as wed expect, its the same

#11.6
#a
withoutData=kanga[complete.cases(kanga),]
withoutData$sex = as.numeric(as.factor(withoutData$sex))
withoutData$species = as.numeric(as.factor(withoutData$species))

temp = prcomp(withoutData)
print(summary(temp))
#a components, 95% covered in first 2, 96% in the third
print(temp$rotation[,1])
#b mandible length, occipitonasal length, palate length, basilar length

temp2 = prcomp(withoutData,scale=TRUE)
print(summary(temp2))
#c components, 95% covered in first 9 PCS, obvi more distributed.
print(temp2$rotation[,1])
print(temp2$rotation[,2])
 #c the first principal component is much mroe balanced, the large values of the lengs of various parts are no longer dominating since they have been scaled
#d the second PC shows a strong corrolation with species, crest width, nasal length, and foramina length.
#d the first one represented that a lot of these kangaroos are a lot more similar, while the second PC, orthogonal to the first, shows that theuy differ in the corresponding high valeud components

#e
kangarob <- cov.rob(withoutData)
md <- mahalanobis(withoutData, center=kangarob$center, cov=kangarob$cov)
n <- nrow(withoutData);p <- ncol(withoutData)
#pdf("kanga.pdf")
plot(qchisq(1:n/(n+1),p), sort(md), xlab=expression(paste(chi^2,"quantiles")), ylab="Sorted Mahalanobis distances")
abline(0,1)
#dev.off()
#as we can see, we got some outliers out there
#f
") Make a scatterplot of the first and second principal components using a different
plotting symbol depending on the sex of the specimen. Do you think these
two components would be effective in determining the sex of a skull?"
pdf("idk.pdf")
plot(x=temp$rotation[,1],y=temp$rotation[,2],xlab="PC1",ylab="PC2")
dev.off()
#13.2
gala
require(alr3)
galamiss = galapagos
rowSums(is.na(galamiss))
#a
glm = lm(Species~.-Endemics,gala)
summary(glm)
#b
gmlm = lm(NS~Area+Anear+Dist+DistSC+Elevation,galamiss)
summary(gmlm)
#compared to non missing data, it obviously has a worse fit. It has less data to work with and thus has a larger RSE, and lower Rsquared.
#it is not that large of a change, but certainly note worthy
#c
gmmeans = colMeans(galamiss,na.rm = TRUE)
imgalamiss = galamiss
for(i in c(2:8)) imgalamiss[is.na(galamiss[,i]),i] <- gmmeans[i]
gmimlm = lm(NS~Area+Anear+Dist+DistSC+Elevation,imgalamiss)
summary(gmimlm)
#the fit is much worse! there is some increase in multi collinearity, mainly between elevation,Area, Anear, all quite high
# also, some of our variables are closer to zero
cor(imgalamiss)
cor(galamiss)
#d
elevlm = lm(Elevation~Area+Anear+Dist+DistSC,galamiss)
rggalamiss=galamiss
galamiss[is.na(galamiss$Elevation),]
rgvals = predict(elevlm,galamiss[is.na(galamiss$Elevation),])
sepcounter=1
for(i in 1:length(galamiss[,1])){
  if(is.na(rggalamiss[i,7]))
  {
    rggalamiss[i,7]=rgvals[sepcounter]
    sepcounter = sepcounter+1
  }
}
rgimlm = lm(NS~Area+Anear+Dist+DistSC+Elevation,rggalamiss)
summary(rgimlm)
#much better than the previous one for mean imputation, we are more confident in our predictors
cor(rggalamiss)
#higher correlations though for the elevation values and the other predictors, not much, but something
#e

gm2 = galamiss[,-2]
gm2 = gm2[,-7]
mimgp = amelia(gm2,m=25)
betasgp=NULL
sesgp=NULL
for(i in 1:mimgp$m)
{
  lmod <- lm(NS~Area+Anear+Dist+DistSC+Elevation, mimgp$imputations[[i]])
  betasgp <- rbind(betasgp ,coef(lmod))
  sesgp <- rbind(sesgp ,coef(summary(lmod))[,2])
}
(cr <- mi.meld(q=betasgp,se=sesgp))
cr$q.mi/cr$se.mi
#Elevation and Anear are significant, improved t values over the original single imputation


#13.3
gala
require(alr3)
pimamiss = pima
pimamiss[,2:8][pimamiss[,2:8]==0]=NA
pimamiss[,9][is.na(pimamiss[,9])]=0
pimamiss[,1][is.na(pimamiss[,1])]=0
rowSums(is.na(pimamiss))
#a
#looking at the rows, it appears that the insulin levels are very likely to be missing, while triceps are next, and diastolic is the least common to be missing values
#b
pimalm = lm(diastolic~.,pimamiss)
summary(pimalm)
#lots of missing observations, and a pretty poor fit
#c
pimameans = colMeans(pimamiss,na.rm = TRUE)
impimamiss = pimamiss
for(i in c(2:8)) impimamiss[is.na(pimamiss[,i]),i] <- pimameans[i]
pimaimlm =  lm(diastolic~.,impimamiss)
summary(pimaimlm)
#the fit is bhetter! glucose is also signifigant now
cor(impimamiss) #correlation between trices and bmi is pretty high, .5
cor(galamiss)
#d
#skipping as it doesnt make sense
#e

pima2 = pimamiss
mimpima = amelia(pima2,m=25)
betaspima=NULL
sespima=NULL
for(i in 1:mimpima$m)
{
  lmod <- lm(diastolic~.,mimpima$imputations[[i]])
  betaspima <- rbind(betaspima ,coef(lmod))
  sespima <- rbind(sespima ,coef(summary(lmod))[,2])
}
(cr <- mi.meld(q=betaspima,se=sespima))
summary(pimaimlm)
cr$q.mi/cr$se.mi
#higher glucose values, and age is slightly lower as well as bmi


#8.1
#a
piplm = lm(Lab~Field,pipeline)
summary(piplm)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
pdf("pipelinevar.pdf")
plot(piplm)
dev.off()
#it appears there is some kind of fan effect here in the top left graph
#b

i <- order(pipeline$Field)
npipe <- pipeline[i,]
ff <- gl(12,9)[-108]
meanfield <- unlist(lapply(split(npipe$Field,ff),mean))
varlab <- unlist(lapply(split(npipe$Lab,ff),var))
idklm = lm(log(varlab)~log(meanfield))
a_0=-1*coef(idklm)[1]
a_1=coef(idklm)[2]
pipdf = data.frame(lvl=log(varlab),lmf=log(meanfield))
pipwlm <- lm(Lab ~ Field, pipeline, weights=1/((Field)^a_1))
summary(pipwlm)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
pdf("pipelinevarw.pdf")
plot(pipwlm)
dev.off()

#c

piptlm <- lm(sqrt(Lab) ~ sqrt(Field), pipeline)
summary(piptlm)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
pdf("pipelinevart.pdf")
plot(piptlm)
dev.off()
#Elevation and Anear are significant, improved t values over the original single imputation

#8.2
#a
divusa
divusalm = lm(divorce~unemployed+femlab+marriage+birth+military,divusa)
summary(divusalm)
dures = residuals(divusalm)
pdf("ovtusa.pdf")
plot(dures,ylab="residuals",xlab="year")
dev.off()
par(mfrow=c(2,2)) # init 4 charts in 1 panel
pdf("duse1.pdf")
plot(divusalm)
dev.off()
#there is a cylcical nature to the erros
#b

glusalm = gls(divorce~unemployed+femlab+marriage+birth+military,divusa,correlation = corAR1(form=~year),method = "ML")
summary(glusalm)
intervals(glusalm,which="var-cov")
#clearly phi is signifigant and and we see a positive correlation between consecitive years
#also unemployed becomes signifigant as well
#c these are divorce rates, we see these during times of war mainly, so right before and right after especially
# people are getting married on a whim thinking they may die, when they come back they realize that they werent meant to be
# thus they divorce at similar times as well

#8.6
c2 = cheddar
clm = lm(taste~.,cheddar)
summary(clm)
c2$time=c(1:30)
cres = residuals(clm)
pdf("ovtc.pdf")
plot(cres,ylab="residuals",xlab="time")
dev.off()
#a) there appears to be some negative trend over "time", but ti isnt that strong, but still present

#b
cgls = gls(taste~.-time,c2,correlation = corAR1(form=~time))
summary(cgls)
intervals(cgls,which="var-cov")
#by this interval, we can tell that there is not much of a correlation here

#c
clm2 = lm(taste~.,c2)
summary(clm2)
#hilarious! time is signifigant !

#d, in the GLS, we are looking at how correlated the error or noise is over "time", or consecutive entries
# unlike our ordinary LS, the time value is being included to see how it may provide information on our response
#the difference lies within the relations. In OLS it changes the signifigance and value based on a linear combination within each entry
# in residuals, we are only considering the impact of the time variable after the coefficients have been established

#e if i was told that the entries were not in chronological order, then this would make it purely coincidental that 
# consecutive entries are related, and we should randomize their order to avoid the seemingly correlated entries


summary(gmlm)
fatpcr = pcr(siri ~ . -brozek -density,data=fat2,ncomp=3)
pcrr= predict(fatpcr,testfat,ncomp=3,interval="prediction")
rmse(pcrr,testfat$siri)

#rmse(hcpred,seatpos$hipcenter)
#in this one, id hold the first 3,
#first PC1 takes pretty much only weight into postivie correlation
#PC2 takes primarily age as a positive indicator
#PC3 is harder to interpret, but ht and htshoes are the main negative correlators
#functions to use, pcr for cross validation (CV)
  #require(pls), pcr(model eq, ncomp=...)
  #Residual Mean Squared Error rmse <- function(x,y) sqrt(mean((x-y)^2)) > rmse(fitted(modlm), trainmeat$fat) 

  #code for mahalanobis distances
  #> require(MASS) 
  #> robfat <- cov.rob(cfat) 
  #> md <- mahalanobis(cfat, center=robfat$center, cov=robfat$cov)
  #>n<- nrow(cfat);p <- ncol(cfat) 
 #> plot(qchisq(1:n/(n+1),p), sort(md), xlab=expression(paste(chi^2," quantiles")), ylab="Sorted Mahalanobis distances")
 #> abline(0,1)

#prcomp does some good work too.
print("done")
