library(ggplot2)
library("faraway", lib.loc="~/R/win-library/3.4")
require(faraway)
library("leaps", lib.loc="~/R/win-library/3.4")
require(MASS)

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

setwd("C:/Users/BDVR/Documents/Github/regressions/hw6")
savePlot = function(name,myPlot)
{
  pdf(name)
  print(myPlot)
  dev.off()
}
tData = (teengamb)
initModel = lm(gamble~sex,data=teengamb)
summary(lm(gamble~sex+income+verbal,data=teengamb))
summary(lm(gamble~sex+income,data=teengamb))


exhaustive = regsubsets(gamble~sex+status+income+verbal,force.in = 1,data=teengamb,method=c("exhaustive"),nbest=8)
pdf('teenGambDiv.pdf')
tg = plot(exhaustive,scale="adjr2")
dev.off()
X = summary(exhaustive)$which
print(coef(exhaustive,1:dim(X)[1],TRUE))
print(summary(initModel))
savePlot('teenGambDiv.pdf',tg)
gData = aatemp
linfo = lm(temp~year,gData)
trend = ggplot(gData,aes(x=gData$year,y=gData$temp))+geom_point()+geom_abline(intercept = linfo$coefficients[1],slope = linfo$coefficients[2])
plot(trend)
savePlot('mayblin.pdf',trend)
tempsOG = gData$temp[2:length(gData$temp)]
tempsPrev = gData$temp[1:length(gData$temp)-1]
diff = lm(tempsOG~tempsPrev)
og = lm(temp~year,gData)
summary(diff)
summary(og)
cor(x=tempsOG,tempsPrev)
tendeg = lm(temp~poly(year,10),gData)
alttendeg = lm(temp~year+I(year^2)+I(year^3)+I(year^4)+I(year^5)+I(year^6)+I(year^7)+I(year^8)+I(year^9)+I(year^10),gData)
alttendeg = lm(year~temp+I(temp^2)+I(temp^3)+I(temp^4)+I(temp^5)+I(temp^6)+I(temp^7)+I(temp^8)+I(temp^9)+I(temp^10),gData)
fivedeg=lm(temp~year+I(year^2)+I(year^3)+I(year^4)+I(year^5),gData)
fourdeg=lm(temp~year+I(year^2)+I(year^3)+I(year^4),gData)
threedeg=lm(temp~year+I(year^2)+I(year^3),gData)
prd <- data.frame(year = seq(from = range(gData$year)[1], to = range(gData$year)[2], length.out = 100)) 
#code stolen from https://stackoverflow.com/questions/23334360/plot-polynomial-regression-curve-in-r
err <- predict(threedeg, newdata = prd, se.fit = TRUE)
prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit
summary(fivedeg)
summary(fourdeg)
summary(threedeg)
newtrend = ggplot(prd,aes(x=year,y=fit))+theme_bw()+geom_line()+geom_point(data=gData,aes(x=year,y=temp))
savePlot('newTrend.pdf',newtrend)
summary(tendeg)
summary(alttendeg)
predict(threedeg,new=data.frame(year=2020),interval="prediction")
splitPoint = gData$year[which(gData$year>1930 & gData$year<=1930)]
splitTren1 = lm(temp~year,gData,subset=gData$year>1930)
splitTren2 = lm(temp~1,gData,subset=gData$year<=1930)
pdf('segmented.pdf')
plot(temp~year,gData)
abline(v=1930)
segments(1854,splitTren2$coef[1],1930,splitTren2$coef[1])
segments(1930, splitTren1$coef[1]+splitTren1$coef[2]*1930, 2000, splitTren1$coef[1]+splitTren1$coef[2]*2000)
dev.off()
splittrend = ggplot(gData,aes(x=gData$year,y=gData$temp))+geom_point()+geom_abline(intercept = splitTren$coefficients[1],slope = splitTren$coefficients[2])
plot(splittrend)
summary(splitTren)

cData=cornnit
cornMod = (lm(yield~nitrogen,cData))
boxcox(cornMod, plotit=T)
pdf('bxcx.pdf')
boxcox(cornMod, plotit=T, lambda=seq(1.5,4.0,by=0.1))
dev.off()
summary(cornMod)
nCornm = lm(((I(yield^3)-1)/3)~nitrogen,cData)
summary(nCornm)
ggplot(cData,aes(x=cData$nitrogen,y=cData$yield))+geom_point()
nCornm2 = lm(yield~nitrogen+I(nitrogen^2),cData)
summary(nCornm2)
oData = ozone
ozMod = lm(O3~temp+humidity+ibh,oData)
boxcox(ozMod,plotit=T)
#pdf('ozbxcx.pdf')
temp = boxcox(ozMod, plotit=T, lambda=seq(0.10,0.5,by=0.1))
#dev.off()
with(temp,x[which.max(y)])
treeData = trees
treeModel = lm(Volume~Girth+Height,treeData)
#pdf('trbxcx.pdf')
boxcox(treeModel,plotit=T,lambda=seq(0.05,0.55,by=0.1))
#dev.off()
#looks like lambda=.333, or cubed root, seems like a good idea
ggplot(treeData,aes(x=treeData$Volume,y=treeData$Girth))+geom_point()
ggplot(treeData,aes(x=treeData$Volume,y=treeData$Height))+geom_point()
# both seems linear, so doesnt seem like a quadratic term of either is a good idea
newTreemod = lm(((I(Volume^(1/3))-1)*3)~Girth+Height,treeData)
summary(treeModel)
summary(newTreemod)
#begin chapter 10 probs
pData = prostate
pModel = lm(lpsa~.,pData)
sumary(pModel)
pModel = update(pModel, .~.-gleason)
sumary(pModel)
pModel = update(pModel, .~.-lcp)
sumary(pModel)
pModel = update(pModel, .~.-pgg45)
sumary(pModel)
pModel = update(pModel, .~.-age)
sumary(pModel)
pModel = update(pModel, .~.-lbph)
sumary(pModel)
#b AIC elimination
stepwise(lm(lpsa~.,data=pData))
aicpos = regsubsets(lpsa~.,data=pData)
aicsum = summary(aicpos)
trueAICs = 97*log(aicsum$rss/97) + (2:9)*2 
aicsum$which
#pdf('aicforprostate.pdf')
plot(trueAICs~I(2:9),ylab="AIC",xlab="number of predictors")
#dev.off()
#pdf('rsqrprostate.pdf')
plot(2:9,aicsum$adjr2,xlab = "number of parameters",ylab="adj R squared")
#dev.off()
#pdf('cstatprostate.pdf')
plot(2:9,aicsum$cp,xlab="num params",ylab="Cp stats")
abline(0,1)
#dev.off()
#plot(aicpos,scale="AIC")
#summary(aicpos)
weirdTModel = lm(log(Volume)~I(Girth^2)+I(Height^2)+Height+Girth+Height*Girth,treeData)
sumary(weirdTModel)
simplified= lm(log(Volume)~Height+Girth+Height*Girth,treeData)
sumary(simplified)

sldata=stackloss
#typo in book? Fit a linear model to the stackloss data with stack.loss as the predictor and
#the other variables as predictors.
smodel = lm(stack.loss~.,sldata)
sumary(smodel)
smodel2 = update(smodel,.~.-Acid.Conc.)
sumary(smodel2)
seeLeverages(smodel2,sldata)
## 1 and 2 are not that interesting
examineStudRes(smodel2)
#point 21 looks sorta dangerous
getCooks(smodel2,sldata)
#point 21 is sorta suspect
seeLeverages(smodel,sldata)
## point 17 is out there
examineStudRes(smodel)
#point 21 looks sorta dangerous still
temp = getCooks(smodel,sldata)
#point 21 is sorta suspect
pModelupd = lm(stack.loss~.,subset = temp < max(temp),sldata)
sumary(pModelupd)
smodelupd2 = update(pModelupd,.~.-Acid.Conc.)
sumary(smodelupd2)
spData = seatpos
spModel = lm(hipcenter~.,spData)
sumary(spModel)
spModel$coefficients
mean(spModel$coefficients[2:length(spModel$coefficients)])
drops = c("hipcenter")
idk = spData[,!(names(spData) %in% drops)]
var(idk)
summary(spModel)
i=0
means = double()
for(i in (1:8)){
  means[i]=  mean(idk[,i])
}
means
predict(spModel,new=data.frame(Age=means[1],Weight=means[2],HtShoes=means[3],Ht=means[4],Seated=means[5],Arm=means[6],Thigh=means[7],Leg=means[8]),interval="prediction")
finalModel = lm(hipcenter~Age+HtShoes+Leg,spData)
sumary(finalModel)        
predict(finalModel,new=data.frame(Age=means[1],HtShoes=means[3],Leg=means[8]),interval="prediction")
