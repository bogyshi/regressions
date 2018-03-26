library(ggplot2)
library("faraway", lib.loc="~/R/win-library/3.4")
require(faraway)
savePlot = function(name,myPlot)
{
  pdf(name)
  print(myPlot)
  dev.off()
}
data(sat)
y=sat$total
stateLM = lm(y~ expend+salary+ratio+takers,sat)
satcv=(ggplot(sat,aes(x=stateLM$fitted.values,y=stateLM$residuals))+geom_point())
savePlot("satcv.pdf",satcv)
#not much to say, looks pretty good to me
qqnorm(residuals(stateLM),ylab="residuals",main="")
qqline(residuals(stateLM))
#still looks good so far, nothing too crazy, it MIGHT be a short tailed distribution, but this isnt too concerning due to the ultimatey normal principal
hats = hatvalues(stateLM)
states = row.names(sat)
x = halfnorm(hats,labs=states,ylab="leverage")
savePlot("satHats.pdf",x)
#Utah and California seem to have something going on. high leverage, > 2 * 7 / 50, and are beyond 2 in the half normal quantiles
studRes = rstudent(stateLM)
studRes[which.max(abs(studRes))]
sort(studRes,decreasing = FALSE)
#west virginia, utah,northDakota,NewHampshire, all have over 2 studentized residuals, west virginia is above 3!
#studentized residuals 
satCook = cooks.distance(stateLM)
png(filename="satCD.png")
halfnorm(satCook,labs=states,ylab="Cook's distance")
dev.off()
#utah has shown up again, very large cooks distance this time around.
#looking at the data, utah has a very low expend value and very high score, and a very low taker score
stateLM2 = lm(total~expend+salary+ratio+takers,subset = satCook < max(satCook),sat)
summary(stateLM)
summary(stateLM2)
termplot(stateLM,partial.resid = TRUE)
#all look pretty good, maybe not takers,
#overall, if utah is removed, we see an improved R squared amongst some other things
#ratio suddenly becomes important when Utah is removed, our model shouldnt be so various from just one observation


constVariance = function(data,dataset)
{
  aplot = ggplot(dataset,aes(x=data$fitted.values,y=data$residuals))+geom_point() 
  print(aplot)
  return(aplot)
}

isNormal = function(model)
{
  qqnorm(residuals(model),ylab="residuals",main="")
  qqline(residuals(model))
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
#BEGIN teengamb
data(teengamb)
tmodel <- lm(gamble~.,teengamb)

savePlot("teengambcv.pdf",constVariance(tmodel,teengamb))

#this looks suspicious, based on the info given by our textbook, looks like some heteroscedasticity
# we may need to transform our y, so that var(h(y)) is constant, book used square root for counts, we can expirment with this.
#constVariance(lm(log(gamble)~.,teengamb),teengamb)
constVariance(lm(exp(gamble)~.,teengamb),teengamb)
savePlot("teengambcv2.pdf",constVariance(lm(sqrt(gamble)~.,teengamb),teengamb))
#none of these really worked, sqrt seems to be much better? show that graph
isNormal(tmodel)
#nope, this aint good, looks like a long tailed distribution which means we may need to use robust methods or let central limit therorem take care of this problem
#could also be an outlier, we definetly have one.
seeLeverages(tmodel,teengamb)
#ony lone of concern is unit or person # 24, row 24. They have a decently high leverage.
examineStudRes(tmodel)
#row six has a very very large studentized residual, while row 24 does not. may need to look at row 5
weirdo = getCooks(tmodel,teengamb)
png(filename="teengambCD.png")
halfnorm(weirdo,ylab="Cook's distance")
dev.off()
#row6 has shown up again, suspicious stuff
#looking at the data, utah has a very low expend value and very high score, and a very low taker score
tmodel2 = lm(gamble~.,subset = weirdo< max(weirdo),teengamb)
summary(tmodel)
summary(tmodel2)
#Looking at the differences, we see a higher r squared, signifigantly different R squared values, and different p-values.
termplot(tmodel,partial.resid = TRUE)
#not sure what to do with this, doesnt make too much sense.

#BEGIN prostate
data(prostate)
pmodel = lm(lpsa~.,prostate)
constVariance(pmodel,prostate)
#we looking prime son
isNormal(pmodel)
#looks good still, may be short tailed.
seeLeverages(pmodel,prostate)
2*9/97
#observations 41 and 32 seem to 
examineStudRes(pmodel)
# potential points of interested, 39, 47, 95, 69
cooksP = getCooks(pmodel,prostate)
png(filename="prostateCD.png")
halfnorm(cooksP,ylab="Cook's distance")
dev.off()
#graphically, 32 and 47 are quite high, along with a couple more strugglers
summary(pmodel)
pmodel2=lm(lpsa~.,subset=cooksP<max(cooksP),prostate)
summary(pmodel2)
# we see some improvement, maybe do this again but with more removed from cooks data set
termplot(pmodel,partial.resid = TRUE)
#BEGIN: swiss data
data(swiss)
smodel=lm(Fertility~.,swiss)
constVariance(smodel,swiss)
#looks good
isNormal(smodel)
#very very nice
seeLeverages(smodel,swiss)
2*6/47
#v de gee and la vallee are quite high
examineStudRes(smodel)
# not much here
cooksS = getCooks(smodel,swiss)
#porren and sierre are decently high
smodel2 = lm(Fertility~.,subset=cooksS<max(cooksS),swiss)
summary(smodel)
summary(smodel2)
# severly improed R squared, its tempting to always do this?
termplot(smodel,partial.resid = TRUE)
#Begin CHEDDAR
data(cheddar)
cmodel = lm(taste~.,cheddar)
constVariance(cmodel,cheddar)
#looks good here too
isNormal(cmodel)
#looks good
seeLeverages(cmodel,cheddar)
2*4/30
#highly shifted, 20 and 26 stand out, but may be nothing to it? 0.266667 is the only standout
examineStudRes(cmodel)
#20 and 26 not on here, but 15 is, quite suspect
cooksC=getCooks(cmodel,cheddar)
png(filename="cheddarCD.png")
halfnorm(cooksC,ylab="Cook's distance")
dev.off()
#12 adn 15 are highlightyed
cmodel2=lm(taste~.,subset=cooksC<max(cooksC),cheddar)
summary(cmodel)
summary(cmodel2)
termplot(cmodel,partial.resid = TRUE)

# the model tends to be preforming better, at least lower p values and higher R squared
#BEGIN: Happy
data(happy)
hmodel = lm(happy~.,happy)
constVariance(hmodel,happy)
#looks pretty good
isNormal(hmodel)
#looks good again
seeLeverages(hmodel,happy)
2*5/39
#looks like its shifted up. a couple values above .256, , rows 6 and 7 are highlighted
examineStudRes(hmodel)
#row 36 has a very very high value, -3.628
png(filename="happyCD.png")
cooksH = getCooks(hmodel,happy)
dev.off()
#36 is clearly an outlier, very high cooks distance
hmodel2=lm(happy~.,subset=cooksH<max(cooksH),happy)
summary(hmodel)
summary(hmodel2)
#we see a siginifigant decrease in p value for sex and money, but interestingly enough, work no longer 
#means as much as its p value increased.
termplot(hmodel,partial.resid = TRUE)
