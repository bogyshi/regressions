library(ggplot2)
library("faraway", lib.loc="~/R/win-library/3.4")
require(faraway)
data(sat)
y=sat$total
stateLM = lm(y~ expend+salary+ratio+takers,sat)
ggplot(sat,aes(x=stateLM$fitted.values,y=stateLM$residuals))+geom_point()
#not much to say, looks pretty good to me
qqnorm(residuals(stateLM),ylab="residuals",main="")
qqline(residuals(stateLM))
#still looks good so far, nothing too crazy, it MIGHT be a short tailed distribution, but this isnt too concerning due to the ultimatey normal principal
hats = hatvalues(stateLM)
states = row.names(sat)
x = halfnorm(hats,labs=states,ylab="leverage")
#Utah and California seem to have something going on. high leverage, > 2 * 7 / 50, and are beyond 2 in the half normal quantiles
studRes = rstudent(stateLM)
studRes[which.max(abs(studRes))]
sort(studRes,decreasing = FALSE)
#west virginia, utah,northDakota,NewHampshire, all have over 2 studentized residuals, west virginia is above 3!
#studentized residuals 
satCook = cooks.distance(stateLM)
halfnorm(satCook,labs=states,ylab="Cook's distance")
#utah has shown up again, very large cooks distance this time around.
#looking at the data, utah has a very low expend value and very high score, and a very low taker score
stateLM2 = lm(total~expend+salary+ratio+takers,subset = satCook < max(satCook),sat)
summary(stateLM)
summary(stateLM2)
termplot(stateLM,partial.resid = TRUE)
#all look pretty good, maybe not takers,
#overall, if utah is removed, we see an improved R squared amongst some other things
#ratio suddenly becomes important when Utah is removed, our model shouldnt be so various from just one observation

constVariance = function(dataset,response)
{
  losers = lm(response ~ .,dataset)
  ggplot(dataset,aes(x=losers$fitted.values,y=losers$residuals))+geom_point() 
  return(losers)
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
tmodel <- constVariance(teengamb,teengamb$gamble)
#this looks suspicious, based on the info given by our textbook, looks like some heteroscedasticity
# we may need to transform our y, so that var(h(y)) is constant, book used square root for counts, we can expirment with this.
constVariance(teengamb,sqrt(teengamb$gamble))
constVariance(teengamb,exp(teengamb$gamble))
constVariance(teengamb,abs(teengamb$gamble))
#none of these really worked
isNormal(tmodel)
#nope, this aint good, looks like a long tailed distribution which means we may need to use robust methods or let central limit therorem take care of this problem
#could also be an outlier, we definetly have one.
seeLeverages(tmodel,teengamb)
#ony lone of concern is unit or person # 24, row 24. They have a decently high leverage.
examineStudRes(tmodel)
#row six has a very very large studentized residual, while row 24 does not. may need to look at row 5
weirdo = getCooks(tmodel,teengamb)
#row6 has shown up again, suspicious stuff
#looking at the data, utah has a very low expend value and very high score, and a very low taker score
tmodel2 = lm(gamble~.,subset = weirdo< max(weirdo),teengamb)
summary(tmodel)
summary(tmodel2)
termplot(stateLM,partial.resid = TRUE)
#all look pretty good, maybe not takers,
#overall, if utah is removed, we see an improved R squared amongst some other things
#ratio suddenly becomes important when Utah is removed, our model shouldnt be so various from just one observation
