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

setwd("C:/Users/BDVR/Documents/Github/regressions/hw7")
savePlot = function(name,myPlot)

  
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