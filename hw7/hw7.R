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
{
  pdf(name)
  print(myPlot)
  dev.off()
}
