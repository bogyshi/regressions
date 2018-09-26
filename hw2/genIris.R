irisData = read.delim("C:/Users/BDVR/Documents/Spring2018/regressions/datasets/iris.txt",TRUE,' ')
sepalWidths = irisData$Sepal.Width
sepalLengths = irisData$Sepal.Length
species = irisData$Species
png("C:/Users/BDVR/Documents/Spring2018/regressions/hw2/iris.png")
plot(sepalWidths,sepalLengths,pch=c(10,20,5)[as.numeric(species)],col=c("orange","purple","green")[as.numeric(species)])
legend("topleft",legend = irisData$Species[!duplicated(irisData$Species)],col=c("orange","purple","green"),pch=c(10,20,5))
dev.off()