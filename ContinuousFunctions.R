library(ape) #utility fns
library(geiger) #utilty fns
library(OUwie)
library(corHMM)
library(phylolm)
library(phytools)
library(rotl)

#You can use code you wrote for the correlation exercise here.


VisualizeData <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
  windows()
  plot(phy)
  print(str(phy))
  print(dim(data))
  windows()
  barplot(data)
  print(table(data))
  windows()
  hist(data[,1])
  windows()
  hist(data[,2])
  plot(phy) 
  }

#treedata() in Geiger is probably my favorite function in R.
CleanData <- function(phy, data) {
	
  Nelumbotree <- treedata(phy, data, sort=TRUE, warnings=TRUE)
  return (Nelumbotree)
}

RunSingleOUwieModel<-function(model, phy, data) {
	print(paste("Now starting model",model))
	return(OUwie(phy, data, model, simmap.tree=FALSE, diagn=FALSE))	
}
