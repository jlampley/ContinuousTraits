#You can use code you wrote for the correlation exercise here.
source("C:\\Users\\JAL\\Desktop\\PhyloMeth\\ContinuousTraits\\ContinuousFunctions.R")
phy <- get_study_tree("ot_485", "tree1")

continuoustrait1 <- sim.char (phy,0.3, 1, model="BM")
taxa.count <- length(continuoustrait1)
print(paste("number of taxa = ", taxa.count))
cleaned.continuous1 <- CleanData(phy, continuoustrait1)
data <- cleaned.continuous1$data
VisualizeData(phy, data)
name.check(phy,data)

angi.trait <- as.vector(read.csv(file="C:\\Users\\JAL\\Desktop\\PhyloMeth\\angitrait.csv",header=F)[,1])
#angi.trait, I coded 0 to be non-angiosperm, and 1 to be angiosperm-JL.

ouwie.matrix <- matrix(data=NA, nrow=taxa.count, ncol=3)
colnames(ouwie.matrix) <- c("Var1", "Var2", "Var3")
ouwie.matrix [,1] <- rownames(continuoustrait1)
ouwie.matrix [,2] <- angi.trait
ouwie.matrix [,3] <- as.vector(as.data.frame(continuoustrait1)[,1])

#First, start basic. What is the rate of evolution of your trait on the tree? 

BM1 <- fitContinuous(phy, cleaned.continuous1, model="BM")
BM1 <- fitContinuous(phy, data, model="BM")
q<-ratematrix(phy, data)

print(paste("The rate of evolution is", BM1$beta, "in units of", 
            "variance of phenotypic change/time"))
#Important: What are the rates of evolution? In what units?




#OUwie runs:
#This takes longer than you may be used to. 
#We're a bit obsessive about doing multiple starts and in general
#performing a thorough numerical search. It took you 3+ years
#to get the data, may as well take an extra five minutes to 
#get an accurate answer
nodeBased.OUMV <- OUwie(phy,ouwie.matrix[,2], model="OUMV", simmap.tree=FALSE, diagn=FALSE)
print(nodeBased.OUMV)
#What do the numbers mean?

#Now run all OUwie models:
models <- c("BM1","BMS","OU1","OUM","OUMV","OUMA","OUMVA")
results <- lapply(models, RunSingleOUwieModel, phy=tree, data=trait)

