##############################
## RC1 PRELIM DATA ANALYSIS ##
##############################


rc2<- read.csv("19RC2 MASTER.csv")
str(rc2)
rc2$Rep<- as.factor(rc2$Rep)

library(ggplot2)
library(Rmisc)

## Rye wt
qplot(x=Trt, y=Ryewt*10, data=rc2, geom="boxplot") + 
  theme_classic()
qplot(x=Rep, y=Ryewt*10, data=rc2, geom="boxplot") + 
  theme_classic()
qplot(x=Trt, y=Ryewt*10,  facets=Rep~., data=rc2) + 
  theme_classic()

# Total weed biomass column
rc2$TotWeedMass<- rowSums(rc2[grep("wt", names(rc2)[-1])+1], na.rm=T)

qplot(x=Trt, y=TotWeedMass, data=rc2, geom="boxplot") +
  theme_classic()
qplot(x=Rep, y=TotWeedMass, data=rc2, geom="boxplot") +
  theme_classic()

# total weed biomass plotted against rye biomass
qplot(x=Ryewt, y=TotWeedMass, data=rc2) +
  theme_classic()
# total weed biomass slightly correlated w/ rye wt but not as strongly as RC1

# total number of tillers column
rc2$Till<- rowSums(rc2[grep("^T\\d", names(rc2)[-1])+1])
# average number of tillers column
rc2$TillAv<- rowMeans(rc2[grep("^T\\d", names(rc2)[-1])+1])

qplot(x=Trt, y=Till, data=rc2, geom="boxplot") +
  theme_classic()
qplot(x=Trt, y=TillAv, data=rc2, geom="boxplot") +
  theme_classic()
qplot(x=Rep, y=Till, data=rc2, geom="boxplot") +
  theme_classic()
qplot(x=Rep, y=TillAv, data=rc2, geom="boxplot") +
  theme_classic()
qplot(x=Till, y=TillAv, data=rc2) +
  theme_classic()
qplot(x=Ryewt, y=TillAv, data=rc2) +
  theme_classic()
# rye wt & number of tillers not really correlated


