##############################
## RC1 PRELIM DATA ANALYSIS ##
##############################


rc1<- read.csv("19RC1 MASTER.csv")
str(rc1)
rc1$Rep<- as.factor(rc1$Rep)

library(ggplot2)
library(Rmisc)

## Rye wt
qplot(x=Trt, y=Ryewt*10, data=rc1, geom="boxplot") + 
  theme_classic()
qplot(x=Rep, y=Ryewt*10, data=rc1, geom="boxplot") + 
  theme_classic()
qplot(x=Trt, y=Ryewt*10,  facets=Rep~., data=rc1) + 
  theme_classic()

# Total weed biomass column
rc1$TotWeedMass<- rowSums(rc1[grep("wt", names(rc1)[-1])+1], na.rm=T)

qplot(x=Trt, y=TotWeedMass, data=rc1, geom="boxplot") +
  theme_classic()
qplot(x=Rep, y=TotWeedMass, data=rc1, geom="boxplot") +
  theme_classic()

# total weed biomass plotted against rye biomass
qplot(x=Ryewt, y=TotWeedMass, data=rc1) +
  theme_classic()
# total weed biomass roughly correlated w/ rye biomass

# total number of tillers column
rc1$Till<- rowSums(rc1[grep("^T\\d", names(rc1)[-1])+1])
# average number of tillers column
rc1$TillAv<- rowMeans(rc1[grep("^T\\d", names(rc1)[-1])+1])

qplot(x=Trt, y=Till, data=rc1, geom="boxplot") +
  theme_classic()
qplot(x=Trt, y=TillAv, data=rc1, geom="boxplot") +
  theme_classic()
qplot(x=Rep, y=Till, data=rc1, geom="boxplot") +
  theme_classic()
qplot(x=Rep, y=TillAv, data=rc1, geom="boxplot") +
  theme_classic()
qplot(x=Till, y=TillAv, data=rc1) +
  theme_classic()
qplot(x=Ryewt, y=TillAv, data=rc1) +
  theme_classic()
# rye wt & number of tillers not really correlated


