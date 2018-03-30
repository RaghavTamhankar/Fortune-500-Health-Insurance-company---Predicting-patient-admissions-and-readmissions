
# Principal components analysi for Place of service

df2=df[,c(1,363:652)]
dfz2=data.frame(scale(df2))

# filling na values
dfz2=dfz2[colSums(!is.na(dfz2)) > 0]
dfz2[is.na(dfz2)] <- 0

set.seed(1236)
choose<-runif(nrow(dfz2))
train2<-dfz2[which(choose>0.5),]
train2[is.na(train2)] <- 0
train2=train2[colSums(!is.na(train2)) > 0]
test2<-dfz2[which(choose<=0.5),]
install.packages("psych",dependencies=TRUE)

library(psych)
pca2<-principal(train2,nfactors=50,rotate="none",scores=T)
install.packages("nFactors",dependencies=TRUE)
library(nFactors)

# get eigenvalues for dfz2 data and observing scree plot
ev1 <- eigen(cor(dfz2)) 
ap1 <- parallel(subject=nrow(dfz2),var=ncol(dfz2),rep=100,cent=.05)
nS1 <- nScree(x=ev$values, aparallel=ap1$eigen$qevpea)
plotnScree(nS1)

# get eigenvalues for train2 data and observing scree plot
ev2 <- eigen(cor(train2)) # get eigenvalues
ap2 <- parallel(subject=nrow(train2),var=ncol(train2),rep=100,cent=.05)
nS2 <- nScree(x=ev2$values, aparallel=ap2$eigen$qevpea)
plotnScree(nS2)

# get eigenvalues for test data data and observing scree plot
ev <- eigen(cor(test)) # get eigenvalues
ap <- parallel(subject=nrow(test),var=ncol(test),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

