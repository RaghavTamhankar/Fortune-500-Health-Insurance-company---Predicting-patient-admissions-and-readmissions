PRINCIPAL COMPONENT ANALYSIS and CORRELATION for disease type:

df <- read.table("C:/Users/carlvader/Desktop/MSBAIM/Projects/data.csv", header=TRUE, sep=",")
df1=df[,c(1,107:362)]
dfz=data.frame(scale(df1))

set.seed(1234)
choose<-runif(nrow(dfz))
train<-dfz[which(choose>0.5),]
test<-dfz[which(choose<=0.5),]

## fill na values
test=test[colSums(!is.na(test)) > 0]
test[is.na(test)] <- 0

install.packages("psych",dependencies=TRUE)
library(psych)
names(train)
pca1<-principal(train,nfactors=50,rotate="none",scores=T)
dfpca <- as.data.frame(pca1$loadings)
pca2 <- principal(test,nfactors = 50, rotate = "none", scores = T)

pca1$values
pca1$loadings

par(mfrow=c(1,2))
plot(pca1$values)
plot(pca2$values)
gimputedValues <- mice(data=Hitters, method="cart", seed=2016) 

# create a new dataset that has imputed values 

Hitters <- complete(imputedValues,1)

install.packages("nFactors",dependencies=TRUE)
library(nFactors)

# get eigenvalues for train data and checking scree plot
ev <- eigen(cor(dfz)) 
ap <- parallel(subject=nrow(dfz),var=ncol(dfz),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# get eigenvalues for train data
ev <- eigen(cor(train)) 
ap <- parallel(subject=nrow(train),var=ncol(train),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# get eigenvalues for test data and check scree plot
ev <- eigen(cor(test)) 
ap <- parallel(subject=nrow(test),var=ncol(test),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

