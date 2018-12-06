## ------------------------------------------------------------------------
Autoloss <- read.csv(url("https://drive.google.com/uc?export=download&id=1-QuNWq7k4w3c8kBJ8BedIK1m-cpwORiV"),na.strings = '?')
Autoloss <-na.omit (Autoloss)

library(leaps)
regfit.fwd=regsubsets(Losses~., data = Autoloss,nvmax=15, method="forward")
summary=summary(regfit.fwd)
preds=summary$which[10,][-1]
names(preds)[preds==TRUE]


## ------------------------------------------------------------------------
min=which.min(summary$cp)
min
preds=summary$which[min,][-1]
names(preds)[preds==TRUE]


## ------------------------------------------------------------------------
library(glmnet)
set.seed(1)
x=model.matrix(Losses~.,data=Autoloss)[,-1]
y=Autoloss$Losses
cv.out=cv.glmnet(x,y,alpha=1,nfolds=5)
bestLam=cv.out$lambda.min
bestLam

## ------------------------------------------------------------------------
cv.out=glmnet(x,y,alpha=1,lambda =  bestLam )
coefs=coef(cv.out)
coefs

