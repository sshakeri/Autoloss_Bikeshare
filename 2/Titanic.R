## ------------------------------------------------------------------------
Titanic <- read.csv("TitanicforLogReg.csv")
Titanic

## ------------------------------------------------------------------------
x=data.frame(model.matrix(~.,data=Titanic)[,-1])

## ------------------------------------------------------------------------
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2) # split data into two subsets
test=-train
x.train=x[train,]
x.test=x[test,]

## ------------------------------------------------------------------------

prop.table(table(x.train$Survived))

## ------------------------------------------------------------------------
prop.table(table(x.test$Survived))

## ------------------------------------------------------------------------
library(glmnet)
model=glm(Survived~.,data=x.train )
summary(model)
  

## ------------------------------------------------------------------------
model=glm(formula = Survived ~ ClassTwo + ClassThree+ Gendermale + Child, data = x.train)
summary(model)

## ------------------------------------------------------------------------
glm.probs=predict(model,newdata=x.test,type="response")
options("digits"=2)
glm.probs[1:5]

glm.pred=rep(0,nrow(x.test))
glm.pred[glm.probs>.5]="1"

glm.pred


## ------------------------------------------------------------------------
table(glm.pred,x.test$Survived)

mean(glm.pred==x.test$Survived)

## ------------------------------------------------------------------------
Titanic <- read.csv("TitanicforKNN.csv")
Titanic
#x=data.frame(model.matrix(~.,data=Titanic)[,-1])
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2) # split data into two subsets
test=-train
x.train=x[train,]
x.test=x[test,]

## ------------------------------------------------------------------------
library(class)
 
set.seed(1)
train=x.train[,-1]
test=x.test[,-1]
trainY=x.train$Survived
testY=x.test$Survived
# run knn with K=1 and store prediction results in a variable called knn.pred
knn.1.pred = knn(train,test,trainY,k=1)
knn.3.pred = knn(train,test,trainY,k=3)
knn.5.pred = knn(train,test,trainY,k=5)

prop.table(table(x.test$Survived))



## ------------------------------------------------------------------------
options("digits"=4)
table(knn.1.pred,testY)
mean(knn.1.pred==testY)


## ------------------------------------------------------------------------
table(knn.3.pred,testY)
mean(knn.3.pred==testY)


## ------------------------------------------------------------------------
table(knn.5.pred,testY)
mean(knn.5.pred==testY)


