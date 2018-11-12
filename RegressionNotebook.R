## ------------------------------------------------------------------------
library('lattice')
Autoloss <- read.csv(url("https://drive.google.com/uc?export=download&id=1cbk_KGdo5sfY0c4_ALB5ULTeYQ87uU6k"),na.strings = '?')
Autoloss <-na.omit (Autoloss)
ByDoors=tapply(Autoloss$Losses, Autoloss$NumDoors, mean)
barplot(ByDoors, col = rainbow(15),xlab="Number of Doors",ylab = "Loss",ylim=c(0,150))


## ------------------------------------------------------------------------
Lowest<- Autoloss[order(Autoloss$Losses,decreasing=FALSE),][1:10,]
Lowest<-tapply(Lowest$Losses,Lowest$BodyStyle , mean)
Lowest


## ------------------------------------------------------------------------
ByWheels<-tapply(Autoloss$Losses,Autoloss$BodyStyle,mean)
ByWheels
boxplot(Losses~BodyStyle,data=Autoloss ,col = rainbow(15))


## ------------------------------------------------------------------------
xyplot(Losses~Price,data=Autoloss, col = rainbow(50))

## ------------------------------------------------------------------------
Bikeshare <- read.csv(url("https://drive.google.com/uc?export=download&id=1jtfh-qmyDM3l_nU3AkPP_ZsepL-jHW8I"),na.strings = '?')
LModel=lm(Rentals~Temperature,data=Bikeshare)
{plot(Bikeshare$Temperature,Bikeshare$Rentals,col='blue') 
abline(LModel,col="red" )}

## ------------------------------------------------------------------------
summary(LModel)

## ------------------------------------------------------------------------
LModel=lm(Rentals~Humidity,data=Bikeshare)
{plot(Bikeshare$Humidity,Bikeshare$Rentals,col='blue') 
abline(LModel,col="red" )}

## ------------------------------------------------------------------------
summary(LModel)

## ------------------------------------------------------------------------
LModel=lm(Rentals~Windspeed,data=Bikeshare)
{plot(Bikeshare$Windspeed,Bikeshare$Rentals,col='blue') 
abline(LModel,col="red" )}

## ------------------------------------------------------------------------
summary(LModel)

## ------------------------------------------------------------------------
pairs(Rentals~Temperature+ Humidity +Windspeed ,data=Bikeshare)

