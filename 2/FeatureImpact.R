## ------------------------------------------------------------------------
library('lattice')
Bikeshare <- read.csv(url("https://drive.google.com/uc?export=download&id=1QwGCiIbESsE1RVjhKu9nivgvfyYFewDG"),na.strings = '?')
Bikeshare$Weekend=Bikeshare$Weekday==5 |Bikeshare$Weekday==6


## ------------------------------------------------------------------------

model <- lm(Rentals~Temperature+ Humidity +Windspeed+Weekend ,data=Bikeshare)
summary(model)

## ------------------------------------------------------------------------

model <- lm(Registered~Temperature+ Humidity +Windspeed+Weekend ,data=Bikeshare)
summary(model)

## ------------------------------------------------------------------------

model <- lm(Casual~Temperature+ Humidity +Windspeed+Weekend ,data=Bikeshare)
summary(model)

## ------------------------------------------------------------------------
Autoloss <- read.csv(url("https://drive.google.com/uc?export=download&id=1-QuNWq7k4w3c8kBJ8BedIK1m-cpwORiV"),na.strings = '?')
Autoloss <-na.omit (Autoloss)
ByDoors=tapply(Autoloss$Losses, Autoloss$NumDoors, mean)

## ------------------------------------------------------------------------
library(leaps)

regfit.full=regsubsets(Losses~., data = Autoloss)
summary=summary(regfit.full)
summary$rsq

## ------------------------------------------------------------------------
function(Gender, IQ, GPA){
  return (45 + Gender*30+IQ*.05,+20*GPA)
}

