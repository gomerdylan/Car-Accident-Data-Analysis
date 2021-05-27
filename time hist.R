CarData <- read.csv(file = 'C:\\Users\\Dylan Gomer\\Car-Accident-Data-Analysis\\motor-vehicle-crash-reports-1.csv')
library(ggplot2)
Time1=CarData[,2]
DayTime=as.integer(format(strptime(Time1,"%H:%M"),'%H'))

#(CarData[2,], geom="histogram")
hist(DayTime, breaks=23)
PeopleInjured = CarData[,41]
hist(PeopleInjured)

#This initializes the length of TimeGivenInjury (time of day given an injury occurs)
count <- 0
for (x in 1:length(PeopleInjured)){
  if (PeopleInjured[x] > 0){
    count <- count+1
  }
}

#This creates TimeGivenInjury (time of day given an injury occurs)
TimeGivenInjury=integer(count)
k <- 1
for (x in 1:length(PeopleInjured)){
  if (PeopleInjured[x] > 0){
    TimeGivenInjury[k] <- DayTime[x]
    k <- k+1
  }
}
#hist(TimeGivenInjury, breaks=23)

#This is a user input function to choose the collision type
UserInput <- function() {
  while (1==1){
    CollisionType <- readline(prompt="Choose a collision type \n 1 for Single Vehicle Crash\n 2 for Rear-end\n 3 for Angle\n 4 for Sideswipe Same Direction\n 5 for Sideswipe Opposite Direction\n 6 for Head on\n 7 for Rear to Rear\n 99 for unknown\n")
    CollisionType <<- as.numeric(CollisionType)
    if (CollisionType==1|CollisionType==2|CollisionType==3|CollisionType==4|CollisionType==5|CollisionType==6|CollisionType==7|CollisionType==99){
      break
    }
    print("That is an invalid entry")
  }
}

#This is a function used to clean data
Clean <- function(Weather,ColumnNumber){
  for (i in 1:length(Weather)){
    if (Weather[i] == "" | Weather[i]=="unk" | Weather[i]=="signal" | Weather[i]=="31" | Weather[i] == "12" |Weather[i] == "10" |Weather[i] == "sideswipe (opp dir)" | Weather[i] == "angle (3?)"){
      Weather[i] <- 99
    }
    if (Weather[i] == "wet"){
      Weather[i] <- 2
    }
    for (j in 1:8){
      if (as.numeric(substr(Weather[i],1,1))==j){
        Weather[i] <- j
      }
    }
  }
  CarData[,ColumnNumber] <<- as.numeric(Weather)
}

#This function creates a histogram of road conditions based off of the collision type
#Takes in the collision type inputted by the user and the CarData Table

ConditionsBarChart <- function(CollisionType, CarData){
  for (x in 1:length(CarData[,7])){
    Counter <- 0
    if (CarData[x,11]==CollisionType){
      Counter <- Counter+1
    }
  }
  HistData <<- integer(Counter)
  Counter <- 0
  for (x in 1:length(CarData[,7])){
    if (CarData[x,11]==CollisionType){
      Counter <- Counter+1
      HistData[Counter] <<- CarData[x,7]

    }
  }
  barplot(unique(CarData[,7]),HistData)
}


Clean(CarData[,7],7)
Clean(CarData[,11],11)
UserInput()
ConditionsBarChart(CollisionType, CarData)
