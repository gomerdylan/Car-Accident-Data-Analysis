CarData <- read.csv(file = 'motor-vehicle-crash-reports-1.csv')
library(ggplot2)
Time1=CarData[,2]
DayTime=as.integer(format(strptime(Time1,"%H:%M"),'%H'))

#(CarData[2,], geom="histogram")
hist(DayTime, breaks=23)
PeopleInjured = CarData[,41]
hist(PeopleInjured)
#This initializes the length of TimeGivenInjury
count <- 0
for (x in 1:length(PeopleInjured)){
  if (PeopleInjured[x] > 0){
    count <- count+1
  }
}
#This creates TimeGivenInjury
TimeGivenInjury=integer(count)
for (x in 1:length(PeopleInjured)){
  if (PeopleInjured[x] > 0){
    TimeGivenInjury[x] <- DayTime[x]
  }
}
hist(TimeGivenInjury, breaks=23)