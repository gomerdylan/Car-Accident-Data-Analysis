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
k <- 1
for (x in 1:length(PeopleInjured)){
  if (PeopleInjured[x] > 0){
    TimeGivenInjury[k] <- DayTime[x]
    k <- k+1
  }
}
hist(TimeGivenInjury, breaks=23)
UserInput <- function() {
  while (1==1){
    my.name <- readline(prompt="Choose a collision type \n 1 for Single Vehicle Crash\n 2 for Rear-end\n 3 for Angle\n 4 for Sideswipe Same Direction\n 5 for Sideswipe Opposite Direction\n 6 for Head on\n 7 for Rear to Rear\n 99 for unknown\n")
    my.name <<- as.numeric(my.name)
    if (my.name==1|my.name==3|my.name==4|my.name==5|my.name==6|my.name==7|my.name==99){
      break
    }
    print("That is an invalid entry")
  }
}
UserInput()