batting <- read.csv('Batting.csv')
head(batting)

str(batting)

head(batting$AB)
head(batting$X2B)

batting$BA <- batting$H/batting$AB
tail(batting$BA,5)

batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

batting$SLG <- (batting$X1B + 2*batting$X2B + 3*batting$X3B + 4*batting$HR)/batting$AB

str(batting)


sal <- read.csv('Salaries.csv')

batting <- subset(batting, yearID >= 1985)

combo <- merge(x = batting, y= sal,by = c('playerID','yearID'))
summary(combo)

# Analyzing the Lost Players

lost_players <- subset(combo, subset = playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players <- subset(lost_players, yearID == 2001)
combo <- subset(combo, yearID == 2001)

lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]

# Replacement Players
library(dplyr)

ggplot(combo, aes(x=OBP,y=salary)) + geom_point(size=2)
combo <- subset(combo,salary < 8000000 & OBP > 0 & AB >= 450)
str(combo)


con1 <- 15*10^6
con2 <- sum(select(lost_players, AB))
con3 <- sum(select(lost_players, OBP))/3

options <- head(arrange(combo,OBP),10)
options[,c('playerID','AB','salary','OBP')]

# 3 Replacement Players 

# heltoto01 
# berkmala01
# gonzalu01

# since thses players have above avg of OBP with reasonable salary