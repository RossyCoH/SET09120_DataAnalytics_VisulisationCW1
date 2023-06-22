library(ggplot2)
library(dplyr)
setwd("D:/Uni/Yr 3 (1)/Data Analytics/Courseworks/coursework 1")
data <- read.csv("dataset.csv")
glimpse(data)
names <- c(1,2,4,7,8)
data[,names] <- lapply(data[,names], factor)
bands<- c("17-27", "28-38", "39-49", "50-60", "61-71", "72-82")
data$Age.Range <- cut(data$Age, breaks=c(17,27,38,49,60,71,82), labels = bands)

newData <- subset(data, Speed < 100)

### NOPES ###
# plot(x = data$Severity, y = data$Age, xlab = "Severity", ylab = "Age", main = "Age & Severity")
# ggplot(data=data, aes(x = Gender, y = Severity)) + geom_bar(stat="identity")

# CAS CLASS
# ggplot(data=data, aes(x=Casualty.Class , y=Speed )) + geom_boxplot()
# ggplot(data=data, aes(x=Casualty.Class , y=Age )) + geom_boxplot()
# ggplot(data=data, aes(x=Casualty.Class , y= Damage)) + geom_boxplot()
# ggplot(data=data, aes(x=Casualty.Class , y= Wind.Speed)) + geom_boxplot()
# ggplot(data=data, aes(x=Casualty.Class , y=Speed, fill=Gender)) + geom_bar(position="dodge", stat="identity")
# ggplot(data=data, aes(x=Casualty.Class , y=Speed, fill=Gender)) + geom_boxplot()
# ggplot(data=data, aes(x=Casualty.Class , y=Speed, fill=Junction.Type)) + geom_bar(position="dodge", stat="identity")

# GENDER
# ggplot(data=data, aes(x=Gender , y=Speed, fill=Road.Type)) + geom_bar(position="dodge", stat="identity")
# ggplot(data=data, aes(x=Gender , y=Damage, fill=Road.Type)) + geom_bar(position="dodge", stat="identity")
# ggplot(data=data, aes(x=Gender , y=Severity, fill=Road.Type)) + geom_bar(position="dodge", stat="identity")
# ggplot(data=data, aes(x=Gender , y=Brightness, fill=Road.Type)) + geom_bar(position="dodge", stat="identity")
# ggplot(data=data, aes(x=Gender , y=Wind.Speed, fill=Road.Type)) + geom_bar(position="dodge", stat="identity")

# YEAR
# ggplot(data=data, aes(x=Year, y=Severity, fill=Road.Type)) + geom_bar(position="dodge", stat="identity")
# ggplot(data=data, aes(x=Year, y=Severity, fill=Road.Type)) + geom_bar(stat="identity")
# ggplot(data=data, aes(x=as.factor(Year), y=Severity, fill=Road.Type)) + geom_bar(stat="identity")
# ggplot(data=data, aes(x=as.factor(Year), y=Severity, fill=Road.Type)) + geom_bar(position="dodge", stat="identity")
# ggplot(data=data, aes(x=as.factor(Year), y=Damage)) + geom_bar(stat="identity")
# ggplot(data=data, aes(x=as.factor(Year), y=Damage, fill=Road.Type)) + geom_bar(position="dodge", stat="identity")
# ggplot(data=data, aes(x=as.factor(Year), y=Speed)) + geom_boxplot()
# ggplot(data=data, aes(x=as.factor(Year), y=Speed, fill=Road.Type)) + geom_boxplot()
# ggplot(data=data, aes(x=as.factor(Year), y=Damage, fill=Road.Type)) + geom_boxplot()
# ggplot(data=data, aes(x=as.factor(Year), y=Severity, fill=Road.Type)) + geom_boxplot()

### Maybe?:
ggplot(data=data, aes(x=Casualty.Class , y= Brightness)) + geom_boxplot() + xlab("Casualty Class") + ylab("Brightness (lux)") + ggtitle("Casualty Classes in Brightnesses")





### Relationships:
# Damage / Severity
plot(x = data$Damage, y= data$Severity, xlab = "Damage (£)", ylab= "Severity (0-100)", main= "Damage & Severity")
#  + geom_boxplot()
ggplot(data=data, aes(x=as.factor(Junction.Type), y=Speed, fill=Age.Range)) + facet_wrap( ~ Casualty.Class) + geom_boxplot()
ggplot(data=data, aes(x=as.factor(Junction.Type), y=Speed, fill=Casualty.Class)) + geom_boxplot()
# Severity / Road Type?
ggplot(data=data, aes(x = Road.Type, y = Severity, fill=Road.Type)) + geom_boxplot() + ggtitle("Severity on Road Types") + xlab("Road Type") + ylab("Severity (0 - 100)")
#
data$Year <- factor(data$Year, levels = c("98", "99", "0", "1", "2"))
ggplot(data=data, aes(x=as.factor(Year), y=Speed, fill=Age.Range)) + facet_wrap( ~ Casualty.Class) + geom_boxplot()
ggplot(data=data, aes(x=as.factor(Year), y=Speed, fill=Casualty.Class)) + geom_boxplot()
ggplot(data=data, aes(x=as.factor(Year), y=Speed)) + facet_wrap( ~ Casualty.Class) + geom_boxplot()

ggplot(data=data, aes(x=Year, y=Severity, fill=Age.Range)) + facet_wrap( ~ Casualty.Class) + geom_boxplot()
ggplot(data=newData, aes(x=Year, y=Severity, fill=Age.Range)) + geom_boxplot()

ggplot(data=data, aes(x=Age.Range, y=Speed, fill=Age.Range)) + facet_wrap( ~ Gender) + geom_boxplot()

#ggplot(data=data, aes(x=Casualty.Class, y=Speed)) + geom_boxplot()

### Outliers:
# Speed
hist(data$Speed, 100, xlab = "Speed (mph)", ylab = "Number of Accidents", main = "Speed in Accidents")
ggplot(data=data, aes(x=Speed)) + geom_histogram(binwidth = 2, color="black", fill="white") + labs(title = "Speed of Accidents", x="Speed (mph)", y="Number of Accidents")
ggplot(data=newData, aes(x=Speed)) + geom_histogram(binwidth = 2, color="black", fill="white") + labs(title = "Speed of Accidents", x="Speed (mph)", y="Number of Accidents")
# Damage
hist(data$Damage, 100)
boxplot(data$Damage)

# VVV This One! VVV
ggplot(data=data, aes(x = Road.Type, y = Damage, fill=Road.Type)) + geom_boxplot() + ggtitle("Damage across Road Types") + xlab("Road Types") + ylab("Damage(£)")
# Severity

### Using Relationships: ###
# Damage / Severity by Road Type
ggplot(data=data, aes(x=Damage, y=Severity, colour=Road.Type, shape=Road.Type)) + geom_point() + labs(title="Damage and Severity of accidents by Road Type", x="Damage (£)", y="Severity (0-100)", color="Road Type", shape="Road Type")
ggplot(data=data, aes(x=Damage, y=Severity)) + facet_wrap( ~ Road.Type) + geom_point() + labs(title="Damage and Severity of accidents by Road Type", x="Damage (£)", y="Severity (0-100)", color="Road Type", shape="Road Type")

# Casualty Class / Brightness
ggplot(data=data, aes(x=Casualty.Class , y= Brightness)) + geom_boxplot() + xlab("Casualty Class") + ylab("Brightness (lux)") + ggtitle("Casualty Classes in Brightnesses")
ggplot(data=data, aes(x=Casualty.Class , y= Brightness)) + geom_boxplot() + xlab("Casualty Class") + ylab("Brightness (lux)") + ggtitle("Casualty Classes in Brightnesses")

# Road Type / Age / Gender OR Age / Severity / Gender / Road Type
ggplot(data=data, aes(x=Age.Range, y=Severity, fill=as.factor(Gender))) + facet_wrap( ~ Road.Type) + geom_boxplot() + xlab("Age") + ylab("Severity (0-100)") + ggtitle("Severity of accidents for Ages and Genders by Road Type")
ggplot(data=data, aes(x=Road.Type, y=Age, fill=as.factor(Gender))) + geom_boxplot() + labs(title = "Number of Accidents on each Road Type by Age & Gender", x="Road Type", y="Age", fill="Gender")
ggplot(data=data, aes(x=Road.Type, y=Age)) + geom_boxplot() + labs(title = "Number of Accidents on each Road Type by Age & Gender", x="Road Type", y="Age")

# Year / Speed / Junction Type / Casualty Class
yearLabels <- c("1998", "1999", "2000", "2001", "2002")
ggplot(data=data, aes(x=as.factor(Year), y=Speed, fill=Junction.Type)) + facet_wrap( ~ Casualty.Class) + geom_boxplot() + labs(title="Speed of Accidents by Year, Junction Type & Casualty Class", x="Year", y="Speed (mph)") + scale_fill_discrete(name = "Junction Type", labels = c("Roundabout", "Crossroad", "T or Y", "Slip Road", "Other")) + scale_x_discrete(labels=yearLabels)
ggplot(data=newData, aes(x=as.factor(Year), y=Speed, fill=Junction.Type)) + facet_wrap( ~ Casualty.Class) + geom_boxplot() + labs(title="Speed of Accidents by Year, Junction Type & Casualty Class", x="Year", y="Speed (mph)") + scale_fill_discrete(name = "Junction Type", labels = c("Roundabout", "Crossroad", "T or Y", "Slip Road", "Other")) + scale_x_discrete(labels=yearLabels)

ggplot(data=data, aes(x=as.factor(Year), y=Speed, fill=Junction.Type)) + geom_boxplot() + labs(title="Speed of Accidents by Year, Junction Type & Casualty Class", x="Year", y="Speed (mph)")  + scale_x_discrete(labels=yearLabels)

# 
ggplot(data=data, aes(x=Age.Range, y=Severity)) + geom_boxplot() + labs(title = "Age and Severity of accidents", x="Age Range", y="Severity")
ggplot(data=data, aes(x=Age.Range, y=Severity, fill=Gender)) + geom_boxplot() + labs(title = "Age and Severity of accidents", x="Age Range", y="Severity")
ggplot(data=data, aes(x=Age.Range, y=Severity, fill=Gender)) + facet_wrap( ~ Junction.Type) + geom_boxplot() + labs(title = "Age and Severity of accidents by Gender", x="Age Range", y="Severity (0-100)", fill="Gender")
