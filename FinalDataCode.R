## Load Data
Commercial <- read.csv("C:/Users/kosti/OneDrive/Desktop/School/Econ Classes/EC 499/Data/RCI/Commercial.csv")
Residential <- read.csv("C:/Users/kosti/OneDrive/Desktop/School/Econ Classes/EC 499/Data/RCI/Residential.csv")
Industrial <- read.csv("C:/Users/kosti/OneDrive/Desktop/School/Econ Classes/EC 499/Data/RCI/Industrial.csv")
Transportation <- read.csv("C:/Users/kosti/OneDrive/Desktop/School/Econ Classes/EC 499/Data/RCI/Transportation.csv")

Stringency <- read.csv("C:/Users/kosti/OneDrive/Desktop/School/Econ Classes/EC 499/Data/Strigency/Strigency_Final.csv")

Temp <- read.csv("C:/Users/kosti/OneDrive/Desktop/School/Econ Classes/EC 499/Data/Temp/50StateTemp.csv") 
GDP <- read.csv("C:/Users/kosti/OneDrive/Desktop/School/Econ Classes/EC 499/Data/GDP/AllStateGDP.csv") 

library(zoo)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lmtest)
library(psych)
library(broom)

## Edit GDP
GDP$NewDate = as.Date(GDP$DATE, "%m/%d/%Y")
GDP$Month = as.numeric(strftime(GDP$NewDate, "%m"))
GDP$Year = as.numeric(strftime(GDP$NewDate, "%Y"))
GDP$State = as.factor(GDP$State)

## Edit Temp
Temp$Month = as.numeric(substr(Temp$Date,5,6))
Temp$Year = as.numeric(substr(Temp$Date,1,4))
Temp$State = as.factor(Temp$State)



## Make GDP Monthly

#GDP$Quarter <- ceiling(as.numeric(GDP$Month) / 3)

#library(lubridate)
#GDP <- GDP[rep(1:.N, each = 3)]
#GDP[, quarter := dates]
#GDP[, dates := {day(dates) <- 1; dates}]
#GDP[, dates := {month(dates) <- month(dates) - 1:(-1); dates}, by = quarter]
#DP[, dates := dates -1]

                      
## Make Quarterly
TempQ = Temp %>% filter(Month == "1"| Month == "4" | Month == "7"| Month == "10")
ResQ = Residential %>% filter(Month == "1"| Month == "4" | Month == "7"| Month == "10")
CommQ = Commercial %>% filter(Month == "1"| Month == "4" | Month == "7"| Month == "10")
IndQ = Industrial %>% filter(Month == "1"| Month == "4" | Month == "7"| Month == "10")
TranQ = Transportation %>% filter(Month == "1"| Month == "4" | Month == "7"| Month == "10")
StringencyQ = Stringency %>% filter(Month == "1"| Month == "4" | Month == "7"| Month == "10")
                        

## Make numeric


ResQ$Customers.Count = gsub(",", "",ResQ$Customers.Count) 
ResQ$Customers.Count = as.numeric(ResQ$Customers.Count)
ResQ$Price = gsub(",", "",ResQ$Price) 
ResQ$Price = as.numeric(ResQ$Price)
ResQ$Sales = gsub(",", "",ResQ$Sales) 
ResQ$Sales = as.numeric(ResQ$Sales)
ResQ$Revenue = gsub(",", "",ResQ$Revenue) 
ResQ$Revenue = as.numeric(ResQ$Revenue)

CommQ$Customers.Count = gsub(",", "",CommQ$Customers.Count) 
CommQ$Customers.Count = as.numeric(CommQ$Customers.Count)
CommQ$Price = gsub(",", "",CommQ$Price) 
CommQ$Price = as.numeric(CommQ$Price)
CommQ$Sales = gsub(",", "",CommQ$Sales) 
CommQ$Sales = as.numeric(CommQ$Sales)
CommQ$Revenue = gsub(",", "",CommQ$Revenue) 
CommQ$Revenue = as.numeric(CommQ$Revenue)

IndQ$Customers.Count = gsub(",", "",IndQ$Customers.Count) 
IndQ$Customers.Count = as.numeric(IndQ$Customers.Count)
IndQ$Price = gsub(",", "",IndQ$Price) 
IndQ$Price = as.numeric(IndQ$Price)
IndQ$Sales = gsub(",", "",IndQ$Sales) 
IndQ$Sales = as.numeric(IndQ$Sales)
IndQ$Revenue = gsub(",", "",IndQ$Revenue) 
IndQ$Revenue = as.numeric(IndQ$Revenue)

TranQ$Customers.Count = gsub(",", "",TranQ$Customers.Count) 
TranQ$Customers.Count = as.numeric(TranQ$Customers.Count)
TranQ$Price = gsub(",", "",TranQ$Price) 
TranQ$Price = as.numeric(TranQ$Price)
TranQ$Sales = gsub(",", "",TranQ$Sales) 
TranQ$Sales = as.numeric(TranQ$Sales)
TranQ$Revenue = gsub(",", "",TranQ$Revenue) 
TranQ$Revenue = as.numeric(TranQ$Revenue)

##Edit Strigency
StringencyQ$State = substring(StringencyQ$RegionCode, 4)
StringencyQ[is.na(StringencyQ)] = 0
DiDStringency = StringencyQ
PostStringency = StringencyQ %>% filter(Year >2019)

## merge data

ResMerged <- merge(TempQ,ResQ,by=c("Year","Month","State"))
ResMerged <- merge(ResMerged,GDP,by=c("Year","Month","State"))
ResMerged$Temp = ResMerged$Value

ResMerged$Sales = gsub(",", "",ResMerged$Sales) 
ResMerged$Sales = as.numeric(ResMerged$Sales)

CommMerged <- merge(TempQ,CommQ,by=c("Year","Month","State"))
CommMerged <- merge(CommMerged,GDP,by=c("Year","Month","State"))
CommMerged$Temp = CommMerged$Value

CommMerged$Sales = gsub(",", "",CommMerged$Sales) 
CommMerged$Sales = as.numeric(CommMerged$Sales)

IndMerged <- merge(TempQ,IndQ,by=c("Year","Month","State"))
IndMerged <- merge(IndMerged,GDP,by=c("Year","Month","State"))
IndMerged$Temp = IndMerged$Value

IndMerged$Sales = gsub(",", "",IndMerged$Sales) 
IndMerged$Sales = as.numeric(IndMerged$Sales)

TranMerged <- merge(TempQ,TranQ,by=c("Year","Month","State"))
TranMerged <- merge(TranMerged,GDP,by=c("Year","Month","State"))
TranMerged$Temp = TranMerged$Value

TranMerged$Sales = gsub(",", "",TranMerged$Sales) 
TranMerged$Sales = as.numeric(TranMerged$Sales)

## Simple Linear Model
ResModel1 = lm(Sales ~ Temp + GDP, ResMerged)
ResModel2 = lm(Sales ~ abs(Temp-65) + GDP, ResMerged)

CommModel1 = lm(Sales ~ Temp + GDP, CommMerged)
CommModel2 = lm(Sales ~ abs(Temp-65) + GDP, CommMerged)

IndModel1 = lm(Sales ~ Temp + GDP, IndMerged)
IndModel2 = lm(Sales ~ abs(Temp-65) + GDP, IndMerged)

TranModel1 = lm(Sales ~ Temp + GDP, TranMerged)
TranModel2 = lm(Sales ~ abs(Temp-65) + GDP, TranMerged)

##Stringency Final Model

ResMerged2 <- merge(ResMerged,PostStringency,by=c("Year","Month","State"))
CommMerged2 <- merge(CommMerged,PostStringency,by=c("Year","Month","State"))
IndMerged2 <- merge(CommMerged,PostStringency,by=c("Year","Month","State"))
TranMerged2 <- merge(TranMerged,PostStringency,by=c("Year","Month","State"))

ResModel3 = lm(Sales ~ abs(Temp-65) + GDP + StringencyIndex + as.factor(State) + as.factor(NewDate), ResMerged2)
CommModel3 = lm(Sales ~ abs(Temp-65) + GDP + StringencyIndex + as.factor(State) + as.factor(NewDate), CommMerged2)
IndModel3 = lm(Sales ~ abs(Temp-65) + GDP + StringencyIndex + as.factor(State) + as.factor(NewDate), IndMerged2)
TranModel3 = lm(Sales ~ abs(Temp-65) + GDP + StringencyIndex + as.factor(State) + as.factor(NewDate), TranMerged2)

summary(ResModel3)[c("(Intercept)", "abs(Temp - 65)", "GDP", "StringencyIndex")]
coef(ResModel3)[c("(Intercept)", "abs(Temp - 65)", "GDP", "StringencyIndex")]

ResTable = summary(ResModel3)$coefficients[ !grepl("State", names(coef(ResModel3)) ) , ,drop=FALSE]

ResTable = stargazer::stargazer(ResModel3,
                     type = "text",
                     omit = c("State", "NewDate"))

tidy_rm3 <- tidy(ResModel3)
tidy_rm3
write.csv(tidy_rm3, "tidy_rm3.csv")

## Summary Stats
ResSumStat = describe(ResQ)


summary(ResQ)
summary(CommQ)
summary(IndQ)
summary(TranQ)

summary(GDP)
summary(TempQ)
summary(PostStringency)

##Visuals for Paper
UsAve = ResMerged2 %>%
  group_by(NewDate) %>%
  summarize(StringencyIndex = mean(StringencyIndex))

UsAve$State = "Ave US"


Ave = ResMerged2 %>%
  filter(State == "TX" | State == "IA" | State == "ND" | State == "FL" | State == "MI" | State == "VT" | State == "CA" |State == "LA" |State == "MA" ) %>%
  select(NewDate, State, StringencyIndex)
  

  


plot1 <- ggplot(data = Ave, mapping = aes(x = NewDate, y=StringencyIndex,  color = State)) +
  geom_line() +
  facet_wrap(vars(State), ncol = 11) +labs(x = "Month", y = "Lockdown", title = "Stringency Level Per State", fill = "State") +
  labs(colour = "State")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  geom_line(mapping = aes(x= NewDate, y= StringencyIndex), data = UsAve) 
plot1



plot2 <- ggplot(data = ResSumStat, mapping = aes(x = NewDate, y=Consumption)) +
  geom_line() +
  labs( x = "Date", y = "Residential Consumption", title = "Residential Consumption over Time") 
  #geom_vline(mapping = aes(x = 2020/03/01), color = "red")
plot2

CommSumStat = CommMerged3 %>%
  group_by(NewDate) %>%
  summarize(Consumption = mean(Sales))

plot3 <- ggplot(data = CommSumStat, mapping = aes(x = NewDate, y=Consumption)) +
  geom_line() + 
  labs( x = "Date", y = "Commercial Consumption", title = "Commercial Consumption over Time")
plot3








### interacting temp with stringency
### DID with interacting stringency 
### One paragraph on each data set
### clustering standard errors for state
## Make stringency grpahs comparing states and US average
## Make aggregate consumption data, residential data