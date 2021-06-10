############################## Final R Script ##############################

########## Ken Hubbard, Tianpei Hu, Ethan Alpern, Patricio Rivera ##########
 
###### Packages ######
library(jsonlite)
library(ggmap)
library(ggplot2)
library(arules)
library(arulesViz)
library(fastDummies)
library(stringr)
library(quanteda)
library(tm)
library(wordcloud)
library(kernlab)
library(tidyr)
library(tidyverse)

###### Pre-Analysis ######

#Loads the data into a dataset
mydata.list <- jsonlite::fromJSON("completeSurvey.json")
survey <- data.frame(mydata.list)
str(survey)

#Creates df1.Recommend.type column
Recommend.type <- ifelse(survey$Likelihood.to.recommend<7,"Detractor","Promoter")
df1 <- data.frame(survey$Likelihood.to.recommend,Recommend.type)
df1$Recommend.type[which(df1$survey.Likelihood.to.recommend==7)] <- "Passive"
df1$Recommend.type[which(df1$survey.Likelihood.to.recommend==8)] <- "Passive"
survey2 <- data.frame(survey,df1$Recommend.type)
View(survey2)

#Creates Age Group column
Age.group <- ifelse(survey2$Age<24,"teenager","adult")
df2 <- data.frame(survey2$Age,Age.group)
df2$Age.group[which(df2$survey2.Age>55)] <- "elder"
survey2 <- data.frame(survey2,df2$Age.group)
tabAgeRec <- table(survey2$df1.Recommend.type,survey2$df2.Age.group)
tabAgeRec

#Creates different filtered datasets: Promoter, Detractor, and Both
PromoterSurvey <- survey2 %>% filter(df1.Recommend.type == "Promoter")
View(PromoterSurvey)
DetractorSurvey <- survey2 %>% filter(df1.Recommend.type == "Detractor")
View(DetractorSurvey)
BothSurvey <- survey2 %>% filter(df1.Recommend.type == "Promoter" | 
                                   df1.Recommend.type == "Detractor")
View(BothSurvey)



###### 4A ######

#######Histograms and box plots for each numeric variable ##########

#Age 
hist(airData$Age)
ggplot(airData, aes(x=Age)) + geom_histogram(bins = 15, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Age)) + geom_boxplot()

#Price Sensitivity 
hist(airData$Price.Sensitivity)
ggplot(airData, aes(x=Price.Sensitivity)) + geom_histogram(bins = 5, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Price.Sensitivity)) + geom_boxplot() 

#Year of First Flights 
hist(airData$Year.of.First.Flight)
ggplot(airData, aes(x=Year.of.First.Flight)) + geom_histogram(bins = 10, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Year.of.First.Flight)) + geom_boxplot()

#Flights Per Year 
hist(airData$Flights.Per.Year)
ggplot(airData, aes(x=Flights.Per.Year)) + geom_histogram(bins = 10, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Flights.Per.Year)) + geom_boxplot()

#Loyalty 
hist(airData$Loyalty)
ggplot(airData, aes(x=Loyalty)) + geom_histogram(bins = 10, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Loyalty)) + geom_boxplot()

#Total Frequent Flyer Accounts 
hist(airData$Total.Freq.Flyer.Accts)
ggplot(airData, aes(x=Total.Freq.Flyer.Accts)) + geom_histogram(bins = 5, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Total.Freq.Flyer.Accts)) + geom_boxplot()

#Shopping Amount at Airport 
hist(airData$Shopping.Amount.at.Airport)
ggplot(airData, aes(x=Shopping.Amount.at.Airport)) + geom_histogram(bins = 20, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Shopping.Amount.at.Airport)) + geom_boxplot()

#Eating and Drinking at airport
hist(airData$Eating.and.Drinking.at.Airport)
ggplot(airData, aes(x=Eating.and.Drinking.at.Airport)) + geom_histogram(bins = 30, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Eating.and.Drinking.at.Airport)) + geom_boxplot()

#Day of Month 
hist(airData$Day.of.Month)
ggplot(airData, aes(x=Day.of.Month)) + geom_histogram(bins = 30, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Day.of.Month)) + geom_boxplot()

#Scheduled Departure Hour 
hist(airData$Scheduled.Departure.Hour)
ggplot(airData, aes(x=Scheduled.Departure.Hour)) + geom_histogram(bins = 20, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Scheduled.Departure.Hour)) + geom_boxplot()

#Departure Delay in Minutes 
hist(airData$Departure.Delay.in.Minutes)
ggplot(airData, aes(x=Departure.Delay.in.Minutes)) + geom_histogram(bins = 50, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Departure.Delay.in.Minutes)) + geom_boxplot()

#Arrival Delay in Minutes 
hist(airData$Arrival.Delay.in.Minutes)
ggplot(airData, aes(x=Arrival.Delay.in.Minutes)) + geom_histogram(bins = 20, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Arrival.Delay.in.Minutes)) + geom_boxplot()

#Flight Time In Minutes 
hist(airData$Flight.time.in.minutes)
ggplot(airData, aes(x=Flight.time.in.minutes)) + geom_histogram(bins = 20, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Flight.time.in.minutes)) + geom_boxplot()

#Flight Distance 
hist(airData$Flight.Distance)
ggplot(airData, aes(x=Flight.Distance)) + geom_histogram(bins = 20, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Flight.Distance)) + geom_boxplot()

#Likelihood to Recommend 
hist(airData$Likelihood.to.recommend)
ggplot(airData, aes(x=Likelihood.to.recommend)) + geom_histogram(bins = 10, color="black", fill="white")

ggplot(airData, aes(x=factor(0), Likelihood.to.recommend)) + geom_boxplot()

##################################

###### 4B ######

#Producing tables of categorical variables(Gender) are often helpful

tabGender <- table(survey2$Gender)
tabGender
tabGenderRec <- table(survey2$df1.Recommend.type,survey2$Gender)
tabGenderRec
#Female is less likely to recommend than male.
tabClassRec <- table(survey2$df1.Recommend.type,survey2$Class)
tabClassRec
#Business class has larger portion to recommend.
Age.group <- ifelse(survey2$Age<24,"teenager","adult")
df2 <- data.frame(survey2$Age,Age.group)
df2$Age.group[which(df2$survey2.Age>55)] <- "elder"
survey2 <- data.frame(survey2,df2$Age.group)
tabAgeRec <- table(survey2$df1.Recommend.type,survey2$df2.Age.group)
tabAgeRec
#Elders are much more dislike the airline than teenager and adult, and adults love our airline.
tabPartnerRec <- table(survey2$df1.Recommend.type,survey2$Partner.Name)
tabPartnerRec
#FlyFast did the worst job.
tabTravelRec <- table(survey2$df1.Recommend.type,survey2$Type.of.Travel)
tabTravelRec
#Personal traveler dislike our airlines.


###### 4C ######

#Various boxplots and tables specific to this section using grouping variables to uncover any relationships
#Gender plot did not reveal much at all as both boxes are the same, maybe more needs to be done with the data
genderPlot <- ggplot(survey, aes(x=Gender,y=Likelihood.to.recommend)) + geom_boxplot() + ggtitle("NPS by Gender") + xlab("Gender") + ylab("Net Promoter Score (NPS)")
genderPlot
#From a general overview it seems that those who travel on business or using millage points are more likely to recommend than those who spend their own money for personal travel using the airline
travelTable <- table(survey2$df1.Recommend.type, survey2$Type.of.Travel)
travelTable
travelPlot <- ggplot(survey, aes(x=Type.of.Travel, y=Likelihood.to.recommend)) + geom_boxplot() + ggtitle("NPS by Type of Travel") + xlab("Type of Travel") + ylab("Net Promoter Score (NPS)")
travelPlot
#Those with a gold, platinum or silver airline status are more likely to recommend than those who are blue status, but this is not completely clear
statusTable <- table(survey2$df1.Recommend.type,survey2$Airline.Status)
statusTable
statusPlot <- ggplot(survey, aes(x=fct_reorder(Airline.Status,Likelihood.to.recommend), y=Likelihood.to.recommend)) + geom_boxplot() + ggtitle("NPS by Airline Status") + xlab("Airline Status") + ylab("Net Promoter Score (NPS)")
statusPlot
#Class plot is similar to the gender plot and needs more work
classTable <- table(survey2$df1.Recommend.type, survey2$Class)
classTable
classPlot <- ggplot(survey, aes(x=Class, y=Likelihood.to.recommend)) + geom_boxplot() + ggtitle("NPS by Class") + xlab("Class") + ylab("Net Promoter Score (NPS)")
classPlot
#Those who have had their flight canceled are slightly less likely to recommend the airlines compared to those who have not had their flight cancelled
cancelTable <- table(survey2$df1.Recommend.type, survey2$Flight.cancelled)
cancelTable
cancelPlot <- ggplot(survey, aes(x=Flight.cancelled,y=Likelihood.to.recommend)) + geom_boxplot()
cancelPlot

###### 4D ######

#barplots can be used to compare different variables 
#to their overall average Net Promoter Score

###### Gender
#gender reveals not much of a difference in gender affecting NPS, with both
#being passive or slight detractors
ggplot(survey, aes(x=Gender,y=Likelihood.to.recommend)) + geom_bar(stat="summary")
#Gender still inconclusive, usually is
ggplot(PromoterSurvey, aes(x=Gender,y=Likelihood.to.recommend)) + geom_bar(stat="summary")
ggplot(DetractorSurvey, aes(x=Gender,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

###### Age Group
ggplot(survey2, aes(x=df2.Age.group,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + ggtitle("Age Group for Likelihood.to.recommend")
ggplot(PromoterSurvey, aes(x=df2.Age.group,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + ggtitle("Promoter Age Group for Likelihood.to.recommend")
ggplot(DetractorSurvey, aes(x=df2.Age.group,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + ggtitle("Detractor Age Group for Likelihood.to.recommend")

#Elder/travel type count/percentages
count(survey2[survey2$df2.Age.group == "elder",])
#26256
length(intersect(which(survey2['Type.of.Travel']=="Business travel"), which(survey2['df2.Age.group']=="elder")))
#9441
length(intersect(which(survey2['Type.of.Travel']=="Personal Travel"), which(survey2['df2.Age.group']=="elder")))
#15315
(9441/26256)*100 #35.96% of elders travel for business reasons
(15315/26256)*100 #58.33% of elders travel for personal reasons

#Adult/travel type count/percentages
count(survey2[survey2$df2.Age.group == "adult",])
#53281
length(intersect(which(survey2['Type.of.Travel']=="Business travel"), which(survey2['df2.Age.group']=="adult")))
#40366
length(intersect(which(survey2['Type.of.Travel']=="Personal Travel"), which(survey2['df2.Age.group']=="adult")))
#8887
(40266/53281)*100 #75.57% of adults travel for business reasons
(8887/53281)*100 #16.68% of adults travel for personal reasons

tabAgeTyp <- table(survey2$Type.of.Travel,survey2$df2.Age.group)
tabAgeTyp

##### Airline Status
#airline status isn't conclusive either, as one might think that the more
#elite the status, the higher the NPS, but each level of status to NPS is
#random
ggplot(survey, aes(x=Airline.Status,y=Likelihood.to.recommend)) + geom_bar(stat="summary")
#Promoters follow the natural hierarchy, but Detractors are more random
pAirlineStatus <- ggplot(PromoterSurvey, aes(x=Airline.Status,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + coord_cartesian(ylim = c(9, 10)) + ggtitle("Promoter Airline Status for Likelihood.to.recommend")
dAirlineStatus <- ggplot(DetractorSurvey, aes(x=Airline.Status,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + coord_cartesian(ylim = c(3, 6)) + ggtitle("Detractor Airline Status for Likelihood.to.recommend")

##### Origin State
#too many different states to form a conclusion, try grouping by region
ggplot(survey, aes(x=Origin.State,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

##### Age
#too many different ages, try grouping by age range. NPS is highest around age 40,
#with a rapid decline at around age 55
ggplot(survey, aes(x=Age,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + coord_cartesian(ylim = c(5, 8.5)) + ggtitle("Age Mean for Likelihood.to.recommend")
#Average age of detractors is 52, average age of promoters is 44
#Between age of 30 and 50 has highest ratings
ggplot(BothSurvey, aes(x=df1.Recommend.type,y=Age)) + geom_bar(stat="summary")
ggplot(PromoterSurvey, aes(x=Age,y=Likelihood.to.recommend)) + geom_line(stat="summary")
ggplot(DetractorSurvey, aes(x=Age,y=Likelihood.to.recommend)) + geom_line(stat="summary")
ggplot(survey2, aes(x=Age,y=Likelihood.to.recommend)) + geom_line(stat="summary")

##### Price Sensitivity
#highest NPS are people with the lowest price sensitivity, showing that people
#who do care the most about price are the least likely promoters of the airline
priceSensitivityBarPlot <- ggplot(survey, aes(x=Price.Sensitivity,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + ggtitle("Price Sensitivity for Likelihood.to.recommend")
#Lower price sensitivity fares well for the Promoters, 
#but higher price sensitivity has highest NPS among Detractors
ggplot(PromoterSurvey, aes(x=Price.Sensitivity,y=Likelihood.to.recommend)) + geom_bar(stat="summary")
ggplot(DetractorSurvey, aes(x=Price.Sensitivity,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

##### Year of First Flight
#inconclusive
firstFlightBarPlot <- ggplot(survey, aes(x=Year.of.First.Flight,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

##### Flights per Year
#interestingly, the more flights per year that a customer has, the less likely
#they are to give a high NPS
flightsPerYearBarPlot <- ggplot(survey, aes(x=Flights.Per.Year,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + ggtitle("Flights per Year for Likelihood.to.recommend")
#Reaffirms that promoters have less flights per year and detractors have
#to fly many more times per year
ggplot(BothSurvey, aes(x=df1.Recommend.type,y=Flights.Per.Year)) + geom_bar(stat="summary")

##### Loyalty
#loyalty doesn't matter
loyaltyBarPlot <- ggplot(survey, aes(x=Loyalty,y=Likelihood.to.recommend)) + geom_bar(stat="summary")
#Detractors do have lower loyalty
ggplot(BothSurvey, aes(x=df1.Recommend.type,y=Loyalty)) + geom_bar(stat="summary")

##### Type of Business Travel
#People who travel for business give the highest NPS, whereas people who 
#travel for personal reasons give an extremely low NPS on average
typeOfTravelBarPlot <- ggplot(survey, aes(x=Type.of.Travel,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + coord_cartesian(ylim = c(5, 8.5)) + ggtitle("Type of Travel for Likelihood.to.recommend")
#Personal travel is by far the lowest of all the Detractors
ggplot(PromoterSurvey, aes(x=Type.of.Travel,y=Likelihood.to.recommend)) + geom_bar(stat="summary")
ggplot(DetractorSurvey, aes(x=Type.of.Travel,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

###### Total Freq Flyer Accts
#inconclusive
totalFreqFlyerAcctsBarPlot <- ggplot(survey, aes(x=Total.Freq.Flyer.Accts,y=Likelihood.to.recommend)) + geom_bar(stat="summary")
#nope nothing
ggplot(PromoterSurvey, aes(x=Total.Freq.Flyer.Accts,y=Likelihood.to.recommend)) + geom_bar(stat="summary")
ggplot(DetractorSurvey, aes(x=Total.Freq.Flyer.Accts,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

##### Class
#all similar, but business class is slightly higher
classBarPlot <- ggplot(survey, aes(x=Class,y=Likelihood.to.recommend)) + geom_bar(stat="summary", fill="Dark Blue") + coord_cartesian(ylim = c(7, 8)) + ggtitle("Seating Class for Likelihood.to.recommend")

##### Flight Date
#inconclusive, try for by month or season
flightDateBarPlot <- ggplot(survey, aes(x=Flight.date,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

##### Partner Name
#inconclusive
partnerNameBarPlot <- ggplot(survey, aes(x=Partner.Name,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

##### Scheduled Departure Hour
#pretty inconclusive, except for people at 1am like the airline more lmao
scheduledDepartureHourBarPlot <- ggplot(survey, aes(x=Scheduled.Departure.Hour,y=Likelihood.to.recommend)) + geom_bar(stat="summary")

##### Eating and Drinking at Airport
#inconclusive for Detractors, but there is a trend of Promoters having lower
#ratings with less money spent of food and drinks, but it maximizes at around
#$100 spent and then dropping off if more is spent
ggplot(PromoterSurvey, aes(x=Eating.and.Drinking.at.Airport,y=Likelihood.to.recommend)) + geom_line(stat="summary", color="Dark Blue") + coord_cartesian(xlim = c(0, 300)) + ggtitle("$ Spent on Food + Drink for Likelihood.to.recommend")
ggplot(DetractorSurvey, aes(x=Eating.and.Drinking.at.Airport,y=Likelihood.to.recommend)) + geom_line(stat="summary")

###### 5. ######

#You can eliminate missing data by using the na.omit() command (but be careful on how 
#much data is being removed – sometimes cleaning the entire dataset to remove all NAs 
#is not a good idea).
survey$Age[is.na(survey$Age)]#None
survey$Likelihood.to.recommend[is.na(survey$Likelihood.to.recommend)]#4 NA
survey$Gender[is.na(survey$Gender)]#None
survey$Price.Sensitivity[is.na(survey$Price.Sensitivity)]#None
survey$Loyalty[is.na(survey$Loyalty)]#None
survey$Type.of.Travel[is.na(survey$Type.of.Travel)]#None
survey$Shopping.Amount.at.Airport[is.na(survey$Shopping.Amount.at.Airport)]#None
survey$Eating.and.Drinking.at.Airport[is.na(survey$Eating.and.Drinking.at.Airport)]#None
survey$olong[is.na(survey$olong)]#None
survey$dlong[is.na(survey$dlong)]#None
survey$Arrival.Delay.in.Minutes[is.na(survey$Arrival.Delay.in.Minutes)]#Many many NA
survey$Departure.Delay.in.Minutes[is.na(survey$Departure.Delay.in.Minutes)]#Many many NA
survey$Flight.cancelled[is.na(survey$Flight.cancelled)]#None
survey$Partner.Name[is.na(survey$Partner.Name)]#None
survey$Flight.Distance[is.na(survey$Flight.Distance)]#None

#We plan to remove 4 observations which have NA in Likelihood to Recommend for all subsequent analysis, 
#and if we try to analyze Arrival.Delay.in.Minutes or Departure.Delay.in.Minutes then we would deal with
#it separately since there are hundreds of NAs in these two variables so we need to handle them carefully.

###### 6A ######

#Dots on the map where the color of the dot indicates the value of Likelihood.to.recommend.

dummyDF <- data.frame(state.name,stringsAsFactors = FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
us <- map_data("state")
map.simple <- ggplot(dummyDF,aes(map_id=state))
map.simple <- map.simple + geom_map(map=us,fill="white",color="black")
map.simple <- map.simple + expand_limits(x=us$long,y=us$lat)
map.simple <- map.simple + coord_map() + ggtitle("map of USA")
map.simple
survey3 <- data.frame(survey,tolower(survey$Origin.State))
survey3$state <- survey3$tolower.survey.Origin.State.
survey3$tolower.survey.Origin.State. <- NULL
Point.map <- map.simple + geom_point(data=survey3,aes(x=olong,y=olat,color=Likelihood.to.recommend))
Point.map <- Point.map + expand_limits(x=us$long,y=us$lat)
Point.map <- Point.map + coord_map() + ggtitle("Point map of Recommend")
Point.map

###### 6B ###### 

#State level mean of Likelihood.to.recommend
RecOstate <- data.frame(survey$Origin.State,survey$Likelihood.to.recommend)
RecOstate <- RecOstate[is.na(RecOstate$survey.Likelihood.to.recommend)=="FALSE",]
meanRecOState <- aggregate(RecOstate,by=list(RecOstate$survey.Origin.State),FUN=mean)
names(meanRecOState) <- c("state","group.1","Likelihood.to.recommend")
meanRecOState$group.1 <- NULL
meanRecOState$state <- tolower(meanRecOState$state)
Rec.map <- ggplot(meanRecOState,aes(map_id=state)) + geom_map(map=us,aes(fill=Likelihood.to.recommend))
Rec.map <- Rec.map + expand_limits(x=us$long,y=us$lat) + coord_map()
Rec.map <- Rec.map + ggtitle("State level mean for Likelihood.to.recommend")
Rec.map #This is the map shows State level mean for Likelihood.to.recommend

###### 6C ######

#Explore origins of the flight (and/or the destinations).
#Origins
CountOstate <- data.frame(table(survey$Origin.State))
names(CountOstate) <- c("state","Freq")
CountOstate$state <- tolower(CountOstate$state)
Ori.map <- ggplot(CountOstate,aes(map_id=state)) + geom_map(map=us,aes(fill=Freq))
Ori.map <- Ori.map + expand_limits(x=us$long,y=us$lat) + coord_map()
Ori.map <- Ori.map + ggtitle("Number of Flights Departed")
Ori.map
#Destination
CountDstate <- data.frame(table(survey$Destination.State))
names(CountDstate) <- c("state","Freq")
CountDstate$state <- tolower(CountDstate$state)
Des.map <- ggplot(CountDstate,aes(map_id=state)) + geom_map(map=us,aes(fill=Freq))
Des.map <- Des.map + expand_limits(x=us$long,y=us$lat) + coord_map()
Des.map <- Des.map + ggtitle("Number of Flights Arrived")
Des.map
#These two maps are almost the same.

count(survey2[survey2$Origin.State == "Texas",]) #7626
count(survey2[survey2$Origin.State == "Idaho",]) #114

##### State takeaway^ Idaho has people rating in extremes (find number of 
#voters), while Texas has mostly all mild low ratings 
#(find avg number if you want)

###### 7. #######

#It could be more informative to show separate plots for “detractors” (people who 
#answered Likelihood.to.recommend < 7) and “promoters” (Likelihood.to.recommend > 8). 
#This could be especially true for the analysis of the comments.

DetractorSurvey <- survey2[which(survey2$df1.Recommend.type=="Detractor"),]
PromoterSurvey <- survey2[which(survey2$df1.Recommend.type=="Promoter"),]

#Detractor analysis
BarGenDet <- ggplot(DetractorSurvey,aes(x=Gender)) + geom_bar() + ggtitle("Gender Detractor")
BarGenDet
#Female is much more than male
BarAgeDet <- ggplot(DetractorSurvey,aes(x=df2.Age.group)) + geom_bar() + ggtitle("Age Detractor")
BarAgeDet
#Teenager is significantly lower than adults and elders.
BarPriceDet <- ggplot(DetractorSurvey,aes(x=Price.Sensitivity)) + geom_bar() + ggtitle("Price Detractor")
BarPriceDet
#Majority of them don't care about price.
DetractorSurvey$Year.of.First.Flight <- as.factor(DetractorSurvey$Year.of.First.Flight)
BarYearDet <- ggplot(DetractorSurvey,aes(x=Year.of.First.Flight)) + geom_bar() + ggtitle("Year Detractor")
BarYearDet
#Many of them started their flight in 2003.
BarStatusDet <- ggplot(DetractorSurvey,aes(x=Airline.Status)) + geom_bar() + ggtitle("Status Detractor")
BarStatusDet
#Majority of them are blue status
BarTypeDet <- ggplot(DetractorSurvey,aes(x=Type.of.Travel)) + geom_bar() + ggtitle("Travel Detractor")
BarTypeDet
#Personal travel is way more than business, and fewest for mileage tickets.
BarClassDet <- ggplot(DetractorSurvey,aes(x=Class)) + geom_bar() + ggtitle("Class Detractor")
BarClassDet
#Eco is the most.
BarPartnerDet <- ggplot(DetractorSurvey,aes(x=Partner.Name)) + geom_bar() + ggtitle("Partner Detractor")
BarPartnerDet <- BarPartnerDet + theme(axis.text.x = element_text(angle=90,hjust=1,vjust=1))
BarPartnerDet
#Cheapest Airline is the most, FlyFast is second, Sigma is the third.

#Promoter analysis
BarGenPro <- ggplot(PromoterSurvey,aes(x=Gender)) + geom_bar() + ggtitle("Gender Promoter")
BarGenPro
#Male and Female are equal
BarAgePro <- ggplot(PromoterSurvey,aes(x=df2.Age.group)) + geom_bar() + ggtitle("Age Promoter")
BarAgePro
#Adult is much more than else.
BarPricePro <- ggplot(PromoterSurvey,aes(x=Price.Sensitivity)) + geom_bar() + ggtitle("Price Promoter")
BarPricePro
#Majority of them don't care about price.
PromoterSurvey$Year.of.First.Flight <- as.factor(PromoterSurvey$Year.of.First.Flight)
BarYearPro <- ggplot(PromoterSurvey,aes(x=Year.of.First.Flight)) + geom_bar() + ggtitle("Year Promoter")
BarYearPro
#Still 2003 is the largest, nothing interesting.
BarStatusPro <- ggplot(PromoterSurvey,aes(x=Airline.Status)) + geom_bar() + ggtitle("Status Promoter")
BarStatusPro
#Besides blue, silver also stands out.
BarTypePro <- ggplot(PromoterSurvey,aes(x=Type.of.Travel)) + geom_bar() + ggtitle("Travel Promoter")
BarTypePro
#Business is much much more than personal.
BarClassPro <- ggplot(PromoterSurvey,aes(x=Class)) + geom_bar() + ggtitle("Class Promoter")
BarClassPro
#Still Eco, nothing interesting.
BarPartnerPro <- ggplot(PromoterSurvey,aes(x=Partner.Name)) + geom_bar() + ggtitle("Partner Promoter")
BarPartnerPro <- BarPartnerPro + theme(axis.text.x = element_text(angle=90,hjust=1,vjust=1))
BarPartnerPro

#Cheapest is the first, Sigma the second, third one is Northwest.

#Things interesting
#1. Why does Texas have significantly lower Likelihood to recommend? 
#2. Why does Flyfast did worst job?
#3. Why does Business Travel do great job versus Personal Travel?
#4. Wordcloud and associative rule mining is interesting.

#Wordcloud for elders, teenagers and adults
surveyComment <- survey2[is.na(survey2$freeText)==FALSE,]
surveyCommentElder <- surveyComment[surveyComment$df2.Age.group=="elder",]
surveyCommentAdult <- surveyComment[surveyComment$df2.Age.group=="adult",]
surveyCommentTeenager <- surveyComment[surveyComment$df2.Age.group=="teenager",]
textElder <- surveyCommentElder$freeText
textTeen <- surveyCommentTeenager$freeText
textAdult <- surveyCommentAdult$freeText 

stop_words <- c(stopwords("english"),"flight","southeast","service","plane",
                "flights","seat","seats","luggage","airlines","airline","staff","crew")
#First we do elder's analysis
corElder <- corpus(textElder)
corElderDFM <- dfm(textElder,remove_punct=TRUE,remove=stop_words)
wordCloudElder <- textplot_wordcloud(corElderDFM, min_count = 1)
m1 <- as.matrix((corElderDFM))
wordCountsElder <- colSums(m1)
wordCountsElder <- sort(wordCountsElder,decreasing=TRUE)
setwd("~/Downloads")
posWords <- scan("positive-words.txt", character(0), sep = "\n")
posWords <- posWords[-1:-34]
negWords <- scan("negative-words.txt", character(0), sep = "\n")
negWords <- negWords[-1:-34]
matchedPElders <- match(names(wordCountsElder), posWords, nomatch = 0)
matchedNElders <- match(names(wordCountsElder), negWords, nomatch = 0)
tolPosElder <- sum(matchedPElders != 0)
tolPosElder
tolNegElder <- sum(matchedNElders != 0)
tolNegElder
#71 positive words, 91 negative words.
#Second, for teenagers
corTeen <- corpus(textTeen)
corTeenDFM <- dfm(textTeen,remove_punct=TRUE,remove=stop_words)
wordCloudTeen <- textplot_wordcloud(corTeenDFM, min_count = 1)
m2 <- as.matrix((corTeenDFM))
wordCountsTeen <- colSums(m2)
wordCountsTeen <- sort(wordCountsTeen,decreasing=TRUE)
matchedPTeen <- match(names(wordCountsTeen), posWords, nomatch = 0)
matchedNTeen <- match(names(wordCountsTeen), negWords, nomatch = 0)
tolPosTeen <- sum(matchedPTeen != 0)
tolPosTeen
tolNegTeen <- sum(matchedNTeen != 0)
tolNegTeen
#28 positive words, 37 negative words.
#Third, for adults
corAdult <- corpus(textAdult)
corAdultDFM <- dfm(textAdult,remove_punct=TRUE,remove=stop_words)
wordCloudAdult <- textplot_wordcloud(corAdultDFM, min_count = 1)
m3 <- as.matrix((corAdultDFM))
wordCountsAdult <- colSums(m3)
wordCountsAdult <- sort(wordCountsAdult,decreasing=TRUE)
matchedPAdult <- match(names(wordCountsAdult), posWords, nomatch = 0)
matchedNAdult <- match(names(wordCountsAdult), negWords, nomatch = 0)
tolPosAdult <- sum(matchedPAdult != 0)
tolPosAdult
tolNegAdult <- sum(matchedNAdult != 0)
tolNegAdult
#134 positive words, 113 negative words.

#SVM part
library(kernlab)
surveySVM <- survey2[which(survey2$df1.Recommend.type != "Passive"),]
surveySVM$df1.Recommend.type <- factor(surveySVM$df1.Recommend.type)
svmdata <- data.frame(surveySVM$Age,surveySVM$Price.Sensitivity,surveySVM$Flights.Per.Year,
                      surveySVM$Loyalty,surveySVM$Shopping.Amount.at.Airport,surveySVM$Eating.and.Drinking.at.Airport,
                      surveySVM$df1.Recommend.type)
randN <- sample(1:dim(svmdata)[1])
cutPoint2_3 <- floor(2*dim(svmdata[1])/3)
trainData <- svmdata[randN[1:cutPoint2_3[1]],]
testData <- svmdata[randN[(cutPoint2_3[1]+1):dim(svmdata)[1]],]
svmModel <- ksvm(surveySVM.df1.Recommend.type~., data=trainData, C = 5, cross = 3)
svmModel
svmPred <- predict(svmModel,testData)
confMatrix <- table(svmPred,testData$surveySVM.df1.Recommend.type)
confMatrix
accuracy <- sum(diag(confMatrix))/sum(confMatrix)
error <- 1-accuracy

#SVM2 separates into Detractor and Non-Detractor
DetNon <- survey2$df1.Recommend.type
DetNon[DetNon == "Passive"] <- "Non-Detractor"
DetNon[DetNon == "Promoter"] <- "Non-Detractor"
surveySVM2 <- data.frame(survey2,factor(DetNon))
svmdata2 <- data.frame(surveySVM2$Age,surveySVM2$Price.Sensitivity,surveySVM2$Flights.Per.Year,
                       surveySVM2$Loyalty,surveySVM2$Shopping.Amount.at.Airport,surveySVM2$Eating.and.Drinking.at.Airport,
                       surveySVM2$factor.DetNon.)
randN2 <- sample(1:dim(svmdata2)[1])
cutPoint2_32 <- floor(2*dim(svmdata2[1])/3)
trainData2 <- svmdata2[randN2[1:cutPoint2_32[1]],]
testData2 <- svmdata2[randN2[(cutPoint2_32[1]+1):dim(svmdata2)[1]],]
svmModel2 <- ksvm(surveySVM2.factor.DetNon.~., data=trainData2, C = 5, cross = 3)
svmModel2
svmPred2 <- predict(svmModel2,testData2)
confMatrix2 <- table(svmPred2,testData2$surveySVM2.factor.DetNon.)
confMatrix2
accuracy2 <- sum(diag(confMatrix2))/sum(confMatrix2)
error2 <- 1-accuracy2

######Linear Modeling for sungular numeric variable
ageModel <- lm(formula = Likelihood.to.recommend ~ Age, data=survey)
summary(ageModel)
plot(survey$Age,survey$Likelihood.to.recommend)
abline(ageModel)

flightTimeModel <- lm(formula = Likelihood.to.recommend ~ Flight.time.in.minutes, data=survey)
summary(flightTimeModel)
plot(survey$Flight.time.in.minutes, survey$Likelihood.to.recommend)
abline(flightTimeModel)

arrivalDelayModel <-  lm(formula = Likelihood.to.recommend ~ Arrival.Delay.in.Minutes, data=survey)
summary(arrivalDelayModel)
plot(survey$Arrival.Delay.in.Minutes, survey$Likelihood.to.recommend)
abline(arrivalDelayModel)

departureDelayModel <-  lm(formula = Likelihood.to.recommend ~ Departure.Delay.in.Minutes, data=survey)
summary(departureDelayModel)
plot(survey$Departure.Delay.in.Minutes, survey$Likelihood.to.recommend)
abline(departureDelayModel)

flightsPerYearModel <-  lm(formula = Likelihood.to.recommend ~ Flights.Per.Year, data=survey)
summary(flightsPerYearModel)
plot(survey$Flights.Per.Year, survey$Likelihood.to.recommend)
abline(departureDelayModel)

loyaltyModel <-  lm(formula = Likelihood.to.recommend ~ Loyalty, data=survey)
summary(loyaltyModel)
plot(survey$Loyalty, survey$Likelihood.to.recommend)
abline(loyaltyModel)

PriceSensitivityModel <-  lm(formula = Likelihood.to.recommend ~ Price.Sensitivity, data=survey)
summary(PriceSensitivityModel)
plot(survey$Price.Sensitivity, survey$Likelihood.to.recommend)
abline(PriceSensitivityModel)

ShoppingModel <-  lm(formula = Likelihood.to.recommend ~ Shopping.Amount.at.Airport, data=survey)
summary(ShoppingModel)
plot(survey$Shopping.Amount.at.Airport, survey$Likelihood.to.recommend)
abline(ShoppingModel)

EatingModel <-  lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport, data=survey)
summary(EatingModel)
plot(survey$Eating.and.Drinking.at.Airport, survey$Likelihood.to.recommend)
abline(EatingModel)

eatingDistanceModel <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Flight.Distance, data=survey)
summary(eatingDistanceModel)
plot(survey$Eating.and.Drinking.at.Airport, survey$Likelihood.to.recommend)
abline(eatingDistanceModel)

DistanceModel <-  lm(formula = Likelihood.to.recommend ~ Flight.Distance, data=survey)
summary(DistanceModel)
plot(survey$Flight.Distance, survey$Likelihood.to.recommend)
abline(DistanceModel)

accountsModel <- lm(formula = Likelihood.to.recommend ~ Total.Freq.Flyer.Accts, data=survey)
summary(accountsModel)


########## Linear models for numeric variables

model3 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model3)

model4 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model4)


model5 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model5)

model6 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model6)

model7 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model7)

model7 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model7)

model8 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model8)

model9 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model9)

model10<- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes +  + Flights.Per.Year, data=survey)
summary(model10)

model11 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model11)

model12 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model12)

model13 <- lm(formula = Likelihood.to.recommend ~ Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model13)

model14 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model14)

model15 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Shopping.Amount.at.Airport + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model15)

model16 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model16)

model17 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model17)

model18 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model18)

model19 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model19)

model20 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes  + Flights.Per.Year, data=survey)
summary(model20)

model21 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model21)

model22 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model22)

model23<- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model23)

model24 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model24)

model25 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model25)

model26 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model26)

model27 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty  + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model27)

model28 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model28)

model29 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model29)

model30 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes  + Flights.Per.Year, data=survey)
summary(model30)

model31 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model31)

model32 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model32)

model33 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model33)

model34 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Shopping.Amount.at.Airport + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model34)

model35 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model35)

model36 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model36)

model37 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Loyalty  + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model37)

model38 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Loyalty + Age  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model38)

model39 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Loyalty + Age + Flight.time.in.minutes  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model39)

model40 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes  + Flights.Per.Year, data=survey)
summary(model40)

model41 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model42)

model43 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model43)

model44 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model44)

model45 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model45)

model46 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model46)

model47 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport   + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model47)

model48 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model48)

model49 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model49)

model50 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model50)

model51 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes  + Flights.Per.Year, data=survey)
summary(model52)

model53 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model53)

model54 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model54)

model55 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model55)

model56 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model56)

model57 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model57)

model58 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport  + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model58)

model59 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model59)

model60 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model60)

model61 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model61)

model62 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes  + Flights.Per.Year, data=survey)
summary(model62)

model63 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model63)

model64 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model64)

model65 <- lm(formula = Likelihood.to.recommend ~  Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model65)

model66 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model66)

model67 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model67)

model68 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport  + Loyalty + Age + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model68)

model69 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model69)

model70 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model70)

model71 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model71)

model72 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Arrival.Delay.in.Minutes  + Flights.Per.Year, data=survey)
summary(model72)

model73 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model73)

model74 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model74)

model75 <- lm(formula = Likelihood.to.recommend ~  Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model75)

model76 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model76)

model77 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model77)

model78 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport  + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model78)

model79 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model79)

model80 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty  + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model80)

model81 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model81)

model82 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes  + Flights.Per.Year, data=survey)
summary(model82)

model83 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes , data=survey)
summary(model83)

model84 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model84)

model85 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model85)

model86 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model86)

model87 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model87)

model88 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport  + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model88)

model89 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model89)

model90 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty  + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model90)

model91 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age  + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model91)

model92 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes  + Flights.Per.Year, data=survey)
summary(model92)

model93 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes , data=survey)
summary(model93)


model94 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model94)

model95 <- lm(formula = Likelihood.to.recommend ~  Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model95)

model96 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model96)

model97 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model97)

model98 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport  + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model98)

model99 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model99)

model100 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty  + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model100)

model101 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data=survey)
summary(model101)

model102 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes  + Departure.Delay.in.Minutes, data=survey)
summary(model102)

model103 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes , data=survey)
summary(model103)

model104 <- lm(formula = Likelihood.to.recommend ~ Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model104)

model105 <-lm(formula = Likelihood.to.recommend ~ Loyalty +  Flights.Per.Year, data = survey)
summary(model05)

model106 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport +  Flight.Distance, data = survey)
summary(model106)

model107 <- lm(formula = Likelihood.to.recommend ~ Age + Loyalty, data = survey)
summary(model107)

model108 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport, data=survey)
summary(model108)

model109 <- lm(formula = Likelihood.to.recommend ~ Price.Sensitivity + Loyalty , data=survey)
summary(model109)

model110 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age, data=survey)
summary(model110)

model111 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes, data=survey)
summary(model111)

model112 <- lm(formula = Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Flight.time.in.minutes, data=survey)
summary(model12)

model13 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age + Flights.Per.Year, data=survey)
summary(model113)

model114 <- lm(formula = Likelihood.to.recommend ~  Loyalty + Age + Flights.Per.Year, data=survey)
summary(model114)

model115 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age , data=survey)
summary(model115)

model116 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Flights.Per.Year, data=survey)
summary(model116)

model117 <- lm(formula = Likelihood.to.recommend ~  Loyalty + Age + Flights.Per.Year, data=survey)
summary(model117)

model118 <- lm(formula = Likelihood.to.recommend ~  Age + Price.Sensitivity, data=survey)
summary(model118)

model119 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Age + Flights.Per.Year, data=survey)
summary(model119)

model120 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity  +  Age + Loyalty, data=survey)
summary(model120)

model121 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model121)

model122 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Price.Sensitivity + Loyalty  + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model122)

model123 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model123)

model124 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model124)

model125 <- lm(formula = Likelihood.to.recommend ~  Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model125)

model126 <- lm(formula = Likelihood.to.recommend ~  Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model126)

model127 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model27)

model128 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model128)

model129 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model129)

model130<- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age  + Arrival.Delay.in.Minutes  + Flights.Per.Year, data=survey)
summary(model130)


model131 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model131)

model132 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes +  Flights.Per.Year, data=survey)
summary(model132)

model133 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model133)

model134 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity  + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model134)

model135 <- lm(formula = Likelihood.to.recommend ~  Loyalty + Age  + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes , data=survey)
summary(model135)

model136 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity  + Loyalty + Age +  Flights.Per.Year, data=survey)
summary(model136)

model137 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty  + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model137)

model138 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty  + Flight.time.in.minutes  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model138)

model139 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model139)

model140 <- lm(formula = Likelihood.to.recommend ~  Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model140)

model141 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model141)

model142 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance  + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model142)

model143 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model143)

model144 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes  + Flights.Per.Year, data=survey)
summary(model144)

model145 <- lm(formula = Likelihood.to.recommend ~  Total.Freq.Flyer.Accts  + Price.Sensitivity + Loyalty + Age + Flights.Per.Year, data=survey)
summary(model145)

model146 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Total.Freq.Flyer.Accts + Price.Sensitivity + Loyalty + Age + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model146)

model147 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model147)

model148 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport + Shopping.Amount.at.Airport + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes  + Flights.Per.Year, data=survey)
summary(model148)

model149 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model149)

model150 <- lm(formula = Likelihood.to.recommend ~  Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport  + Price.Sensitivity + Loyalty + Age   + Flights.Per.Year, data=survey)
summary(model150)

model151 <- lm(formula = Likelihood.to.recommend ~  Total.Freq.Flyer.Accts + Price.Sensitivity + Loyalty + Age  + Flights.Per.Year, data=survey)
summary(model151)

model152 <- lm(formula = Likelihood.to.recommend ~ Flight.Distance + Total.Freq.Flyer.Accts + Price.Sensitivity + Loyalty + Age + Flight.time.in.minutes  + Departure.Delay.in.Minutes + Flights.Per.Year, data=survey)
summary(model52)


#Linear Modeling with dummy variables

survey3 <- dummy_cols(survey2, select_columns = c("Airline.Status", "Gender", "Type.of.Travel", "Class", "Partner.Name", "df2.Age.group"), remove_first_dummy = TRUE)
survey3 <- subset(survey3, select = -c(Scheduled.Departure.Hour, Flight.cancelled, Destination.City, Origin.City, Airline.Status, Gender, Type.of.Travel, Class, Flight.date, Partner.Code, Partner.Name, Origin.State, Destination.State, olong, olat, dlong, dlat, freeText, df1.Recommend.type, df2.Age.group ))
names(survey3) <- str_replace_all(names(survey3), c(" " = "_", "&" = ""))


model0 <- lm(formula = Likelihood.to.recommend ~., data=subset(survey3, select= -c(Type.of.Travel_Mileage_tickets, Type.of.Travel_Personal_Travel)))
summary(model0)

model1 <- lm(formula = Likelihood.to.recommend ~., data=subset(survey3, select= -c(Airline.Status_Gold, Airline.Status_Platinum, Airline.Status_Silver)))
summary(model1)

model2 <-  lm(formula = Likelihood.to.recommend ~., data=subset(survey3, select= -c(Partner.Name_CoolYoung_Airlines_Inc., Partner.Name_EnjoyFlying_Air_Services, Partner.Name_FlyFast_Airways_Inc.,
                                                                                    Partner.Name_FlyToSun_Airlines_Inc., Partner.Name_GoingNorth_Airlines_Inc., Partner.Name_GoingNorth_Airlines_Inc.,
                                                                                    Partner.Name_Northwest_Business_Airlines_Inc., Partner.Name_OnlyJets_Airlines_Inc., Partner.Name_Oursin_Airlines_Inc.,
                                                                                    Partner.Name_Paul_Smith_Airlines_Inc., Partner.Name_Sigma_Airlines_Inc., Partner.Name_Southeast_Airlines_Co.,
                                                                                    Partner.Name_West_Airways_Inc.)))
summary(model2)

final_model <- lm(formula = Likelihood.to.recommend ~., data=survey3)
summary(final_model)

#Various boxplots and tables specific to this section using the recommend type variable to uncover relationships
#Does not say too much, maybe another visual would be better
pricesensTable <- table(survey2$df1.Recommend.type, survey2$Price.Sensitivity)
pricesensTable
pricesensPlot <- ggplot(survey2,aes(x=df1.Recommend.type, y=Price.Sensitivity)) + geom_boxplot() + ggtitle("Price Sensitivity by Recommend Type") + xlab("Recommend Type") + ylab("Price Sensitivity")
pricesensPlot
#It seems that the more your fly per year the less likely you are to end up being a promoter of the brand
yearlyTable <- table(survey2$df1.Recommend.type,survey2$Flights.Per.Year)
yearlyTable
#Calculations for a seperate table shown in the report
detractor27 <- sum(yearlyTable[1,0:28])
passive27 <- sum(yearlyTable[2,0:28])
promoter27 <- sum(yearlyTable[3,0:28])
detractor92 <- sum(yearlyTable[1,29:92])
passive92 <- sum(yearlyTable[2,29:92])
promoter92 <- sum(yearlyTable[3,29:92])
yearlyPlot <- ggplot(survey2,aes(x=df1.Recommend.type, y=Flights.Per.Year)) + geom_boxplot() + ggtitle("Flights per Year by Recommend Type") + xlab("Recommend Type") + ylab("# of Flights per Year")
yearlyPlot
#It seems that those who are brand promoters are also more likely to exhibit brand loyalty
meanLoyalty <-  aggregate(Loyalty~df1.Recommend.type, data=survey2, mean)
medianLoyalty <- aggregate(Loyalty~df1.Recommend.type, data=survey2, median)
loyaltyPlot <- ggplot(survey2,aes(x=df1.Recommend.type, y=Loyalty)) + geom_boxplot() + ggtitle("Company Loyalty by Recommend Type") + xlab("Recommend Type") + ylab("Company Loyalty")
loyaltyPlot
#It seems that those who are brand promoters are also more likely to have a larger amount of frequent flyer accounts versus those who are brand detractors
frequentTable <- table(survey2$df1.Recommend.type,survey2$Total.Freq.Flyer.Accts)
frequentPlot <- ggplot(survey2,aes(x=df1.Recommend.type, y=Total.Freq.Flyer.Accts)) + geom_boxplot() + ggtitle("Freq Flyer Accounts by Recommend Type") + xlab("Recommend Type") + ylab("# of Frequent Flyer Accounts")
frequentPlot
#This does not show too much, maybe another visual would be better
breaks <- c(0,100,200,300,400,500,600,700,800)
labels <- c("$0-$100","$101-$200","$201-$300","301-400","401-500","501-600","601-700","701-800")
shoppingBins <- cut(survey2$Shopping.Amount.at.Airport, breaks=breaks, include.lowest=TRUE, right=FALSE, labels=labels)
summary(shoppingBins)
shoppingPlot <- ggplot(survey2,aes(x=df1.Recommend.type, y=Shopping.Amount.at.Airport)) + geom_boxplot() + ggtitle("Shopping Amount by Recommend Type") + xlab("Recommend Type") + ylab("$USD on Shopping per Customer")
shoppingPlot
#Does not show too much, maybe another visual would be better. Although it appears the bulk of promoters spend more money than detractors
breaks2 <- c(0,100,200,300,400,500,600,700,800,900)
labels2 <- c("$0-$100","$101-$200","$201-$300","301-400","401-500","501-600","601-700","701-800","801-900")
foodBins <- cut(survey2$Eating.and.Drinking.at.Airport, breaks=breaks2, include.lowest=TRUE, right=FALSE, labels=labels2)
foodPlot <- ggplot(survey2,aes(x=df1.Recommend.type, y=Eating.and.Drinking.at.Airport)) + geom_boxplot() + ggtitle("$USD on Food by Recommend Type") + xlab("Recommend Type") + ylab("$USD on Food per Customer")
foodPlot
#Does not show too much, maybe another visual would be better. Although it appears the bulk of detractors tend to have longer flight delays than promoters or passive consumers.
breaks3 <- c(0,60,120,180,240,300,360,420,480,540,600,660,720,780,840,900,960,1000)
labels3 <- c('0-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','10-11','11-12','12-13','13-14','14-15','15-16','16-17')
departureBins <- cut(survey2$Departure.Delay.in.Minutes, breaks=breaks3, include.lowest=TRUE, right=FALSE, labels=labels3)
departurePlot <- ggplot(survey2, aes(x=df1.Recommend.type, y=Departure.Delay.in.Minutes)) + geom_boxplot() + ggtitle("Departure Delay by Recommend Type") + xlab("Recommend Type") + ylab("Departure Delay (in minutes)")
departurePlot
#Similar to the departure delay plot
arrivalBins <- cut(survey2$Arrival.Delay.in.Minutes, breaks=breaks3, include.lowest=TRUE, right=FALSE, labels=labels3)
arrivalPlot <- ggplot(survey2, aes(x=df1.Recommend.type, y=Arrival.Delay.in.Minutes)) + geom_boxplot() + ggtitle("Arrival Delay by Recommend Type") + xlab("Recommend Type") + ylab("Arrival Delay (in minutes)")
arrivalPlot

#Converting the data frame into a transactions matrix
transSurvey <- as(transData, "transactions")
#The set of rules for predicting when the recommend type is detractor
detractorRules <- apriori(transSurvey, parameter = list(supp=0.09, conf=0.55), control=list(verbose=F), appearance=list(default="lhs",rhs=("df1.Recommend.type=Detractor")))
inspect(detractorRules)
#The set of rules for predicting when the recommend type is promoter
promoterRules <- apriori(transSurvey, parameter = list(supp=0.09, conf=0.55), control=list(verbose=F), appearance=list(default="lhs",rhs=("df1.Recommend.type=Promoter")))
inspect(promoterRules)

#Creating a subset of the transData data frame with just information about flight origin, flight destination and partner name
locationSet <- subset(transData, select=c(Destination.City, Origin.City, Partner.Name, Origin.State, Destination.State, df1.Recommend.type))
#Converting the above data frame into a transactions matrix
transLocation <- as(locationSet, "transactions")

#The set of rules for predicting when the recommend type is detractor. This uses the location and partner name data set to see any correlation
DlocationRules <- apriori(transLocation, parameter = list(supp=0.0009, conf=0.55), control=list(verbose=F), appearance=list(default="lhs",rhs=("df1.Recommend.type=Detractor")))
inspect(DlocationRules)
#The set of rules for predicting when the recommend type is promoter. This uses the location and partner name data set to see any correlation
PlocationRules <- apriori(transLocation, parameter = list(supp=0.0009, conf=0.55), control=list(verbose=F), appearance=list(default="lhs",rhs=("df1.Recommend.type=Promoter")))
inspect(PlocationRules)

#Creating dummy columns for the categorical variables in the data set which are listed below. We are removing the first dummy due to conflicts when running a linear regression model.
linearSet <- dummy_cols(survey2, select_columns = c("Airline.Status", "Gender", "Type.of.Travel", "Class", "Partner.Name", "df2.Age.group"), remove_first_dummy = TRUE)
#Removing all of the columns for which we created dummy variables and other columns which will be unecessary in a linear regression model.
linearSet <- subset(linearSet, select = -c(Scheduled.Departure.Hour, Flight.cancelled, Destination.City, Origin.City, Airline.Status, Gender, Type.of.Travel, Class, Flight.date, Partner.Code, Partner.Name, Origin.State, Destination.State, olong, olat, dlong, dlat, freeText, df1.Recommend.type, df2.Age.group ))
#Replacing all of the spaces in the airline partner names with underscores to avoid issues when calling the variable names
names(linearSet) <- str_replace_all(names(linearSet), c(" " = "_", "&" = ""))

#ModelNoTravel uses likelihood to recommend as the y variable and all of the variables in linearSet except the type of travel dummies as the explanatory variables.
modelNoTravel <- lm(formula = Likelihood.to.recommend ~., data=subset(linearSet, select= -c(Type.of.Travel_Mileage_tickets, Type.of.Travel_Personal_Travel)))
#Adjusted R-squared = 0.2399
summary(modelNoTravel)

#ModelNoStatus uses likelihood to recommend as the y variable and all of the variables in linearSet except the airline status dummies as the explanatory variables.
modelNoStatus <- lm(formula = Likelihood.to.recommend ~., data=subset(linearSet, select= -c(Airline.Status_Gold, Airline.Status_Platinum, Airline.Status_Silver)))
#Adjusted R-squared = 0.3565
summary(modelNoStatus)

#ModelNoPartner uses likelihood to recommend as the y variable and all of the variables in linearSet except the partner name dummies as the explanatory variables
modelNoPartner <-  lm(formula = Likelihood.to.recommend ~., data=subset(linearSet, select= -c(Partner.Name_CoolYoung_Airlines_Inc., Partner.Name_EnjoyFlying_Air_Services, Partner.Name_FlyFast_Airways_Inc.,
                                                                                              Partner.Name_FlyToSun_Airlines_Inc., Partner.Name_GoingNorth_Airlines_Inc., Partner.Name_GoingNorth_Airlines_Inc.,
                                                                                              Partner.Name_Northwest_Business_Airlines_Inc., Partner.Name_OnlyJets_Airlines_Inc., Partner.Name_Oursin_Airlines_Inc.,
                                                                                              Partner.Name_Paul_Smith_Airlines_Inc., Partner.Name_Sigma_Airlines_Inc., Partner.Name_Southeast_Airlines_Co.,
                                                                                              Partner.Name_West_Airways_Inc.)))
#Adjusted R-squared = 0.3969
summary(modelNoPartner)

#The final model that we are using incorporates all of the explanatory variables in the linearSet including all of the categorical dummies.
modelFinal <- lm(formula = Likelihood.to.recommend ~., data=linearSet)
#Adjusted R-squared = 0.408
summary(modelFinal)