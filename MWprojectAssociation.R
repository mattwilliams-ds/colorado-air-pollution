# DSC550 Project
# Air Pollution in Colorado
#
# Matt Williams

library(arules)    #association rules library
library(lubridate) #for dealing with dates
library(dplyr)     #

# Load Data
setwd("/home/matt/Sync/school/Fall 2022/DSC550/Project/")
setwd("/Users/matthewwilliams/Documents/Sync/school/Fall 2022/DSC550/Project")
coAirData <- read.csv("coAirData.csv")

names(coAirData)

# Save county code, date, and AQI values to use for association rules
associationData <- coAirData[,c(7,9,14,19,24,29)]
remove(coAirData)

# Clean Data Set
# remove observations that have 'NA' for AQI values
associationData <- associationData[!is.na(associationData$NO2.AQI),]
associationData <- associationData[!is.na(associationData$SO2.AQI),]
associationData <- associationData[!is.na(associationData$CO.AQI),]
associationData <- associationData[!is.na(associationData$O3.AQI),]

# 1. factor the high/low values for the AQI's
associationData$NO2.HIGH <- 0
associationData$SO2.HIGH <- 0
associationData$CO.HIGH <- 0
associationData$O3.HIGH <- 0
associationData$NO2.HIGH[associationData$NO2.AQI > 50] <- 1
associationData$SO2.HIGH[associationData$SO2.AQI > 50] <- 1
associationData$CO.HIGH[associationData$CO.AQI > 50] <- 1
associationData$O3.HIGH[associationData$O3.AQI > 50] <- 1

# 2. filter out observations with no high readings
associationData <- associationData[rowSums(associationData[,7:10]) > 0,]

# 3. use the date column to factor by season
associationData$Month <- month(ymd(associationData$Date.Local))
associationData$Spring[associationData$Month >= 3 & associationData$Month <= 5] <- 1
associationData$Summer[associationData$Month >= 6 & associationData$Month <= 8] <- 1
associationData$Fall[associationData$Month >= 9 & associationData$Month <= 11] <- 1
associationData$Winter[associationData$Month == 12 |
                       associationData$Month == 1  |
                       associationData$Month == 2  ] <- 1

# 3.5 factor counties
associationData$County.Adams <- 0
associationData$County.Denver <- 0
associationData$County.Jackson <- 0
associationData$County.Adams[associationData$County == "Adams"] <- 1
associationData$County.Denver[associationData$County == "Denver"] <- 1
associationData$County.Jackson[associationData$County == "Jackson"] <- 1

# 4. convert zeros to NA for the apriori algorithm
ruleData <- associationData[,c(c(7:10),c(12:18))]
ruleData[ruleData=="0"] <- NA

# 5. convert values to factors
ruleData <- data.frame(lapply(ruleData,factor))
str(ruleData)
rules <- apriori(minlen=3, ruleData, parameter=list(supp=0.01,conf=0.5))
inspect(rules)
write.csv(as(rules, "data.frame"), "newRulesOutput.csv")

