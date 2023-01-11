# DSC550 Project
# Air Pollution in Colorado
#
# Matt Williams

library(praznik)

# Load all data
setwd("/home/matt/Sync/school/Fall 2022/DSC550/Project/")
setwd("/Users/matthewwilliams/Documents/Sync/school/Fall 2022/DSC550/Project")
coAirData <- read.csv("uspollution_pollution_us_2000_2016.csv",header=T)

# Slice off Colorado Observations
coAirData <- coAirData[(coAirData$State =="Colorado"),]
write.csv(coAirData, "coAirData.csv", row.names = FALSE)

# look at attributes available
names(coAirData)

# select mean pollutant data
correlationData <- coAirData[,c(11,16,21,26)] # mean values

# look at value ranges & basic statistics
summary(correlationData)

# remove observations with negative pollutant levels
correlationData <- correlationData[!(correlationData$NO2.Mean <=0.0),]
correlationData <- correlationData[!(correlationData$SO2.Mean <=0.0),]
correlationData <- correlationData[!(correlationData$CO.Mean <=0.0),]
summary(correlationData)

# create & view the correlation matrix
cor(correlationData)

# create performance analytics plot - praznik
pairs(correlationData)
