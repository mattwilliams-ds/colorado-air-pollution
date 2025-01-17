# DSC550 Project
# Air Pollution in Colorado
#
# Matt Williams

# Load all data
setwd("/home/matt/Sync/school/Fall 2022/DSC550/Project/")
allAirData <- read.csv("uspollution_pollution_us_2000_2016.csv",header=T)

# Slice off Colorado Observations
coAirData <- allAirData[(allAirData$State =="Colorado"),]
write.csv(coAirData, "coAirData.csv", row.names = FALSE)

# Get allAirData out of memory
remove(allAirData)

# ======Correlation Analysis======================================

# Load Data
setwd("/home/matt/Sync/school/Fall 2022/DSC550/Project/")
coAirData <- read.csv("coAirData.csv",header=T)
summary(coAirData)

# Lines 28-34 are for performing a correlation analysis of the MEAN values
# Lines 36-39 are for performing a correlation analysis of the MAX values
# Pick one and move on to line 41 and beyond.

# Clean Data Set for MEAN value analysis
# remove records that have negative mean values (not possible)
coAirData <- coAirData[!(coAirData$NO2.Mean <=0.0),]
coAirData <- coAirData[!(coAirData$SO2.Mean <=0.0),]
coAirData <- coAirData[!(coAirData$CO.Mean <=0.0),]
# create a new data frame with just the mean pollutant levels
correlationData <- coAirData[,c(12,17,22,27)] # mean values

# Clean Data Set for MAX value analysis
# remove records that have negative max values (not possible)
coAirData <- coAirData[!(coAirData$SO2.1st.Max.Value <=0.0),]
correlationData <- coAirData[,c(13,18,23,28)] # max values

summary(correlationData)
correlation_matrix <- cor(correlationData)

# create performance analytics plot
library(praznik)
pairs(correlationData)

remove(coAirData, correlationData)

# ======Association Rules==========================================

# Load Data
setwd("/home/matt/Sync/school/Fall 2022/DSC550/Project/")
coAirData <- read.csv("coAirData.csv")

# Clean Data Set
# remove records that have 'NA' for AQI values
coAirData <- coAirData[!is.na(coAirData$NO2.AQI),]
coAirData <- coAirData[!is.na(coAirData$SO2.AQI),]
coAirData <- coAirData[!is.na(coAirData$CO.AQI),]
coAirData <- coAirData[!is.na(coAirData$O3.AQI),]

# Write data to CSV for processing in spreadsheet
write.csv(coAirData, "associationData.csv", row.names = FALSE)

# in the spreadsheet:
# 1. factor the county and high/low values for the AQI's
# 2. use the date column to flag & factor by month of reading
# 3. filter out observations with no high readings
# 4. export to csv

# read in processed data
coAirRules <- read.csv("rulesData.csv", header=T, colClasses='factor')

# convert zeros to NA for the apriori algorithm
coAirRules[coAirRules=="0"] <- NA

library(arules)
rules <- apriori(minlen=3, coAirRules, parameter=list(supp=0.01,conf=0.5))
inspect(rules)
write.csv(as(rules, "data.frame"), "rulesOutput.csv")

# ======k-means====================================================
setwd("/Users/matthewwilliams/school/Fall 2022/DSC550/Project")
setwd("/home/matt/Sync/school/Fall 2022/DSC550/Project/")
coAirData <- read.csv("coAirData.csv",header=T)

# Clean Data Set for MEAN value analysis
# remove records that have negative mean values (not possible)
coAirData <- coAirData[!(coAirData$NO2.Mean <=0.0),]
coAirData <- coAirData[!(coAirData$SO2.Mean <=0.0),]
coAirData <- coAirData[!(coAirData$CO.Mean <=0.0),]

# Create a month column & a year column
library(stringr)
coAirData$Month <- as.integer(str_sub(coAirData$Date.Local[],6,7))
coAirData$Year <- as.integer(str_sub(coAirData$Date.Local[],1,4))

# isolate county data
county <- "Jackson" # Adams, Denver, or Jackson
countyMeanAirData <- coAirData[coAirData$County==county,c(11,16,21,26)]
head(countyMeanAirData)
summary(countyMeanAirData)
#adamsMaxAirData <- coAirData[coAirData$County.Code==1,c(12,17,22,27,30)]

# scaling method that scales all attributes to between 0 & 1
library(caret)
scaling <- preProcess(countyMeanAirData, method = c("range"))
countyMeanScaled <- predict(scaling, as.data.frame(countyMeanAirData))

summary(countyMeanScaled)
head(countyMeanScaled)

# Load clustering libraries
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# Find k with Silhouette method, runs in less than 1 minute
fviz_nbclust(countyMeanScaled, kmeans, method="silhouette")

# build clusters
countyMeanClu <- kmeans(countyMeanScaled, 2, iter.max = 20, nstart = 25)
clusters <- data.frame(countyMeanClu$size, countyMeanClu$centers)
t(clusters)
fviz_cluster(countyMeanClu, data = countyMeanScaled, labelsize = 0)

t(aggregate(countyMeanScaled, by=list(cluster=countyMeanClu$cluster), mean))

countyMeanAirData$Month <- coAirData[coAirData$County==county,30]
countyMeanAirData$Year <- coAirData[coAirData$County==county,31]
countyMeanAirData$Cluster <- countyMeanClu$cluster

# Count qty of cluster 1 and cluster 2 in a given month

table(countyMeanAirData$Cluster[countyMeanAirData$Month==1])