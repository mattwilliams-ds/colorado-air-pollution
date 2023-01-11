# DSC550 Project
# Air Pollution in Colorado
#
# Matt Williams

library(stringr)
library(cluster)    # clustering algorithm
library(factoextra) # cluster visualization
library(caret)      # used for scaling data

# Load all data
setwd("/Users/matthewwilliams/school/Fall 2022/DSC550/Project")
setwd("/home/matt/Sync/school/Fall 2022/DSC550/Project/")
coAirData <- read.csv("coAirData.csv", header=T)

# Clean Data Set for MEAN value analysis
# remove records that have negative mean values (not possible)
coAirData <- coAirData[!(coAirData$NO2.Mean <=0.0),]
coAirData <- coAirData[!(coAirData$SO2.Mean <=0.0),]
coAirData <- coAirData[!(coAirData$CO.Mean <=0.0),]

# Create a month column & a year column
coAirData$Month <- as.integer(str_sub(coAirData$Date.Local[],6,7))
coAirData$Year <- as.integer(str_sub(coAirData$Date.Local[],1,4))

# isolate data by county
adamsMeanAirData <- coAirData[coAirData$County=="Adams", c(11,16,21,26)]
denverMeanAirData <- coAirData[coAirData$County=="Denver", c(11,16,21,26)]
jacksonMeanAirData <- coAirData[coAirData$County=="Jackson", c(11,16,21,26)]

remove(coAirData)

#adamsMaxAirData <- coAirData[coAirData$County.Code==1,c(12,17,22,27,30)]

# scaling method that scales all attributes to between 0 & 1
scaling <- preProcess(adamsMeanAirData, method = c("range"))
adamsMeanAirData <- predict(scaling, as.data.frame(adamsMeanAirData))
scaling <- preProcess(denverMeanAirData, method = c("range"))
denverMeanAirData <- predict(scaling, as.data.frame(denverMeanAirData))
scaling <- preProcess(jacksonMeanAirData, method = c("range"))
jacksonMeanAirData <- predict(scaling, as.data.frame(jacksonMeanAirData))

# Find k with Silhouette method
fviz_nbclust(adamsMeanAirData, kmeans, method="silhouette")
fviz_nbclust(denverMeanAirData, kmeans, method="silhouette")
fviz_nbclust(jacksonMeanAirData, kmeans, method="silhouette")

# build clusters
adamsMeanClu <- kmeans(adamsMeanAirData, 2, iter.max = 20, nstart = 25)
clusters <- data.frame(adamsMeanClu$size, adamsMeanClu$centers)
t(clusters)

denverMeanClu <- kmeans(denverMeanAirData, 2, iter.max = 20, nstart = 25)
clusters <- data.frame(denverMeanClu$size, denverMeanClu$centers)
t(clusters)

jacksonMeanClu <- kmeans(jacksonMeanAirData, 2, iter.max = 20, nstart = 25)
clusters <- data.frame(jacksonMeanClu$size, jacksonMeanClu$centers)
t(clusters)

fviz_cluster(countyMeanClu, data = countyMeanScaled, labelsize = 0)

# stop

# add cluster no. & month to county data
adamsMeanAirData$Month <- coAirData[coAirData$County=="Adams",30]
adamsMeanAirData$Cluster <- adamsMeanClu$cluster

denverMeanAirData$Month <- coAirData[coAirData$County=="Denver",30]
denverMeanAirData$Cluster <- denverMeanClu$cluster

jacksonMeanAirData$Month <- coAirData[coAirData$County=="Jackson",30]
jacksonMeanAirData$Cluster <- jacksonMeanClu$cluster

# Count qty of cluster 1 and cluster 2 in a given month

# NEED A DIFFERENT WAY TO DO THIS
adamsResults <- table(adamsMeanAirData$Month, adamsMeanAirData$Cluster)
denverResults <- table(denverMeanAirData$Month, denverMeanAirData$Cluster)
jacksonResults <- table(jacksonMeanAirData$Month, jacksonMeanAirData$Cluster)

# GGPLOTS BY COUNTY