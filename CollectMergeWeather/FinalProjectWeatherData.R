# Load all the necessary packages
library(urltools)
library(httr)
library(tidyverse)
library(tidyr)
library(chron)
# COLLECT WEATHER DATA
# Define the query path to the weather API
#query_path <- 'https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=1&startDateTime=2017-01-01T00%3A00%3A00&endDateTime=2019-12-31T00%3A00%3A00&collectStationContributions=false&maxStations=-1&maxDistance=-1&includeNormals=false&shortColumnNames=false&sendAsDatasource=false&allowAsynch=false&contentType=csv&unitGroup=us&key=CCZKWZ2TPJCPLQ5JDCH5QPCA8&locations=Gaithersburg%2C%20MD'
#url <- URLdecode(query_path)
#base_path = paste(url_parse(url)$scheme,'://',url_parse(url)$domain,'/',url_parse(url)$path,sep = "")
#urlbreakdown <- param_get(url)

#request <- GET(url=base_path,
#               query=list(aggregateHours=urlbreakdown$aggregateHours[1],
#                          startDateTime=urlbreakdown$startDateTime[1],
#                          endDateTime=urlbreakdown$endDateTime[1],
#                          shortColumnNames=urlbreakdown$shortColumnNames[1],
#                          contentType=urlbreakdown$contentType[1],
#                          unitGroup=urlbreakdown$unitGroup[1],
#                          key=urlbreakdown$key[1],
#                          locations=urlbreakdown$locations[1]))

#response <- content(request, as = "text", encoding = "UTF-8")


# MERGE WEATHER AND TRAFFIC VIOLATION DATA

weather <- read.csv("hrlyWeather01-2017_12-2019.csv")
traffic <- read.csv("Traffic_Violations2.csv")

# Basic cleaning of the weather data
# Remove two unnecessary columns
dropCols <- c("Location","Resolved.Address")
weather <- weather[,!(names(weather) %in% dropCols)]
# Address to a character value
weather$Address <- as.character(weather$Address)
# Date to a date type value
weather$Date.time <- strptime(weather$Date.time, format = "%m/%d/%Y %H:%M")
# All NAs are from zeros in specific weather features, so replace them with 0
weather[is.na(weather)] <- 0

# Traffic Data
# Remove the extra index column
traffic <- traffic[,!(names(traffic) %in% "X")]
# Make sure traffic date column is in same format
# Convert to Characters
traffic$Date.Of.Stop <- as.character(traffic$Date.Of.Stop)
traffic$Time.Of.Stop <- as.character(traffic$Time.Of.Stop)
# Merge into one column and drop the old ones
traffic <- unite(traffic, Date.time, c(Date.Of.Stop,Time.Of.Stop), sep = " ", remove = TRUE)
traffic$Date.time <- strptime(traffic$Date.time, format = "%Y-%m-%d %H:%M:%S")

# Merge traffic and weather data by dates and times
# Order both by Date.time to check work observationally
traffic <- traffic[order(traffic$Date.time),]
weather <- weather[order(weather$Date.time),]
# Round traffic violation date and time to the nearest hour to match weather data
traffic$Date.time <- round(traffic$Date.time, units="hours")
# Join the weather and traffic data
trafficWeatherMerged <- merge(traffic,weather, by=intersect(names(traffic),names(weather)),sort=FALSE)
# Drop weather address column
trafficWeatherMerged <- subset(trafficWeatherMerged,select = -Address)
# Write to a csv
write.csv(trafficWeatherMerged,"MergedTrafficWeather.csv")

