#import required libraries
library(lubridate)
library(stringr)

# Import the Data & Make Conversions --------------------------------------

#import the data and check the quality of the import
TrafficDF <- read.csv('Traffic_Violations_Clean.csv', header = TRUE)

str(TrafficDF)
head(TrafficDF)

#convert all columns to appopriate types
TrafficDF$X            <- NULL   #drop the index column
TrafficDF$SeqID        <- as.character(TrafficDF$SeqID)   #unqiue case number
TrafficDF$Date.time    <- as.POSIXlt(TrafficDF$Date.time, format = '%Y-%m-%d %H:%M:%S')  #convert to datetime bject
TrafficDF$Description  <- as.character(TrafficDF$Description)   #relatively unique inputs for each
TrafficDF$Location     <- as.character(TrafficDF$Location)   #relatively unique inputs for each
TrafficDF$Year         <- as.integer(as.character(TrafficDF$Year))   #convert to INT for easier manipulation
TrafficDF$Make         <- as.character(TrafficDF$Make)   #too many to types to check against (from personal cars to farm tractor brands)
TrafficDF$Model        <- as.character(TrafficDF$Model)   #relatively unique inputs
TrafficDF$Charge       <- as.character(TrafficDF$Charge)   #relatively unique inputs
TrafficDF$Driver.City  <- as.character(TrafficDF$Driver.City)   #relatively unique inputs

TrafficDF$Heat.Index[TrafficDF$Heat.Index == 0] <- NA
TrafficDF$Wind.Chill[TrafficDF$Temperature > 40 & TrafficDF$Wind.Chill == 0] <- NA


# Generate Features ---------------------------------------------
#generate feature for highways and major roadways from the location data
TrafficDF$Highway <- grepl('270|495', TrafficDF$Location)

#determined using Google Maps 'yellow roads' which are multi-lane
majorRoad <- '270|495|OLD GEORGETOWN|187|355|WISCONSIN| 
              200|29|190|191|188|410|185|390|
              97|193|586|193|VEIRS MILL|GEORGIA|112|
              28|200|27|320|650|500|UNIVERSITY|
              CONNECTICUT|NEW HAMPSHIRE|RIVER RD|BRADLEY BLVD|WILSON LN|
              16TH|QUEENS CHAPEL|RIDGE RD|PINEY BRANCH'

TrafficDF$MajorRoad <- grepl(majorRoad, TrafficDF$Location)

#create a general charge code (i.e. 21-801, 21-201) to allow for featurization of this category
#the general municipal code is sufficient to issue the incident question
TrafficDF$ShortCharge <- str_extract(TrafficDF$Charge, '\\d+-\\d\\d\\d')

for (i in seq(1, length(TrafficDF$ShortCharge))) {
  if (is.na(TrafficDF[i, 'ShortCharge'])){
    TrafficDF[i, 'ShortCharge'] <- substring(TrafficDF[i, 'Charge'], 1, 2)
  } 
}

#convert to a factor
TrafficDF$ShortCharge <- as.factor(TrafficDF$ShortCharge)

sort(table(TrafficDF$Short))

#NOTE: CRTL+SHIFT+R inputs a chuck, which keeps the code clean

#save the dataframe with the new features
write.csv(TrafficDF, file = 'Traffic_Violations_Clean.csv')
