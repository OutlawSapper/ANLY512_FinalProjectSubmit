#import required libraries
library(lubridate)

# Initial Import & Downsize (DON'T RUN AGAIN) -----------------------------------------------
##read-in and check the quality of the full-sized Traffic_Violations Dataset import
# DF <- read.csv('Traffic_Violations.csv')
# 
# head(DF)
# dim(DF)
# str(DF)
# 
# #convert 'Date.Of.Stop' to a Date object for easier manipulation
# DF$Date.Of.Stop <- as.Date(DF$Date.Of.Stop, format = '%m/%d/%Y')
# 
# #reduce the size of the dataset for easier computation (three-year period)
# TrafficDF <- DF[DF$Date.Of.Stop > as.Date('12/31/2016', format = '%m/%d/%Y'), ]
# TrafficDF <- TrafficDF[TrafficDF$Date.Of.Stop < as.Date('01/01/2020', format = '%m/%d/%Y'), ]
# 
# #save the subsetted dataset
# write.csv(TrafficDF, file = 'Traffic_Violations2.csv')


# Data Import & Preliminary Cleaning -------------------------------------------
##import reduced size dataset and check quality of import
TrafficDF <- read.csv('MergedTrafficWeather.csv',  na.strings = '')
head(TrafficDF)
dim(TrafficDF)
str(TrafficDF)

#convert all columns to appopriate types
TrafficDF$X            <- NULL   #drop the index column
TrafficDF$SeqID        <- as.character(TrafficDF$SeqID)   #unqiue case number
TrafficDF$Date.time    <- as.POSIXlt(TrafficDF$Date.time, format = '%Y-%m-%d %H:%M:%S')  #convert to datetime bject
# TrafficDF$Time.Of.Stop <- hms(TrafficDF$Time.Of.Stop)   #convert to Time Object
TrafficDF$Agency       <- NULL  #all records are 'MCP', so drop
TrafficDF$Description  <- as.character(TrafficDF$Description)   #relatively unique inputs for each
TrafficDF$Location     <- as.character(TrafficDF$Location)   #relatively unique inputs for each
TrafficDF$Heat.Index[TrafficDF$Heat.Index == 0] <- NA   #original a 0 was a NA value, NA is better for statistics
TrafficDF$Wind.Chill[TrafficDF$Temperature > 40 & TrafficDF$Wind.Chill == 0] <- NA   #original a 0 was a NA value, NA is better for statistics
TrafficDF$Search.Reason.For.Stop <- as.character(TrafficDF$Search.Reason.For.Stop)   #relatively unique inputs for each
TrafficDF$Year         <- as.integer(as.character(TrafficDF$Year))   #convert to INT for easier manipulation
TrafficDF$Make         <- as.character(TrafficDF$Make)   #too many to types to check against (from personal cars to farm tractor brands)
TrafficDF$Model        <- as.character(TrafficDF$Model)   #relatively unique inputs
TrafficDF$Charge       <- as.character(TrafficDF$Charge)   #relatively unique inputs
TrafficDF$Driver.City  <- as.character(TrafficDF$Driver.City)   #relatively unique inputs
TrafficDF$Geolocation  <- NULL  #already recorded in Lat/Long Columns

#confirm column type changes were appropriately registered
str(TrafficDF)




# Understand Dataset Cleanliness ------------------------------------------

score_clean <- function(TrafficDF){
  
  #extract column names for easy recall later
  featureNames <- colnames(TrafficDF)
  
  #evaluate the number of missing values in the data set
  missing <- sapply(TrafficDF, function(x) sum(is.na(x)))
  
  ##evaluate each column for values out of range
  #SeqID:           Unique Identifier...only invalid if NA
  #Date.Of.Stop:    Culled During Dataset Reduction
  #Time.Of.Stop:    Cohersion Ensured Cleanliness...inappropriate values would be returned NA
  #Description:     How to asses if appropriate...its a custom input field...
  #Location:        How to asses if appropriate...its a custom input field...
  #Latitude: Range(38.90 to 39.33)
  LatWrong   <- length(which(TrafficDF$Latitude > 39.50)) + length(which(TrafficDF$Latitude < 38.90)) - length(which(TrafficDF$Latitude == 0))
  LatMissing <- length(which(TrafficDF$Latitude==0))
  
  #Longitude: Range (-76.88 to -77.52)
  LonWrong   <- length(which(TrafficDF$Longitude < -77.52)) + length(which(TrafficDF$Longitude > -76.88)) - length(which(TrafficDF$Longitude == 0))
  LonMissing <- length(which(TrafficDF$Longitude==0))
  
  #Year: Range (invalid if before 1929 or after 2020)
  yearWrong   <- length(which(TrafficDF$Year < 1929)) + length(which(TrafficDF$Year > 2020)) - length(which(TrafficDF$Year == 0))
  yearMissing <- length(which(TrafficDF$Year == 0))
  
  #check all features with categorical variables for appropriate values
  # for (i in featureNames){
  #   if (class(TrafficDF[,i]) == 'factor') {
  #     print(i)
  #     print(levels(TrafficDF[,i]))}
  # }
  
  ##investigate the following features more indepth: State, Driver.State, DL.State
  #initialize a vector containing acceptable state abbreviations, including:
  #Canada Provinces, Territories, and ('XX' = Pedestrian or Similiar Violation, 'US' = Government Vehicle)
  #Except: MB (Manitoba (typo for MD))
  stateList <- append(state.abb, c('AB', 'BC', 'MB', 'NB', 'NL', 'NS', 'NT', 'NU', 'ON', 'QC', 'SK', 'YT', 'DC', 'AS', 'GU', 'PR', 'VI', 'US', 'XX'))
  
  #State Name (US Territories, DC, & Canadian Provinces)
  stateWrong <- length(TrafficDF$State) - length(which(TrafficDF$State %in% stateList))
  
  #Driver.State (Include Canadian Provinces)
  driverWrong <- length(TrafficDF$Driver.State) - length(which(TrafficDF$Driver.State %in% stateList))
  
  #DL.State (Include Canadian Provinces)
  DLWrong <- length(TrafficDF$DL.State) - length(which(TrafficDF$DL.State %in% stateList))
  
  #sum total number of missing data
  missing['Latitude']  <- missing['Latitude'] + LatMissing
  missing['Longitude'] <- missing['Longitude'] + LonMissing
  
  #print number of missing values in the dataset
  # print(missing)
  
  #sum number of missing/incorrect values in the dataset
  missing_wrong <- missing
  missing_wrong['Latitude']     <- missing_wrong['Latitude'] + LatWrong
  missing_wrong['Longitude']    <- missing_wrong['Longitude'] + LonWrong
  missing_wrong['Year']         <- missing_wrong['Year'] + yearWrong
  missing_wrong['State']        <- missing_wrong['State'] + stateWrong
  missing_wrong['Driver.State'] <- missing_wrong['Driver.State'] + driverWrong
  missing_wrong['DL.State']     <- missing_wrong['DL.State'] + DLWrong
  
  #print the total number of missing/incorrect values in the dataset
  print(missing_wrong)
  sum(missing_wrong)
  
  #return total DF cleanliness score (percent of available spaces appropriately filled)
  missingbad <- sum(missing_wrong) / (length(TrafficDF$SeqID) * length(featureNames))
  cat(round(missingbad*100, 2), '% of the data frame has misssing or incorrect values', sep = '')
}

score_clean(TrafficDF)



# Clean the Dataset -------------------------------------------------------

#drop rows with missing values from columns that are principal response
TrafficDF <- TrafficDF[complete.cases(TrafficDF[,c('Violation.Type', 'Fatal', 'Personal.Injury')]),]

#drop rows with low density missing values
TrafficDF <- TrafficDF[complete.cases(TrafficDF[,c('Model', 'Driver.City')]),]

#'Latitude' & 'Longitude' will be left with missing values as it should have limited impact on the analysis, while retaining a maximal number of rows
#drop rows from 'Latitude' with values outside of MOCO: Range(38.90 to 39.33)
TrafficDF <- TrafficDF[!(TrafficDF$Latitude > 39.33),]
TrafficDF <- TrafficDF[!(TrafficDF$Latitude < 38.90 & TrafficDF$Latitude > 0),]
TrafficDF$Latitude[TrafficDF$Latitude == 0] <- NA 

#drop rows from 'Latitude' with values outside of MOCO: Range (-76.88 to -77.52)
TrafficDF <- TrafficDF[!(TrafficDF$Longitude < -77.52),]
TrafficDF <- TrafficDF[!(TrafficDF$Longitude > -76.88 & TrafficDF$Longitude > 0),]
TrafficDF$Longitude[TrafficDF$Longitude == 0] <- NA 

##Search Columns: 
#drop 'Search.Outcome' & 'Search.Reason.For.Stop' (no apparent use to our research and results in large amounts of missing values)
TrafficDF$Arrest <- TrafficDF$Search.Outcome == 'Arrest'
TrafficDF$Search.Outcome <- NULL
TrafficDF$Search.Reason.For.Stop <- NULL
TrafficDF$Arrest.Reason <- TrafficDF$Search.Arrest.Reason
TrafficDF$Search.Arrest.Reason <- NULL
TrafficDF$Search.Conducted[is.na(TrafficDF$Search.Conducted)] <- 'No'
TrafficDF$Arrest[is.na(TrafficDF$Arrest)] <- FALSE

#'State': retain records where states are in the accepted 'stateList'
TrafficDF <- TrafficDF[(TrafficDF$State %in% stateList), ]

#for Canadian states check that vehicle plate matches Driver's License
#IOT eliminate typos (MB for MD or NB for NJ/NY)
TrafficDF <- TrafficDF[!(TrafficDF$State == 'MB' & TrafficDF$DL.State == 'MD'), ]
TrafficDF <- TrafficDF[!(TrafficDF$State == 'NB' & TrafficDF$DL.State == 'NJ'), ]
TrafficDF <- TrafficDF[!(TrafficDF$State == 'NB' & TrafficDF$DL.State == 'NY'), ]
TrafficDF <- TrafficDF[!(TrafficDF$State == 'PA' & TrafficDF$DL.State == 'NJ'), ]

#'Year'
#remove if year is unavailable for an automobile or light truck
TrafficDF <- TrafficDF[!(TrafficDF$Year == 0 & TrafficDF$VehicleType =='02 - Automobile'), ]
TrafficDF <- TrafficDF[!(TrafficDF$Year == 0 & TrafficDF$VehicleType =='03 - Station Wagon'), ]
TrafficDF <- TrafficDF[!(TrafficDF$Year == 0 & TrafficDF$VehicleType =='05 - Light Duty Truck'), ]

#remove if year is out of an acceptable range: 1925 to 2020
TrafficDF$Year[is.na(TrafficDF$Year)] <- 0   #must convert NA to 0 for the following evaluations
TrafficDF <- TrafficDF[!(TrafficDF$Year < 1925 & TrafficDF$Year > 0),]
TrafficDF <- TrafficDF[!(TrafficDF$Year > 2020),]
TrafficDF$Year[TrafficDF$Year == 0] <- NA   #must convert to NA for future computations

#'Driver.State': retain records where states are in the accepted 'stateList'
TrafficDF <- TrafficDF[(TrafficDF$Driver.State %in% stateList), ]

#'DL.State': retain records where states are in the accepted 'stateList'
TrafficDF <- TrafficDF[(TrafficDF$DL.State %in% stateList), ]

#'Article': add a level to the factor to address Safety Violations &
#fill all na values with this new level since the blanks only occur with Charge 'ESERO'
levels(TrafficDF$Article) <- c(levels(TrafficDF$Article), 'Safety Violation')
TrafficDF$Article[is.na(TrafficDF$Article)] <- 'Safety Violation'

#rename 'Arrest.Type' for ease of understanding
TrafficDF$Asset.Type <- TrafficDF$Arrest.Type
TrafficDF$Arrest.Type <- NULL

#ensure that all incidents charging pedestrians have similiar NA/None variables
TrafficDF$State[grepl('PEDESTRIAN FAILURE', TrafficDF$Description)] <- 'XX'
TrafficDF$State[grepl('PEDESTRIAN CROSS', TrafficDF$Description)] <- 'XX'
TrafficDF$State[grepl('PEDESTRIAN UNSAFELY', TrafficDF$Description)] <- 'XX'

TrafficDF$VehicleType[grepl('PEDESTRIAN', TrafficDF$Description) & TrafficDF$State == 'XX'] <- '29 - Unknown' 
TrafficDF$Year[grepl('PEDESTRIAN', TrafficDF$Description) & TrafficDF$State == 'XX'] <- NA
TrafficDF$Make[grepl('PEDESTRIAN', TrafficDF$Description) & TrafficDF$State == 'XX'] <- 'None'
TrafficDF$Model[grepl('PEDESTRIAN', TrafficDF$Description) & TrafficDF$State == 'XX'] <- 'None'
TrafficDF$Color[grepl('PEDESTRIAN', TrafficDF$Description) & TrafficDF$State == 'XX'] <- 'N/A'

#rearrange columns for easier use
TrafficDF <- TrafficDF[c(2, 1, 3, 4, 5, 6, 7, 37, 38, 39, 40, 41, 42, 43, 44, 45, 
                         46, 47, 48, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
                         19, 21, 22, 20, 23, 24, 25, 26, 27, 28, 29, 30, 32,
                         31, 33, 34, 35, 36, 49, 50, 51)]

#export cleaned dataset for future use
#write.csv(TrafficDF, file = 'Traffic_Violations_Clean.csv')

score_clean(TrafficDF)

#calculate effective dataset cleanliness
(577932+577932+577932+3568+585269)/(length(TrafficDF$SeqID)*length(colnames(TrafficDF)))

####NEXT STEPS####
#NOTE: CRTL+SHIFT+R inputs a chuck, which keeps the code clean
#binned categories
#generate for highway (495, 270, etc.)
#conduct summary statistical analysis

