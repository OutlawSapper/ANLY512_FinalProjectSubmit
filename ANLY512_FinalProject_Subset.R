######################
# ANLY 512 Project   #
# Subset Selection   #
######################

############### AI: Initiate Sink ######
sink("subsetRF.txt")
######################################## 

####################### Data Cleaning Process: Courtesy of Tom ####################### 

#import required libraries
library(lubridate)
library(stringr)
library(geohashTools)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(randomForest)
library(gbm)
library(glmnet)
library(caret)

options(scipen=999)


# Import the Data & Make Conversions --------------------------------------

#import the data and check the quality of the import
TrafficDF <- read.csv('Traffic_Violations_Clean.csv', header = TRUE)

#convert all fillumns to appopriate types
TrafficDF$X            <- NULL   #drop the index fillumn
TrafficDF$SeqID        <- as.character(TrafficDF$SeqID)   #unqiue case number
TrafficDF$Date.time    <- as.POSIXct.Date(TrafficDF$Date.time, format = '%Y-%m-%d %H:%M:%S')  #convert to datetime bject
TrafficDF$Description  <- as.character(TrafficDF$Description)   #relatively unique inputs for each
TrafficDF$Location     <- as.character(TrafficDF$Location)   #relatively unique inputs for each
TrafficDF$Year         <- as.integer(as.character(TrafficDF$Year))   #convert to INT for easier manipulation
TrafficDF$Make         <- as.character(TrafficDF$Make)   #too many to types to check against (from personal cars to farm tractor brands)
TrafficDF$Model        <- as.character(TrafficDF$Model)   #relatively unique inputs
TrafficDF$Charge       <- as.character(TrafficDF$Charge)   #relatively unique inputs
TrafficDF$Driver.City  <- as.character(TrafficDF$Driver.City)   #relatively unique inputs

#all records have 'No' so it is not required...DROP
TrafficDF$Commercial.License <- NULL

# Geohash Manipulation ----------------------------------------------------
#translate latitude and longitude to geohash to enable better ML
TrafficDF$geohash <- as.factor(gh_encode(TrafficDF$Latitude, TrafficDF$Longitude, precision = 7L))

#dqcnnwr-4th Precicnt
#dqcq0h0-3rd Precicnt
#dqcnk0k-Correctional Facility



# Generate SubDF for Evaluation -------------------------------------------
#filter the dataframe to Warnings & Citations
subDF <- subset(TrafficDF, Violation.Type == 'Warning' | Violation.Type == 'Citation')

#convert Vehicle State and DL State to factors for DMV or otherwise for analysis about ticketing
subDF$VehState <- as.factor(ifelse(subDF$State %in% c('MD', 'DC', 'VA'), 'DMV', 'Other'))
subDF$DLState <- as.factor(ifelse(subDF$DL.State %in% c('MD', 'DC', 'VA'), 'DMV', 'Other'))

#create multiple infraction column for traffic stops with duplicate SeqID
dupIDs <- unique(subDF[duplicated(subDF$SeqID),'SeqID'])
subDF$MultiInfr <- subDF$SeqID %in% dupIDs

#both evaluations show an apparent change in citation rates based on DoW
#create a new column to capture this information
subDF$DoW <- as.factor(weekdays(subDF$Date.time))

#the hour of the traffic incident appears to be a significant factor in citation v. warning
#create a new column to capture this information
subDF$Hour <- format(subDF$Date.time, '%H')

##understand the relationship between categorical variables and the reponse variable
#seperate select categorical columns for analysis
factCols <- c('SubAgency', 'Conditions', 'Accident', 'Belts', 'Personal.Injury',
              'Property.Damage', 'Fatal', 'Commercial.License', 'HAZMAT',
              'Alcohol', 'Work.Zone', 'Search.Conducted','Contributed.To.Accident',
              'Search.Disposition', 'VehicleType', 'Race', 'Gender',
              'Asset.Type', 'Highway', 'MajorRoad', 'DLState', 'VehState', 'MultiInfr')


#reduce the number of levels in the 'Charge' Feature to enable ML algorithms
subDF$ShortCharge <- as.factor(str_extract(as.character(subDF$ShortCharge), '\\d\\d-\\d'))

# Random Forest (Understand Features)-------------------------------------------
#drop unique features or other factors unlikely to be useful during random forest
dropCols <- c('SeqID', 'Date.time', 'Description', 'Location', 'Latitude',
              'Longitude', 'Maximum.Temperature', 'Minimum.Temperature', 
              'Wind.Chill', 'Heat.Index', 'Snow.Depth', 'Wind.Gust', 'State',
              'Make', 'Model', 'Charge', 'Article', 'Driver.City', 'Driver.State',
              'DL.State', 'Arrest.Reason', 'Search.Type','Search.Reason', 
              'Search.Disposition', 'HighHeat', 'ExCold', 'geohash')

rfDF <- data.frame(subDF %>% select(-one_of(dropCols)))

#ShortCharge is too long for randomForest...clipping it to just the state title (first two numbers)
rfDF$ShortCharge <- as.factor(str_extract(as.character(rfDF$ShortCharge), '\\d\\d'))

#convert 'DoW' and 'Hour' to appropriate types
subDF$DoW <- as.factor(subDF$DoW)
subDF$Hour <- as.integer(subDF$Hour)

#drop any rows with NA values & drop factors with empty levels
rfDF <- rfDF[complete.cases(rfDF), ]
rfDF[] <- lapply(rfDF, function(x) if(is.factor(x)) factor(x) else x)

rfDF$Fatal              <- NULL
rfDF$HAZMAT             <- NULL
rfDF$Commercial.Vehicle <- NULL
rfDF$Work.Zone          <- NULL
rfDF$Alcohol            <- NULL
rfDF$Contributed.To.Accident <- NULL
#############################################################################################



######################## AI: Set up train and test sets ###################################### 
#set.seed(1234)
library(leaps)
library(gam)
library(ISLR)
n = length(rfDF$Violation.Type)
train = sample(n, 0.80*n)
dt.train = rfDF[train, ]
dt.test = rfDF[-train, ]
#############################################################################################



######################## AI: Full Logistic Model ############################################
model_logi <- glm(Violation.Type~., data = dt.train, family = 'binomial')
summary(model_logi)
preds <- predict(model_logi, newdata = dt.test, type = "response")
probabilities <- ifelse(preds < 0.5, "Citation", "Warning")
class.pred = table(probabilities, dt.test$Violation.Type)
class.pred
cat("The test error of logistic subset is", 1-sum(diag(class.pred))/sum(class.pred),"\n")
#############################################################################################



######################## AI: Full random forest Model  ######################################
model <- randomForest(Violation.Type~., data = dt.train, mtry = 4, ntree = 50, localImp = TRUE)
preds <- predict(model, newdata = dt.test)
class.pred = table(preds, dt.test$Violation.Type)
cat("The test error of RF total is", 1-sum(diag(class.pred))/sum(class.pred), "\n")
#############################################################################################



######################## AI: Forward Subset Selection   ######################################
reg.fit1 <- regsubsets(Violation.Type ~ ., data = dt.train, method = "forward", nvmax = 300)
reg1.summary <- summary(reg.fit1)
#open the file
jpeg("forward_subset.jpg")
par(mfrow = c(2, 2))
#BIC
plot(reg1.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg1.summary$bic), reg1.summary$bic[which.min(reg1.summary$bic)], col = "red", cex = 2, pch = 20)
#AIC
plot(reg1.summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
points(which.min(reg1.summary$rss), reg1.summary$rss[which.min(reg1.summary$rss)], col = "red", cex = 2, pch = 20)
#Rsqur
plot(reg1.summary$adjr2, xlab = "Number of variables", ylab = "R-Squared", type = "l")
points(which.max(reg1.summary$adjr2), reg1.summary$adjr2[which.max(reg1.summary$adjr2)], col = "red", cex = 2, pch = 20)
#Cp
plot(reg1.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
points(which.min(reg1.summary$cp), reg1.summary$cp[which.min(reg1.summary$cp)], col = "red", cex = 2, pch = 20)
#close it out
dev.off()
#############################################################################################



######################## AI: Backward Subset Selection   ######################################
reg.fit2 <- regsubsets(Violation.Type ~ ., data = dt.train, method = "backward", nvmax = 300)
reg2.summary <- summary(reg.fit2)
#open the file
jpeg("backward_subset.jpg")
par(mfrow = c(2, 2))
#BIC
plot(reg2.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg2.summary$bic), reg2.summary$bic[which.min(reg2.summary$bic)], col = "red", cex = 2, pch = 20)
#AIC
plot(reg2.summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
points(which.min(reg2.summary$rss), reg2.summary$rss[which.min(reg2.summary$rss)], col = "red", cex = 2, pch = 20)
#Rsqur
plot(reg2.summary$adjr2, xlab = "Number of variables", ylab = "R-Squared", type = "l")
points(which.max(reg2.summary$adjr2), reg2.summary$adjr2[which.max(reg2.summary$adjr2)], col = "red", cex = 2, pch = 20)
#Cp
plot(reg2.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
#close it out
points(which.min(reg2.summary$cp), reg2.summary$cp[which.min(reg2.summary$cp)], col = "red", cex = 2, pch = 20)
#close it out
dev.off()
#############################################################################################



########################## AI: Final Data Frame #############################################
data.frame(
  Method = c('Foward', 'Backward'),
  BIC = c(which.min(reg1.summary$bic),which.min(reg2.summary$bic)),
  RSS = c(which.min(reg1.summary$rss),which.min(reg2.summary$rss)),
  Adj.R2 = c(which.max(reg1.summary$adjr2),which.max(reg2.summary$adjr2)),
  CP = c(which.min(reg1.summary$cp),which.min(reg2.summary$cp))
)
#############################################################################################


############Quick look
sort(coef(reg.fit1, id = 128), decreasing = FALSE)[1:50]
#######################


########################## AI: Re-Run of Logistic model with subset #########################
##SubAgency, Temperature, Cloud.Cover, Relative.Humidity, Conditions, Accident, Belts, Personal.Injury, Property.Damage, Search.Conducted, VehicleType, Year, Color, Race, Gender, Arrest, Asset.Type, Highway, MajorRoad, ShortCharge, VehState
sub_model <- glm(Violation.Type~Accident+Personal.Injury+Property.Damage+Search.Conducted+VehicleType+Color+Race+Gender+Arrest+Asset.Type+Highway+ShortCharge+VehState+ MultiInfr, data = dt.train, family = 'binomial')
preds <- predict(sub_model, newdata = dt.test, type = "response")
probabilities <- ifelse(preds < 0.5, "Citation", "Warning")
class.pred = table(probabilities, dt.test$Violation.Type)
class.pred
cat("The test error of logistic subset is", 1-sum(diag(class.pred))/sum(class.pred),"\n")
#############################################################################################


########################## AI: Re-Run of RF model with subset ###################################
sub_model <- randomForest(Violation.Type~Accident+Personal.Injury+Property.Damage+Search.Conducted+VehicleType+Color+Race+Gender+Arrest+Asset.Type+Highway+ShortCharge+VehState+ MultiInfr, data = dt.train, mtry = 4, ntree = 50, localImp = TRUE) #, family = 'binomial')
preds <- predict(sub_model, newdata = dt.test)
class.pred = table(preds, dt.test$Violation.Type)
class.pred
cat("The test error of RF subset is", 1-sum(diag(class.pred))/sum(class.pred), "\n")
#################################################################################################

sink()
