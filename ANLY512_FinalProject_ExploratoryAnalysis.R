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
library(doParallel)
library(caret)
library(randomForestExplainer)
library(ROCR)

options(scipen=999)

# Import the Data & Make Conversions --------------------------------------

#import the data and check the quality of the import
TrafficDF <- read.csv('Traffic_Violations_Clean.csv', header = TRUE)

str(TrafficDF)
head(TrafficDF)

#convert all fillumns to appopriate types
TrafficDF$X            <- NULL   #drop the index fillumn
TrafficDF$SeqID        <- as.character(TrafficDF$SeqID)   #unqiue case number
TrafficDF$Date.time    <- as.POSIXlt(TrafficDF$Date.time, format = '%Y-%m-%d %H:%M:%S')  #convert to datetime bject
TrafficDF$Description  <- as.character(TrafficDF$Description)   #relatively unique inputs for each
TrafficDF$Location     <- as.character(TrafficDF$Location)   #relatively unique inputs for each
TrafficDF$Year         <- as.integer(as.character(TrafficDF$Year))   #convert to INT for easier manipulation
TrafficDF$Make         <- as.character(TrafficDF$Make)   #too many to types to check against (from personal cars to farm tractor brands)
TrafficDF$Model        <- as.character(TrafficDF$Model)   #relatively unique inputs
TrafficDF$Charge       <- as.character(TrafficDF$Charge)   #relatively unique inputs
TrafficDF$Driver.City  <- as.character(TrafficDF$Driver.City)   #relatively unique inputs

summary(TrafficDF)

#look at Commercial Vehicle further
summary(TrafficDF$Commercial.Vehicle)

#all records have 'No' so it is not required...DROP
TrafficDF$Commercial.License <- NULL

# Geohash Manipulation ----------------------------------------------------
#translate latitude and longitude to geohash to enable better ML
TrafficDF$geohash <- as.factor(gh_encode(TrafficDF$Latitude, TrafficDF$Longitude, precision = 7L))

summary(TrafficDF$geohash)
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

#confirm the edits
summary(subDF)
nrow(subDF)
summary(subDF$Violation.Type)


# Preliminary Plots -----------------------------------------------------------
#####EDA with 'Violation.Type' as the response variable
#bar chart of response variable
ggplot(subDF, aes(Violation.Type)) +
    geom_bar()

##plot Violation.Type relative to Date.Time information
#bar chart and table of Day of Week information
ggplot(subDF, aes(wday(subDF$Date.time, label=TRUE), fill = Violation.Type)) +
  geom_bar() +
  theme_tufte() +
  scale_fill_manual(values=c('#E07715', '#157EE0')) +
  theme(text = element_text(size = 10, family = 'TT Arial')) +
  labs(x = 'Day', y = 'Total Count', title = 'Traffic Violations',
       subtitle = 'Comparison of Traffic Citations and Warnings by Day of Week',
       fill = 'Violation Type')

subDF %>% 
  group_by(weekdays(Date.time)) %>%
  count(Violation.Type) %>% 
  mutate(DailyFreq = n/sum(n))

#both evaluations show an apparent change in citation rates based on DoW
#create a new column to capture this information
subDF$DoW <- as.factor(weekdays(subDF$Date.time))

#bar chart and table for day of the month (i.e. do cops write more tickets towards the end of the month)
ggplot(subDF, aes(format(Date.time, '%dd'), fill = Violation.Type)) +
  geom_bar()

subDF %>% 
  group_by(format(subDF$Date.time, '%dd')) %>%
  count(Violation.Type) %>% 
  mutate(DailyFreq = n/sum(n))
#^^day of month appears not to be significant factor in citation v. warning

#bar chart and table for hour of the day information
ggplot(subDF, aes(format(Date.time, '%H'), fill = Violation.Type)) +
  geom_bar()

ggplot(subDF, aes(format(Date.time, '%H'), fill = Violation.Type)) +
  geom_bar() +
  theme_tufte() +
  scale_fill_manual(values=c('#E07715', '#157EE0')) +
  theme(text = element_text(size = 10, family = 'TT Arial')) +
  labs(x = 'Hour (24)', y = 'Total Count', title = 'Traffic Violations',
       subtitle = 'Comparison of Traffic Citations and Warnings by Hour',
       fill = 'Violation Type') +
  scale_x_discrete(breaks = c('00', '04', '08', 12, 16, 20, 23))

subDF %>% 
  group_by(format(subDF$Date.time, '%H')) %>%
  count(Violation.Type) %>% 
  mutate(DailyFreq = n/sum(n))

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

#iterate through plotting and generating summary tables to understand these factors
#save resulting information to a .csv and plot to a .png for easier access and reading
for (i in factCols) {
  g <- ggplot(subDF, aes_string(i, fill = subDF$Violation.Type)) + 
    geom_bar() +
    theme_tufte() +
    labs(y = 'Count', fill = "Legend (Color)") +
    ggtitle(paste('Violation Type for', i))

  name <- paste("SummaryStats/ANLY512_FinalProject_ViolationCharts_", i,".png", sep = "")

  ggsave(name, g, height = 7, width = 5)
  
  name <- paste("SummaryStats/ANLY512_FinalProject_ViolationTable_", i,".csv", sep = "")
  
  subDF %>% 
    group_by(!!sym(i)) %>%
    count(Violation.Type) %>% 
    mutate(DailyFreq = n/sum(n)) %>% 
    filter(Violation.Type == 'Citation') %>% 
    write.csv(name, row.names = FALSE)
}

##understand response variable relative to continuous variables
#histogram for temperature at time of traffic stop
ggplot(subDF, aes(Temperature, fill = Violation.Type)) +
  geom_histogram(binwidth = 5, color = 'white')

subDF %>% 
  mutate(bin = ntile(Temperature, 10)) %>%
  group_by(bin) %>%
  count(Violation.Type) %>% 
  mutate(DailyFreq = n/sum(n)) %>% 
  filter(Violation.Type == 'Citation')
#slightly less likely to get a ticket during higher temperatures 
#much more likely to get a temperature when its cold

#histogram & table for precipitation at time of traffic stop
subDF %>% 
  select(Precipitation, Violation.Type) %>% 
  filter(Precipitation != 0) %>% 
  ggplot(aes(Precipitation, fill = Violation.Type)) +
    geom_histogram(bins = 10, color = 'white')
#^doesn't tell you anything

subDF %>% 
  mutate(bin = ntile(Precipitation, 5)) %>%
  group_by(bin) %>%
  count(Violation.Type) %>% 
  mutate(DailyFreq = n/sum(n)) %>% 
  filter(Violation.Type == 'Citation')
#slightly less likely to get a ticket when it is raining

#histogram & table for precipitation at time of traffic stop
ggplot(subDF, aes(Wind.Speed, fill = Violation.Type)) +
  geom_histogram(bins = 10, color = 'white')
#limited value

subDF %>% 
  mutate(bin = ntile(Wind.Speed, 10)) %>%
  group_by(bin) %>%
  count(Violation.Type) %>% 
  mutate(DailyFreq = n/sum(n)) %>% 
  filter(Violation.Type == 'Citation')
#not much change depending on windspeed after a certain point--maybe impractical to write a ticket?

#histogram & table for vehicle year
ggplot(subDF, aes(Year, fill = Violation.Type)) +
  geom_histogram(bins = 50, color = 'white')
#limited value

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

#run a Random Forest model to understand feature importance (VERY TIME CONSUMING)
#trafficRF <- randomForest(Violation.Type ~ ., data = rfDF, mtry = 4, ntree = 100, importance = TRUE)

#understand the feature mportance metrics & export for posterity
sink('ANLY512_FinalProject_InitialRF.txt')
trafficRF
importance(trafficRF)
sink()

#generate the variable importance plot & export for posterity
png('ANLY512_FinalProject_InitialRF.png', width = 1500, height = 1000)
varImpPlot(trafficRF, main = 'Initial Random Forest')
dev.off()

#understand the results & save the model
trafficRF
save(trafficRF, file = "my_model1.rda")

#drop variables with low prediction capability in Random Forest &
#those with very, very low variance (i.e. <1000 observations)
rfDF$Fatal              <- NULL
rfDF$HAZMAT             <- NULL
rfDF$Commercial.Vehicle <- NULL
rfDF$Work.Zone          <- NULL
rfDF$Alcohol            <- NULL
rfDF$Contributed.To.Accident <- NULL

# GBM Model (DON'T RERUN) ---------------------------------------------------------------
#subset the data for the training set
# trainDF <- rfDF[sample(nrow(rfDF), size = nrow(rfDF)*.6), ]

#construct the control parameters and enable parallel computing
# ctrl <- trainControl(method = "repeatedcv", number = 5, allowParallel = T)

#build grid for variable combinations to be evaluated
# grid <- expand.grid(n.trees = c(10, 50, 100), interaction.depth=c(1:3), 
                    # shrinkage=c(.01, 0.1), n.minobsinnode=c(20))

#setup parallel computing to speed up process
# registerDoParallel(detectCores() - 1)

#train the model
# unwantedoutput <- capture.output(gbm_fit <- train(Violation.Type ~ ., data = trainDF, 
                                                   # method = 'gbm', trControl = ctrl, 
                                                   # tuneGrid = grid))

#make modifications to varImp for final model to allow for clean export
temp <- varImp(gbm_fit$finalModel)
temp <- cbind(Variable = rownames(temp), temp)
rownames(temp) <- 1:nrow(temp)

#understand the results and capture in a .txt file
sink('ANLY512_FinalProject_InitGBM.txt')
cat('##########', 'Initial GBM Results', '##########\n')
gbm_fit
cat('\n##########', 'Initial GBM Variable Importance', '##########\n')
varImp(gbm_fit)
cat('\n##########', 'Initial GBM Best Tune', '##########\n')
gbm_fit$bestTune
cat('\n##########', 'Initial GBM Final Model', '##########\n')
pretty.gbm.tree(gbm_fit$finalModel)
cat('\n##########', 'Initial GBM Final Model Variable Importance', '##########\n')
temp[order(-temp$Overall),]
sink()

#export GBM Model Plot
png('ANLY512_FinalProject_InitialGBM.png', width = 1500, height = 1000)
plot(gbm_fit)
dev.off()

##run predictive analysis using the test data set
#seperate the test rows from the training rows
# sampleRows <- as.numeric(rownames(trainDF))
# testDF <- subDF[-sampleRows,]

#make the predictions based on the testDF
preds <- predict(gbm_fit, newdata = testDF)

#understand and export the results
sink('ANLY512_FinalProject_InitGBM.txt', append = TRUE)
cat('\n##########', 'Initial GBM Predict Confusion Matrix', '##########\n')
confusionMatrix(preds, testDF$Violation.Type)
sink()

save(gbm_fit, file = "gbm_model.rda")

# Random Forest for Prediction(DON'T RERUN) --------------------------------------------
trainDF <- subDF[sample(nrow(rfDF), size = nrow(rfDF)*.70), ]

#build three Random Forest models to learn ideal tree size
# rf_fit50  <- randomForest(Violation.Type ~ ., data = trainDF, mtry = 4, ntree = 50, importance = TRUE)
# rf_fit100 <- randomForest(Violation.Type ~ ., data = trainDF, mtry = 4, ntree = 100, importance = TRUE)
# rf_fit150 <- randomForest(Violation.Type ~ ., data = trainDF, mtry = 4, ntree = 150, importance = TRUE)

#build three Random Forest models to learn ideal mtry using ntree 50 which provided very good results
# rf_fit2  <- randomForest(Violation.Type ~ ., data = trainDF, mtry = 2, ntree = 50, importance = TRUE)
# rf_fit6 <- randomForest(Violation.Type ~ ., data = trainDF, mtry = 6, ntree = 50, importance = TRUE)

#understand the results
sink('ANLY512_FinalProject_FinalRF.txt', append = TRUE)
cat('\n##########', 'Final RF Training Results', '##########\n')
cat('\n##########', '50 Trees', '##########\n')
rf_fit50
cat('\n##########', '100 Trees', '##########\n')
rf_fit100
cat('\n##########', '150 Trees', '##########\n')
rf_fit150
cat('\n##########', '2 Mtry', '##########\n')
rf_fit2
cat('\n##########', '6 Mtry', '##########\n')
rf_fit6
sink()

#evaulate models against the test set & record results to output file
sampleRows <- as.numeric(rownames(trainDF))
testDF <- subDF[-sampleRows,]

sink('ANLY512_FinalProject_FinalRF.txt', append = TRUE)
cat('\n##########', 'Final RF Training Results Confusion Matrix', '##########\n')
cat('\n##########', '50 Trees Confusion Matrix', '##########\n')
preds50  <- predict(rf_fit50, newdata = testDF)
confusionMatrix(preds50, testDF$Violation.Type)

cat('\n##########', '100 Trees Confusion Matrix', '##########\n')
preds100 <- predict(rf_fit100, newdata = testDF)
confusionMatrix(preds100, testDF$Violation.Type)

cat('\n##########', '150 Trees Confusion Matrix', '##########\n')
preds150 <- predict(rf_fit150, newdata = testDF)
confusionMatrix(preds150, testDF$Violation.Type)

cat('\n##########', '2 Mtry Confusion Matrix', '##########\n')
preds2 <- predict(rf_fit2, newdata = testDF)
confusionMatrix(preds2, testDF$Violation.Type)

cat('\n##########', '6 Mtry Confusion Matrix', '##########\n')
preds6 <- predict(rf_fit6, newdata = testDF)
confusionMatrix(preds6, testDF$Violation.Type)
sink()

png('ANLY512_FinalProject_FinalRFVarImp.png', width = 1500, height = 1000)
varImpPlot(rf_fit50)
dev.off()

png('ANLY512_FinalProject_FinalRFTreeError.png', width = 1500, height = 1000)
varImpPlot(rf_fit150, n.var = 10, main = 'Random Forest Feature Importance', cex = 1.25)
dev.off()

plot(rf_fit50)

save(rf_fit50, file = "rf_fit50.rda")

# RF Explain (TEST) -------------------------------------------------------
#run a test RF with interaction effects considered (time consuming)
# rf_fit502  <- randomForest(Violation.Type ~ ., data = trainDF, mtry = 4, ntree = 50, localImp = TRUE)
summary(rf_fit502)

#calculate confusion matrix
preds502 <- predict(rf_fit502, newdata = testDF)
confusionMatrix(preds502, testDF$Violation.Type)
##^^did not outperform the best model which is rf_fit50

#evaulate the best model for mean depth of feature in trees
sink('ANLY512_FinalProject_RFminDepth.txt', append = TRUE)
# min_depth_frame <- min_depth_distribution(rf_fit50)
save(min_depth_frame, file = "min_depth_frame.rda")
print(min_depth_frame)
sink()

#understand the resulting data frame
head(min_depth_frame)

#save resulting plot for future use
png('ANLY512_FinalProject_minDepthFrame.png', width = 1250, height = 1000)
plot_min_depth_distribution(min_depth_frame, k = 15, mean_sample = "top_trees",
                            main = 'Feature Distribution by Minimal Depth and Means')
dev.off()

#evaluate the final model for feature importance
sink('ANLY512_FinalProject_RFmeasureImp.txt', append = TRUE)
# importance_frame <- measure_importance(rf_fit50)
save(importance_frame, file = "importance_frame.rda")
print(importance_frame)
sink()

#understand the resulting data frame
head(importance_frame)

#save resulting plots for future use (two variations)
png('ANLY512_FinalProject_VarImportance1.png', width = 1250, height = 1000)
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes", 
                          no_of_labels = 10, main = 'Variable Importance Scatter Plot')
dev.off()

png('ANLY512_FinalProject_VarImportance2.png', width = 1250, height = 1000)
plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease", 
                          y_measure = "gini_decrease", size_measure = "times_a_root", 
                          no_of_labels = 5, main = 'Variable Importance Scatter Plot',
                          xlab = 'Gini Decrease')
dev.off()

importance_frame

importance_frame$rank <- ifelse(importance_frame$accuracy_decrease > .045 |
                                importance_frame$gini_decrease > 12400 , 'Top', 'Other')

ggplot(importance_frame, aes(x = accuracy_decrease, y = gini_decrease)) +
  geom_point(aes(size = times_a_root, color = rank)) +
  theme_tufte() +
  theme(text = element_text(family = 'TT Arial'),
        plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) +
  scale_color_manual(values=c('#6B6B6B', '#223DB4')) +
  labs(x = 'Accuracy Decrease', y = 'Gini Decrease', title = 'Random Forest Feature Importance',
       subtitle = "Understanding multiple aspects of feature importance",
       size = 'Avg. Root Node', color = 'Top Variable?')




#compare the measures using ggpairs
#save resulting plot for future use (two variations)
png('ANLY512_FinalProject_MeasureImp.png', width = 1250, height = 1000)
plot_importance_ggpairs(importance_frame,measures = c('gini_decrease', 'mean_min_depth', 'no_of_nodes', 'accuracy_decrease'))
dev.off()

plot_importance_rankings(importance_frame)

# Logistic Regression -----------------------------------------------------
#create local copy of the subDF for regression analysis
regDF <- subDF

#drop unique features or other factors unlikely to be useful during logistic regression
#drop features identified as weak predictors from Random Forest as well
dropCols <- append(dropCols, c('Fatal', 'HAZMAT', 'Commercial.Vehicle', 'Work.Zone',
                               'Alcohol', 'Contributed.To.Accident'))
regDF <- data.frame(regDF %>% select(-one_of(dropCols)))

#convert hour to a factor for logistic regression
regDF$Hour <- as.factor(regDF$Hour)

#understand the data's structure
str(regDF)
summary(regDF)

#remove low density factors from the dataframe & drop the unused levels
dropColor  <- c('CAMOUFLAGE', 'CHROME')
dropCharge <- c('10', '12', '15', '18', '26', '27', '70')
dropAsset  <- c('K - Aircraft Assist')
dropCharge <- c('10-3', '10-4', '11-8', '12-3', '13-2', '13-5', '15-1', '15-3',
                '16-2', '18-1', '22-3', '23-3', '23-4', '24-2', '24-4', '26-1',
                '26-2', '26-3', '27-1')
dropVehicle<- c('09 - Farm Vehicle',
                "11 - Cross Country Bus",
                "13 - Ambulance(Emerg)",
                "14 - Ambulance(Non-Emerg)",
                "15 - Fire(Emerg)",
                "17 - Police(Emerg)",
                "18 - Police(Non-Emerg)",
                "21 - Tandem Trailer",
                "22 - Mobile Home",
                "23 - Travel/Home Trailer",
                "24 - Camper",
                "26 - Boat Trailer",
                "27 - Farm Equipment")
  
regDF <- subset(regDF, !(regDF$Color %in% dropColor))
regDF <- subset(regDF, !(regDF$ShortCharge %in% dropCharge))
regDF <- subset(regDF, !(regDF$Asset.Type %in% dropAsset))
regDF <- subset(regDF, !(regDF$VehicleType %in% dropCharge))
regDF <- subset(regDF, !(regDF$VehicleType %in% dropVehicle))

regDF <- regDF[complete.cases(regDF), ]
regDF <- droplevels(regDF)

#generate train and test data sets
sampleInd <- sample(nrow(regDF), size = nrow(regDF)*.7)
trainDF <- regDF[sampleInd, ]
testDF  <- regDF[-sampleInd, ]

#convert the predictors into a matrix and dummy code all factors
x <- model.matrix(Violation.Type ~ ., trainDF)[,-1]
y <- ifelse(trainDF$Violation.Type == 'Citation', 1, 0)

#register cores for parallel processing
registerDoParallel(detectCores() - 1)

#execute a cross-validated, bionomial logistic lasso regression to identify best lambda value 
# cvfit <- cv.glmnet(x, y, alpha = 1, family = 'binomial', nfolds = 5, parallel = TRUE)
save(cvfit, file = "lassocvfit.rda")
load("lassocvfit.rda")

#plot the results
plot(cvfit)
#optimal lambda occurs at the 1se mark

#understand the coefficient set at the optimal value of lambda
coefDF <- coef(cvfit, s = cvfit$lambda.1se)
temp <- coefDF[order(coefDF[,1], decreasing = TRUE),]

temp <- round(temp, 4)[1:12]
exp(temp) / (1 + exp(temp))

temp2 <- coefDF[order(coefDF[,1], decreasing = FALSE),]

temp2 <- round(temp2, 4)[1:12]
exp(temp2) / (1 + exp(temp2))

sort(unique(regDF$ShortCharge))

#generate the matrix and predictor list for the test set
x2 <- model.matrix(Violation.Type ~ ., testDF)[,-1]
y2 <- ifelse(testDF$Violation.Type == 'Citation', 1, 0)

cvpreds <- predict(cvfit, newx = x2, s = cvfit$lambda.1se, type = 'class')
cvpreds <- as.factor(ifelse(cvpreds == 1, 'Citation', 'Warning'))
sink('ANLY512_FinalProject_Lasso1.txt', append = TRUE)
confusionMatrix(cvpreds, testDF$Violation.Type)
sink()

#rebuild the model by removing poor coefficients and those potential collinearity conflicts
regDF$Color       <- NULL
regDF$DLState     <- NULL
regDF$Conditions  <- NULL
regDF$MajorRoad   <- NULL

#generate train and test data sets
sampleInd <- sample(nrow(regDF), size = nrow(regDF)*.7)
trainDF <- regDF[sampleInd, ]
testDF  <- regDF[-sampleInd, ]

#convert the predictors into a matrix and dummy code all factors
x3 <- model.matrix(Violation.Type ~ ., trainDF)[,-1]
y3 <- ifelse(trainDF$Violation.Type == 'Citation', 1, 0)

head(x3)

#register cores for parallel processing
registerDoParallel(detectCores() - 1)

#execute a cross-validated, bionomial logistic lasso regression to identify best lambda value 
cvfit2 <- cv.glmnet(x3, y3, alpha = 1, family = 'binomial', nfolds = 5, parallel = TRUE)
save(cvfit2, file = "lassocvfit2.rda")
load("lassocvfit2.rda")

plot(cvfit2)
#optimal lambda appears to be about lambda = 1se

#understand the coefficient set at the optimal value of lambda
coef(cvfit2, s = cvfit2$lambda.min)

#generate the matrix and predictor list for the test set
x4 <- model.matrix(Violation.Type ~ ., testDF)[,-1]
y4 <- ifelse(testDF$Violation.Type == 'Citation', 1, 0)

cvpreds2 <- predict(cvfit2, newx = x4, s = cvfit2$lambda.min, type = 'class')
cvpreds2 <- as.factor(ifelse(cvpreds2 == 1, 'Citation', 'Warning'))
confusionMatrix(cvpreds, testDF$Violation.Type)

#work collinearity with personal injury, property damage...to check for collinearity with Accident
#run t-test between variables!
