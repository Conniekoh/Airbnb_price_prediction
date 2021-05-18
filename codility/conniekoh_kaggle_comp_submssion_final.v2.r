#Introductory Steps

#Install Packages
#Initiate Libraries
install.packages('tidyverse')
install.packages('leaps')
install.packages('caret')
install.packages('ggthemes')
install.packages('glmnet')
install.packages('dplyr')
install.packages('dataiku')
install.packages('ggplot')
library(tidyverse)
library(dplyr)
library(leaps)
library(caret)
library(ggthemes)
library(glmnet)
library(tidyr)
library(ggplot2)
# read dataset
analysisData = read.csv('C://Users/Connie/Documents/APAN5200/kaggle/analysisData.csv', stringsAsFactors = F)
str(analysisData)
summary(analysisData)

# Construct a simple model
model = lm(price~minimum_nights+number_of_reviews,analysisData)
summary(model)

# read in scoring data and apply model to generate predictions
scoringData = read.csv('C://Users/Connie/Documents/APAN5200/kaggle/scoringData.csv')
summary(scoringData)
str(scoringData)
colnames(scoringData)
str(analysisData)

pred = predict(model, newdata=scoringData)

# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'C://Users/Connie/Documents/APAN5200/kaggle/sample_submission.csv', row.names=F)

# Add the price column to scoringData because it is not included. Note = we are setting the value to 0
# Create a column name called price. Add the column above to the scoringData with a zero value .Reorder the scoringData dataset to be id, price, then all everything else that isn't id and price
namevector <- c("price")
scoringData[,namevector] <- 0
scoringData <- scoringData %>%
  select(id,price, everything())
summary(scoringData)

#Next we set the zipcode to a character field (it originally tried to import them as numbers) 
#and moved all the values in the analysisData to logicals where R guessed wrong. 
#Moving these back to logicals ensures we can combine the two datasets. 
#If we did not create consistency between the column types in both datasets they would not stack.

colnames(analysisData)
str(analysisData)

analysisData$zipcode <- as.character(analysisData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)

analysisData$host_is_superhost <- as.logical(analysisData$host_is_superhost)
analysisData$host_has_profile_pic <- as.logical(analysisData$host_has_profile_pic)
analysisData$host_identity_verified <- as.logical(analysisData$host_identity_verified)
analysisData$instant_bookable <- as.logical(analysisData$instant_bookable)
analysisData$require_guest_profile_picture <- as.logical(analysisData$require_guest_profile_picture)
analysisData$require_guest_phone_verification <- as.logical(analysisData$require_guest_phone_verification)
analysisData$is_location_exact <- as.logical(analysisData$is_location_exact)

## If everything is OK we should only see a "Different number of rows" difference between the two datasets.
all_equal(analysisData, scoringData, convert = TRUE)

#Step 1
#To build the models for our Kaggle submission, we should use the same pattern we have used in class. 
#Take the analysisData dataset and divide it into a Train and Test using a stratified sampling technique
#based on our target, the price column. Splitting the dataset like this ensures that we have a representative sample and better understand if our model is overtraining

set.seed(5656)
ksplit <- createDataPartition(y = analysisData$price, p=.7, list=F, groups=50)
train <- analysisData[ksplit,]
test <- analysisData[-ksplit,]
nrow(train)
nrow(test)
nrow(analysisData)

#Step2 
#To ensure that any transformations we make occur on the train, test, and scoringData datasets, we should place them together. 
#However, we must identify which row belongs to which dataset, so we create an identifier field "train_test_score" which will direct data later on in the process. 
#After making the new field, we merge the three datasets with the dplyr function bind_rows.

train$train_test_score <- "train"
test$train_test_score <- "test"
scoringData$train_test_score <- "score"
baseData <- bind_rows(train, test, scoringData)

glimpse(baseData)

#Step3 Start of the Data Wrangling Processes 
# use glimpse function from dplyr package to get a glimpse of the data
glimpse(baseData)

Summary(is.na(baseData)) 
baseData$bed_type <- factor(baseData$bed_type)
baseData$property_type <- factor(baseData$property_type)
baseData$instant_bookable <- factor(baseData$instant_bookable)

# clean data  
baseData$bedrooms <- as.numeric(baseData$bedrooms)
baseData$beds <- as.numeric(baseData$beds)
baseData$access <- as.character(baseData$access)
baseData$host_location <- as.factor(baseData$host_location)
baseData$host_neighbourhood <- as.factor(baseData$host_neighbourhood)
baseData$host_verifications <- as.factor(baseData$host_verifications)
baseData$street <- as.factor(baseData$street)
baseData$neighbourhood <- as.factor(baseData$neighbourhood)
baseData$neighbourhood_cleansed <- as.factor(baseData$neighbourhood_cleansed)
baseData$neighbourhood_group_cleansed <- as.factor(baseData$neighbourhood_group_cleansed)
baseData$city <- as.factor(baseData$city)
baseData$room_type <- as.factor(baseData$room_type)
baseData$smart_location <- as.factor(baseData$smart_location)
baseData$cancellation_policy <- as.factor(baseData$cancellation_policy)

baseData$latitude <- as.numeric(baseData$latitude)
baseData$longitude <- as.numeric(baseData$longitude)
baseData$calendar_updated <-as.factor(baseData$calendar_updated)
baseData$amenities <- as.factor(baseData$amenities)
baseData$calendar_updated  <- as.factor(baseData$calendar_updated)

## sum.na() to identify how many na values within baseData
## there were total 224969 na values within baseData
## Out of 97 variables, there are 18 variables which contain NA values
summary(is.na(baseData))

#### okay then, replace na value to mean/median.... and then
#### makes sure factor values present in train dataset, 

#replace NA's in weekly_price variables with median of the remaining values
baseData$bathrooms[is.na(baseData$bathrooms)]<-median(baseData$bathrooms, na.rm=TRUE)
baseData$bedrooms[is.na(baseData$bedrooms)]<-median(baseData$bedrooms, na.rm=TRUE)
baseData$beds[is.na(baseData$beds)]<-median(baseData$beds, na.rm=TRUE)
baseData$weekly_price[is.na(baseData$weekly_price)]<-median(baseData$weekly_price,na.rm=TRUE)
baseData$monthly_price[is.na(baseData$monthly_price)]<-median(baseData$monthly_price,na.rm=TRUE)
baseData$security_deposit[is.na(baseData$security_deposit)]<-median(baseData$security_deposit, na.rm=TRUE)
baseData$cleaning_fee[is.na(baseData$cleaning_fee)]<-median(baseData$cleaning_fee, na.rm=TRUE)
baseData$review_scores_rating[is.na(baseData$review_scores_rating)]<-median(baseData$review_scores_rating, na.rm=TRUE)
baseData$review_scores_accuracy[is.na(baseData$review_scores_accuracy)]<-median(baseData$review_scores_accuracy, na.rm=TRUE)
baseData$review_scores_cleanliness[is.na(baseData$review_scores_cleanliness)]<-median(baseData$review_scores_cleanliness, na.rm=TRUE)
baseData$review_scores_checkin[is.na(baseData$review_scores_checkin)]<-median(baseData$review_scores_checkin, na.rm=TRUE)
baseData$review_scores_communication[is.na(baseData$review_scores_communication)]<-median(baseData$review_scores_communication, na.rm=TRUE)
baseData$review_scores_location[is.na(baseData$review_scores_location)]<-median(baseData$review_scores_location, na.rm=TRUE)
baseData$review_scores_value[is.na(baseData$review_scores_value)]<-median(baseData$review_scores_value, na.rm=TRUE)
baseData$reviews_per_month[is.na(baseData$reviews_per_month)]<-median(baseData$reviews_per_month, na.rm=TRUE)
baseData$square_feet[is.na(baseData$square_feet)]<-median(baseData$square_feet, na.rm=TRUE)
baseData$host_listings_count[is.na(baseData$host_listings_count)]<-median(baseData$host_listings_count, na.rm=TRUE)
baseData$host_total_listings_count[is.na(baseData$host_total_listings_count)]<-median(baseData$host_total_listings_count, na.rm=TRUE)

# Step 4 - The Curious Case of NA or How Do We Deal with Missing Data
#Step 5 - Resplit the data across Train - Test - Score
### AFTER cleaning and replace NA value to mean/median? then 
### MUST RESPLIT the data (train, test, score) to update RMSE score

train <- baseData  %>% 
  filter(train_test_score == "train")
test <- baseData  %>% 
  filter(train_test_score == "test")
score <- baseData  %>% 
  filter(train_test_score == "score")

### Test to ensure our datasets match in terms of the number of rows
nrow(analysisData); nrow(train); nrow(test); nrow(score)

#Step6:
library(ranger)
library(randomForest)
library(xgboost)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(gbm)
library(vtreat)
library(ROCR)
library(caTools)

#CORRELATION WITH NUMERIC 
baseData_numeric1 = baseData[,!sapply(baseData, is.character)]
baseData_numeric2 = baseData_numeric1[,!sapply(baseData_numeric1, is.logical)] # 60 factor/integer/numeric only
baseData_numeric3 = baseData_numeric2[,!sapply(baseData_numeric2, is.factor)] # 44 integer/numeric only 

glimpse(baseData_numeric3)
set.seed(1031)
split2 = createDataPartition(y=baseData_numeric3$price,p = 0.7,list = F,groups = 100)
train2 = baseData_numeric3[split2,]
test2 = baseData_numeric3[-split2,]

nrow(train2)
nrow(test2)
nrow(baseData_numeric3)
class(test2)

library(ggthemes)
library(corrplot)
cor(train2[,-44])
round(cor(train2[,-44]),4)*100


#FORWARD SELECTION # Step:  AIC=344704.5
start_mod = lm(price~1,data=train2)
empty_mod = lm(price~1,data=train2)
full_mod = lm(price~.,data=train2)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
summary(forwardStepwise)

#BACKWARD SELECTION 344704.5
start_mod = lm(price~.,data=train2)
empty_mod = lm(price~1,data=train2)
full_mod = lm(price~.,data=train2)
backwardStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='backward')
summary(backwardStepwise)

#HYBRID SELECTION 344708.5
start_mod = lm(price~1,data=train2)
empty_mod = lm(price~1,data=train2)
full_mod = lm(price~.,data=train2)
hybridStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='both')
summary(hybridStepwise)

summary(hybridStepwise)
summary(forwardStepwise)
summary(backwardStepwise)

hybridStepwisecoeff <- as.data.frame(summary(hybridStepwise)$coefficients)
backwardStepwisecoeff <- as.data.frame(summary(backwardStepwise)$coefficients)
forwardStepwisecoeff <- as.data.frame(summary(forwardStepwise)$coefficients)
print ('Forward Stepwise')
forwardStepwisecoeff

#LASSO
#Investigated the 27 variables out of 44 variables seems correlated 

library(broom)
x = model.matrix(price~.-1,data=train2)
y = train2$price
lassoModel = glmnet(x,y,alpha=1)
set.seed(617)
cv.lasso = cv.glmnet(x,y,alpha=0) #10fold cross validation is default
plot(cv.lasso)
coef(cv.lasso)

#FINAL MODEL 1 - RANGER
#PUBLIC RMSE SCORE 291.65571 (RMSE score 248.5316 (After the competition was closed))
forest_ranger3 = ranger(price~room_type+neighbourhood_group_cleansed+host_neighbourhood+
                          property_type+
                          cancellation_policy+
                          latitude+
                          host_listings_count+
                          host_total_listings_count+
                          accommodates+
                          bathrooms+
                          bedrooms+
                          beds+
                          weekly_price+
                          monthly_price+
                          security_deposit+
                          cleaning_fee+ 
                          extra_people+
                          guests_included+
                          maximum_nights+
                          minimum_nights+
                          review_scores_location+
                          maximum_minimum_nights+
                          minimum_minimum_nights+
                          minimum_maximum_nights+
                          minimum_nights_avg_ntm+
                          maximum_nights_avg_ntm+
                          availability_365+
                          availability_30+
                          availability_60+
                          availability_90+
                          calculated_host_listings_count+
                          calculated_host_listings_count_entire_homes, data=train, num.trees = 1000)
pred3 = predict(forest_ranger3, data = test,num.trees = 1000)
rmse_forest_ranger3 = sqrt(mean((pred3$predictions-test$price)^2)); rmse_forest_ranger3 
predRangerTest3 <- predict(forest_ranger3, data=score)
submissionFile = data.frame(id = score$id, price = predRangerTest3$predictions)
write.csv(submissionFile, 'C://Users/Connie/Documents/APAN5200/kaggle/03submission.csv', row.names=F)


#FINAL MODEL2 - TUNE WITH RANGER
#PUBLIC RMSE SCORE 288.63676 (RMSE score 238.8789() (After the competition was closed)
trControl=trainControl(method="cv",number=5)
tuneGrid = expand.grid(mtry=1:4, 
                       splitrule = c('variance','extratrees','maxstat'), 
                       min.node.size = c(2,5,10,15,20,25))
set.seed(617)
cvModel = train(price~room_type+neighbourhood_group_cleansed+host_neighbourhood+
                  property_type+
                  cancellation_policy+
                  latitude+
                  host_listings_count+
                  host_total_listings_count+
                  accommodates+
                  bathrooms+
                  bedrooms+
                  beds+
                  weekly_price+
                  monthly_price+
                  security_deposit+
                  cleaning_fee+ 
                  extra_people+
                  guests_included+
                  maximum_nights+
                  minimum_nights+
                  review_scores_location+
                  maximum_minimum_nights+
                  minimum_minimum_nights+
                  minimum_maximum_nights+
                  minimum_nights_avg_ntm+
                  maximum_nights_avg_ntm+
                  availability_365+
                  availability_30+
                  availability_60+
                  availability_90+
                  calculated_host_listings_count+
                  calculated_host_listings_count_entire_homes,
                data=train,
                method="ranger",
                num.trees=1000,
                trControl=trControl,
                tuneGrid=tuneGrid)
cv_forest_ranger = ranger(price~room_type+neighbourhood_group_cleansed+host_neighbourhood+
                            property_type+
                            cancellation_policy+
                            latitude+
                            host_listings_count+
                            host_total_listings_count+
                            accommodates+
                            bathrooms+
                            bedrooms+
                            beds+
                            weekly_price+
                            monthly_price+
                            security_deposit+
                            cleaning_fee+ 
                            extra_people+
                            guests_included+
                            maximum_nights+
                            minimum_nights+
                            review_scores_location+
                            maximum_minimum_nights+
                            minimum_minimum_nights+
                            minimum_maximum_nights+
                            minimum_nights_avg_ntm+
                            maximum_nights_avg_ntm+
                            availability_365+
                            availability_30+
                            availability_60+
                            availability_90+
                            calculated_host_listings_count+
                            calculated_host_listings_count_entire_homes,
                          data=train,
                          num.trees = 1000, 
                          mtry=cvModel$bestTune$mtry, 
                          min.node.size = cvModel$bestTune$min.node.size, 
                          splitrule = cvModel$bestTune$splitrule)
pred = predict(cv_forest_ranger, data =test, num.trees = 1000)
rmse_cv_forest_ranger = sqrt(mean((pred$predictions-test$price)^2)); rmse_cv_forest_ranger
pred_Rangertune <- predict(cv_forest_ranger, data=score)
submissionFile = data.frame(id = score$id, price = pred_Rangertune$predictions)
write.csv(submissionFile, 'C://Users/Connie/Documents/APAN5200/kaggle/10submission.csv', row.names=F)
