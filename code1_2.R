
setwd("C:\\reetika\\useful study material\\ms sem 2\\dmpa project")
df<-read.csv("Sample_Ramya.csv")
nf<- read.csv("new_cleaned_with_amenities.csv")
nf1 <- read.csv("Train Y.csv")
df1 <- read.csv("Sample Train Y Ramya.csv")
test <- read.csv("Cleaned Test - v6.csv")
names(df)
names(test)

##df$high_booking_rate <- df1$high_booking_rate
rfdata <- df
rfdata$high_booking_rate <- df1$high_booking_rate
nf$high_booking_rate <- nf1$high_booking_rate
nf <- subset(nf, select = -c(X))
xf <- nf[,1:26]

source('helper_functions.R')  
library(randomForest)  
library(e1071)  
library(caret)  
library(ggplot2)  

df$high_booking_rate <- as.factor(df$high_booking_rate)
df$host_identity_verified <- as.factor(df$host_identity_verified)
df$host_is_superhost <- as.factor(df$host_is_superhost)
df$is_location_exact <- as.factor(df$is_location_exact)
df$instant_bookable <- as.factor(df$instant_bookable)
df$require_guest_phone_verification <- as.factor(df$require_guest_phone_verification)
df$require_guest_profile_picture <- as.factor(df$require_guest_profile_picture)
df$requires_license <- as.factor(df$requires_license)
df$bed_type <- as.factor(df$bed_type)
df$city_name <- as.factor(df$city_name)
df$cancellation_policy <- as.factor(df$cancellation_policy)
df$host_response_time <- as.factor(df$host_response_time)
df$room_type <- as.factor(df$room_type)
df$property_type <- as.factor(df$property_type)
df$Pool<- as.factor(df$Pool)
df$Kitchen <- as.factor(df$Kitchen)
df$Gym <- as.factor(df$Gym)
df$Heating <- as.factor(df$Heating)
df$Washer<- as.factor(df$Washer)
df$Dryer <- as.factor(df$Dryer)
df$Essentials <- as.factor(df$Essentials)
df$Shampoo <- as.factor(df$Shampoo)
df$TV<- as.factor(df$TV)
df$Internet<- as.factor(df$Internet)
df$Hangers<- as.factor(df$Hangers)
df$Iron<- as.factor(df$Iron)
df$Keypad<- as.factor(df$Keypad)
df$email<- as.factor(df$email)
df$phone<- as.factor(df$phone)
df$google<- as.factor(df$google)
df$reviews<- as.factor(df$reviews)
df$facebook<- as.factor(df$facebook)
df$jumio<- as.factor(df$jumio)
df$kba<- as.factor(df$kba)
df$government_id<- as.factor(df$government_id)
df$linkedin<- as.factor(df$linkedin)
df$selfie<- as.factor(df$selfie)
df$identity_manual<- as.factor(df$identity_manual)
df$work_email<- as.factor(df$work_email)

df$bathrooms <- as.numeric(df$bathrooms)
df$bedrooms <- as.numeric(df$bedrooms)

#rfdata transformations
rfdata$high_booking_rate <- as.factor(rfdata$high_booking_rate)
rfdata$host_identity_verified <- as.factor(rfdata$host_identity_verified)
rfdata$host_is_superhost <- as.factor(rfdata$host_is_superhost)
rfdata$is_location_exact <- as.factor(rfdata$is_location_exact)
rfdata$instant_bookable <- as.factor(rfdata$instant_bookable)
rfdata$require_guest_phone_verification <- as.factor(rfdata$require_guest_phone_verification)
rfdata$require_guest_profile_picture <- as.factor(rfdata$require_guest_profile_picture)
rfdata$requires_license <- as.factor(rfdata$requires_license)
rfdata$bed_type <- as.factor(rfdata$bed_type)
rfdata$city_name <- as.factor(rfdata$city_name)
rfdata$cancellation_policy <- as.factor(rfdata$cancellation_policy)
rfdata$host_response_time <- as.factor(rfdata$host_response_time)
rfdata$room_type <- as.factor(rfdata$room_type)
rfdata$property_type <- as.factor(rfdata$property_type)
rfdata$Pool<- as.factor(rfdata$Pool)
rfdata$Kitchen <- as.factor(rfdata$Kitchen)
rfdata$Gym <- as.factor(rfdata$Gym)
rfdata$Heating <- as.factor(rfdata$Heating)
rfdata$Washer<- as.factor(rfdata$Washer)
rfdata$Dryer <- as.factor(rfdata$Dryer)
rfdata$Essentials <- as.factor(rfdata$Essentials)
rfdata$Shampoo <- as.factor(rfdata$Shampoo)
rfdata$TV<- as.factor(rfdata$TV)
rfdata$Internet<- as.factor(rfdata$Internet)
rfdata$Hangers<- as.factor(rfdata$Hangers)
rfdata$Iron<- as.factor(rfdata$Iron)
rfdata$Keypad<- as.factor(rfdata$Keypad)
rfdata$email<- as.factor(rfdata$email)
rfdata$phone<- as.factor(rfdata$phone)
rfdata$google<- as.factor(rfdata$google)
rfdata$reviews<- as.factor(rfdata$reviews)
rfdata$facebook<- as.factor(rfdata$facebook)
rfdata$jumio<- as.factor(rfdata$jumio)
rfdata$kba<- as.factor(rfdata$kba)
rfdata$government_id<- as.factor(rfdata$government_id)
rfdata$linkedin<- as.factor(rfdata$linkedin)
rfdata$selfie<- as.factor(rfdata$selfie)
rfdata$identity_manual<- as.factor(rfdata$identity_manual)
rfdata$work_email<- as.factor(rfdata$work_email)

rfdata$bathrooms <- as.numeric(rfdata$bathrooms)
rfdata$bedrooms <- as.numeric(rfdata$bedrooms)

#nf transformations
nf$high_booking_rate <- as.factor(nf$high_booking_rate)
nf$host_identity_verified <- as.factor(nf$host_identity_verified)
nf$host_is_superhost <- as.factor(nf$host_is_superhost)
nf$is_location_exact <- as.factor(nf$is_location_exact)
nf$instant_bookable <- as.factor(nf$instant_bookable)
nf$require_guest_phone_verification <- as.factor(nf$require_guest_phone_verification)
nf$require_guest_profile_picture <- as.factor(nf$require_guest_profile_picture)
nf$requires_license <- as.factor(nf$requires_license)
nf$bed_type <- as.factor(nf$bed_type)
nf$city_name <- as.factor(nf$city_name)
nf$cancellation_policy <- as.factor(nf$cancellation_policy)
nf$host_response_time <- as.factor(nf$host_response_time)
nf$room_type <- as.factor(nf$room_type)
nf$property_type <- as.factor(nf$property_type)
nf$Pool<- as.factor(nf$Pool)
nf$Kitchen <- as.factor(nf$Kitchen)
nf$Gym <- as.factor(nf$Gym)
nf$Heating <- as.factor(nf$Heating)
nf$Washer<- as.factor(nf$Washer)
nf$Dryer <- as.factor(nf$Dryer)
nf$Essentials <- as.factor(nf$Essentials)
nf$Shampoo <- as.factor(nf$Shampoo)
nf$TV<- as.factor(nf$TV)
nf$Internet<- as.factor(nf$Internet)
nf$Hangers<- as.factor(nf$Hangers)
nf$Iron<- as.factor(nf$Iron)
nf$Keypad<- as.factor(nf$Keypad)
nf$email<- as.factor(nf$email)
nf$phone<- as.factor(nf$phone)
nf$google<- as.factor(nf$google)
nf$reviews<- as.factor(nf$reviews)
nf$facebook<- as.factor(nf$facebook)
nf$jumio<- as.factor(nf$jumio)
nf$kba<- as.factor(nf$kba)
nf$government_id<- as.factor(nf$government_id)
nf$linkedin<- as.factor(nf$linkedin)
nf$selfie<- as.factor(nf$selfie)
nf$identity_manual<- as.factor(nf$identity_manual)
nf$work_email<- as.factor(nf$work_email)
nf$Wireless_Internet<- as.factor(nf$Wireless_Internet)
nf$Air_conditioning<- as.factor(nf$Air_conditioning)
nf$availability_365<- as.factor(nf$availability_365)
nf$Free_parking_on_premises<- as.factor(nf$Free_parking_on_premises)
nf$Hot_tub<- as.factor(nf$Hot_tub)
nf$Indoor_fireplace<- as.factor(nf$Indoor_fireplace)
nf$Suitable_for_events<- as.factor(nf$Suitable_for_events)
nf$Dishwasher<- as.factor(nf$Dishwasher)
nf$Lock_on_bedroom_door<- as.factor(nf$Lock_on_bedroom_door)
nf$work_email<- as.factor(nf$work_email)
nf$Cable_TV<- as.factor(nf$Cable_TV)
nf$Elevator<- as.factor(nf$Elevator)
nf$Laptop_friendly_workspace<- as.factor(nf$Laptop_friendly_workspace)
nf$Hair_dryer<- as.factor(nf$Hair_dryer)
nf$Smoke_detector<- as.factor(nf$Smoke_detector)
nf$Carbon_monoxide_detector<- as.factor(nf$Carbon_monoxide_detector)
nf$First_aid_kit<- as.factor(nf$First_aid_kit)
nf$Fire_extinguisher<- as.factor(nf$Fire_extinguisher)
nf$Self_Check_In<- as.factor(nf$Self_Check_In)
nf$Wheelchair_accessible<- as.factor(nf$Wheelchair_accessible)
nf$Pets_allowed<- as.factor(nf$Pets_allowed)
nf$Pets_live_on_this_property<- as.factor(nf$Pets_live_on_this_property)
nf$Refrigerator<- as.factor(nf$Refrigerator)
nf$Microwave<- as.factor(nf$Microwave)
nf$Stove<- as.factor(nf$Stove)
nf$Oven<- as.factor(nf$Oven)
nf$Breakfast<- as.factor(nf$Breakfast)



nf$bathrooms <- as.numeric(nf$bathrooms)
nf$bedrooms <- as.numeric(nf$bedrooms)
nf$cleaning_fee<- as.numeric(nf$cleaning_fee)


#test transformations
test$host_identity_verified <- as.factor(test$host_identity_verified)
test$host_is_superhost <- as.factor(test$host_is_superhost)
test$is_location_exact <- as.factor(test$is_location_exact)
test$instant_bookable <- as.factor(test$instant_bookable)
test$require_guest_phone_verification <- as.factor(test$require_guest_phone_verification)
test$require_guest_profile_picture <- as.factor(test$require_guest_profile_picture)
test$requires_license <- as.factor(test$requires_license)
test$bed_type <- as.factor(test$bed_type)
test$city_name <- as.factor(test$city_name)
test$cancellation_policy <- as.factor(test$cancellation_policy)
test$host_response_time <- as.factor(test$host_response_time)
test$room_type <- as.factor(test$room_type)
test$property_type <- as.factor(test$property_type)
test$Pool<- as.factor(test$Pool)
test$Kitchen <- as.factor(test$Kitchen)
test$Gym <- as.factor(test$Gym)
test$Heating <- as.factor(test$Heating)
test$Washer<- as.factor(test$Washer)
test$Dryer <- as.factor(test$Dryer)
test$Essentials <- as.factor(test$Essentials)
test$Shampoo <- as.factor(test$Shampoo)
test$TV<- as.factor(test$TV)
test$Internet<- as.factor(test$Internet)
test$Hangers<- as.factor(test$Hangers)
test$Iron<- as.factor(test$Iron)
test$Keypad<- as.factor(test$Keypad)
test$email<- as.factor(test$email)
test$phone<- as.factor(test$phone)
test$google<- as.factor(test$google)
test$reviews<- as.factor(test$reviews)
test$facebook<- as.factor(test$facebook)
test$jumio<- as.factor(test$jumio)
test$kba<- as.factor(test$kba)
test$government_id<- as.factor(test$government_id)
test$linkedin<- as.factor(test$linkedin)
test$selfie<- as.factor(test$selfie)
test$identity_manual<- as.factor(test$identity_manual)
test$work_email<- as.factor(test$work_email)

test$accommodates <- as.integer(test$accommodates)
test$maximum_nights <- as.integer(as.character(test$maximum_nights))
test$extra_people <- as.integer(test$extra_people)
test$guests_included <- as.integer(test$guests_included)
test$host_listings_count <- as.integer(test$host_listings_count)
test$host_total_listings_count <- as.integer(test$host_total_listings_count)
test$minimum_nights <- as.integer(test$minimum_nights)
test$price <- as.integer(test$price)

test$bathrooms <- as.numeric(test$bathrooms)
test$bedrooms <- as.numeric(test$bedrooms)


sapply(test, class)
sapply(df, class)

#this function is to make levels same in both test and train
common <- intersect(names(df), names(test)) 
for (p in common) { 
  if (class(df[[p]]) == "factor") { 
    levels(test[[p]]) <- levels(df[[p]]) 
  } 
}

#partitioning of data 
set.seed(123)
valid_instn = sample(nrow(df), 0.1*nrow(df))#partition 10%as valid
df_valid <- df[valid_instn,] #make a new dataframe with valid data in it 
df_train <- df[-valid_instn,] #this is the remaining data left that is 90%

valid_instn1 = sample(nrow(df1), 0.1*nrow(df1))#partition 10%as valid
df1_valid <- df1[valid_instn1,] #make a new dataframe with valid data in it 
df1_train <- df1[-valid_instn1,] #this is the remaining data left that is 90%

valid_instn2 = sample(nrow(rfdata), 0.1*nrow(rfdata))#partition 10%as valid
rfdata_valid <- rfdata[valid_instn2,] #make a new dataframe with valid data in it 
rfdata_train <- rfdata[-valid_instn2,] #this is the remaining data left that is 90%

svmdata <- rfdata
valid_instn3 = sample(nrow(svmdata), 0.8*nrow(svmdata))#partition 60%as valid
svmdata_valid <- svmdata[valid_instn3,] #make a new dataframe with valid data in it 
svmdata_train <- svmdata[-valid_instn3,] #this is the remaining data left that is 40%

valid_instn4 = sample(nrow(nf), 0.2*nrow(nf))#partition 20%as valid
nf_valid <- nf[valid_instn4,] #make a new dataframe with valid data in it 
nf_train <- nf[-valid_instn4,] #this is the remaining data left that is 80%

valid_instn6 = sample(nrow(nf1), 0.2*nrow(nf1))#partition 20%as valid
nf1_valid <- nf1[valid_instn6,] #make a new dataframe with valid data in it 
nf1_train <- nf1[-valid_instn6,] #this is the remaining data left that is 80%

valid_instn5 = sample(nrow(xf), 0.2*nrow(xf))#partition 20%as valid
xf_valid <- xf[valid_instn5,] #make a new dataframe with valid data in it 
xf_train <- xf[-valid_instn5,] #this is the remaining data left that is 80%



#################
#PCA
library(scatterplot3D)
library(FactoMineR)

matrix <- as.numeric(as.matrix(nf_train))

pca1= PCA(matrix, graph = TRUE)
pca1$eig
pca1$var$coord

cor(nf_train)

####random forest 

rf = randomForest(high_booking_rate ~. ,data=rfdata_train, na.action = na.exclude,mtry=6, ntree=1000)#random forest
plot(rf)
print(rf)

varImpPlot(rf, sort = T, n.var=10, main="Top 10 - Variable Importance") #top 10 important variables
var.imp = data.frame(importance(rf, type=2))
var.imp$Variables = row.names(var.imp)  # make row names as columns
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

preddf_train <- rfdata_train
preddf_valid<- rfdata_valid
#pred_train <- nf_train
#pred_valid <- nf_valid

preddf_train$predictedresponsetrain = predict(rf , rfdata_train)
preddf_valid$predictedresponsevalid = predict(rf, rfdata_valid)
test$high_booking_rate = predict(rf, test)
confusionMatrix(data = preddf_valid$predictedresponsevalid, reference = rfdata_valid$high_booking_rate, positive = NULL)

#for nf
pred_train$predictedresponse = predict(rf , nf_train)
pred_valid$predictedresponse = predict(rf, nf_valid)
test$high_booking_rate = predict(rf, test)
confusionMatrix(data = pred_valid$predictedresponse, reference = nf_valid$high_booking_rate, positive = NULL)


x = which(preddf_valid$predictedresponsevalid != rfdata_valid$high_booking_rate)
which(preddf_train$predictedresponsetrain != rfdata_train$high_booking_rate)
rfdata_train <- rfdata_train[-c(x), ]

bestmtry <- tuneRF(high_booking_rate~., data=nf_train, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

df_final<- data.frame(test$high_booking_rate)
names(df_final) <- c("high_booking_rate")

write.csv(df_final, file = "test_y.csv")

##################


#logistic model
log_model <- glm(high_booking_rate~., data= df_train, family=binomial)
summary(log_model)

log_train_preds <- predict(log_model,newdata=df_train, type = "response")#prediction on training
log_valid_preds <- predict(log_model,newdata=df_valid, type = "response")#prediction on validation
log_class <- ifelse(log_valid_preds>.5,1,0)
accuracy = sum(ifelse(df_valid$high_booking_rate==log_class,1,0))/nrow(df_valid)

###################
#svm
svmmodel <- svm(high_booking_rate ~ ., data = svmdata_train)
preddf_train <- svmdata_train
preddf_valid<- svmdata_valid

preddf_train$svmresponse = predict(svmmodel, svmdata_train)
preddf_valid$svmresponse = predict(svmmodel, svmdata_valid)
#test$high_booking_rate = predict(rf, test)
confusionMatrix(data = preddf_valid$svmresponse, reference = svmdata_valid$high_booking_rate, positive = NULL)

x <- svmdata_train$high_booking_rate
y <- svmdata_train[,-c(56)]

svm_tune <- tune(svmmodel, train.x=x, train.y=y, 
                 kernel="radial", cost=1:100, gamma=c(.5,1,2))

print(svm_tune)

###############
#xgboost model
require(xgboost)


#we have df_train which is our data of the matrix data 
df1_train_hbr <- df1_train$high_booking_rate #this is our label
df_train_matrix = data.matrix(df_train)
df1_train_hbr_matrix= data.matrix(df1_train_hbr)
#now for test i.e. valid
df1_valid_hbr <- df1_valid$high_booking_rate#this is label for test
df_valid_matrix <- data.matrix(df_valid)
df1_valid_hbr_matrix <- data.matrix(df1_valid_hbr)

dtrain = xgb.DMatrix(data= df_train_matrix, label= df1_train_hbr_matrix)#data of the form data and labels for train
dtest = xgb.DMatrix(data=df_valid_matrix, label=df1_valid_hbr_matrix)

xgb <- xgboost(data = dtrain, max.depth = 20, eta=1, booster="gbtree",
               nthread = 2, #number of cpu threads
               nround = 100,#max number of boosting iterations
               gamma = 1,
               alpha = 10,
               objective = "binary:logistic")
pred <- predict(xgb, dtest)
err <- mean(as.numeric(pred > 0.5) != df1_valid_hbr_matrix)
print(paste("test-error=", err))

xgb_class <- ifelse(pred>.90,1,0)
accuracy = sum(ifelse(df1_valid$high_booking_rate==xgb_class,1,0))/nrow(df1_valid)
