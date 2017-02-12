# Loading train dataset
train <- read.csv("train.csv")

# Loading test dataset
test <- read.csv("test.csv")

train_xgb <- train
test_xgb <- test

# Removing trip ID's 
train_xgb$Trip_ID <- NULL

test_xgb$Trip_ID <- NULL

train_xgb$source <- "train"
test_xgb$source <- "test"

# Imputing missing values
train_xgb[is.na(train_xgb$Life_Style_Index),]$Life_Style_Index <- mean(train_xgb$Life_Style_Index, na.rm = T)
train_xgb[is.na(train_xgb$Var1),]$Var1 <- median(train_xgb$Var1, na.rm = T)
train_xgb[is.na(train_xgb$Customer_Since_Months),]$Customer_Since_Months <- median(train_xgb$Customer_Since_Months, na.rm = T)

test_xgb[is.na(test_xgb$Life_Style_Index),]$Life_Style_Index <- mean(test_xgb$Life_Style_Index, na.rm = T)
test_xgb[is.na(test_xgb$Var1),]$Var1 <- median(test_xgb$Var1, na.rm = T)
test_xgb[is.na(test_xgb$Customer_Since_Months),]$Customer_Since_Months <- median(test_xgb$Customer_Since_Months, na.rm = T)

total <- rbind(train_xgb[,c(1:12,14)],test_xgb)

# Binning trip distance

total$trip_cat <- ifelse(total$Trip_Distance<10,1,
                         ifelse(total$Trip_Distance>10 & total$Trip_Distance<20,2,
                                ifelse(total$Trip_Distance>20 & total$Trip_Distance<30,3,
                                       ifelse(total$Trip_Distance>30 & total$Trip_Distance<40,4,
                                              ifelse(total$Trip_Distance>40 & total$Trip_Distance<50,5,
                                                     ifelse(total$Trip_Distance>50 & total$Trip_Distance<60,6,
                                                            ifelse(total$Trip_Distance>60 & total$Trip_Distance<70,7,
                                                                   ifelse(total$Trip_Distance>70 & total$Trip_Distance<80,8,
                                                                          ifelse(total$Trip_Distance>80 & total$Trip_Distance<90,9,
                                                                                 ifelse(total$Trip_Distance>90 & total$Trip_Distance<100,10,
                                                                                        ifelse(total$Trip_Distance>100 & total$Trip_Distance<104,11,12)))))))))))
# Biining customer rating
total$rating <- ifelse(total$Customer_Rating <1,1,
                         ifelse(total$Customer_Rating>1 & total$Customer_Rating<2,2,
                                ifelse(total$Customer_Rating>2 & total$Customer_Rating<3,3,
                                       ifelse(total$Customer_Rating>3 & total$Customer_Rating<4,4,5))))

# Preprocessing
total$Type_of_Cab <- as.numeric(total$Type_of_Cab)
total$Gender <- as.numeric(total$Gender)
total$Confidence_Life_Style_Index <- as.numeric(total$Confidence_Life_Style_Index)
total$Destination_Type <- as.numeric(total$Destination_Type)

total[is.na(total)] <- c(-1)

train_final <- total[total$source == "train", ]
test_final <- total[total$source == "test", ]

train_final <- cbind(train_final,"Surge_Pricing_Type" = train_xgb$Surge_Pricing_Type)
train_final$Surge_Pricing_Type <- train_final$Surge_Pricing_Type - 1

train_final$Trip_Distance <- NULL
test_final$Trip_Distance <- NULL

train_final$Customer_Rating <- NULL
test_final$Customer_Rating <- NULL

library(xgboost)
dtrain = xgb.DMatrix(data = as.matrix(train_final[,c(1:11,13)]), label = train_final$Surge_Pricing_Type)
dtest = xgb.DMatrix(data = as.matrix(test_f[,c(2:10)]), label = test_f$Survived)

set.seed(23)
param <- list(objective = "multi:softmax",
              eval_metric = "merror",
              max_depth = 5,
              eta = 0.15,
              gamma = 0, 
              subsample = 0.8,
              colsample_bytree = 0.8, 
              min_child_weight = 1,
              num_class = 3)

model <- xgb.cv(params = param, dtrain, nfold = 5,
                nrounds = 100, verbose = 1,
                early.stop.round = 50)


modelf <- xgb.train(dtrain,params = param,nrounds = 100, verbose = 1)

pred <- predict(modelf, as.matrix(test_final[,c(1:11,13)])) + 1

sub <- data.frame("Trip_ID" = test$Trip_ID, "Surge_Pricing_Type" = pred)

write.csv(sub, "submission.csv",row.names = F)

