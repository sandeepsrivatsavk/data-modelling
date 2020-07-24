library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(finalfit)
library(plyr)
library(e1071)

######################### loading the Data
kc_data = read.csv("kc_house_data.csv",stringsAsFactors = FALSE)


######## to clean the bathrooms column
find <- c(1.25,1.75,2.25,2.75)
replace <- c(1,1.5,2,2.5)
found <- match(kc_data$bathrooms,find)
ifelse(is.na(found), kc_data$bathrooms , replace[found])

######################removing insignificant column
kc_data$id <- NULL

###### Correcting the Date Format
kc_data$date <- gsub('.{7}$' , '', kc_data$date)
kc_data$date <- ymd(kc_data$date)


##### to check for NA values
colSums(is.na(kc_data))


##### to check categorical and continuous variables 
ff_glimpse(kc_data)

#####################################################################################
library(hydroGOF)
library(caret)
library(xgboost)

########## Data Spliiting

shuffle_index <- sample(1:nrow(kc_data))
head(kc_data)

kc_data <- kc_data[shuffle_index,]
head(kc_data)

set.seed(123)
index = sample(1:nrow(kc_data))%>% 
  createDataPartition(p = 0.7, list = FALSE)            #####Data partition

train.data  <- kc_data[index, ]
test.data <- kc_data[-index, ]

dim(train.data)
dim(test.data)

###########################################################################
##### Initial Model #######################################################

model_train <- lm(log(price) ~ . , data = train.data)

pred.model <- predict(model_train, newdata = test.data)


########### original model
data.frame(
  RMSE = RMSE(pred.model, test.data$price),
  R2 = R2(pred.model, test.data$price)
)

#################################################################################
################### This Week -  Model No. 1######################################
###################################################################################

library(MASS)

step_model <- stepAIC(model_train, direction = "both", trace = FALSE)

summary(step_model)

new_pred<-predict(step_model,newdata = test.data)

summary(step_model)


data.frame(
  RMSE = RMSE(new_pred, test.data$price),      
  R2 = R2(new_pred, test.data$price)
)

##### getting the same RMSE and R2 values as the initial models

#################################################################################
############### this week  - Model nO. 2 ######################################3
############################################

library(car)

car::vif(step_model)

####  remove all the variables with vif >5 

model2 <- lm(log(price) ~ . -sqft_above , -sqft_living, data = train.data)
summary(model2)


pred_new <- model2 %>% predict(test.data)

data.frame(
  RMSE = RMSE(pred_new, test.data$price),
  R2 = R2(pred_new, test.data$price)
)
#### again getting the same RMSE and R2 values... ###############################


####################################################################################
################# Model No. 3 ############################################
############ Extreme Gradient Boosting   ###############


library(hydroGOF)
library(caret)
library(xgboost)

set.seed(123)
kc_model <- train(
  log(price) ~. -date, data = train.data, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

kc_model

# Best tuning parameter
kc_model$bestTune


# Make predictions on the test data
pred.1 <- kc_model %>% predict(test.data)
head(pred.1)

# Compute model prediction accuracy rate
mean(pred.1 == test.data$price)

data.frame(
  RMSE = RMSE(pred.1, test.data$price),
  R2 = R2(pred.1, test.data$price)
)

###### The RMSE and R2 values reduced considerably. 


