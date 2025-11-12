
######### svm
library(e1071)
library(lubridate)
library(dplyr)
library(tidyverse)
#library(nnet)

data <- read.csv("Coffe_sales.csv")

# combine date and time
data$datetime_str <- paste(data$Date, data$Time)
data$datetime <- ymd_hms(data$datetime_str)
data$coffee_name <- factor(data$coffee_name)
data$Time_of_Day <- factor(data$Time_of_Day)
data$Weekday <- factor(data$Weekday)
data$Month_name <- factor(data$Month_name)

model_data <- data %>%
  dplyr::select(coffee_name, hour_of_day, money, Time_of_Day, Weekday, Month_name, Weekdaysort, Monthsort) %>%
  na.omit()


# train and test sets
train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))  # 70% train

train <- model_data[train_index, ]
test  <- model_data[-train_index, ]


#### Linear SVM
svm_lin <- svm(coffee_name ~ money + Time_of_Day*Month_name,
                 data = train,
                 kernel = "linear",   # linear, polynomial, sigmoid, radial
                 cost = 100,
                 gamma = 0)

# predict
lin_pred <- predict(svm_lin, test)

# accuracy
lin_accuracy <- mean(lin_pred == test$coffee_name)
print(paste("SVM Accuracy:", round(lin_accuracy * 100, 2), "%"))

table(Predicted = svm_pred, Actual = test$coffee_name)



### Polynomial SVM
svm_pol <- svm(coffee_name ~ money + Time_of_Day*Month_name,
               data = train,
               kernel = "polynomial",   # linear, polynomial, sigmoid, radial
               cost = 125,
               gamma = 0.1)

# predict
pol_pred <- predict(svm_pol, test)

# accuracy
pol_accuracy <- mean(pol_pred == test$coffee_name)
print(paste("SVM Accuracy:", round(pol_accuracy * 100, 2), "%"))
table(Predicted = pol_pred, Actual = test$coffee_name)



### Radial SVM
svm_rad <- svm(coffee_name ~ money + Time_of_Day*Month_name,
               data = train,
               kernel = "radial",   # linear, polynomial, sigmoid, radial
               cost = 625,
               gamma = 0.01)

# predict
rad_pred <- predict(svm_rad, test)

# accuracy
rad_accuracy <- mean(rad_pred == test$coffee_name)
print(paste("SVM Accuracy:", round(rad_accuracy * 100, 2), "%"))
table(Predicted = rad_pred, Actual = test$coffee_name)


### Sigmoid SVM
svm_sig <- svm(coffee_name ~ money + Time_of_Day*Month_name,
               data = train,
               kernel = "sigmoid",   # linear, polynomial, sigmoid, radial
               cost = 500,
               gamma = 0.01)

# predict
sig_pred <- predict(svm_sig, test)

# accuracy
sig_accuracy <- mean(sig_pred == test$coffee_name)
print(paste("SVM Accuracy:", round(sig_accuracy * 100, 2), "%"))
table(Predicted = sig_pred, Actual = test$coffee_name)


# all four accuracies
lin_accuracy
pol_accuracy # best but only slightly
rad_accuracy
sig_accuracy



# linear, gamma=0, cost=100, 1408 SVs 
# polynomial, gamma = 0.1, cost=125, 1431 SVs, degree = 3
# radial, gamma = 0.01, cost = 625, 1408 SVs
# sigmoid, gamma=0.01, cost =500, # SVs = 1449




# Hyperparameter tuning
tuned <- tune.svm(coffee_name ~ money + Time_of_Day*Month_name,
                  data = train,
                  kernel = "radial",
                  cost = c(500,625,750),
                  gamma = c(0, 0.01,0.05))

summary(tuned)
best_model <- tuned$best.model
# interpret support vectors





