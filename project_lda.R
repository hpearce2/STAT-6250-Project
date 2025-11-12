library(lubridate)
library(dplyr)
library(tidyverse)
library(nnet)

data <- read.csv("Coffe_sales.csv")

# to find number of each coffee in dataset
latte <- data %>%
  count(coffee_name == "Cappuccino") 


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


# lda
library(MASS)

lda_model <- lda(coffee_name ~ money + Time_of_Day*Month_name,
                 data = train)

# 63.57% for hour_of_day + money + Time_of_Day + Weekday + Month_name
# 48.08% for just money
# 48.08% for money + hour_of_day
# 49.2% for money + weekday
# 51.64% for money + time of day
# 51.92% for money + hour_of_day*Time_of_Day
# 62.72% for money + month name
# 64.04% for money + month_name + time_of_day
# 65.73% for money + Month_name*Time_of_Day + Weekday
# 65.73% for money + Month_name*Time_of_Day + Weekday + Time_of_Day
# 66.01% for money + Month_name:Time_of_Day, : or *

# don't use weekday sort and month sort bc causes collinearity and lda assumes variables not perfectly correalted

# predict on test data
lda_pred <- predict(lda_model, test)
# predict 1065 which is 30% test set

# # lda classes
lda_classes <- lda_pred$class
summary(lda_classes) # predicts 1065, 30% test set

# lda accuracy
lda_accuracy <- mean(lda_classes == test$coffee_name)
print(paste("LDA Accuracy:", round(lda_accuracy * 100, 2), "%"))
# 66.2%

# confusion matrix
table(Predicted = lda_classes, Actual = test$coffee_name)





