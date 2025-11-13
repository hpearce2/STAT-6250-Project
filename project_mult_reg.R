library(lubridate) 
library(tidyverse)
library(nnet)
library(stats)

data <- read.csv("Coffe_sales.csv")

# combine date and time
data$datetime_str <- paste(data$Date, data$Time)
data$datetime <- ymd_hms(data$datetime_str)
data$coffee_name <- factor(data$coffee_name)
data$Time_of_Day <- factor(data$Time_of_Day)
data$Weekday <- factor(data$Weekday)
data$Month_name <- factor(data$Month_name)

data <- data %>%
  dplyr::select(coffee_name, hour_of_day, money, Time_of_Day, Weekday, Month_name, Weekdaysort, Monthsort) %>%
  na.omit()

x = scale(model.matrix(coffee_name ~ Time_of_Day*Month_name, data=data)[,-1])
y = data$coffee_name
train <- sample(1:nrow(data), nrow(data) / 2)
test <- setdiff(1:nrow(data), train)

### full
multi_full <- multinom(coffee_name ~., data = data[train, ])
multi_pred <- predict(multi_model, data[test, ]) 
table(Predicted = multi_pred, Actual = data$coffee_name[test])
mean(multi_pred == data$coffee_name[test])

# chosen model
multi_model <- multinom(coffee_name ~ money + Time_of_Day*Month_name, data = data[train, ])
multi_pred <- predict(multi_model, data[test, ]) 
table(Predicted = multi_pred, Actual = data$coffee_name[test])
mean(multi_pred == data$coffee_name[test])



