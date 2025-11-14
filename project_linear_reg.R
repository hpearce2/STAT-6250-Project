library(lubridate) 
library(tidyverse)

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

train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))  # 70% train

train <- model_data[train_index, ]
test  <- model_data[-train_index, ]

# actual full model
full <- lm(money~., data=data)
summary(full)
full_pred <- predict(full,test)

rmse <- sqrt(mean((full_pred - test$money)^2)) 
mae <- mean(abs(full_pred - test$money))
r2 <- cor(full_pred, test$money)^2 


# predict price based on drink
price_model <- lm(money ~ coffee_name, data=data)
price_pred <- predict(price_model, test)

rmse <- sqrt(mean((price_pred - test$money)^2)) 
mae <- mean(abs(price_pred - test$money))
r2 <- cor(price_pred, test$money)^2 


# chosen model
chosen_model <- lm(money~ coffee_name + Month_name + Time_of_Day, data=data)
summary(full_model)

chosen_pred <- predict(chosen_model,test)

rmse <- sqrt(mean((chosen_pred - test$money)^2)) 
mae <- mean(abs(chosen_pred - test$money))
r2 <- cor(chosen_pred, test$money)^2 





