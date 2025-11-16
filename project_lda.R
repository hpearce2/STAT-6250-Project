library(lubridate)
library(dplyr)
library(tidyverse)
library(MASS)

data <- read.csv("Coffe_sales.csv")

# to find number of each coffee in dataset
latte <- data %>%
  count(coffee_name == "Latte") 

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


# scale 
num_cols <- c("hour_of_day","money")

train[, num_cols] <- scale(train[, num_cols])
test[, num_cols] <- scale(test[, num_cols])


### lda

# did model selection and this is best model
# don't use weekday sort and month sort bc causes collinearity and lda assumes variables not perfectly correalted
lda_model <- lda(coffee_name ~ money + Time_of_Day*Month_name,
                 data = train)

# predict on test data
lda_pred <- predict(lda_model, test)
# predict 1065 which is 30% test set

# # lda classes
lda_classes <- lda_pred$class
summary(lda_classes) # predicts 1065, 30% test set

# lda accuracy
lda_accuracy <- mean(lda_classes == test$coffee_name)
print(paste("LDA Accuracy:", round(lda_accuracy * 100, 2), "%"))
#same as when testing observed priors below

# confusion matrix
table(Predicted = lda_classes, Actual = test$coffee_name)



### applying different priors 
obs_priors = prop.table(table(train$coffee_name)) |> as.numeric()

#uniform classes 
uni_priors <- rep(1 / length(levels(train$coffee_name)), length(levels(train$coffee_name)))

#skewed: favoring the most common drink
skw_priors = replace(uni_priors, which.max(obs_priors), 0.8)
skw_priors = skw_priors / sum(skw_priors)

#combine priors into list
priors_list = list(
  Observed = obs_priors,
  Uniform = uni_priors,
  Skewed = skw_priors
)

#LDA models with different priors
lda_results = map_df(names(priors_list), function(pr_name) {
  lda_fit = lda(coffee_name~ money + Time_of_Day*Month_name,
                data= train,
                prior = priors_list[[pr_name]])
  pred = predict(lda_fit, test)$class
  tibble(
    Setting = pr_name,
    Accuracy = mean(pred == test$coffee_name)
  )
})

#numeric accuracy output
lda_results

