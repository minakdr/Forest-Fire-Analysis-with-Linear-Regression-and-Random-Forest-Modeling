pacman::p_load(pacman, rio )
data <- import ("D:\\Projects\\forest fire\\forestfires.csv")
head(data)
summary(data)

# Check for duplicate rows and remove them we already know there is no mising values by reading the documentation
sum(duplicated(data))
data<-data[!duplicated(data),]


# we can use this data$month<- as.numeric(factor(data$month)) to numerize the data but id rather do it in a more orrgnaized and supervised way 
month_conversion <- c("jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, 
                      "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)

day_conversion <- c("mon" = 1, "tue" = 2, "wed" = 3, "thu" = 4, "fri" = 5, "sat" = 6, "sun" = 7)

data$month <- month_conversion[data$month]
data$day <- day_conversion[data$day]
# visualisation 
plot(data)
library(ggplot2)
library(dplyr)
fires_by_month <- data %>%
  group_by(month) %>%
  summarize(total_fires = n())

fires_by_month %>% 
  ggplot(aes(x = month, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of forest fires in data by month",
    y = "Fire count",
    x = "Month"
  )

#fires happen the most on august and septembre

fires_by_day <- data%>%
  group_by(day)%>%
  summarize(total_fires =n())

fires_by_day%>% 
  ggplot(aes(x=day, y=total_fires))+
  geom_col()

#fires happen the most on sundays and the least on wednesdays 

library(tidyr)
forest_fires_long <- data %>% 
# we reshape our data to long format because it helps with the visualization 
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", 
             "ISI", "temp", "RH", 
             "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

forest_fires_long %>% 
  ggplot(aes(x = month, y = value)) +
  geom_boxplot() +
#creates separated plots (facets) for each variable , allowing each facet to have its own y-axis scale.
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable changes over month",
    x = "Month",
    y = "Variable value"
  )

forest_fires_long %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Value of column",
    y = "Area burned (hectare)"
  )

#since th area variable is skewed we apply a log transformation to normalize it. This can improve model performance. 
data$log_area <- log1p(data$area)

#correlation matrix
correlation_matrix<-cor(data)
correlation_matrix

#there is no clear corelation between any variable with the log area one therefor the linear regression might not do too well
library(caret)
library(lattice)
library(ggplot2)
library(randomForest)
library(Metrics)
library(xgboost)
library(pdp)

set.seed(123)
splitIndex <- createDataPartition(data$log_area, p = 0.8, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]

#linear Regression Model
lm_model <- lm(log_area ~ ., data = train_data)
summary(lm_model)

#random Forest Model
rf_model <- randomForest(log_area ~ ., data = train_data)
print(rf_model)

# predictions and Evaluation
lm_predictions <- predict(lm_model, newdata = test_data)
rf_predictions <- predict(rf_model, newdata = test_data)


# Evaluation Metrics
lm_rmse <- rmse(test_data$log_area, lm_predictions)
rf_rmse <- rmse(test_data$log_area, rf_predictions)

cat("Linear Regression RMSE:", lm_rmse, "\n")
cat("Random Forest RMSE:", rf_rmse, "\n")

# fature Importance (Random Forest)
importance(rf_model)

#plotVariable Importance for Random Forest
varImpPlot(rf_model)

#Summary of Results
cat("Model Performance Summary:\n")
cat("Linear Regression RMSE:", lm_rmse, "\n")
cat("Random Forest RMSE:", rf_rmse, "\n")
