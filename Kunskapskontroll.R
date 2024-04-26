# Load the libraries
library(readr)
library(car)
library(leaps)
library(glmnet)
library(Metrics)

# Load the dataset
cars <- read.csv("Blocket_Bilar_grupp2.csv", sep = ";")

# Check the structure of the dataset
colnames(cars)

str(cars)

head(cars)

table(g)

names(cars_train)

str(res)

table(cars$CarName_Brand)

# Create a vector of values to be removed
values_to_remove <- c("2016", "2021", "VW", "XC60", "Söker")

# Filter the data to exclude rows with these values
filtered_cars <- cars[!cars$CarName_Brand %in% values_to_remove, ]


table(filtered_cars$CarName_Brand)


#Train, Validate, Test
spec <- c(train <- 0.6, validate <- 0.2, test <- 0.2)

set.seed(99)
g <- sample(cut(
  seq(nrow(filtered_cars)),
  nrow(filtered_cars)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(filtered_cars, g)

cars_train <- res[[1]]
cars_val <- res[[2]]
cars_test <- res[[3]]


#looking at correlation
cor(cars_train$CarName_ModelYear, cars_train$CarName_Miles)

# Performing multiple regression analysis using the full model
model1 <- lm(I(log(CarName_Price)) ~ ., data = filtered_cars)
summary(model1)

par(mfrow = c(2, 2))
plot(model1)


#Performing multiple regression analysis using "logic"
model2 <- lm(I(log(CarName_Price)) ~ CarName_ModelYear + CarName_Miles + CarName_Engine +
               CarName_Brand, data = cars_train)
summary(model2)

vif(model2)

coef(model2)

par(mfrow = c(2, 2))
plot(model2)

#Performing a lasso regression
x <- data.matrix(cars_test[, c('CarName_ModelYear', 'CarName_Miles',  'CarName_Engine', 'CarName_Brand',
                               'CarName_Dealer', 'CarName_Model', 'CarName_Region', 'CarName_gears')])

y <- cars_test$CarName_Price

#perform k-fold CV to find optimal lambda
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

#evaluation
val_pred_m1 <- predict(model1, newdata = cars_val)
val_pred_m2 <- predict(model2, newdata = cars_val)




results <- data.frame(
  Model = c("Model 1", "Model 2"),
  RMSE_val_data = c(rmse(cars_val$CarName_Price, val_pred_m1),
                    rmse(cars_val$CarName_Price, val_pred_m2)),
  Adj_R_squared = c(summary(model1)$adj.r.squared,
                    summary(model2)$adj.r.squared),
  BIC = c(BIC(model1), BIC(model2))
)

results
