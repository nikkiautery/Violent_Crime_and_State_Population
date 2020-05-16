# Multiple Linear Regression

library(caTools)
library(ggplot2)

#### Importing the Dataset ####

dataset <- read.csv('crime_and_incarceration_by_state.csv')
dataset <- dataset[1:5]

# Encoding categorical data
dataset$jurisdiction = factor(dataset$jurisdiction,
                       levels = c('ALABAMA', 'ALASKA',  'ARIZONA', 'ARKANSAS',
                                  'CALIFORNIA', 'COLORADO','CONNECTICUT', 
                                  'DELAWARE', 'FLORIDA', 'GEORGIA', 'HAWAII',
                                  'IDAHO', 'ILLINOIS', 'INDIANA', 'IOWA',
                                  'KANSAS', 'KENTUCKY', 'LOUISIANA', 'MAINE',
                                  'MARYLAND', 'MASSACHUSETTS', 'MICHIGAN', 
                                  'MINNESOTA', 'MISSISSIPPI', 'MISSOURI', 
                                  'MONTANA', 'NEBRASKA', 'NEVADA',
                                  'NEW HAMPSHIRE', 'NEW JERSEY', 'NEW MEXICO', 
                                  'NEW YORK', 'NORTH CAROLINA', 'NORTH DAKOTA',
                                  'OHIO', 'OKLAHOMA', 'OREGON', 'PENNSYLVANIA',
                                  'RHODE ISLAND', 'SOUTH CAROLINA',
                                  'SOUTH DAKOTA', 'TENNESSEE', 'TEXAS', 'UTAH',
                                  'VERMONT', 'VIRGINIA', 'WASHINGTON', 
                                  'WEST VIRGINIA', 'WISCONSIN', 'WYOMING'),
                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                                  15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
                                  26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
                                  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
                                  48, 49, 50))

# Splitting the Dataset into the Training Set and Test Set

set.seed(123)

split = sample.split(dataset$violent_crime_total, SplitRatio = 0.8)

training_set = subset(dataset, split == TRUE)

test_set = subset(dataset, split == FALSE)


# Fitting Multiple Linear Regression to the Training Set

violent_reg = lm(formula = violent_crime_total ~ .,
               data = training_set)

summary(regressor)

murder_reg = lm(formula = murder_manslaughter ~ .,
               data = training_set)

summary(regressor)

rape_reg = lm(formula = rape_legacy ~ .,
               data = training_set)

summary(regressor)

# Predicting the Test Set Results

y_pred = predict(violent_reg, newdata = test_set)
y_pred

y_pred = predict(murder_reg, newdata = test_set)
y_pred

y_pred = predict(rape_reg, newdata = test_set)
y_pred



##

