#### Does More Crime Occur in More Populous States? ####

## Simple Linear Regression Model

library(caTools)
library(ggplot2)

#### Importing the Dataset ####

dataset = read.csv('crime_and_incarceration_by_state.csv')

na <- is.na(dataset$jurisdiction)

na

summary(dataset)

#### Splitting the Dataset into a Training Set and Test Set ####

set.seed(123)

split <- sample.split(dataset$violent_crime_total,
                      SplitRatio = 2/3)

training_set <- subset(dataset, split == TRUE)

test_set <- subset(dataset, split == FALSE)

regressor <- lm(formula = violent_crime_total ~ state_population,
                data = training_set)

summary(regressor)

#### Predicting Test Set Results ####

## Vector of Predictions

y_pred <- predict(regressor, newdata = test_set)
y_pred

#### Plotting: Visualizing Training Set Results ####

ggplot() + 
    geom_point(aes(x = training_set$state_population,
                   y = training_set$violent_crime_total),
               color = 'orange') +
    geom_line(aes(x = training_set$state_population,
                  y = predict(regressor, newdata = training_set)),
              color = 'purple') +
    ggtitle('Population vs. Violent Crime (Training Set)') +
    xlab('State Population') +
    ylab('Violent Crime')


#### Visualizing Test Set Results ####

ggplot() +
    geom_point(aes(x = test_set$state_population,
                   y = test_set$violent_crime_total),
               color = 'green') +
    geom_line(aes(x = training_set$state_population,
                  y = predict(regressor, newdata = training_set)),
              color = 'purple') +
    ggtitle('Population vs. Violent Crime (Test Set)') +
    xlab('State Population') +
    ylab('Violent Crime')

