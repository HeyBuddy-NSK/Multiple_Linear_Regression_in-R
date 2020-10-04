# MLR

# importing dataset
mlr_data = read.csv("50_Startups.csv")

# Encoding categorical data.
mlr_data$State = factor(mlr_data$State,
                        levels = c("New York", "California", "Florida"),
                        labels = c(1,2,3))

# splitting dataset into train & test set
library(caTools)
split = sample.split(mlr_data$Profit,SplitRatio = 0.8)
train = subset(mlr_data, split == TRUE)
test = subset(mlr_data, split==FALSE)

# Training mlr model on train set
mlr = lm(formula = Profit ~ .,
         data = train)

# predicting the results on test set.
pred = predict(mlr, newdata = test)

# Building the optimal model using backward elimination

mlr_opt = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend +State,
             data = mlr_data)
summary(mlr_opt)
mlr_opt = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend ,
             data = mlr_data)
summary(mlr_opt)
mlr_opt = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
             data = mlr_data)
summary(mlr_opt)
mlr_opt = lm(formula = Profit ~ R.D.Spend,
             data = mlr_data)
summary(mlr_opt)

pred = predict(mlr_opt, newdata = test)