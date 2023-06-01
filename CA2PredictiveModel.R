sleep_data <- read.csv("sleep.csv")

# Examine initial linearity between variables in the dataset
library(psych)
pairs.panels(sleep_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals



scatter.smooth(x = sleep_data$snor_rate,
               y = sleep_data$stress_level,
               xlab = "snor_rate",
               ylab = "Stress_level", main = "Correlation of stress_level ~ snor_rate")
cor(sleep_data$stress_level, sleep_data$snor_rate)

scatter.smooth(x = sleep_data$resp_rate,
               y = sleep_data$stress_level,
               main = "Correlation of stress level ~ Respiration Rate",
               xlab = "Respiration rate %",
               ylab = "Stress level")
cor(sleep_data$stress_level, sleep_data$resp_rate)

scatter.smooth(x = sleep_data$body_temp,
               y = sleep_data$stress_level,
               main = "Correlation of stress level ~ Body_temp",
               xlab = "Body temp",
               ylab = "Stress level")
cor(sleep_data$stress_level, sleep_data$body_temp)

scatter.smooth(x = sleep_data$limb_mov,
               y = sleep_data$stress_level,
               main = "Correlation of stress level ~ Limb movement",
               xlab = "Limb movement",
               ylab = "Stress level")
cor(sleep_data$stress_level, sleep_data$limb_mov)

scatter.smooth(x = sleep_data$blood_oxy,
               y = sleep_data$stress_level,
               main = "Correlation of stress level ~ Blood Oxygen",
               xlab = "Blood Oxygen",
               ylab = "Stress level")
cor(sleep_data$stress_level, sleep_data$blood_oxy)

scatter.smooth(x = sleep_data$rem,
               y = sleep_data$stress_level,
               main = "Correlation of stress level ~ Rapid eye movement",
               xlab = "REM",
               ylab = "Stress level")
cor(sleep_data$stress_level, sleep_data$rem)

scatter.smooth(x = sleep_data$sleep_hours,
               y = sleep_data$stress_level,
               main = "Correlation of stress level ~ Sleep_hours",
               xlab = "Sleep Hours",
               ylab = "Stress level")
cor(sleep_data$stress_level, sleep_data$sleep_hours)

scatter.smooth(x = sleep_data$heart_rate,
               y = sleep_data$stress_level,
               main = "Correlation of stress level ~ heart rate",
               xlab = "Heart rate",
               ylab = "Stress level")
cor(sleep_data$stress_level, sleep_data$heart_rate)

#check for outliers

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 1)) # divide graph area in 3 rows by 2 columns
attach(sleep_data)
boxplot(stress_level,
        main = "Stress_level",
        sub = paste("Outlier rows: ",
                    boxplot.stats(stress_level)$out)) # box plot for 'Stress level'
boxplot(snor_rate,
        main = "Snoring rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(snor_rate)$out)) # box plot for 'Snoring'
boxplot(resp_rate,
        main = "Respiration Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(resp_rate)$out)) # box plot for 'Respiration Rate'
boxplot(body_temp,
        main = "Body Temperature",
        sub = paste("Outlier rows: ",
                    boxplot.stats(body_temp)$out)) # box plot for 'Body Temperature'
boxplot(limb_mov,
        main = "Limb Movement",
        sub = paste("Outlier rows: ",
                    boxplot.stats(limb_mov)$out)) # box plot for 'Limb Movement'
boxplot(blood_oxy,
        main = "Blood Oxygen",
        sub = paste("Outlier rows: ",
                    boxplot.stats(blood_oxy)$out)) # box plot for 'Blood Oxygen'
boxplot(rem,
        main = "Rapid Eye movement",
        sub = paste("Outlier rows: ",
                    boxplot.stats(rem)$out)) # box plot for 'Rapid eye movement'

boxplot(sleep_hours,
        main = "Sleeping Hours",
        sub = paste("Outlier rows: ",
                    boxplot.stats(sleep_hours)$out)) # box plot for 'Sleep Hours'
boxplot(heart_rate,
        main = "heart rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_rate)$out)) # box plot for 'Heart_rate'

outlier_values <- boxplot.stats(heart_rate)$out # outlier values.
paste("snoring rate outliers: ", paste(outlier_values, collapse=", "))

outliers <- boxplot(sleep_data)$out
print(outliers)

# Check for normality
# Skewness function to examine normality
install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,1)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical
plot(density(sleep_data$snor_rate),
     main = "Density plot : snoring rate",
     ylab = "Frequency", xlab = "snoring rate",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$snor_rate), 2)))
# fill the area under the plot
polygon(density(sleep_data$snor_rate), col = "red")

plot(density(sleep_data$resp_rate),
     main = "Density plot : Respiration Rate",
     ylab = "Frequency", xlab = "Respiration Rate",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$resp_rate), 2)))
polygon(density(sleep_data$resp_rate), col = "red")

plot(density(sleep_data$body_temp),
     main = "Density plot : Temperature",
     ylab = "Frequency", xlab = "Temperature",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$body_temp), 2)))
# fill the area under the plot
polygon(density(sleep_data$body_temp), col = "red")
plot(density(sleep_data$limb_mov),
     main = "Density plot : Limb Movement",
     ylab = "Frequency", xlab = "Limb movement",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$limb_mov), 2)))
polygon(density(sleep_data$limb_mov), col = "red")
plot(density(sleep_data$blood_oxy),
     main = "Density plot : Blood Oxygen",
     ylab = "Frequency", xlab = "Blood oxygen",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$blood_oxy), 2)))
# fill the area under the plot
polygon(density(sleep_data$blood_oxy), col = "red")
plot(density(sleep_data$rem),
     main = "Density plot : Rapid eye movement",
     ylab = "Frequency", xlab = "Rapid eye movement",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$rem), 2)))
# fill the area under the plot
polygon(density(sleep_data$rem), col = "red")
plot(density(sleep_data$sleep_hours),
     main = "Density plot : Sleeping hours",
     ylab = "Frequency", xlab = "sleeping hours",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$sleep_hours), 2)))
# fill the area under the plot
polygon(density(sleep_data$sleep_hours), col = "red")
plot(density(sleep_data$heart_rate),
     main = "Density plot : Heart rate",
     ylab = "Frequency", xlab = "Heart rate",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$heart_rate), 2)))
# fill the area under the plot
polygon(density(sleep_data$heart_rate), col = "red")
plot(density(sleep_data$stress_level),
     main = "Density plot : Stress level",
     ylab = "Frequency", xlab = "Stress level",
     sub = paste("Skewness : ", round(e1071::skewness(sleep_data$stress_level), 2)))
# fill the area under the plot
polygon(density(sleep_data$stress_level), col = "red")
par(opar)

# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0-5 = approx symetric.

paste("Skewness for Snore rate : ", round(e1071::skewness(sleep_data$snor_rate), 2))
paste("Skewness for Respiration rate : ", round(e1071::skewness(sleep_data$resp_rate), 2))
paste("Skewness for Body temp : ", round(e1071::skewness(sleep_data$body_temp), 2))
paste("Skewness for limb movement : ", round(e1071::skewness(sleep_data$limb_mov), 2))
paste("Skewness for Blood oxygen : ", round(e1071::skewness(sleep_data$blood_oxy), 2))
paste("Skewness for REM : ", round(e1071::skewness(sleep_data$rem), 2))
paste("Skewness for Sleep hours : ", round(e1071::skewness(sleep_data$sleep_hours), 2))
paste("Skewness for heart rate : ", round(e1071::skewness(sleep_data$heart_rate), 2))
paste("Skewness for stress level : ", round(e1071::skewness(sleep_data$stress_level), 2))


# p-value < 0.05 indicate that the data is not normally distributed
shapiro.test(sleep_data$snor_rate)
shapiro.test(sleep_data$resp_rate)
shapiro.test(sleep_data$body_temp)
shapiro.test(sleep_data$limb_mov)
shapiro.test(sleep_data$blood_oxy)
shapiro.test(sleep_data$rem)
shapiro.test(sleep_data$sleep_hours)
shapiro.test(sleep_data$heart_rate)
shapiro.test(sleep_data$stress_level)

install.packages("MASS")
library(MASS)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1

Box = boxcox(sleep_data$snor_rate ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_snor_rate = (sleep_data$snor_rate ^ lambda - 1)/lambda
hist(sleep_data$snor_rate)
shapiro.test(transformed_snor_rate)

Box = boxcox(sleep_data$resp_rate ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_resp_rate = (sleep_data$resp_rate ^ lambda - 1)/lambda
hist(sleep_data$resp_rate)
shapiro.test(transformed_resp_rate)

Box = boxcox(sleep_data$body_temp ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_body_temp = (sleep_data$body_temp ^ lambda - 1)/lambda
hist(sleep_data$body_temp)
shapiro.test(transformed_body_temp)

Box = boxcox(sleep_data$limb_mov ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_limb_mov = (sleep_data$limb_mov ^ lambda - 1)/lambda
hist(sleep_data$limb_mov)
shapiro.test(transformed_limb_mov)

Box = boxcox(sleep_data$blood_oxy ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_blood_oxy = (sleep_data$blood_oxy ^ lambda - 1)/lambda
hist(sleep_data$blood_oxy)
shapiro.test(transformed_blood_oxy)

Box = boxcox(sleep_data$rem ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_rem = (sleep_data$rem ^ lambda - 1)/lambda
hist(sleep_data$rem)
shapiro.test(transformed_rem)

Box = boxcox(sleep_data$sleep_hours ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_sleep_hours = (sleep_data$sleep_hours ^ lambda - 1)/lambda
hist(sleep_data$sleep_hours)
shapiro.test(transformed_sleep_hours)

Box = boxcox(sleep_data$heart_rate ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_heart_rate = (sleep_data$heart_rate ^ lambda - 1)/lambda
hist(sleep_data$heart_rate)
shapiro.test(transformed_heart_rate)


sleep_data$transformed_snor_rate <- transformed_snor_rate
sleep_data$transformed_resp_rate <- transformed_resp_rate
sleep_data$transformed_body_temp <- transformed_body_temp
sleep_data$transformed_limb_mov <- transformed_limb_mov
sleep_data$transformed_blood_oxy <- transformed_blood_oxy
sleep_data$transformed_rem <- transformed_rem
sleep_data$transformed_sleep_hours <- transformed_sleep_hours
sleep_data$transformed_heart_rate <- transformed_heart_rate



#splitting data into training and testing
set.seed(1)
no_rows_data <- nrow(states)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- sleep_data[sample, ]
testing_data <- sleep_data[-sample, ]

unmodified_model <- lm(stress_level ~ snor_rate + resp_rate + body_temp + limb_mov + 
                         blood_oxy + rem + sleep_hours + heart_rate, data=training_data)
summary(unmodified_model)

# Examine which combination of independent variables best fits the model
install.packages("leaps")
library(leaps)
# See https://cran.r-project.org/web/packages/leaps/leaps.pdf
# Regression Subset Selection
MLR_subset_selection <-regsubsets(stress_level ~ snor_rate + resp_rate + body_temp + 
                                  limb_mov + blood_oxy + rem + sleep_hours + heart_rate, 
                                  data=training_data, nbest=6)
plot(MLR_subset_selection, scale="adjr2")

stepAIC(unmodified_model, direction="backward")

modified_model <- lm(stress_level ~ transformed_snor_rate 
                     + transformed_resp_rate + transformed_body_temp 
                     + transformed_limb_mov + transformed_blood_oxy + transformed_rem
                     + transformed_sleep_hours + transformed_heart_rate,data = training_data)
summary(modified_model)

MLR_subset_selection_modified <-regsubsets(stress_level ~ transformed_snor_rate 
                                           + transformed_resp_rate + transformed_body_temp 
                                           + transformed_limb_mov + transformed_blood_oxy
                                           + transformed_rem + transformed_sleep_hours 
                                           + transformed_heart_rate,data = training_data, nbest=6)
plot(MLR_subset_selection_modified, scale="adjr2")     

confint(unmodified_model)

confint(modified_model)

# Examine outliers for the unmodified model
library(car)
qqPlot(unmodified_model, 
       labels=row.names(training_data$name), 
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot for unmodified model")

training_data[26,]
training_data[29,]

fitted(unmodified_model)[26]
fitted(unmodified_model)[29]

studentized_fit <- rstudent(unmodified_model)
hist(studentized_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

rug(jitter(studentized_fit), col="brown")
curve(dnorm(x, mean=mean(studentized_fit), sd=sd(studentized_fit)), add=TRUE, col="blue", lwd=2)
lines(density(studentized_fit)$x, density(studentized_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)
outlierTest(unmodified_model)

#applying logistic regression:
install.packages("ISLR")
library(ISLR)

attach(sleep_data)
log_model <- glm(stress_level ~ transformed_snor_rate 
                 + transformed_resp_rate + transformed_body_temp 
                 + transformed_limb_mov + transformed_blood_oxy
                 + transformed_rem + transformed_sleep_hours 
                 + transformed_heart_rate, 
                 data=training_data)
options(scipen=999)
summary(log_model)

install.packages("pscl")
library(pscl)
pscl::pR2(log_model)["McFadden"]

install.packages("caret")
# Higher values indicate more importance. 
# These results match up nicely with the p-values 
# from the model. Balance is by far the most important 
# predictor variable, followed by student status and then income.
library(caret)
varImp(log_model)

library(car)
vif(log_model)

install.packages('devtools')
devtools::install_github("selva86/InformationValue")

library(InformationValue)
# Evaluate optimum cutoff for dependent variable
# convert dependent variable first
# defaults information from "Yes" and "No" to 1's and 0's
testing_data$will_default <- ifelse(testing_data$default=="Yes", 1, 0)
predicted_sample <- predict(log_model, testing_data, type="response")

optimal <- optimalCutoff(testing_data$will_default, predicted_sample)[1]
optimal
actuals_predictions <- data.frame(cbind(actuals = testing_data$will_default,
                                        predicted = predicted_sample,
                                        optimum =ifelse(predicted_sample >= optimal, 1, 0)))
head(actuals_predictions, 20)

predicted_data <- data.frame(body_temp = 99, heart_rate = 70, stress_level = c("Yes", "No"))
predicted <- predict(log_model, predicted_data, type="response")
predicted

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

confusionMatrix(testing_data$will_default, actuals_predictions$optimum)

#calculate sensitivity
# true positive rate
sensitivity(testing_data$will_default, actuals_predictions$optimum)

#calculate specificity
specificity(testing_data$default, actuals_predictions$optimum)

#calculate total misclassification error rate
# This is the total misclassification
# error rate for the model.
# Error rate = 2.5%. Lower error rate
# means the better the model can
# predict outcomes
misClassError(testing_data$will_default, actuals_predictions$optimum, threshold=optimal)

#plot the ROC curve
plotROC(testing_data$will_default, actuals_predictions$optimum)

