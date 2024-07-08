setwd("C:/Internship")
print(getwd())

#load data
train <- read.csv('demographic2.csv')

#create training and validation data from given data
install.packages('caTools')
library(caTools)

#cats = apply(train, 2, function(x) nlevels(as.factor(x)))
#cats


#prints current datatype
#print(sapply(train, class))

#train[, c("score",	"stay",	"travel_mode",	"travel_dur",	"cont_del",	"exam_mode",	"tech_org",	"extra_curr")] <- lapply(train[, c("score",	"stay",	"travel_mode",	"travel_dur",	"cont_del",	"exam_mode",	"tech_org",	"extra_curr")], factor)


#set.seed(88)
split <- sample.split(train$fe_score, SplitRatio = 0.75)

#get training and test data
demtrain <- subset(train, split == TRUE)
demtest <- subset(train, split == FALSE)

demtest

length(demtest$score)

#logistic regression model
model <- glm(demtrain$fe_score ~ . , data = demtrain, family = binomial, na.action = "na.exclude")
summary(model)

# Generate predicted probabilities for test data
predicted_probs <- predict(model, newdata = demtest, type = "response")
predicted_prob


# Convert predicted probabilities to predicted binary outcomes
predicted <- ifelse(predicted_probs > 0.5, 1, 0)
predicted

# Calculate accuracy of predicted vs. actual data
accuracy <- mean(predicted == demtest$fe_score)

# Print accuracy
accuracy
