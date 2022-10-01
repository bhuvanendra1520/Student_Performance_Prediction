library(dplyr) # for data manipulation
library(stringr) # for data manipulation
library(caret) # for sampling
library(caTools) # for train/test split
library(ggplot2) # for data visualization

df<-read.csv("C:/Users/bhuva/Documents/StudentsPerformance.csv")
head(df)
class(df)
dim(df)
str(df)
summary(df)
df$test.preparation.course<-toupper(df$test.preparation.course)
head(df$test.preparation.course)
head(unique(df))
head(duplicated(df))
df %>% distinct()
colSums(is.na(df))
df[is.na(df)] = 0
head(df)
colSums(is.na(df))
colnames(df)
namesOfColumns <- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(df) <- namesOfColumns
colnames(df)
### Prediction model
randIndex <- sample(1:dim(df)[1])
#  # In order to split data, create a 2/3 cutpoint and round the number
cutpoint2_3 <- floor(2*dim(df)[1]/3)

# create train data set, which contains the first 2/3 of overall data
trainData <- df[randIndex[1:cutpoint2_3],]
dim(trainData)
head(trainData)

# create test data, which contains the left 1/3 of the overall data
testData <- df[randIndex[(cutpoint2_3+1):dim(df)[1]],]
dim(testData)   # check test data set
head(testData)

#------------------------------------------------------lm model
model <- lm(Math_Score ~ Writing_Score + Gender + Race + Lunch + Parent_Education + Test_Prep,data=trainData)
summary(model)

lmPred <- predict(model,testData,interval = "prediction", level=0.95)
summary(lmPred)
head(lmPred)

#--------------------------------------------------------Decision Tree

library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Math_Score ~ . , df, method = 'class')
predicted_val <- predict(decisionTree_model, df, type = 'class')
probability <- predict(decisionTree_model, df, type = 'prob')
rpart.plot(decisionTree_model)

# 1. Add predictions 
mydata1 <- cbind(testData, lmPred)
head(mydata1)


# 2. Regression line + confidence intervals
p <- ggplot(mydata1, aes( fit, Math_Score)) +
  geom_point() +
  stat_smooth(method = lm)
p

# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  xlab("Predicted Scores") + ylab("Test Scores")