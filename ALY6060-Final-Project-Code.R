install.packages("GGally")
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(grid)



# ---Loading the data--- #
admitdata = read.csv("Admission_Predict.csv", header = TRUE, sep = ",")


# ---Exploratory Data Analysis--- #

dim(admitdata)
str(admitdata)

#Missing Values
sum(is.na(admitdata))
names(admitdata)

head(admitdata)
tail(admitdata)


#Minimum and Maximum Chance of Admit
min(admitdata$Chance.of.Admit)*100
max(admitdata$Chance.of.Admit)*100

#Minimum and Maximum GRE & TOEFL Score
range(admitdata$GRE.Score)
range(admitdata$TOEFL.Score)

#Minimum and Maximum SOP & LOR  Score
range(admitdata$SOP)
range(admitdata$LOR)


str(admitdata)
names(admitdata)

mean(admitdata$GRE.Score)
sd(admitdata$GRE.Score)

mean(admitdata$TOEFL.Score)
sd(admitdata$TOEFL.Score)

mean(admitdata$Chance.of.Admit)
sd(admitdata$Chance.of.Admit)

summary(admitdata)


# ---CORRELATION--- #

cor(admitdata$GRE.Score, admitdata$Chance.of.Admit)
cor(admitdata$TOEFL.Score, admitdata$Chance.of.Admit)
cor(admitdata$University.Rating, admitdata$Chance.of.Admit)
cor(admitdata$SOP, admitdata$Chance.of.Admit)
cor(admitdata$LOR, admitdata$Chance.of.Admit)
cor(admitdata$CGPA, admitdata$Chance.of.Admit)


# ---Understanding the co-relation between different factors responsible for admission--- #




#Chances of admission based on "GRE Score" :

str(admitdata$GRE.Score)
summary(admitdata$GRE.Score)
hist(admitdata$GRE.Score,
     col = "Yellow",
     ylab = "Frequency of scores", 
     xlab = "GRE Scores", 
     main = "Distribution of GRE Scores")

boxplot(admitdata$GRE.Score,
        col = "Yellow",
        main = "Descriptive Analysis of GRE Score")

chisq.test(admitdata$GRE.Score, admitdata$Chance.of.Admit) #to check if there is any dependency of Chances of admission on GRE Score


# To check for any outliers between GRE score and Chance of admission
ggplot(admitdata, aes(x = admitdata$GRE.Score, y = admitdata$Chance.of.Admit))+geom_point()+geom_smooth()+ggtitle("Chances of Admit vs GRE Score")

#To obtain scatter plot and correlation values
library(GGally)
ggpairs(admitdata)





#Chances of admission based on "TOEFL Score" :


boxplot(admitdata$TOEFL.Score,
        horizontal = TRUE,
        main = "Boxplot TOEFL Score",
        xlab = "TOEFL Score",
        col = "Green")


hist(admitdata$TOEFL.Score,
     col = "Green",
     ylab = "Frequency of scores", 
     xlab = "TOEFL Scores", 
     main = "Distribution of TOEFL Scores")

chisq.test(admitdata$TOEFL.Score, admitdata$Chance.of.Admit)


# To check for any outliers between TOEFL score and Chance of admission
ggplot(admitdata, 
       aes(x = admitdata$TOEFL.Score, 
           y = admitdata$Chance.of.Admit, 
           col = admitdata$University.Rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Chances of Admit vs TOEFL Score")





#Chances of admission based on "CGPA" :


boxplot(admitdata$CGPA,
        horizontal = TRUE,
        main = "Boxplot CGPA",
        xlab = "CGPA",
        col = "Red")


hist(admitdata$CGPA,
     col = "Red",
     ylab = "Chance of admission", 
     xlab = "CGPA", 
     main = "Distribution of CGPA")

chisq.test(admitdata$CGPA, admitdata$Chance.of.Admit)


ggplot(admitdata, 
       aes(x = admitdata$CGPA, y = admitdata$Chance.of.Admit, col = admitdata$Research))+
  geom_point()+
  geom_smooth()+
  ggtitle("Chances of Admit wrt CGPA")



# ---CORRELATION MATRIX--- #

z = cor(admitdata)
corrplot(z, method = 'number')




# ---Predictive Analytics--- #


adc = admitdata[ , -c(1)]
head(adc)


#Splitting into training and testing set

library(caTools)
set.seed(243)

sample = sample.split(adc$Chance.of.Admit, SplitRatio = 0.8)

test_set = subset(adc, sample == FALSE)
train_set = subset(adc, sample == TRUE)

str(test_set)
str(train_set)


#Multiple Linear Regression Model

adc_lm_model = lm(Chance.of.Admit~. , train_set)
summary(adc_lm_model)


#To identify variables that are insignificant 

adc_lm_model_2 = lm(Chance.of.Admit~GRE.Score+LOR+CGPA+Research, train_set)
summary(adc_lm_model_2)


Chance.of.Admit_model = predict(adc_lm_model_2, test_set)
finaladc = cbind(test_set, Chance.of.Admit_model)

write.csv(finaladc, "Admit_Prediction_Final.csv")

head(finaladc)