## Project:  STA 215, Fall 2024, Final Project
# Located:   iCloud
# File Name: Stats-Final-Project
# Date:      2024_12_17
# Who:       Anna Kicinska

#Load packages
#Install "haven" packag
#install.packages("haven")
library("readr")
library("dplyr")
library("haven")
library("psych")
library("ggplot2")

#Load the data
setwd("H:/sta215")
raw_data<-read.csv("raw_data.csv")
data <- na.omit(raw_data)

#########################################################################################
############### Table 1: descriptive statistics  ########################################
#########################################################################################
table(data$budget_cuts,data$tax)

table(data$salary)

summary(data$income)
sd(data$income)

summary(data$graduation)
sd(data$graduation)

summary(data$percent_of._low_income_students)
sd(data$percent_of._low_income_students)

##########################################################################################
################ Table 2: contingency table ##############################################
##########################################################################################
# table 2
table(data$tax,data$budget_cuts)
#Chi-Squared Test(Tax & Budget Cuts)
chisq.test(data$tax,data$budget_cuts)
result<-chisq.test(data$tax,data$budget_cuts)
print(result)

##########################################################################################
################# Figure 1: boxplot ######################################################
##########################################################################################
#ANOVA
boxplot(percent_of._low_income_students ~ salary, data = raw_data)
anova <- aov(percent_of._low_income_students ~ salary, data = raw_data)
summary(anova)

##########################################################################################
################# Figure 2: scatter plot #################################################
##########################################################################################
#Scatter Plot
plot(data$percent_of._low_income_students, data$graduation)
#Calculate mean lines
meanx <- mean(data$percent_of._low_income_students)
meany <- mean(data$graduation)

abline(v=meanx)
abline(h=meany)


#Linear Regression
#Add the linear regression line to the scatter plot
model<- lm(graduation ~ percent_of._low_income_students,data=data)
summary(model)
abline(model)

#########################################################################################
############### Figure 3: residual plot #################################################
#########################################################################################
#Plot the residuals
plot(data$percent_of._low_income_students,residuals(model))
abline(h=0)
