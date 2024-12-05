setwd("H:/sta215")
#Load the data
raw_data<-read.csv("raw_data.csv")
data <- na.omit(raw_data)
#Install "haven" packag
#install.packages("haven")

#Load packageS
library("readr")
library("dplyr")
library("haven")
library("psych")
library("ggplot2")

#descriptive statistics
table(data$budget_cuts,data$tax)

table(data$salary)


summary(data$income)
sd(data$income)


summary(data$graduation)
sd(data$graduation)

summary(data$percent_of._low_income_students)
sd(data$percent_of._low_income_students)



# table 2
table(data$tax,data$budget_cuts)
#Chi-Squared Test(Tax & Budget Cuts)
chisq.test(data$tax,data$budget_cuts)
result<-chisq.test(data$tax,data$budget_cuts)
print(result)

#ANOVA
boxplot(percent_of._low_income_students ~ salary, data = raw_data)
anova <- aov(percent_of._low_income_students ~ salary, data = raw_data)
summary(anova)

#linear regression
#Scatter Plot
plot(data$percent_of._low_income_students, data$graduation)
#Calculate mean lines
meanx <- mean(data$percent_of._low_income_students)
meany <- mean(data$graduation)

abline(v=meanx)
abline(h=meany)

       
#Linear Regression
model<- lm(graduation ~ percent_of._low_income_students,data=data)
summary(model)
abline(model)


#residuals

plot(data$percent_of._low_income_students,residuals(model))
abline(h=0)
