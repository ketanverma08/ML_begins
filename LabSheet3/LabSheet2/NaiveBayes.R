rm(list=ls(all=T))      #To clear Global environment variables
install.packages('e1071')
install.packages("dplyr")
install.packages("psych")
install.packages('ggplot')
library('e1071')
library("dplyr")
library("psych")
library("ggplot2")
library(readxl)
Analysis_Grade <- read_excel("/home/sachin/Desktop/Analysis_Grade.xlsx")
str(Analysis_Grade)
head(Analysis_Grade)
summary(Analysis_Grade)
Analysis_2 <- subset(Analysis_Grade, select = Year:Grade)
str(Analysis_2)
View("Analysis_2")
Analysis_2$`Attendance %` <- NULL
Analysis_2$Year <- NULL
Analysis_2$`M/F` <- NULL
Analysis_2$CGPA <- NULL
Analysis_2$Grade <- as.factor(Analysis_2$Grade)
pairs.panels(Analysis_2[-6])
set.seed(1234)
ind <- sample(2, nrow(Analysis_2), replace =T , prob = c(0.8, 0.2))
train <- Analysis_2[ind == 1,]
test <- Analysis_2[ind ==2,]

NB_model <- naiveBayes(Grade ~ ., data = train , useKernel = T)
NB_model

train_p <- predict(NB_model, train , type ='class')
head(cbind(train_p,train))

train_p1 <- predict(NB_model,train)
(train_tab <- table( train_p1,train$Grade))  

1 - sum(diag(train_tab)) / sum(train_tab)

test_p <- predict(NB_model, test , type ='class')
head(cbind(test_p,test))

test_p1 <- predict(NB_model,test)
(test_tab <- table( test_p1,test$Grade))  

1 - sum(diag(test_tab)) / sum(test_tab)




