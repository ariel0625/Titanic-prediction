# Load standard libraries
library(tidyverse)
library(dplyr)
library(MASS) # Modern applied statistics functions library(fst)
library(titanic)
library(bayesQR)

titanic_dataset<-read.csv(file = '/Users/leechenhsin/Desktop/Study@USA/07_UW_School/IMT573/titanic.csv')

data(Titanic)
summary(Titanic)

head(titanic_dataset)
summary(titanic_dataset)
#the reason of why the body is missing might because that if someone is survived,
#then his/her body may not be found.Besides, if someone is not survived, then
#his/her body may also not be found.

#I would utilize simple liner regression model to determine the survival of
#passengers since this model can take into account of the relation between
#pclass and whether survived together.
model <- glm(survived ~ pclass, data =titanic_dataset,family=binomial)
summary(model)


#according to the model, since the p-value of the fare is less than 0.05
#so it shows a regression between pclass and survived.

#I would utilize simple liner regression model to determine the survival of
#passengers since this model can take into account of the relation between
#pclass and whether survived together.
model <- glm(survived ~ pclass, data =titanic_dataset,family=binomial)
summary(model)


#according to the model, since the p-value of the fare is less than 0.05
#so it shows a regression between pclass and survived.


titanic_dataset$child <-ifelse(titanic_dataset$age<14, "1", "0")

model4.1<- glm(survived ~ sex, data =titanic_dataset,family=binomial)
summary(model4.1)


a1<-ggplot(titanic_dataset ,aes(sex,fill=survived))+
  geom_bar(aes(fill=factor(survived)),position = "fill")+
  scale_fill_brewer(palette = "Set1")+
  ylab("survival")+
  ggtitle("survival by Pclass")
a1


#in the chart, it's clear that the survival of women is higher than men
model4.2 <- glm(survived ~ age, data =titanic_dataset,family=binomial)
summary(model4.2)


a2<-ggplot(titanic_dataset ,aes(age,fill=survived))+
  geom_bar(aes(fill=factor(survived)),position = "fill")+
  scale_fill_brewer(palette = "Set1")+
  ylab("survival")+
  ggtitle("survival by Pclass")
a2


#in the chart, it's clear that the survival of the oldest and the youngest
#is higher
model4.3 <- glm(survived ~ pclass, data =titanic_dataset,family=binomial)
summary(model4.3)


a3<-ggplot(titanic_dataset ,aes(pclass,fill=survived))+
  geom_bar(aes(fill=factor(survived)),position = "fill")+
  scale_fill_brewer(palette = "Set1")+
  ylab("survival")+
  ggtitle("survival by Pclass")
a3


library(simEd)

set.seed(1)
train_row <- sample(1309,1309*0.8)
train <- titanic_dataset[train_row, ]
test <- titanic_dataset[-train_row, ]

model5 <- glm(survived ~ pclass, data =train,family=binomial)
summary(model5)

library(broom)
library(dplyr)
titanic_dataset_new <- augment(model5,newdata=test,type.predict = 'response')%>%
  mutate(survive_predict=round(.fitted))

install.packages('SDMTools', repos = "http://cran.us.r-project.org")
library(SDMTools)

confMatrixNew<-confusion.matrix(titanic_dataset_new$survived,titanic_dataset_new$survive_p
confMatrixNew

titanic_dataset_new$survive_predict<- if(titanic_dataset_new$.fitted > 0.4){
  titanic_dataset_new$survive_predict=1 }else{
  }
titanic_dataset_new$survive_predict=0


confMatrixNew2<-confusion.matrix(titanic_dataset_new$survived,titanic_dataset_new$survive_
confMatrixNew2

#number of false =0+169=169
#number of positive =0+93=93
#when the threshold is decreased, the number of false increase.
#As a result, the accuracy of the matrix decreases.


