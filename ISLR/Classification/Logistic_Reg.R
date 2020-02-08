rm(list=ls())
library(ISLR); library(corrplot); library(ggplot2)
Smarket = ISLR::Smarket
names(Smarket)
dim(Smarket)
summary(Smarket)

corrplot(cor(Smarket[,-9]))
attach(Smarket)


glm_fit =glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume , 
             data=Smarket, family=binomial)
summary(glm_fit)


glm_probs = predict(glm_fit, type="response")
glm_probs[1:10]
contrasts(Direction)

glm_pred = rep("Down",nrow(Smarket))
glm_pred[glm_probs>0.5] = "UP"

glm_tab = table(glm_pred, Direction)
glm_tab
sum(diag(glm_tab))/sum(glm_tab)
mean(glm_pred==Direction)

train = (Year<2005)
Smarket_2005 = Smarket[!train,]
dim(Smarket_2005)
Direction_2005 = Direction[!train]


glm_tr_fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                 data=Smarket, family=binomial, subset=train)
summary(glm_tr_fit)
glm_ts_probs = predict(glm_tr_fit, Smarket_2005, type="response")

glm_ts_pred = rep("Down",252)
glm_ts_pred[glm_ts_probs>0.5] = "Up"
glm_ts_tab = table(glm_ts_pred, Direction_2005)
sum(diag(glm_ts_tab))/sum(glm_ts_tab)

mean(glm_ts_pred==Direction_2005)
mean(glm_ts_pred!=Direction_2005)


glm_fit = glm(Direction~Lag1+Lag2,
              data=Smarket, family=binomial, subset=train)
glm_probs = predict(glm_fit, Smarket_2005, type="response")
glm_pred = rep("Down",252)
glm_pred[glm_probs>0.5]="Up"
glm_tab = table(glm_pred,Direction_2005)
glm_tab
sum(diag(glm_tab))/sum(glm_tab)
mean(glm_pred==Direction_2005)

mean(glm_pred!=Direction_2005)


predict(glm_fit, newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type='response')

