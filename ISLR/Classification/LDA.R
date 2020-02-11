library(ISLR); library(MASS)
Smarket = ISLR::Smarket
attach(Smarket)


train = (Year<2005)
Smarket_2005 = Smarket[!train,]
dim(Smarket_2005)
Direction_2005 = Direction[!train]

lda_fit = lda(Direction ~ Lag1+Lag2, 
    data=Smarket, subset=train)
lda_fit
summary(lda_fit)

plot(lda_fit)

lda_pred = predict(lda_fit, Smarket_2005)
names(lda_pred)
lda_pred$class


lda_class = lda_pred$class
table(lda_class,Direction_2005)
mean(lda_class == Direction_2005)

sum(lda_pred$posterior[,1]>=0.5)
sum(lda_pred$posterior[,1]<0.5)

lda_pred$posterior[1:20,1]
lda_class[1:20]

sum(lda_pred$posterior[,1]>0.9)



# QDA
qda_fit = qda(Direction ~ Lag1+Lag2, 
              data=Smarket, subset=train)
qda_fit

qda_class = predict(qda_fit, Smarket_2005)$class
table(qda_class, Direction_2005)
mean(qda_class == Direction_2005)

# KNN
library(class)
train_X = cbind(Lag1,Lag2)[train,]
test_X = cbind(Lag1,Lag2)[!train,]
train_Direction = Direction[train]

set.seed(1)
knn_pred = knn(train_X,test_X,train_Direction, k=1)
tab_knn = table(knn_pred, Direction_2005)
tab_knn
sum(diag(tab_knn))/sum(tab_knn)

knn_pred = knn(train_X,test_X,train_Direction, k=3)
tab_knn = table(knn_pred, Direction_2005)
tab_knn
sum(diag(tab_knn))/sum(tab_knn)




