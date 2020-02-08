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

lda_pred = predict(lda_fit, Smarket_2005)
names(lda_pred)

lda_class = lda_pred$class

table(lda_class,Direction_2005)

sum(lda_pred$posterior[,1]>=0.5)
sum(lda_pred$posterior[,1]<0.5)

lda_pred$posterior[1:20,1]
lda_class[1:20]

sum(lda_pred$posterior[,1]>0.9)


