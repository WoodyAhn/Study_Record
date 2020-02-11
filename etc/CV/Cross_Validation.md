Cross Validation
================
Jay
2020 2 11

### Import Libraries

``` r
rm(list=ls())
library(caret); library(rpart); library(dplyr); library(rpart.plot)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

### Data Generation

``` r
n = 1000; p = 100
set.seed(2019)
x.mat = matrix(rnorm(n*p),n,p)
b.vec = 1/(1:p)
y.vec = exp(x.mat%*%b.vec)/(1+exp(x.mat%*%b.vec))
y.vec = (y.vec>0.5)+0
data = data.frame(y.vec,x.mat)
data$y.vec = as.factor(data$y.vec)
```

### Train-Test Split

``` r
set.seed(2019)
id=createDataPartition(data$y.vec,p=0.7,list=FALSE)
train = data[id,]
test = data[-id,]
```

### Prediction Without Validation

``` r
tree = rpart(y.vec~.,data=train)
pred = predict(tree,test,type="class")
tab = table(test$y.vec,pred)
acc = sum(diag(tab))/sum(tab)
acc
```

    ## [1] 0.76

Validation
==========

### Train(pt)-Validation Split

``` r
set.seed(2019)
pt.id = createDataPartition(train$y.vec,p=0.7,list=FALSE)
pt.train = train[pt.id,]
pt.val = train[-pt.id,]
```

### Make dataframes for Grid search and saving results

``` r
tree.grid = expand.grid(cp=c(0.01,0.1,1), maxdepth=c(4,6,10))
perf.tree = data.frame(acc = rep(0,nrow(tree.grid)))
```

### Grid Search Algoritm runs

``` r
for (i in 1:nrow(perf.tree)){
  pt.tree = rpart(y.vec~.,data=pt.train,
                  control=rpart.control(cp=tree.grid[i,"cp"],
                  maxdepth=tree.grid[i,"maxdepth"]))
  pt.pred = predict(pt.tree,pt.val,type="class")
  pt.tab = table(pt.val$y.vec,pt.pred)
  acc = sum(diag(pt.tab))/sum(pt.tab)
  perf.tree[i,] = acc
}
```

### Get optimal values for tuning parameter based on Validation acc

``` r
final.perf.tree = cbind(tree.grid,perf.tree)
final.perf.tree
```

    ##     cp maxdepth       acc
    ## 1 0.01        4 0.8516746
    ## 2 0.10        4 0.8373206
    ## 3 1.00        4 0.5406699
    ## 4 0.01        6 0.8181818
    ## 5 0.10        6 0.8373206
    ## 6 1.00        6 0.5406699
    ## 7 0.01       10 0.8181818
    ## 8 0.10       10 0.8373206
    ## 9 1.00       10 0.5406699

``` r
q = which.max(final.perf.tree[,"acc"])
opt = final.perf.tree[q,]
opt
```

    ##     cp maxdepth       acc
    ## 1 0.01        4 0.8516746

### Prediction(Test-set) with optimal tuning parameter obtained by Validation

``` r
tree = rpart(y.vec ~ ., data=train, control = rpart.control(cp=opt$cp,maxdepth=opt$maxdepth))
pred = predict(tree,test,type="class")
tab = table(test$y.vec,pred)
acc = sum(diag(tab)) / sum(tab)
acc
```

    ## [1] 0.77

Cross Validation
================

### Create Folds(5)

``` r
n.fold = 5
set.seed(2019)
fold.id = createFolds(train$y.vec,k=n.fold,returnTrain=FALSE)
```

### Make dataframes for Grid search & saving results

``` r
tree.grid = expand.grid(cp=c(0.01,0.1,1), maxdepth=c(4,6,10))
perf.cv.tree = data.frame(acc = rep(0,nrow(tree.grid)))
```

### CV & Grid search Algorithm runs

``` r
for (j in 1:nrow(perf.cv.tree)){
  cv.err.vec = NULL
  for (i in 1:n.fold){
    tree = rpart(y.vec ~ ., data=train[-fold.id[[i]],], 
                 control = rpart.control(cp=tree.grid[j,"cp"],maxdepth = tree.grid[j,"maxdepth"]))
    pred = predict(tree, train[fold.id[[i]],],type="class")
    tab = table(train$y.vec[fold.id[[i]]], pred)
    acc = sum(diag(tab)) / sum(tab)
    cv.err.vec = c(cv.err.vec, acc)
  }
  perf.cv.tree[j,1] = mean(cv.err.vec)
}
```

### Get optimal values for tuning parameter based on CV acc

``` r
final.perf.cv.tree = cbind(tree.grid, perf.cv.tree)
final.perf.cv.tree
```

    ##     cp maxdepth       acc
    ## 1 0.01        4 0.8371191
    ## 2 0.10        4 0.8071081
    ## 3 1.00        4 0.5400008
    ## 4 0.01        6 0.8371191
    ## 5 0.10        6 0.8071081
    ## 6 1.00        6 0.5400008
    ## 7 0.01       10 0.8371191
    ## 8 0.10       10 0.8071081
    ## 9 1.00       10 0.5400008

``` r
q = which.max(final.perf.cv.tree[,"acc"])
opt = final.perf.tree[q,]
opt
```

    ##     cp maxdepth       acc
    ## 1 0.01        4 0.8516746

### Prediction(Test-set) with optimal tuning parameter obtained by CV

``` r
tree = rpart(y.vec ~ ., data=train, control = rpart.control(cp=opt$cp,maxdepth=opt$maxdepth))
pred = predict(tree,test,type="class")
tab = table(test$y.vec,pred)
acc = sum(diag(tab)) / sum(tab)
```
