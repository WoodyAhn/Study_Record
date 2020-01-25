# Assginment
----
과제: sample size __n__ 과 standard deviation __s__ 의 변화에 따른 __변수선택__의 정확도 모델간 비교

모델: LSE/OLSE/LASSO/RIDGE/SCAD1/SCAD2/Forward/Backward

측도: Sensitivity, Specificity, MSE, Accuracy

---

### Load Library
```R
library(glmnet); library(ncvreg); library(MASS); library(purrr); library(CVTuningCov)
```

### Estimation Function
Argument
- y.vec: Response Variable
- x.mat: Design Matrix
- g.vec: Gamma vector for SCAD1
- m.mod: the number of Models

Return
- b.mat: 각 모델별 추정된 beta 값들을 컬럼에 저장한 매트릭스

Purpose
- Input data를 받아 각 모델별로 fitting 시킴
- 추정된 beta 값들을 매트릭스 형태로 저장

```R
e.fun = function(y.vec,x.mat,g.vec,m.mod=5){
  b.mat = matrix(0,ncol(x.mat),m.mod)
  # LSE
  b.mat[,1] = coef(lm(y.vec ~ x.mat))[-1]
  # OLSE
  b.mat[tb.vec!=0,2] = coef(lm(y.vec ~ x.mat[,tb.vec!=0]))[-1]
  # LASSO
  b.mat[,3] = coef(cv.glmnet(x.mat,y.vec))[-1]
  # RIDGE
  b.mat[,4] = coef(cv.glmnet(x.mat,y.vec,alpha=0))[-1]
  # SCAD1
  ret = NULL
  for (g in g.vec){
    fit = cv.ncvreg(x.mat,y.vec,gamma=g)
    opt = which.min(fit$cve)
    ret = cbind(ret,c(fit$lambda[opt], fit$cve[opt]))
  }
  opt = which.min(ret[2,])
  opt.parm = c(ret[1,opt], g.vec[opt])
  b.mat[,5] = coef(ncvreg(x.mat,y.vec,lambda=opt.parm[1],gamma=opt.parm[2]))[-1]
  
  # SCAD2
  b.mat[,6] = coef(cv.ncvreg(x.mat,y.vec))[-1]
  
  # Forward
  xy.df = data.frame(y.vec,x.mat)
  n.mod = lm(y.vec ~ 1, data=xy.df)
  f.mod = lm(y.vec ~ ., data=xy.df)
  fit = stepAIC(n.mod, direction="forward",scope=list(upper=f.mod,lower=n.mod),trace=FALSE)
  id = as.numeric(substr(names(coef(fit))[-1], start=2,stop=2))
  b.mat[id,7] = coef(fit)[-1]
  
  # Backward
  fit = stepAIC(f.mod, direction="backward",trace=FALSE)
  id = as.numeric(substr(names(coef(fit))[-1], start=2,stop=2))
  b.mat[id,8] = coef(fit)[-1]
  
  return(b.mat)
  }
```

### Measurement Function
Argument
- b.mat: 모델별로 추정된 beta 들어있는 Matrix
- tb.vec: true beta vector

Return
- Matrix 형태(Sensitivity, Specificity, Accuracy, MSE)

Purpose
- 모델별 Sen/Spe/Acc/MSE 계산 및 출력

```R
m.fun = function(b.mat,tb.vec){
  sen = colMeans(b.mat[tb.vec!=0,]!=0)
  spe = colMeans(b.mat[tb.vec==0,]==0)
  acc = (sen + spe)==2
  # (b.mat[tb.vec!=0,]!=0)*(b.mat[tb.vec==0,]==0)
  mse = colSums((b.mat - tb.vec)^2)
  return(cbind(sen,spe,acc,mse))
}
```

### Data Generate

```R
set.seed(1)
n.vec = c(50,100,200,400,800,1600)
s.vec = c(1,3)
tb.vec = c(3,1.5,0,0,2,0,0,0)
g.vec = 2*(1:10)+0.1
m.mod = 8
p = length(tb.vec)
rst = NULL
```

### Computation

```R
iteration = 0
for (n in n.vec){
  for (s in s.vec){
    iteration = iteration+1
    print(iteration)
    b.list = list()
    for (iter in 1:100){
      x.cov = AR1(p,rho=0.5)
      x.mat = mvrnorm(n,mu=rep(0,p),Sigma=x.cov)
      # x.mat = matrix(runif(n*p),n,p)
      y.vec = x.mat%*%tb.vec+s*rnorm(n)
      b.mat = e.fun(y.vec,x.mat,g.vec=g.vec,m.mod=m.mod)
      b.list[[iter]] = m.fun(b.mat,tb.vec)
    }
    rst = rbind(rst,cbind(s,n,t(Reduce("+",b.list)/100)))
  }
}

colnames(rst) = c("s","n","LSE","OLSE","LASSO","RIDGE","SCAD1","SCAD2","forward","backward")

rst
```










