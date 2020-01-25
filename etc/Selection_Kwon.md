# Selection Function by Kwon

목적: Forward/Backward Selection function developing

방법론: 변수선택 Measure(AIC, Adjusted R^2, F-statistic, CV_error)

### Data Generation

```R
rm(list=ls())

n = 100
p = 10
x.mat = matrix(runif(n*p),n,p)
b.vec = 1/(1:p)
y.vec = as.vector(x.mat%*%b.vec)+rnorm(n)
```


## Forward

```R

aic.fun = function(y.vec,x.mat,b.vec){ # b.vec includes intercept 
  ret = nrow(x.mat)*log(mean((y.vec-drop(cbind(1,x.mat)%*%b.vec))^2))+2*sum(b.vec!=0) 
  return(ret)
}

forward.fun = function(y.vec,x.mat,mod=c("GIC","AR","p","F"),weight=2,p.max=0.05,iter.max=100){ 
  p = ncol(x.mat); n = nrow(x.mat)
  set = ret = rep(F,p)
  m.vec = rep(T,p)
  lm.fit = lm(y.vec~1)
  if(mod=="GIC") o.val = extractAIC(fit=lm.fit,k=weight)[2]
  if(mod=="AR") o.val = summary(lm.fit)$adj.r.squared 
  #if(mod=="p") o.val = max(summary(lm.fit)$coef[,4])
  if(mod=="F") {
    f.vec = NULL
    for (k in (1:p)){
      g = lm(y.vec ~ x.mat[,k])
      f.val = -summary(g)$fstatistic[1]
      f.vec[k] = f.val
    }
    opt = which.min(f.vec)
    set[opt] = T
    m.vec[opt] = F
    o.val = f.val
  }
  if(mod=="CV"){
    o.val = my.cv.fun(y.vec,x.mat)
  }
  for(iter in 1:p){
    print(which(set==T))
    n.val = rep(NA,sum(m.vec))
    if (mod=="F"){
      if (length(set)==1) break
    }
    for(id in (1:p)[m.vec]){
      sset = set
      sset[id] = T
      lm.fit = lm(y.vec~.,data=data.frame(y.vec,x.mat[,sset]))
      if(mod=="GIC") n.val[id] = extractAIC(lm.fit,k=weight)[2]
      if(mod=="AR") n.val[id] = -summary(lm.fit)$adj.r.squared
      #if(mod=="p") n.val[id] = max(summary(lm.fit)$coef[,4])
      if(mod=="F") n.val[id] = -summary(lm.fit)$fstatistic[1]
      if(mod=="CV") n.val[id] = my.cv.fun(y.vec,x.mat[,sset[id]])
    }
    opt = which.min(n.val)
    if(n.val[opt]>=o.val) break 
    set[opt] = T
    m.vec[opt] = F
    o.val = n.val[opt]
  }
  
  # m.mat = x.mat[,set]
  selection = which(set==T)
  if (mod=="GIC") mod = c(mod,weight)
  return(list(mod=mod,selection=selection))
}

forward.fun(y.vec,x.mat,mod="GIC", weight=2)
forward.fun(y.vec,x.mat,mod="AR")
forward.fun(y.vec,x.mat,mod="F")

```

### Backward

```R

aic.fun = function(y.vec,x.mat,b.vec){ # b.vec includes intercept 
  ret = nrow(x.mat)*log(mean((y.vec-drop(cbind(1,x.mat)%*%b.vec))^2))+2*sum(b.vec!=0) 
  return(ret)
}


x.mat = matrix(rnorm(120),20,6)
y.vec = rnorm(20)


back.fun = function(y.vec,x.mat,mod=c("GIC","AR","p","F"),weight=2,p.max=0.05,iter.max=100){ 
  p = ncol(x.mat); n = nrow(x.mat)
  set = 1:p 
  lm.fit = lm(y.vec~.,data=data.frame(y.vec,x.mat))
  if(mod=="GIC") o.val = extractAIC(fit=lm.fit,k=weight)[2]
  if(mod=="AR") o.val = summary(lm.fit)$adj.r.squared 
  #if(mod=="p") o.val = max(summary(lm.fit)$coef[,4])
  if(mod=="F") o.val = -summary(lm.fit)$fstatistic[1]
  if(mod=="CV"){
    o.val = my.cv.fun(y.vec,x.mat)
  }
  for(iter in 1:p){
    print(set)
    n.val = rep(NA,length(set))
    if (mod=="F"){
      if (length(set)==1) break
    }
    for(id in 1:length(set)){
      lm.fit = lm(y.vec~.,data=data.frame(y.vec,x.mat[,set[-id]]))
      if(mod=="GIC") n.val[id] = extractAIC(lm.fit,k=weight)[2]
      if(mod=="AR") n.val[id] = -summary(lm.fit)$adj.r.squared
      #if(mod=="p") n.val[id] = max(summary(lm.fit)$coef[,4])
      if(mod=="F") n.val[id] = -summary(lm.fit)$fstatistic[1]
      if(mod=="CV") n.val[id] = my.cv.fun(y.vec,x.mat[,set[-id]])
    }
    opt = which.min(n.val)
    if(n.val[opt]>=o.val) break 
    set = set[-opt]  
    o.val = n.val[opt]
  }
  
  # m.mat = x.mat[,set]
  if (mod=="GIC") mod = c(mod,weight)
  return(list(mod=mod,selection=set))
}

back.fun(y.vec,x.mat,mod="GIC", weight=2)
back.fun(y.vec,x.mat,mod="AR")
back.fun(y.vec,x.mat,mod="F")
```




