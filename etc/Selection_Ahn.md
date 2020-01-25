# Selection Function by Ahn

목적: Forward/Backward Selection function developing

방법론: 변수선택 Measure(AIC, Adjusted R^2, F-statistic)

## Forward

### First Try 

```R
rm(list=ls())

n = 100
p = 10
x.mat = matrix(runif(n*p),n,p)
b.vec = 1/(1:p)
y.vec = as.vector(x.mat%*%b.vec)+rnorm(n)


########################################################################
# Forward Selection using AIC
########################################################################

aic.fun = function(y.vec,x.mat,b.vec){ # b.vec includes intercept 
  ret = nrow(x.mat)*log(mean((y.vec-drop(cbind(1,x.mat)%*%b.vec))^2))+2*sum(b.vec!=0) 
  return(ret)
}


forward.fun = function(y.vec, x.mat, mod=c("AIC","F","AR")){
  
  sv = 0; ud = 1; rng = 1:ncol(x.mat); eng = 1:ncol(x.mat)
  
  if (mod == "AIC"){
    lk = AIC(lm(y.vec ~ 1))
    while(ud > 0) {
      ud=0; m=0
      for(k in rng){
        axx = x.mat[, c(sv,k)]
        gg = lm(y.vec ~ axx)
        lkk = AIC(gg)
        if(lkk < lk){
          lk=lkk; m=k; ud=ud+1
        }
      }
      sv = c(sv,m)
      rng = eng[-sv]
    }
    if (sv[length(sv)]==0){
      sv = sv[1:length(sv)-1]
    }
  }
  
  if (mod == "AR"){
    g = lm(y.vec ~ 1)
    gs = summary(g)
    lk = gs$adj.r.squared
    
    while(ud > 0) {
      ud=0; m=0
      for(k in rng){
        axx = x.mat[, c(sv,k)]
        gg = lm(y.vec ~ axx)
        lkk = summary(gg)$adj.r.squared
        if(lkk > lk){
          lk=lkk; m=k; ud=ud+1
        }
      }
      sv = c(sv,m)
      rng = eng[-sv]
    }
    if (sv[length(sv)]==0){
      sv = sv[1:length(sv)-1]
    }
  }
  
  if (mod == "F"){
    f.vec = NULL
    for(k in rng){
      axx = x.mat[, k]
      g = lm(y.vec ~ axx)
      f.val = summary(g)$fstatistic[1]
      f.vec[k] = f.val
    }
    m = which.max(f.vec)
    lk = max(f.vec)
    sv = c(sv,m)
    rng = eng[-sv]
    
    while(ud > 0) {
      ud=0; m=0
      for(k in rng){
        axx = x.mat[, c(sv,k)]
        gg = lm(y.vec ~ axx)
        lkk = summary(gg)$fstatistic[1]
        if(lkk > lk){
          lk=lkk; m=k; ud=ud+1
        }
      }
      sv = c(sv,m)
      rng = eng[-sv]
    }
    if (sv[length(sv)]==0){
      sv = sv[1:length(sv)-1]
    }
  }
  
  return(sv)
}


forward.fun(y.vec,x.mat,mod="AR")

```

### Second (교수님 점검 후)

```R
rm(list=ls())

n = 100
p = 10

x.mat = matrix(runif(n*p),n,p)
b.vec = 1/(1:p)
y.vec = as.vector(x.mat%*%b.vec)+rnorm(n)

################################################################################
# by AIC / F-stat / adjR^2(AR)
################################################################################

forward.fun = function(y.vec, x.mat, mod=c("AIC","F","AR")){
  ud = 1; p = ncol(x.mat)
  if (mod == "AIC"){
    set = ret = rep(F,p)
    m.vec = rep(T,p)
    g = lm(y.vec~1)
    # o.aic = aic.fun(y.vec=y.vec,x.mat=x.mat,b.vec=coef(fit))
    o.aic = extractAIC(g)[2]
    while(ud > 0) {
      ud=0; m=0
      for(id in (1:p)[m.vec]){
        sset = set 
        sset[id] = T
        gg = lm(y.vec~.,data=data.frame(y.vec,x.mat[,sset]))
        # n.aic = c(aic.vec,aic.fun(y.vec,x.mat[,sset],coef(fit)))
        n.aic = extractAIC(gg)[2]
        if (n.aic < o.aic){
          o.aic=n.aic; m=id;ud=ud+1
        }
      }
      set[m] = T
      m.vec[m] = F
      ret = cbind(ret,set)
    }
  }
  
  if (mod == "AR"){
    set = ret = rep(F,p)
    m.vec = rep(T,p)
    g = lm(y.vec~1)
    o.ar = summary(g)$adj.r.squared
    while(ud > 0) {
      ud=0; m=0
      for(id in (1:p)[m.vec]){
        sset = set 
        sset[id] = T 
        gg = lm(y.vec~.,data=data.frame(y.vec,x.mat[,sset]))
        n.ar = summary(gg)$adj.r.squared
        if (n.ar > o.ar){
          o.ar=n.ar; m=id; ud=ud+1
        }
      }
      set[m] = T
      m.vec[m] = F
      ret = cbind(ret,set)
    }
  }
  
  if (mod == "F"){
    set = ret = rep(F,p)
    m.vec = rep(T,p)
    f.vec = NULL
    for (k in (1:p)){
      g = lm(y.vec ~ x.mat[,k])
      f.val = summary(g)$fstatistic[1]
      f.vec[k] = f.val
    }
    m = which.max(f.vec)
    set[m] = T
    m.vec[m] = F
    ret = cbind(ret, set)
    o.f = summary(g)$fstatistic[1]
    while(ud > 0) {
      ud=0; m=0
      for(id in (1:p)[m.vec]){
        sset = set 
        sset[id] = T
        gg = lm(y.vec~.,data=data.frame(y.vec,x.mat[,sset]))
        n.f = summary(gg)$fstatistic[1]
        if (is.null(n.f)) break
        if (n.f > o.f){
          o.f=n.f; m=id; ud=ud+1
        }
      }
      set[m] = T
      m.vec[m] = F
      ret = cbind(ret,set)
    }
  }
  
  if (ret[dim(ret)[2]]==ret[dim(ret)[2]-1]){
    ret = ret[,1:dim(ret)[2]-1]
  }
  return(ret)
}


forward.fun(y.vec, x.mat, mod="AIC")
forward.fun(y.vec, x.mat, mod="AR")
forward.fun(y.vec, x.mat, mod="F")

```

## Backward
```R
rm(list=ls())

n = 100
p = 10
x.mat = matrix(runif(n*p),n,p)
b.vec = 1/(1:p)
y.vec = as.vector(x.mat%*%b.vec)+rnorm(n)

################################################################################
# by AIC / F-stat / adjR^2(AR)
################################################################################

aic.fun = function(y.vec,x.mat,b.vec){ # b.vec includes intercept 
  ret = nrow(x.mat)*log(mean((y.vec-drop(cbind(1,x.mat)%*%b.vec))^2))+2*sum(b.vec!=0) 
  return(ret)
}

back.fun = function(y.vec,x.mat,mod=c("AIC","F","AR")){ 
  ud = 1; p = ncol(x.mat)
  if (mod == "AIC"){
    set = ret = rep(T,p)
    g = lm(y.vec~.,data=data.frame(y.vec,x.mat))
    # o.aic = aic.fun(y.vec=y.vec,x.mat=x.mat,b.vec=coef(fit))
    o.aic = extractAIC(g)[2]
    while(ud > 0) {
      ud=0; m=0
      for(id in (1:p)[set]){
        sset = set 
        sset[id] = F 
        gg = lm(y.vec~.,data=data.frame(y.vec,x.mat[,sset]))
        # n.aic = c(aic.vec,aic.fun(y.vec,x.mat[,sset],coef(fit)))
        n.aic = extractAIC(gg)[2]
        if (n.aic < o.aic){
          o.aic=n.aic; m=id;ud=ud+1
        }
      }
      set[m] = F
      ret = cbind(ret,set)
    }
  }
  
  if (mod == "AR"){
    set = ret = rep(T,p)
    g = lm(y.vec~.,data=data.frame(y.vec,x.mat))
    o.ar = summary(g)$adj.r.squared
    while(ud > 0) {
      ud=0; m=0
      for(id in (1:p)[set]){
        sset = set 
        sset[id] = F 
        gg = lm(y.vec~.,data=data.frame(y.vec,x.mat[,sset]))
        n.ar = summary(gg)$adj.r.squared
        if (n.ar > o.ar){
          o.ar=n.ar; m=id; ud=ud+1
        }
      }
      set[m] = F
      ret = cbind(ret,set)
    }
  }
  
  if (mod == "F"){
    set = ret = rep(T,p)
    g = lm(y.vec~.,data=data.frame(y.vec,x.mat))
    o.f = summary(g)$fstatistic[1]
    while(ud > 0) {
      ud=0; m=0
      for(id in (1:p)[set]){
        sset = set 
        sset[id] = F 
        gg = lm(y.vec~.,data=data.frame(y.vec,x.mat[,sset]))
        n.f = summary(gg)$fstatistic[1]
        if (is.null(n.f)) break
        if (n.f > o.f){
          o.f=n.f; m=id; ud=ud+1
        }
      }
      set[m] = F
      ret = cbind(ret,set)
    }
  }
  
  if (ret[dim(ret)[2]]==ret[dim(ret)[2]-1]){
    ret = ret[,1:dim(ret)[2]-1]
  }
   return(ret)
}



back.fun(y.vec,x.mat,mod="AIC")
back.fun(y.vec,x.mat,mod="AR")
back.fun(y.vec,x.mat,mod="F")
```






