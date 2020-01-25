# Selection Function by Ahn

목적: Forward/Backward Selection function developing

방법론: 변수선택 Measure(AIC, Adjusted R^2, F-statistic)

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



