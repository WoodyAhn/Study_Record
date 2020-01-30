
<!-- README.md is generated from README.Rmd. Please edit that file -->
### Prerequisites

``` r
library(CVTuningCov); library(orcutt);library(astsa); library(car)
## Loading required package: lmtest
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
## Loading required package: carData
```

### Generate auto-correlated Data

``` r
y.vec = c(349.7,353.5,359.2,366.4,376.5,385.7,391.3,398.9,404.2,414.0,423.4,430.5,
          440.4,451.8,457.0,460.9,462.9,443.4,445.0,449.0)
x.vec = c(133.6,135.4,137.6,140.0,143.8,147.1,148.8,151.4,153.3,156.5,160.8,163.6,
          166.9,171.4,174.0,175.4,180.5,184.9,187.1,188.7)
```

### Fit OLS

``` r
fit = lm(y.vec ~ x.vec)
```

``` r
summary(fit)
## 
## Call:
## lm(formula = y.vec ~ x.vec)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.959  -8.874   2.035   9.035  16.623 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   89.234     26.721   3.339  0.00365 ** 
## x.vec          2.024      0.166  12.196 3.89e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.98 on 18 degrees of freedom
## Multiple R-squared:  0.892,  Adjusted R-squared:  0.886 
## F-statistic: 148.7 on 1 and 18 DF,  p-value: 3.888e-10
```

### Plotting Residuals

``` r
<<<<<<< HEAD
fit_residual = fit$residuals
plot(fit_residual)
```

<img src="README_figs/README-unnamed-chunk-6-1.png" width="672" />

### Durbin Watson Test

``` r
DWT = durbinWatsonTest(fit)

DWT
##  lag Autocorrelation D-W Statistic p-value
##    1       0.7461337      0.312374       0
##  Alternative hypothesis: rho != 0
```

``` r
P_DWT = durbinWatsonTest(fit, alternative = "positive")

P_DWT
##  lag Autocorrelation D-W Statistic p-value
##    1       0.7461337      0.312374       0
##  Alternative hypothesis: rho > 0
```

### Estimate

$\\hat{\\rho}$

``` r
r_ii = fit_residual[1:19]
r_i = fit_residual[2:20]

fit2 = lm(r_i ~ r_ii-1)
rho_hat = coef(fit2)
rho_hat
##     r_ii 
## 0.891002
```

### Get

$\\hat{V}$

``` r
V_hat = AR1(p=20, rho=rho_hat)
V_hat[1:6,1:6]
##           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]
## [1,] 1.0000000 0.8910020 0.7938846 0.7073528 0.6302528 0.5615565
## [2,] 0.8910020 1.0000000 0.8910020 0.7938846 0.7073528 0.6302528
## [3,] 0.7938846 0.8910020 1.0000000 0.8910020 0.7938846 0.7073528
## [4,] 0.7073528 0.7938846 0.8910020 1.0000000 0.8910020 0.7938846
## [5,] 0.6302528 0.7073528 0.7938846 0.8910020 1.0000000 0.8910020
## [6,] 0.5615565 0.6302528 0.7073528 0.7938846 0.8910020 1.0000000
V_hat = 1/(1-rho_hat^2)*V_hat
```

### Estimate

$\\hat{\\beta\_{GLS}}$

``` r

x.mat = cbind(1,x.vec)
V_inverse = solve(V_hat)
# V_inverse = 1/(1-rho_hat^2)*solve(V_hat)

coef_AR = solve(t(x.mat)%*%V_inverse%*%x.mat)%*%t(x.mat)%*%V_inverse%*%y.vec

coef_AR
##             [,1]
##       131.745681
## x.vec   1.714326
```

### Cross Checking

*T*<sup>−1</sup>*y* = *T*<sup>−1</sup>*X**β*

``` r
Lamb = diag(eigen(V_hat)$values)
C = eigen(V_hat)$vectors

all(round(head(C%*%Lamb%*%t(C)),5) == round(head(V_hat),5))
## [1] TRUE


T.mat = C%*%Lamb^(1/2) 
T_inverse = solve(T.mat)

T_y.vec = T_inverse%*%y.vec
T_x.mat = T_inverse%*%x.mat

fit_AR = lm(T_y.vec ~ T_x.mat-1)
summary(fit_AR)
## 
## Call:
## lm(formula = T_y.vec ~ T_x.mat - 1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.985  -5.122  -1.635   6.707  11.633 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## T_x.mat      131.7457    58.3347   2.258 0.036569 *  
## T_x.matx.vec   1.7143     0.3573   4.798 0.000144 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.983 on 18 degrees of freedom
## Multiple R-squared:  0.9881, Adjusted R-squared:  0.9867 
## F-statistic: 744.9 on 2 and 18 DF,  p-value: < 2.2e-16
```

``` r
plot(fit_AR$residuals)
=======
fit_residual = fit<img src="/School/Semina/Linear_Regression/GLS/tex/624c2299c1fb6d9a3a3ea384d748ba14.svg?invert_in_darkmode&sanitize=true" align=middle width=708.09078945pt height=355.0684929pt/>\\hat{\\rho}<img src="/School/Semina/Linear_Regression/GLS/tex/6566a166b2bca88cb2e064c7efc5a95c.svg?invert_in_darkmode&sanitize=true" align=middle width=421.81469054999997pt height=118.35616319999997pt/>\\hat{V}<img src="/School/Semina/Linear_Regression/GLS/tex/158056989016a3033e446a47319e5e4c.svg?invert_in_darkmode&sanitize=true" align=middle width=3291.07125435pt height=118.35616319999997pt/>\\hat{\\beta\_{GLS}}<img src="/School/Semina/Linear_Regression/GLS/tex/dd75d71d8c190113747ad64c10391ff8.svg?invert_in_darkmode&sanitize=true" align=middle width=633.8576705999999pt height=284.3835621pt/>values)
C = eigen(V_hat)<img src="/School/Semina/Linear_Regression/GLS/tex/ce8a2aaf4f52ff910fed796bce99ef95.svg?invert_in_darkmode&sanitize=true" align=middle width=853.9337993999999pt height=402.73972860000003pt/>residuals)
>>>>>>> 9d9bd281d824ad5dc559e857af25f65b2f2d1f87
```

<img src="README_figs/README-unnamed-chunk-13-1.png" width="672" />
