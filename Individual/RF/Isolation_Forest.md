# Isolation Forest
--- 
목적: Anomaly Detection
방법론: tree ensemble
방식: Unsupervised Learning

``` r
library(solitude)
library(rsample)

data = attrition # 기본 data인 attrition 사용
index = sample(ceiling(nrow(attrition)*0.2))

isf = isolationForest$new() # initialize
isf$fit(attrition[index,]) # fitting the training data
isf$scores # get the fitting score

```
