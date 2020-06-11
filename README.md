che# blblm

<!-- badges: start -->
<!-- badges: end -->

## Examples

``` r
library(blblm)
fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
coef(fit)
#> (Intercept)          wt          hp       wt:hp 
#> 48.88428523 -7.88702986 -0.11576659  0.02600976
confint(fit, c("wt", "hp"))
#>           2.5%       97.5%
#> wt -10.7902240 -5.61586271
#> hp  -0.1960903 -0.07049867
sigma(fit)
#> [1] 1.838911
sigma(fit, confidence = TRUE)
#>    sigma      lwr      upr 
#> 1.838911 1.350269 2.276347
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#>        1        2 
#> 21.55538 18.80785
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#>        fit      lwr      upr
#> 1 21.55538 20.02457 22.48764
#> 2 18.80785 17.50654 19.71772
```

``` r
fit2 <- blbglm(Species ~ Sepal.Length * Sepal.Width, data = iris, m = 3, B = 100, parallel = TRUE)
coef(fit)
#>  (Intercept)             Sepal.Length              Sepal.Width Sepal.Length:Sepal.Width 
#>  98.33274                 40.65145               -175.44904                 13.62439 
confint(fit, c("wt", "hp"))
#>                   2.5%      97.5%
#> Sepal.Length -145.8562 148.693574
#> Sepal.Width  -461.8428   6.671181
```
