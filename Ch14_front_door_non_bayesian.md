Ch14 front door non bayesian
================

Data Generating Process
-----------------------

the **true** effects from X-&gt;Y would be 1

``` r
set.seed(840708)
N <- 1e3
U <- rnorm(N)
X <- U + rnorm(N)
M <- X + rnorm(N)
Y <- U + M + rnorm(N)
dat <- list(X = X, M = M, Y = Y)
```

Naive Regression
----------------

Because of the existence of unmeasured U that both causes X and Y, even if we do `Y ~ X + M`, the coef of X would be different than 0

``` r
lm(Y~X+M) %>% summary
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X + M)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9369 -0.8441 -0.0093  0.8656  3.4088 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.02961    0.03900  -0.759    0.448    
    ## X            0.54342    0.04777  11.377   <2e-16 ***
    ## M            0.96835    0.03865  25.052   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.233 on 997 degrees of freedom
    ## Multiple R-squared:  0.772,  Adjusted R-squared:  0.7715 
    ## F-statistic:  1688 on 2 and 997 DF,  p-value: < 2.2e-16

As predicted, the coef from this linear model is not only different than 0 but also significant

Front door adjustment
---------------------

This means that you need to understand precisely the mechanism by which X (let's now say it's smoking) affects Y (lung cancer). Let's say it all flows through variable M (tar in lungs): Y (smoking) affects M (tar), and M (tar) affects Y; there is no direct effect. Then, to find the effect of X on Y, compute the effect of smoking on tar, and then the effect of tar on cancer and multiply the effect of Y on M with the effect of M on Y.

Here, front-door adjustment works because there is no open back-door path from *X* to *M*. The path *X* ← *U* → *Y* ← *M* is blocked. This is because the arrows "collide" in *Y*. So the *X* → *M* effect is identified.

``` r
m <- lm(M ~ X) 

m %>% summary
```

    ## 
    ## Call:
    ## lm(formula = M ~ X)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9711 -0.6959 -0.0075  0.7572  2.7327 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.02558    0.03193  -0.801    0.423    
    ## X            0.98902    0.02345  42.175   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.009 on 998 degrees of freedom
    ## Multiple R-squared:  0.6406, Adjusted R-squared:  0.6402 
    ## F-statistic:  1779 on 1 and 998 DF,  p-value: < 2.2e-16

``` r
effect1 <- m %>% coef %>% .[[2]]
```

So effect from *X* → *M* would be 0.989024.

Similarly, the *M* → *Y* effect is identified because the only back-door path from *M* to *Y* runs over *X*, so we can adjust for it using the back-door strategy.

``` r
m2 <- lm(Y ~ M + X)
m2 %>% summary
```

    ## 
    ## Call:
    ## lm(formula = Y ~ M + X)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9369 -0.8441 -0.0093  0.8656  3.4088 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.02961    0.03900  -0.759    0.448    
    ## M            0.96835    0.03865  25.052   <2e-16 ***
    ## X            0.54342    0.04777  11.377   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.233 on 997 degrees of freedom
    ## Multiple R-squared:  0.772,  Adjusted R-squared:  0.7715 
    ## F-statistic:  1688 on 2 and 997 DF,  p-value: < 2.2e-16

``` r
effect2 <- coef(m2)[[2]]
```

``` r
total_effect <- effect1 * effect2
```

In sum, you can identify the "submechanisms", and there is no direct effect, so you can piece together the submechanisms to estimate the overall effect. This will not work if *U* infuences *M*, because then identifying the submechanisms does not work.

in this case the total effects works out to be 0.9577174, super close to 1 :-)

Testing Time!
-------------

would this work for effects other than 1? say, if effect of *X* → *Y* is only 0.2?

``` r
set.seed(840708)
N <- 1e3
U <- rnorm(N)
X <- U + rnorm(N)
M <- X + rnorm(N)
Y <- U + 0.2* M + rnorm(N)
dat <- list(X = X, M = M, Y = Y)
```

Naive Regression
----------------

Because of the existence of unmeasured U that both causes X and Y, even if we do `Y ~ X + M`, the coef of X would be different than 0

``` r
lm(Y~X+M) %>% summary
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X + M)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9369 -0.8441 -0.0093  0.8656  3.4088 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.02961    0.03900  -0.759    0.448    
    ## X            0.54342    0.04777  11.377  < 2e-16 ***
    ## M            0.16835    0.03865   4.355 1.47e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.233 on 997 degrees of freedom
    ## Multiple R-squared:  0.3886, Adjusted R-squared:  0.3873 
    ## F-statistic: 316.8 on 2 and 997 DF,  p-value: < 2.2e-16

``` r
m <- lm(M ~ X) 

m %>% summary
```

    ## 
    ## Call:
    ## lm(formula = M ~ X)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9711 -0.6959 -0.0075  0.7572  2.7327 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.02558    0.03193  -0.801    0.423    
    ## X            0.98902    0.02345  42.175   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.009 on 998 degrees of freedom
    ## Multiple R-squared:  0.6406, Adjusted R-squared:  0.6402 
    ## F-statistic:  1779 on 1 and 998 DF,  p-value: < 2.2e-16

``` r
effect1 <- m %>% coef %>% .[[2]]

m2 <- lm(Y ~ M + X)
m2 %>% summary
```

    ## 
    ## Call:
    ## lm(formula = Y ~ M + X)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9369 -0.8441 -0.0093  0.8656  3.4088 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.02961    0.03900  -0.759    0.448    
    ## M            0.16835    0.03865   4.355 1.47e-05 ***
    ## X            0.54342    0.04777  11.377  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.233 on 997 degrees of freedom
    ## Multiple R-squared:  0.3886, Adjusted R-squared:  0.3873 
    ## F-statistic: 316.8 on 2 and 997 DF,  p-value: < 2.2e-16

``` r
effect2 <- coef(m2)[[2]]

total_effect <- effect1 * effect2

total_effect
```

    ## [1] 0.1664982
