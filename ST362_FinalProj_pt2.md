ST362_Final_Project_pt2
================
Joseph Aurini - 200544520
2023-07-22

## Part 2: Predicting an MLB Pitcher’s Salary; Pre-Moneyball

Upload the data set we generated

``` r
pitchers <- read.csv("pitcher_salary_sim.csv")
```

## Two Heavily Correlated Predictor’s

``` r
library(corrgram)
plot(pitchers$height,pitchers$hand_length)
```

![](ST362_FinalProj_pt2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
suppressWarnings(corrgram(pitchers,order=TRUE,upper.panel=panel.cor))
```

![](ST362_FinalProj_pt2_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->
The strong linear relationship in the plot above shows that height and
hand length are heavily positively corrleated. The correlation matrix
confirms this hypothesis, and also indicates that height and hand length
seem to be the only highly correlated predictor variables.

## Which Correlated Variable is more Significant?

Let’s fit two models with all our preidctor variables, take height away
from one model and hand length away from the other, then run an anova
test on both models.

``` r
model_no_height <- lm(salary~.-height,pitchers)
model_no_hand <- lm(salary~.-hand_length,pitchers)
anova(model_no_height,model_no_hand)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: salary ~ (pitching_arsenal + two_pitches + three_pitches + four_pitches + 
    ##     height + hand_length + fball_velo + age + win_loss + op_avg + 
    ##     throws_left + charisma + attractiveness + throws_funny) - 
    ##     height
    ## Model 2: salary ~ (pitching_arsenal + two_pitches + three_pitches + four_pitches + 
    ##     height + hand_length + fball_velo + age + win_loss + op_avg + 
    ##     throws_left + charisma + attractiveness + throws_funny) - 
    ##     hand_length
    ##   Res.Df        RSS Df   Sum of Sq F Pr(>F)
    ## 1   1044 1.2805e+13                        
    ## 2   1044 1.2928e+13  0 -1.2295e+11

Looks like our anova test is running well and residual sum of squares is
smaller in the model without hand length, than in the model without
height. For this reason we’ll use hand length in our model and disregard
height entirely, setting the coefficient for our height variable to zero
(i.e. $\beta_4 = 0$).

## Categorical Variables

Now let’s look at some boxplots to show that pitching arsenal is a
categorical variable, with salary differing signficantly based on how
many pitches a player could throw.

``` r
boxplot(salary~pitching_arsenal,pitchers)
```

![](ST362_FinalProj_pt2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Checking Residual Plots

``` r
# Set our preliminary model to account for all variables except height
model_1 <- model_no_height

#Residuals vs Fitted Values & Q-Q Plot
plot(model_1,1)
```

![](ST362_FinalProj_pt2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot(model_1,2)
```

![](ST362_FinalProj_pt2_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
summary(model_1)
```

    ## 
    ## Call:
    ## lm(formula = salary ~ . - height, data = pitchers)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -521559  -74117  -13206   61092 1026037 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            -201348     178027  -1.131    0.258    
    ## pitching_arsenalthree  -207383       8540 -24.283  < 2e-16 ***
    ## pitching_arsenaltwo    -406206       8274 -49.095  < 2e-16 ***
    ## two_pitches                 NA         NA      NA       NA    
    ## three_pitches               NA         NA      NA       NA    
    ## four_pitches                NA         NA      NA       NA    
    ## hand_length              15016       2209   6.796  1.8e-11 ***
    ## fball_velo               10614       1726   6.150  1.1e-09 ***
    ## age                     -26795       1382 -19.391  < 2e-16 ***
    ## win_loss               2394484      24252  98.734  < 2e-16 ***
    ## op_avg                -1631179     142419 -11.453  < 2e-16 ***
    ## throws_left             244517       8696  28.119  < 2e-16 ***
    ## charisma                 81869       1784  45.887  < 2e-16 ***
    ## attractiveness           26165       1826  14.325  < 2e-16 ***
    ## throws_funny           -438661      24041 -18.247  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 110700 on 1044 degrees of freedom
    ## Multiple R-squared:  0.9391, Adjusted R-squared:  0.9384 
    ## F-statistic:  1463 on 11 and 1044 DF,  p-value: < 2.2e-16

Looking at the residuals vs. fitted plot, since the residuals are
centered around zero, our model’s estimators are almost completely
unbiased and will generally correctly predict a pitcher’s true salary.
This suggests the assumption that the relationship between our predictor
and response variables is linear is reasonable. Also, since the
residuals seem to form a horizontal band around zero, this indicates
stable variance in our model. This means that our predicted values for
pitcher salary do not increase or decrease as our predictor variables
change in magnitude, indicating there is no heteroscedasticity. Also,
since the residuals are centered around zero, our model’s estimators are
almost completely unbiased and will generally correctly predict a
pitcher’s true salary.

Looking at the Q-Q plot, we can see the residuals are not normally
distributed, instead looking quite positively skewed. Let’s address this
issue by looking for non-linear relationships between our predictors and
the response variable. (Now, I know I just claimed a linear relationship
can reasonably fit the data, and it still can, but I have a hunch one of
our predictors has a polynomial form).

## Normalizing Residuals

Specifically, let’s look at the relationship between charisma and
salary.

``` r
library(ggplot2)
#plot(pitchers$charisma,pitchers$salary)
ggplot(data = pitchers, aes(x = charisma, y = salary)) +
        geom_point() + geom_smooth()
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](ST362_FinalProj_pt2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
How shocking! It looks like charisma has a polynomial relationship with
salary, to the third-degree no less!

## Adding Polynomial Terms

``` r
# Let's add a squared and cubed charisma term to our preliminary model (a first-degree charisma term is already included)
poly_model_1 <- lm(salary~.-height+I(charisma^2)+I(charisma^3),pitchers)

plot(poly_model_1,1)
```

![](ST362_FinalProj_pt2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
plot(poly_model_1,2)
```

![](ST362_FinalProj_pt2_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
In this new residual vs fitted plot, we make the same observations as
before, and also note the magnitude of the absolute value of our
residuals is smaller, which is good.

Looking at this Q-Q plot, we see that our residuals now follow a normal
distribution.

## Comparing Models

``` r
# Let's compare our preliminary model to our polynomial one with an anova test
anova(model_1,poly_model_1)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: salary ~ (pitching_arsenal + two_pitches + three_pitches + four_pitches + 
    ##     height + hand_length + fball_velo + age + win_loss + op_avg + 
    ##     throws_left + charisma + attractiveness + throws_funny) - 
    ##     height
    ## Model 2: salary ~ (pitching_arsenal + two_pitches + three_pitches + four_pitches + 
    ##     height + hand_length + fball_velo + age + win_loss + op_avg + 
    ##     throws_left + charisma + attractiveness + throws_funny) - 
    ##     height + I(charisma^2) + I(charisma^3)
    ##   Res.Df        RSS Df  Sum of Sq       F    Pr(>F)    
    ## 1   1044 1.2805e+13                                    
    ## 2   1042 1.0124e+09  2 1.2804e+13 6589016 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

From the anova test above, we first observe the extremely small p-value,
indicating our polynomial model is significantly better than our
preliminary model. We also observe an extra sum of squares attributable
to the polynomial model, meaning that variance in the response variable
can be better explained by the predictors in the polynomial model
compared to the predictors in the preliminary model.

## Outliers & Influential Observations

``` r
# Identify outliers
std_residuals <- rstandard(poly_model_1)
outliers<-std_residuals[abs(std_residuals) > 3]
outliers
```

    ##        52       383       525      1051 
    ##  3.446036 -3.076658 -3.530384  3.004430

``` r
# Identify influential observations 
cooks_dist <- cooks.distance(poly_model_1)
influential_observations<-cooks_dist[cooks_dist > 1]
influential_observations
```

    ## named numeric(0)

Although we have a handful of outliers, since no influential
observations exist, we can conclude that no individual point has a
significant impact on the regression. This means if we were to remove
any singular point from our data, our model would not be significantly
impacted. The lack of influential observations indicates our data
generating process is strong in terms of creating similar models from
different generated datasets.
