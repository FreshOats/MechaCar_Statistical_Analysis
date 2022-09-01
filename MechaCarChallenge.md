Mecha Car Challenge
================
Justin R. Papreck
2022-09-01

## Exploratory Processing of MechaCars

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
Mecha_df <- read.csv("MechaCar_mpg.csv")
head(Mecha_df, 5)
```

    ##   vehicle_length vehicle_weight spoiler_angle ground_clearance AWD      mpg
    ## 1       14.69710       6407.946      48.78998         14.64098   1 49.04918
    ## 2       12.53421       5182.081      90.00000         14.36668   1 36.76606
    ## 3       20.00000       8337.981      78.63232         12.25371   0 80.00000
    ## 4       13.42849       9419.671      55.93903         12.98936   1 18.94149
    ## 5       15.44998       3772.667      26.12816         15.10396   1 63.82457

The information from the tables establishes that the measurements for
the cars were length, weight, spoiler angle, ground clearance, all wheel
drive and miles per gallon. In order to assess the effects that each of
the physical parameters of the vehicle of the car have on the miles per
gallon, a multiple regression was performed.

``` r
multi <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, Mecha_df)

multi
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
    ##     ground_clearance + AWD, data = Mecha_df)
    ## 
    ## Coefficients:
    ##      (Intercept)    vehicle_length    vehicle_weight     spoiler_angle  
    ##       -1.040e+02         6.267e+00         1.245e-03         6.877e-02  
    ## ground_clearance               AWD  
    ##        3.546e+00        -3.411e+00

``` r
summary(multi)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
    ##     ground_clearance + AWD, data = Mecha_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -19.4701  -4.4994  -0.0692   5.4433  18.5849 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -1.040e+02  1.585e+01  -6.559 5.08e-08 ***
    ## vehicle_length    6.267e+00  6.553e-01   9.563 2.60e-12 ***
    ## vehicle_weight    1.245e-03  6.890e-04   1.807   0.0776 .  
    ## spoiler_angle     6.877e-02  6.653e-02   1.034   0.3069    
    ## ground_clearance  3.546e+00  5.412e-01   6.551 5.21e-08 ***
    ## AWD              -3.411e+00  2.535e+00  -1.346   0.1852    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.774 on 44 degrees of freedom
    ## Multiple R-squared:  0.7149, Adjusted R-squared:  0.6825 
    ## F-statistic: 22.07 on 5 and 44 DF,  p-value: 5.35e-11

## Exploratory Analysis

The standard p-value cutoff is 0.05, so in interpreting which parameters
have a very low probability of affecting the mpg, any value less than
0.05 under Pr(\>\|t\|) are likely not contributing to the mpg. The
vehicle length, at 2.6e-12, and ground clearance, at 5.21e-08, are very
unlikely to provide random amounts of variance to the mpg in this linear
model.

On the other hand, the vehicle weight, 0.08, spoiler angle, 0.31, and
AWD, 0.19, all had show that there is a potentially significant
contribution to providing variance to the mpg.

The p-value of 5.35e-11 suggests that there is an extremely low
probability that the variance is caused by random variation, and that
the parameters investigated indeed have a significant correlation to the
mpg data measured. The R-squared value of 0.71 shows a strong linear
correlation, thus the linear model is not considered to have a zero
slope. Combining the R-squared finding, demonstrating a strong linear
correlation, and the p-value, demonstrating that the variance is very
unlikely to be due to random variation, it can be concluded that there
is a significant linear correlation between mpg and the parameters
investigated.

At this point in the exploratory analysis, it is unlikely the linear
model can effectively predict the mpg of prototype cars. While there is
a strong and significant correlation, there are several features in the
data that make this analysis problematic, thus further analyses will be
necessary to determine whether linear regression is the best model.

The biggest issue is the AWD column. AWD is not a continuous variable -
it is dichotomous, and thus must be treated as categorical data. This
cannot be used in a regression analysis, and any findings including the
AWD parameter cannot be used. To investigate whether AWD has a
significant impact on the mpg, a two-sample t-test would be performed.
Something very present in the data is that the coefficient associated
with AWD is the only negative coefficient with a high order of magnitude
comparatively, thus no further conclusions can be made about these data
until AWD removal.

Further considerations, if the data were all continuous, it is still
unclear whether there is indeed a linear relationship. The parameters
should each be investigated individually, especially those that do seem
to impact the correlation with the mpg. If these data are visualized,
which is not meaningful in a multiple regression, it can be observed
whether the data have a better correlation in a logistic regression.

## Removing Categorical Data in the Analysis

``` r
multi_no_awd <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance, Mecha_df)
multi_no_awd
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
    ##     ground_clearance, data = Mecha_df)
    ## 
    ## Coefficients:
    ##      (Intercept)    vehicle_length    vehicle_weight     spoiler_angle  
    ##       -1.076e+02         6.240e+00         1.276e-03         8.031e-02  
    ## ground_clearance  
    ##        3.659e+00

``` r
summary(multi_no_awd)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
    ##     ground_clearance, data = Mecha_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -21.3395  -4.1155  -0.2094   6.8789  17.2672 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -1.076e+02  1.576e+01  -6.823 1.87e-08 ***
    ## vehicle_length    6.240e+00  6.609e-01   9.441 3.05e-12 ***
    ## vehicle_weight    1.277e-03  6.948e-04   1.837   0.0728 .  
    ## spoiler_angle     8.031e-02  6.656e-02   1.207   0.2339    
    ## ground_clearance  3.659e+00  5.394e-01   6.784 2.13e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.853 on 45 degrees of freedom
    ## Multiple R-squared:  0.7032, Adjusted R-squared:  0.6768 
    ## F-statistic: 26.65 on 4 and 45 DF,  p-value: 2.277e-11

After removing the categorical data, the two parameters that could not
be removed due to random variation remain: spoiler angle and vehicle
weight. The R-squared value dropped slightly from 0.71 to 0.70, still
indicating a strong linear relationship, and the p-value remains far
below 0.05, indicating that this is a significant relationship.

Finally, in testing whether there is a statistical difference in mpg for
cars with or without AWD, a two-sample t-test can be used:

``` r
no_awd <- Mecha_df[Mecha_df$AWD == 0,]$mpg
w_awd <- Mecha_df[Mecha_df$AWD == 1,]$mpg

t.test(no_awd,w_awd)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  no_awd and w_awd
    ## t = 0.99152, df = 45.947, p-value = 0.3266
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.499691 13.235437
    ## sample estimates:
    ## mean of x mean of y 
    ##  47.31253  42.94466

Interestingly, while the AWD seemed to show that there was a correlation
with mpg from the regression analysis, it failed to disprove the null
hypothesis in the Welch Two-Sample t-Test. The p-value of 0.33 is much
greater than 0.05, which would be required to disopve that the true
difference in means is not equal to 0. The confidence interval makes
this very clear, in that the test states that we are 95% confident that
the true mean of the population lies between -4.50 and 13.23, which
clearly includes 0. Therefore, in just looking at the multiple
regression blindly, false conclusions can be made regarding AWD and gas
mileage.

## Exploratory Processing of Suspension Coil Data

``` r
Suspension <- read.csv("Suspension_Coil.csv")
head(Suspension,4)
```

    ##   VehicleID Manufacturing_Lot  PSI
    ## 1    V40858              Lot1 1499
    ## 2    V40607              Lot1 1500
    ## 3    V31443              Lot1 1500
    ## 4     V6004              Lot1 1500

``` r
total_summary <- Suspension %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
total_summary
```

    ##      Mean Median Variance       SD
    ## 1 1498.78   1500 62.29356 7.892627

According to the specification requirement that the variance must be
below 100 PSI, these data appear to support that this specification is
met.\*

``` r
lot_summary <- Suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot_summary
```

    ## # A tibble: 3 × 5
    ##   Manufacturing_Lot  Mean Median Variance     SD
    ##   <chr>             <dbl>  <dbl>    <dbl>  <dbl>
    ## 1 Lot1              1500   1500     0.980  0.990
    ## 2 Lot2              1500.  1500     7.47   2.73 
    ## 3 Lot3              1496.  1498.  170.    13.0

\*Per the design specifications for the MechaCar Suspension coils, the
variance must not exceed 100 pounds per square inch. Unfortunately, this
is an unreliable specification, because the variance cannot be measured
in units of PSI, but rather PSI squared. When considering the different
lots, only Lot 3 had a variance exceeding 100-PSI squared, at 170.29
sq-PSI, whereas Lot 1 was far below, at 0.98 sq-PSI, and Lot 2 as well,
at 7.47 sq-PSI. So if the true specification is to limit the variance to
100 PSI-squared, or a variation in the coils of 10 PSI, then the coils
from Lot 3 would not meet this specification.  
(If, however, the specification is in the actual pressure, PSI, then
there would be no issue with any of the coils, as the SD is the
indicator of variation in the units PSI. The pressure variation in Lot 1
is 0.99 PSI, in Lot 2 is 2.73 PSI, and in Lot 3 13.05 PSI. All three of
the lots were well below 100 PSI.)

## T-Tests on Suspension Coils

To determine whether the suspension coils installed in the lots are
different from the population mean of the suspension coils, a one-sample
t-test was used, with a significance cutoff at 0.05. Initially, all of
the grouped lots were investigated.

``` r
All_Lots_t <- t.test(Suspension$PSI, mu=1500)

Lot1 <- Suspension %>% subset(Manufacturing_Lot == "Lot1")
Lot2 <- Suspension %>% subset(Manufacturing_Lot == "Lot2")
Lot3 <- Suspension %>% subset(Manufacturing_Lot == "Lot3")
 
t1 <- t.test(Lot1$PSI, mu=1500)
t2 <- t.test(Lot2$PSI, mu=1500)
t3 <- t.test(Lot3$PSI, mu=1500)
```

``` r
All_Lots_t
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Suspension$PSI
    ## t = -1.8931, df = 149, p-value = 0.06028
    ## alternative hypothesis: true mean is not equal to 1500
    ## 95 percent confidence interval:
    ##  1497.507 1500.053
    ## sample estimates:
    ## mean of x 
    ##   1498.78

The results of the test yielded a p-value of 0.06, which is higher than
the 0.05 cutoff. This indicates that the null hypothesis cannot be
rejected, and therefore there is no significant difference between the
mean of the sampled three lots to the mean of the population, which is
1500.

However, based on the previous findings, where the one lot produced
coils that exceeded the variance parameters established by the
manufacturer, it is also important to investigate whether the production
at each lot also maintains that there is no difference from the
population mean.

``` r
t1
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Lot1$PSI
    ## t = 0, df = 49, p-value = 1
    ## alternative hypothesis: true mean is not equal to 1500
    ## 95 percent confidence interval:
    ##  1499.719 1500.281
    ## sample estimates:
    ## mean of x 
    ##      1500

The p-value from Lot 1 is 1. In other words, the mean value of the the
sample, Lot 1, is exactly the same as the mu value, the population mean.
Clearly the null hypothesis is not rejected, because there is no
difference between the means.

``` r
t2
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Lot2$PSI
    ## t = 0.51745, df = 49, p-value = 0.6072
    ## alternative hypothesis: true mean is not equal to 1500
    ## 95 percent confidence interval:
    ##  1499.423 1500.977
    ## sample estimates:
    ## mean of x 
    ##    1500.2

The p-value for second lot is 0.61, still far above the cutoff p-value
of 0.05, again failing to reject the null hypothesis, supporting that
there is no difference between the sample, Lot 2, and the population.

``` r
t3
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Lot3$PSI
    ## t = -2.0916, df = 49, p-value = 0.04168
    ## alternative hypothesis: true mean is not equal to 1500
    ## 95 percent confidence interval:
    ##  1492.431 1499.849
    ## sample estimates:
    ## mean of x 
    ##   1496.14

The p-value for Lot 3, which did not meet the variance specifications,
was 0.04. This falls below the 0.05 cutoff, and thus the sample mean,
1496.14, is considered significantly different from the population mean,
1500. The confidence interval suggests that with 95% certainty, the true
mean of the population should be between 1492.3 and 1499.8, whereas we
know that the true mean of the population is actually 1500, not in the
range observed from this sample, Lot 3.

## Study Design: MechaCar vs Competition

When comparing a vehicle to the competition, there are several factors
that consumers are interested in. Some of the top priorities that
consumers have are related to the safety features and aesthetics of the
vehicle, which will vary depending on the price models of the vehicles.
However, for the real automobile connoisseurs, the specifications they
will be interested are more related to the fuel economy, horsepower,
quarter mile time, hauling capacity, and whether the vehicle has a moon
roof in addition to the parameters already recorded. Since consumers
usually have a price-point in mind when purchasing a new vehicle, it is
important include both the style and price of the vehicle to make a fair
comparison.

``` r
df <- Mecha_df %>% mutate(model=NA,hp=NA,qsec=NA,haul_cap=NA,moon_roof=NA, price=NA, safety=NA, mnt_cost=NA)
df <- df[,c(7,12,14,13,6,8,9,1,2,4,10,3,5,11)] # Change the order of the columns, while maintaining values
head(df)
```

    ##   model price mnt_cost safety      mpg hp qsec vehicle_length vehicle_weight
    ## 1    NA    NA       NA     NA 49.04918 NA   NA       14.69710       6407.946
    ## 2    NA    NA       NA     NA 36.76606 NA   NA       12.53421       5182.081
    ## 3    NA    NA       NA     NA 80.00000 NA   NA       20.00000       8337.981
    ## 4    NA    NA       NA     NA 18.94149 NA   NA       13.42849       9419.671
    ## 5    NA    NA       NA     NA 63.82457 NA   NA       15.44998       3772.667
    ## 6    NA    NA       NA     NA 48.54268 NA   NA       14.45357       7286.595
    ##   ground_clearance haul_cap spoiler_angle AWD moon_roof
    ## 1         14.64098       NA      48.78998   1        NA
    ## 2         14.36668       NA      90.00000   1        NA
    ## 3         12.25371       NA      78.63232   0        NA
    ## 4         12.98936       NA      55.93903   1        NA
    ## 5         15.10396       NA      26.12816   1        NA
    ## 6         13.10695       NA      30.58568   0        NA

The data types required would be as follows: model: nominal price:
continuous maintenance_cost: interval (1:3) safety_rating: interval
(1:10) mpg: continuous hp: continuous mpg: continuous qsec: continuous
vehicle_length: continuous vehicle_weight: continuous ground_clearance:
continuous hauling_capacity: continuous spoiler_angle: continuous AWD:
dichotomous moon_roof: dichotomous

Common assumptions that people make when purchasing a vehicle include
how higher price is associated with safety, acceleration, and
maintenance costs. Specifically that more expensive cars are 1. safer,
2. can accelerate faster and thus have a shorter quarter mile time, and
3. often have higher maintenance costs.

1.  The Null hypothesis in this case is that the safety rating is no
    different in cars at or above \$55,000 (the cost of an
    standard-level luxury vehicle) than vehicles under that price point.
    The alternative hypothesis specifies that there is a difference in
    safety rating between cars at or above \$55,000 compared to those
    under that price. To make this comparison, a two-sample t-test could
    be used to see if there is a significant difference between the two
    populations. Alternatively, if the price points were grouped in
    \$20,000 bins, an Chi-Squared test could be used to compare the
    price categories with the frequencies of ratings from the safety
    categories.

2.  To determine whether price is related to the acceleration, consider
    the hypotheses: There is no correlation between the price of
    vehicles and the quarter mile time of vehicles. The alternative
    hypothesis is that there is a correlation between the price of
    vehicles and their quarter mile times. To determine this, a linear
    regression can be used since both variables are continuous.
    Furthermore, a multiple regression analysis could be used to
    determine what other factors could be associated with the quarter
    mile time, being careful only to select the continuous data. This
    would include the price, miles per gallon, horsepower, length,
    weight, ground clearance, hauling capacity and spoiler angle, for
    those vehicles with spoilers. Although miles per gallon, like the
    qsec, is typically the dependent variable as a result of the other
    factors, a correlation could give insight to further relationships
    as to how the two could be linked.

3.  Expensive cars are typically associated with higher maintenance
    costs. The null hypothesis in this scenario states that there as
    vehicle price increases, there is no correlation with the
    maintenance cost of those vehicles. Alternatively, as the price of a
    vehicle increases, there is a significant change in the cost of
    maintenance. It is important to not specify an increase or decrease,
    as the prior information is simply hearsay and should have no
    bearing on the data. If there is a correlation, it is more important
    to note that either yes, or no, there is a difference, and only then
    specify that it increased or decreased. Otherwise, if the
    alternative states that it explicitly an increase, it is possible
    that we would have to reject both hypotheses if a decrease in costs
    was observed. This also avoids having to do two tests, one to state
    that there is no increase in maintenance cost with price, followed
    by another post hoc analysis stating that there is no decrease. To
    test this, since the maintenance costs are categorical but the price
    is continuous, an ANOVA test would provide the distribution means.
    This remains true even if we were to increase the categories from 3
    to 5 in the maintenance cost measurements.

4.  One final set of tests could be whether there is a correlation
    between vehicles with or without AWD and/or a moon roof and
    maintenance costs or safety ratings. Because all of these parameters
    can be considered categorical, the AWD and moon roof are dichotomous
    while the maintenance costs and safety ratings are scaled interval
    data but can also be considered ordinal, the only test equipped to
    compare sets of categorical data is the Chi-squared test.
