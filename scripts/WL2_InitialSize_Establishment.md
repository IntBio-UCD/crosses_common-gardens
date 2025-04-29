---
title: "WL2_InitialSize_Establishment"
author: "Brandie QC"
date: "2025-04-29"
output: 
  html_document: 
    keep_md: true
---

# Checking the effects of initial size on establishment probability 
Establishment = survival first 3 weeks post transplant 



## Libraries

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(lme4)
```

```
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

## Load Initial Size

``` r
initial_size <- read_csv("../input/WL2_2024_Data/CorrectedCSVs/WL2_Initial_DNA_size_survey_20240607_corrected.csv") %>% 
  mutate(unique.ID=as.character(unique.ID)) %>% 
  select(unique.ID:long.leaf.cm)
```

```
## Rows: 1515 Columns: 9
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (4): cryo.box, cryo.pos, DNA.y.n, notes
## dbl (5): rack, unique.ID, height.cm, hypocotyl. length.cm, long.leaf.cm
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(initial_size)
```

```
## # A tibble: 6 × 4
##   unique.ID height.cm `hypocotyl. length.cm` long.leaf.cm
##   <chr>         <dbl>                  <dbl>        <dbl>
## 1 1               1.4                    0.8          0.6
## 2 2               0.8                    0.4          0.5
## 3 3               1.2                    0.5          0.6
## 4 4               1.3                    0.6          1  
## 5 5               1.7                    0.9          1.5
## 6 6               1.7                    1.2          1.2
```

## Load Survival

``` r
wl2_surv <- read_csv("../input/WL2_2024_Data/CorrectedCSVs/WL2_mort_pheno_20241023_corrected.csv") %>% 
  select(block:unique.ID, death.date, missing.date, survey.notes)
```

```
## Rows: 1217 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (12): block, bed, col, unique.ID, bud.date, flower.date, fruit.date, las...
## dbl  (1): row
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(wl2_surv)
```

```
## # A tibble: 6 × 8
##   block bed     row col   unique.ID death.date missing.date survey.notes        
##   <chr> <chr> <dbl> <chr> <chr>     <chr>      <chr>        <chr>               
## 1 <NA>  A         1 A     TM2_6_11  <NA>       <NA>         TM2 fruiting from 2…
## 2 <NA>  A         6 B     CC_3_3    6/18/24    <NA>         <NA>                
## 3 <NA>  A        16 B     BH_3_3    6/18/24    <NA>         6/18/24 No tag      
## 4 <NA>  A        17 A     BH_7_3    10/2/24    <NA>         <NA>                
## 5 <NA>  A        18 A     BH_4_3    8/20/24    <NA>         6/18/24 No tag /mis…
## 6 <NA>  A        24 A     WL2_7_9   10/2/24    <NA>         <NA>
```

## Load pop info

``` r
pop_info <- read_csv("../input/WL2_2024_Data/Final_2023_2024_Pop_Loc_Info.csv") %>% 
  select(Pop.Type:bed, row=bedrow, col=bedcol, pop:unique.ID)
```

```
## Rows: 1217 Columns: 15
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (8): Pop.Type, status, block, loc, bed, bedcol, pop, unique.ID
## dbl (7): bed.block.order, bed.order, AB.CD.order, column.order, bedrow, mf, rep
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
unique(pop_info$Pop.Type)
```

```
## [1] "2023-TM2-fruit" "2023-survivor"  NA               "F2"            
## [5] "Parent"         "F1"
```

``` r
unique(pop_info$status)
```

```
## [1] "2023-TM2-fruit" "2023-survivor"  "buffer"         "available"     
## [5] NA
```

## Merge

``` r
size_surv <- left_join(wl2_surv, initial_size)
```

```
## Joining with `by = join_by(unique.ID)`
```

``` r
size_surv_pops <- left_join(pop_info, size_surv)
```

```
## Joining with `by = join_by(block, bed, row, col, unique.ID)`
```

``` r
size_surv_2024pops <- size_surv_pops %>% filter(Pop.Type != "2023-TM2-fruit", Pop.Type != "2023-survivor")
unique(size_surv_2024pops$death.date)
```

```
##  [1] "7/2/24"   "7/23/24"  NA         "7/9/24"   "7/16/24"  "6/25/24" 
##  [7] "6/18/24"  "9/3/24"   "8/13/24"  "7/30/24"  "8/6/24"   "10/9/24" 
## [13] "8/20/24"  "9/24/24"  "10/16/24" "9/17/24"  "8/27/24"  "10/2/24" 
## [19] "9/10/24"  "10/23/24"
```

``` r
unique(size_surv_2024pops$missing.date)
```

```
## [1] NA         "6/25/24"  "10/16/24" "7/9/24"   "10/23/24"
```

## Establishment

``` r
establishment <- size_surv_2024pops %>% 
  mutate(Est_Surv=if_else(is.na(death.date), 1, 
                          if_else(death.date=="6/18/24" | death.date=="6/25/24" | death.date=="7/2/24", 0,
                                       1))) %>% 
  mutate(Est_Surv = if_else(is.na(missing.date), Est_Surv, 
                            if_else(missing.date=="6/25/24", 0, Est_Surv)))

head(establishment)
```

```
## # A tibble: 6 × 18
##   Pop.Type status    block loc   bed     row col   pop        mf   rep unique.ID
##   <chr>    <chr>     <chr> <chr> <chr> <dbl> <chr> <chr>   <dbl> <dbl> <chr>    
## 1 F2       available A     C_4_A C         4 A     (WV x …    NA    13 280      
## 2 F2       available A     C_4_B C         4 B     (LV1 x…    NA     7 949      
## 3 Parent   available A     C_5_A C         5 A     TM2        NA    70 540      
## 4 F2       available A     C_5_B C         5 B     (TM2 x…    NA    13 317      
## 5 F2       available A     C_6_B C         6 B     (WL2 x…    NA    14 1175     
## 6 F2       available A     C_7_A C         7 A     (TM2 x…    NA    10 1091     
## # ℹ 7 more variables: death.date <chr>, missing.date <chr>, survey.notes <chr>,
## #   height.cm <dbl>, `hypocotyl. length.cm` <dbl>, long.leaf.cm <dbl>,
## #   Est_Surv <dbl>
```

``` r
establishment %>% filter(missing.date=="6/25/24") #double check it gave 0s to ones that went missing 
```

```
## # A tibble: 5 × 18
##   Pop.Type status    block loc    bed     row col   pop       mf   rep unique.ID
##   <chr>    <chr>     <chr> <chr>  <chr> <dbl> <chr> <chr>  <dbl> <dbl> <chr>    
## 1 F2       available B     C_31_D C        31 D     (SQ3 …    NA     5 853      
## 2 Parent   available C     C_46_D C        46 D     DPR       NA     1 429      
## 3 F1       available F     D_42_A D        42 A     LV1 x…    NA     8 1232     
## 4 Parent   available G     D_56_A D        56 A     TM2       NA    88 558      
## 5 F2       available I     E_45_A E        45 A     (LV1 …    NA     8 1025     
## # ℹ 7 more variables: death.date <chr>, missing.date <chr>, survey.notes <chr>,
## #   height.cm <dbl>, `hypocotyl. length.cm` <dbl>, long.leaf.cm <dbl>,
## #   Est_Surv <dbl>
```

## Quick Plots 

``` r
establishment %>% 
  ggplot(aes(`hypocotyl. length.cm`, Est_Surv)) +
  geom_point()
```

```
## Warning: Removed 7 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](WL2_InitialSize_Establishment_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

``` r
establishment %>% 
  ggplot(aes(height.cm, Est_Surv)) +
  geom_point()
```

```
## Warning: Removed 6 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](WL2_InitialSize_Establishment_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

``` r
establishment %>% 
  ggplot(aes(long.leaf.cm, Est_Surv)) +
  geom_point()
```

```
## Warning: Removed 9 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](WL2_InitialSize_Establishment_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

## PopType Averages

``` r
est_summary <- establishment %>% 
  group_by(Pop.Type) %>% 
  summarise(meanEst=mean(Est_Surv, na.rm=TRUE), 
            meanHyp = mean(`hypocotyl. length.cm`, na.rm=TRUE),
            meanHeight = mean(height.cm, na.rm=TRUE), 
            meanLength = mean(long.leaf.cm, na.rm=TRUE))
est_summary
```

```
## # A tibble: 3 × 5
##   Pop.Type meanEst meanHyp meanHeight meanLength
##   <chr>      <dbl>   <dbl>      <dbl>      <dbl>
## 1 F1         0.532    1.79       3.12       1.67
## 2 F2         0.5      1.72       2.95       1.79
## 3 Parent     0.392    1.92       3.31       1.43
```

``` r
#F1s had the highest establishment success, parents the lowest 
#parents had largest hypocotyl lengths and F2s had the lowest
#parents were tallest, F2s the shortest
#F2s had longest leaves, parents had the shortest 
```

## Quick Models

``` r
hyp_for_models <- establishment %>% filter(!is.na(`hypocotyl. length.cm`))

hyp_est_model1 <- glm(Est_Surv ~ `hypocotyl. length.cm`, family=binomial, data=hyp_for_models)
summary(hyp_est_model1)
```

```
## 
## Call:
## glm(formula = Est_Surv ~ `hypocotyl. length.cm`, family = binomial, 
##     data = hyp_for_models)
## 
## Coefficients:
##                         Estimate Std. Error z value Pr(>|z|)
## (Intercept)            -0.150995   0.144428  -1.045    0.296
## `hypocotyl. length.cm`  0.004108   0.070433   0.058    0.953
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1136.7  on 822  degrees of freedom
## Residual deviance: 1136.7  on 821  degrees of freedom
## AIC: 1140.7
## 
## Number of Fisher Scoring iterations: 3
```

``` r
hyp_est_model2 <- glmer(Est_Surv ~ `hypocotyl. length.cm` + (1|Pop.Type), family=binomial, data=hyp_for_models)
summary(hyp_est_model2) #no significant relationship with hypocotyl length 
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Est_Surv ~ `hypocotyl. length.cm` + (1 | Pop.Type)
##    Data: hyp_for_models
## 
##      AIC      BIC   logLik deviance df.resid 
##   1138.5   1152.6   -566.3   1132.5      820 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.0436 -0.9784 -0.8168  1.0181  1.2313 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Pop.Type (Intercept) 0.04608  0.2147  
## Number of obs: 823, groups:  Pop.Type, 3
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)
## (Intercept)            -0.16621    0.19517  -0.852    0.394
## `hypocotyl. length.cm`  0.01927    0.07129   0.270    0.787
## 
## Correlation of Fixed Effects:
##             (Intr)
## `hypcty.l.` -0.657
```


``` r
height_for_models <- establishment %>% filter(!is.na(height.cm))

height_est_model1 <- glm(Est_Surv ~ height.cm, family=binomial, data=height_for_models)
summary(height_est_model1)
```

```
## 
## Call:
## glm(formula = Est_Surv ~ height.cm, family = binomial, data = height_for_models)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) -0.47771    0.14839  -3.219  0.00129 **
## height.cm    0.10912    0.04262   2.560  0.01046 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1138.2  on 823  degrees of freedom
## Residual deviance: 1130.8  on 822  degrees of freedom
## AIC: 1134.8
## 
## Number of Fisher Scoring iterations: 3
```

``` r
height_est_model2 <- glmer(Est_Surv ~ height.cm + (1|Pop.Type), family=binomial, data=height_for_models)
summary(height_est_model2) #sig pos relationship with height (taller survived better)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Est_Surv ~ height.cm + (1 | Pop.Type)
##    Data: height_for_models
## 
##      AIC      BIC   logLik deviance df.resid 
##   1131.8   1146.0   -562.9   1125.8      821 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.3463 -0.9408 -0.7403  1.0322  1.4411 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Pop.Type (Intercept) 0.05141  0.2267  
## Number of obs: 824, groups:  Pop.Type, 3
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) -0.49358    0.20266  -2.435  0.01487 * 
## height.cm    0.11751    0.04287   2.741  0.00613 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##           (Intr)
## height.cm -0.654
```


``` r
length_for_models <- establishment %>% filter(!is.na(long.leaf.cm))

length_est_model1 <- glm(Est_Surv ~ long.leaf.cm, family=binomial, data=length_for_models)
summary(length_est_model1)
```

```
## 
## Call:
## glm(formula = Est_Surv ~ long.leaf.cm, family = binomial, data = length_for_models)
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.3808     0.1883  -7.334 2.23e-13 ***
## long.leaf.cm   0.7653     0.1077   7.105 1.20e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1134.2  on 820  degrees of freedom
## Residual deviance: 1074.2  on 819  degrees of freedom
## AIC: 1078.2
## 
## Number of Fisher Scoring iterations: 4
```

``` r
length_est_model2 <- glmer(Est_Surv ~ long.leaf.cm + (1|Pop.Type), family=binomial, data=length_for_models)
summary(length_est_model2) #sig pos relationship with height (longer survived better)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Est_Surv ~ long.leaf.cm + (1 | Pop.Type)
##    Data: length_for_models
## 
##      AIC      BIC   logLik deviance df.resid 
##   1080.2   1094.3   -537.1   1074.2      818 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -10.4304  -0.8613  -0.6117   0.9600   1.9034 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Pop.Type (Intercept) 0.002199 0.04689 
## Number of obs: 821, groups:  Pop.Type, 3
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.3724     0.2024  -6.781 1.19e-11 ***
## long.leaf.cm   0.7606     0.1139   6.678 2.43e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## long.lef.cm -0.921
```

