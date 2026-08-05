---
title: "WL2_2025_TotalFitness"
author: "Brandie QC"
date: "2026-08-05"
output: 
  html_document: 
    keep_md: true
---



# Total Fitness for plants planted in 2025
Takes into account 2025 and 2026 data

## Libraries

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.2.1     ✔ readr     2.2.0
## ✔ forcats   1.0.1     ✔ stringr   1.6.0
## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
sem <- function(x, na.rm=FALSE) {  #for calculating standard error
  sd(x,na.rm=na.rm)/sqrt(length(na.omit(x)))
} 
```

## Read in data

``` r
wintsurv_2025_2026 <- read_csv("../input/WL2_2026_Data/CorrectedCSVs/WL2_2026_WinterSurv_corrected.csv")
```

```
## Rows: 972 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (7): bed, col, Unique.ID, bud.date, flower.date, death.date, survey.notes
## dbl (1): row
## lgl (3): fruit.date, last.FL.date, last:FR.date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
unique(wintsurv_2025_2026$death.date) #contains all year 1 info too
```

```
##  [1] NA         "6/17/25"  "7/3/25"   "6/9/25"   "5/14/26"  "10/20/25"
##  [7] "6/27/25"  "8/7/25"   "7/18/25"  "8/28/25"  "9/11/25"  "9/24/25" 
## [13] "8/21/25"  "9/18/25"  "9/4/25"   "8/1/25"   "9/29/25"  "7/10/25" 
## [19] "8/14/25"  "6/3/25"   "Oct-25"   "7/25/25"  "8/25/25"  "10/28/25"
```

``` r
surv_2026 <- read_csv("../input/WL2_2026_Data/CorrectedCSVs/WL2_mort_pheno_20260724_corrected.csv") #for surv to budding - need to update this with end of season data
```

```
## New names:
## Rows: 838 Columns: 12
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (11): bed, col, Unique.ID, bud.date, flower.date, fruit.date, last.FL.da... dbl
## (1): row
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...12`
```

``` r
unique(surv_2026$bud.date)
```

```
##  [1] NA        "5/28/26" "6/5/26"  "6/10/26" "6/26/26" "6/17/26" "7/2/26" 
##  [8] "5/28/25" "7/10/26" "6/26/24"
```

``` r
fruits_2026 <- read_csv("../input/WL2_2026_Data/CorrectedCSVs/WL2_ann_cens_20260731_corrected.csv") #for fruit number - need to update this with end of season data
```

```
## Rows: 838 Columns: 16
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (7): bed, col, Unique.ID, phen, survey. date, collected. date, survey.notes
## dbl (9): row, total. branch, diam. mm, height. cm, overhd .diam, overhd .per...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
unique(fruits_2026$`num. fruit`)
```

```
##  [1] NA 12 17 32  0  7 11 30  4  2 15 16 10 19 34  3 22 33
```

## Pop Info

``` r
pop_info_2025 <- read_csv("../input/WL2_2025_Data/2025_Pop_Loc_Info Updated.csv") %>% 
  select(status:Unique.ID) %>% 
  mutate(Pop.Type=if_else(str_detect(pop.id, "\\) x"), "F2",
                          if_else(str_detect(pop.id, "x"), "F1",
                          if_else(pop.id=="buffer" | is.na(pop.id), NA,
                                  "Parent"
                          )))) #define different pop types 
```

```
## Rows: 976 Columns: 16
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (10): status, block, bed, col, pop.id, mf, dame_mf, sire_mf, Unique.ID, ...
## dbl  (6): bed.block.order, bed.order, AB.CD.order, column.order, row, rep
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
pop_info_2026 <- read_csv("../input/WL2_2026_Data/Buffer New Bed Map_Corrected.csv") %>% rename(rep=Rep, Pop.Type=Type)
```

```
## Rows: 838 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (11): pop.id, mf, dame_mf, sire_mf, bed, col, status, Type, block_2025, ...
## dbl  (2): Rep, row
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## Elevation Info / Climate distance


## Calculate y1 fitness and remove plants not planted in 2025

``` r
wintsurv_2025_2026 %>%  
  left_join(pop_info_2025) %>% 
  filter(status=="available") %>% filter(!is.na(bud.date)) #some plants flowering at wint surv check 
```

```
## Joining with `by = join_by(bed, row, col, Unique.ID)`
```

```
## # A tibble: 5 × 19
##   bed     row col   Unique.ID bud.date flower.date fruit.date last.FL.date
##   <chr> <dbl> <chr> <chr>     <chr>    <chr>       <lgl>      <lgl>       
## 1 C        34 A     1745      5/14/26  <NA>        NA         NA          
## 2 C        53 D     2640      5/14/26  <NA>        NA         NA          
## 3 E        51 B     1800      5/14/26  <NA>        NA         NA          
## 4 G        30 B     2157      5/14/26  5/14/26     NA         NA          
## 5 G        31 D     1765      5/14/26  <NA>        NA         NA          
## # ℹ 11 more variables: `last:FR.date` <lgl>, death.date <chr>,
## #   survey.notes <chr>, status <chr>, block <chr>, pop.id <chr>, mf <chr>,
## #   dame_mf <chr>, sire_mf <chr>, rep <dbl>, Pop.Type <chr>
```

``` r
y1_fitness <- wintsurv_2025_2026 %>% 
  select(bed:Unique.ID, bud.date, death.date, survey.notes) %>% 
  left_join(pop_info_2025) %>% 
  filter(Unique.ID != "buffer") %>% #remove buffers
  filter(status=="available") %>%  #keep only plants planted in 2025
  select(block, bed:Unique.ID, Pop.Type, pop.id:rep, bud.date:death.date) %>% 
  filter(Unique.ID!="2605", Unique.ID!="1950",
         Unique.ID!="1675") %>% #remove plants that were dead at planting 
  mutate(death.date=mdy(death.date)) %>% #convert to date format 
  mutate(Establishment = if_else(is.na(death.date), 1, 
                                 if_else(death.date < "2025-06-21", 0, 1)), #establishment = first 3 weeks post-transplant 
         Y1Surv = if_else(Establishment==0, NA, #can't survive year 1 if you didn't establish 
                          if_else(is.na(death.date), 1, 
                                        if_else(death.date < "2025-11-01", 0, 1))), #year 1 ended in Oct
         WintSurv = if_else(Establishment==0 | Y1Surv==0, NA, #can only have wint surv if survived y1 
                            if_else(is.na(death.date), 1, 0))) 
```

```
## Joining with `by = join_by(bed, row, col, Unique.ID)`
```

``` r
y1_fitness %>% filter(WintSurv==1) #126 plants survived the winter 
```

```
## # A tibble: 126 × 16
##    block bed     row col   Unique.ID Pop.Type pop.id mf    dame_mf sire_mf   rep
##    <chr> <chr> <dbl> <chr> <chr>     <chr>    <chr>  <chr> <chr>   <chr>   <dbl>
##  1 A     C         7 B     2095      F2       (SQ3 … 3_13… 3       13-B        4
##  2 A     C         8 A     2291      F2       (WL2 … 1_2   1       2           2
##  3 B     C        31 B     2442      F2       (LV1 … 6_16… 6       16-A        3
##  4 B     C        33 A     2523      F2       (WL2 … 1_2   1       2          10
##  5 B     C        34 A     1745      Parent   TM2    5     5       <NA>       41
##  6 C     C        42 A     1747      Parent   TM2    6     6       <NA>       43
##  7 C     C        43 B     2522      F2       (WL2 … 1_2   1       2           9
##  8 C     C        44 A     2558      F2       (WV x… 3_1   3       1           2
##  9 C     C        44 B     2169      F2       (TM2)… 2-1_2 1-Feb   2           1
## 10 C     C        45 B     2328      F2       (WL2)… 3-14… 14-Mar  2          13
## # ℹ 116 more rows
## # ℹ 5 more variables: bud.date <chr>, death.date <date>, Establishment <dbl>,
## #   Y1Surv <dbl>, WintSurv <dbl>
```

## Calculate y2 fitness and remove plants not planted in 2025

``` r
surv_2026 %>% left_join(pop_info_2026) %>% filter(status=="2025-survivor") %>% filter(is.na(death.date)) %>% filter(is.na(bud.date)) #1 2025 plant alive that has not reproduced yet 
```

```
## Joining with `by = join_by(bed, row, col, Unique.ID)`
```

```
## # A tibble: 2 × 21
##   bed     row col   Unique.ID bud.date flower.date fruit.date last.FL.date
##   <chr> <dbl> <chr> <chr>     <chr>    <chr>       <chr>      <chr>       
## 1 C        53 D     2640      <NA>     6/5/26      7/2/26     <NA>        
## 2 F         4 B     2528      <NA>     <NA>        <NA>       <NA>        
## # ℹ 13 more variables: last.FR.date <chr>, death.date <chr>,
## #   survey.notes <chr>, ...12 <chr>, pop.id <chr>, mf <chr>, dame_mf <chr>,
## #   sire_mf <chr>, rep <dbl>, status <chr>, Pop.Type <chr>, block_2025 <chr>,
## #   block <chr>
```

``` r
y2_fitness <- pop_info_2026 %>% 
  left_join(surv_2026) %>% 
  left_join(fruits_2026) %>% 
  filter(Unique.ID!="buffer", !is.na(Unique.ID)) %>% 
  filter(status=="2025-survivor") %>% 
  mutate(SurvtoBud=if_else(Unique.ID=="2640", 1, #missed the bud date for this plant but it did reproduce 
                           if_else(!is.na(bud.date), 1, 0))) %>% 
  select(Unique.ID, pop.id, mf, rep, SurvtoBud, num.fruit=`num. fruit`)
```

```
## Joining with `by = join_by(bed, row, col, Unique.ID)`
## Joining with `by = join_by(bed, row, col, Unique.ID, survey.notes)`
```

## Merge y1 and y2 fitness ---> total fitnes

``` r
total_fit_2025plants <- y1_fitness %>% 
  select(-bud.date, -death.date) %>% 
  #mutate(mf=as.character(mf)) %>% #needed for the merge
  left_join(y2_fitness) %>% 
  mutate(ProbFruit=if_else(is.na(num.fruit) | num.fruit==0, 0, 1),
         TotalFitness=if_else(ProbFruit==0, 0, num.fruit))
```

```
## Joining with `by = join_by(Unique.ID, pop.id, mf, rep)`
```

``` r
head(total_fit_2025plants)
```

```
## # A tibble: 6 × 18
##   block bed     row col   Unique.ID Pop.Type pop.id  mf    dame_mf sire_mf   rep
##   <chr> <chr> <dbl> <chr> <chr>     <chr>    <chr>   <chr> <chr>   <chr>   <dbl>
## 1 A     C         4 A     2566      F2       (WV x … 2_2   2       2           7
## 2 A     C         4 B     2595      F2       (SQ3 x… 12_1… 12      16-A       23
## 3 A     C         5 A     2642      Parent   TM2     <NA>  <NA>    <NA>       87
## 4 A     C         6 A     2509      F2       (WL2 x… 1_2   1       2           1
## 5 A     C         6 B     2549      F2       (WL2 x… 1_1   1       1           4
## 6 A     C         7 A     2452      F2       (WL2 x… 2-3_… 3-Feb   3-Feb       8
## # ℹ 7 more variables: Establishment <dbl>, Y1Surv <dbl>, WintSurv <dbl>,
## #   SurvtoBud <dbl>, num.fruit <dbl>, ProbFruit <dbl>, TotalFitness <dbl>
```

``` r
summary(total_fit_2025plants)
```

```
##     block               bed                 row           col           
##  Length:682         Length:682         Min.   : 4.0   Length:682        
##  Class :character   Class :character   1st Qu.:15.0   Class :character  
##  Mode  :character   Mode  :character   Median :25.0   Mode  :character  
##                                        Mean   :26.8                     
##                                        3rd Qu.:40.0                     
##                                        Max.   :58.0                     
##                                                                         
##   Unique.ID           Pop.Type            pop.id               mf           
##  Length:682         Length:682         Length:682         Length:682        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##    dame_mf            sire_mf               rep        Establishment   
##  Length:682         Length:682         Min.   : 1.00   Min.   :0.0000  
##  Class :character   Class :character   1st Qu.: 3.00   1st Qu.:1.0000  
##  Mode  :character   Mode  :character   Median : 7.00   Median :1.0000  
##                                        Mean   :15.67   Mean   :0.9135  
##                                        3rd Qu.:18.00   3rd Qu.:1.0000  
##                                        Max.   :96.00   Max.   :1.0000  
##                                                                        
##      Y1Surv          WintSurv        SurvtoBud        num.fruit    
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   : 2.00  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:10.75  
##  Median :1.0000   Median :0.0000   Median :0.0000   Median :15.50  
##  Mean   :0.7448   Mean   :0.2716   Mean   :0.4921   Mean   :16.42  
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:22.00  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :34.00  
##  NA's   :59       NA's   :218      NA's   :556      NA's   :670    
##    ProbFruit       TotalFitness    
##  Min.   :0.0000   Min.   : 0.0000  
##  1st Qu.:0.0000   1st Qu.: 0.0000  
##  Median :0.0000   Median : 0.0000  
##  Mean   :0.0176   Mean   : 0.2889  
##  3rd Qu.:0.0000   3rd Qu.: 0.0000  
##  Max.   :1.0000   Max.   :34.0000  
## 
```

## Means

``` r
total_fit_2025plants_summary <- total_fit_2025plants %>% 
  group_by(Pop.Type, pop.id) %>% 
  summarise(meanEst=mean(Establishment, na.rm=TRUE), semEst=sem(Establishment, na.rm = TRUE),
            meanY1Surv=mean(Y1Surv, na.rm=TRUE), semY1Surv=sem(Y1Surv, na.rm = TRUE),
            meanWintSurv=mean(WintSurv, na.rm=TRUE), semWintSurv=sem(WintSurv, na.rm = TRUE),
            meanSurvtoBud=mean(SurvtoBud, na.rm=TRUE), semSurvtoBud=sem(SurvtoBud, na.rm = TRUE),
            meanFruit=mean(num.fruit, na.rm=TRUE), semFruit=sem(num.fruit, na.rm = TRUE),
            meanProbFruit=mean(ProbFruit, na.rm=TRUE), semProbFruit=sem(ProbFruit, na.rm = TRUE),
            meanTotalFit=mean(TotalFitness, na.rm=TRUE), semTotalFit=sem(TotalFitness, na.rm = TRUE))
```

```
## `summarise()` has regrouped the output.
## ℹ Summaries were computed grouped by Pop.Type and pop.id.
## ℹ Output is grouped by Pop.Type.
## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
## ℹ Use `summarise(.by = c(Pop.Type, pop.id))` for per-operation grouping
##   (`?dplyr::dplyr_by`) instead.
```

``` r
total_fit_2025plants_summary
```

```
## # A tibble: 57 × 16
## # Groups:   Pop.Type [3]
##    Pop.Type pop.id meanEst  semEst meanY1Surv semY1Surv meanWintSurv semWintSurv
##    <chr>    <chr>    <dbl>   <dbl>      <dbl>     <dbl>        <dbl>       <dbl>
##  1 F1       BH x …   0.929  0.0714      0.692     0.133        0.111       0.111
##  2 F1       DPR x…   0.9    0.1         0.556     0.176        0.6         0.245
##  3 F1       LV1 x…   1      0           0.667     0.167        0.167       0.167
##  4 F1       SQ3 x…   0.714  0.184       1         0            0.2         0.2  
##  5 F1       TM2 x…   0.929  0.0714      0.769     0.122        0.1         0.1  
##  6 F1       WL1 x…   0.917  0.0833      0.818     0.122        0.111       0.111
##  7 F1       WL2 x…   1      0           0.5       0.5          0          NA    
##  8 F1       WL2 x…   0.857  0.143       0.833     0.167        0.2         0.2  
##  9 F1       WL2 x…   0.909  0.0909      0.7       0.153        0.143       0.143
## 10 F1       WL2 x…   1     NA           1        NA            1          NA    
## # ℹ 47 more rows
## # ℹ 8 more variables: meanSurvtoBud <dbl>, semSurvtoBud <dbl>, meanFruit <dbl>,
## #   semFruit <dbl>, meanProbFruit <dbl>, semProbFruit <dbl>,
## #   meanTotalFit <dbl>, semTotalFit <dbl>
```

## Figures

### Winter Surv

``` r
total_fit_2025plants_summary %>% 
  filter(Pop.Type=="Parent") %>% 
  mutate(meanWintSurv=if_else(meanWintSurv=="NaN", NA, meanWintSurv)) %>% 
  filter(!is.na(meanWintSurv)) %>% 
  ggplot(aes(x=fct_reorder(pop.id, meanWintSurv), y=meanWintSurv)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=meanWintSurv-semWintSurv,
                    ymax=meanWintSurv+semWintSurv),width=.2, 
                position =position_dodge(0.75)) +
  labs(x="Population", y="Avg Winter Survival") +
  scale_y_continuous(expand = c(0.01, 0)) +
  theme_classic()
```

![](WL2_2025_TotalFitness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

``` r
total_fit_2025plants_summary %>% 
  filter(Pop.Type=="F1") %>% 
  mutate(meanWintSurv=if_else(meanWintSurv=="NaN", NA, meanWintSurv)) %>% 
  filter(!is.na(meanWintSurv)) %>% 
  ggplot(aes(x=fct_reorder(pop.id, meanWintSurv), y=meanWintSurv)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=meanWintSurv-semWintSurv,
                    ymax=meanWintSurv+semWintSurv),
                width=.2, position =position_dodge(0.75)) +
  theme_classic() +
  scale_y_continuous(expand = c(0.01, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x="F1", y="Avg Winter Survival")
```

![](WL2_2025_TotalFitness_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

``` r
total_fit_2025plants_summary %>% 
  filter(Pop.Type=="F2") %>% 
  mutate(meanWintSurv=if_else(meanWintSurv=="NaN", NA, meanWintSurv)) %>% 
  filter(!is.na(meanWintSurv)) %>% 
  ggplot(aes(x=fct_reorder(pop.id, meanWintSurv), y=meanWintSurv)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=meanWintSurv-semWintSurv,
                    ymax=meanWintSurv+semWintSurv),
                width=.2, position =position_dodge(0.75)) +
  theme_classic() +
  scale_y_continuous(expand = c(0.01, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x="F2", y="Avg Winter Survival")
```

![](WL2_2025_TotalFitness_files/figure-html/unnamed-chunk-9-3.png)<!-- -->
