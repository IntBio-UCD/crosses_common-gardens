---
title: "WL2_2025_Mortality"
author: "Brandie QC"
date: "2026-04-13"
output: 
  html_document: 
    keep_md: true
---



# Survival of plants planted in 2025
Note: This currently doesn't account for establishment!!

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
sem <- function(x, na.rm=FALSE) {           #for calculating standard error
  sd(x,na.rm=na.rm)/sqrt(length(na.omit(x)))
} 
```

## Load data

## Mort/pheno

``` r
mort_pheno_2025 <- read_csv("../input/WL2_2025_Data/CorrectedCSVs/WL2_mort_pheno_20250929_corrected.csv")
```

```
## New names:
## Rows: 972 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (12): bed, col, Unique.ID, bud.date, flower.date, fruit.date, last.FL.da... dbl
## (1): row
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...13`
```

``` r
mort_pheno_2025 %>% filter(!is.na(Unique.ID), Unique.ID!="buffer") %>% filter(is.na(as.numeric(Unique.ID))) #only non-numeric ID is yose plant from 2023
```

```
## Warning: There was 1 warning in `filter()`.
## ℹ In argument: `is.na(as.numeric(Unique.ID))`.
## Caused by warning:
## ! NAs introduced by coercion
```

```
## # A tibble: 1 × 13
##   bed     row col   Unique.ID bud.date flower.date fruit.date last.FL.date
##   <chr> <dbl> <chr> <chr>     <chr>    <chr>       <chr>      <chr>       
## 1 G         5 D     YO7_5_7   6/27/25  7/3/25      7/18/25    7/25/25     
## # ℹ 5 more variables: last.FR.date <chr>, death.date <chr>,
## #   round2.rep.dates <chr>, survey.notes <chr>, ...13 <chr>
```

## Pop Info 

``` r
pop_info_2025 <- read_csv("../input/WL2_2025_Data/2025_Pop_Loc_Info Updated.csv") %>% 
  select(status:Unique.ID)
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

## Merge

``` r
mort_pheno_2025_pops <- mort_pheno_2025 %>% 
  select(bed:Unique.ID, bud.date, death.date) %>% 
  left_join(pop_info_2025) %>% 
  filter(Unique.ID!="buffer", !is.na(Unique.ID))
```

```
## Joining with `by = join_by(bed, row, col, Unique.ID)`
```

## Elevation Info

``` r
elev_info <- read_csv("../input/Strep_tort_locs.csv")
```

```
## Rows: 54 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (6): Species epithet, Species Code, Site, Site code, Lat, Long
## dbl (1): Elevation (m)
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
elev_info_yo <- elev_info %>% 
  mutate(pop = str_replace(`Site code`, "YOSE(\\d+)", "YO\\1")) %>% select(Lat, Long, elev_m=`Elevation (m)`, pop) %>% 
  rename(pop.id=pop)
head(elev_info_yo)
```

```
## # A tibble: 6 × 4
##   Lat      Long       elev_m pop.id
##   <chr>    <chr>       <dbl> <chr> 
## 1 37.40985 -119.96458   511. BH    
## 2 39.55355 -121.4329    283. BB    
## 3 39.58597 -121.43311   313  CC    
## 4 38.6382  -120.1422   2422. CP1   
## 5 38.66169 -120.13065  2244. CP2   
## 6 38.70649 -120.08797  2266. CP3
```

``` r
unique(elev_info_yo$pop.id)
```

```
##  [1] "BH"    "BB"    "CC"    "CP1"   "CP2"   "CP3"   "DP"    "DPR"   "FR"   
## [10] NA      "HH"    "IH"    "KC1"   "KC2"   "KC3"   "LV1"   "LV2"   "LV3"  
## [19] "LVTR1" "LVTR2" "LVTR3" "SQ1"   "SQ2"   "SQ3"   "SHA"   "SC"    "TM1"  
## [28] "TM2"   "WR"    "WV"    "WL1"   "WL2"   "WL3"   "WL4"   "YO1"   "YO10" 
## [37] "YO11"  "YO12"  "YO13"  "YO2"   "YO3"   "YO4"   "YO5"   "YO6"   "YO7"  
## [46] "YO8"   "YO9"
```

## 2025 Plants Only

``` r
mort_pheno_2025_pops_2025plants <- mort_pheno_2025_pops %>% 
  filter(status=="available") %>% 
  mutate(Pop.Type=if_else(str_detect(pop.id, "\\) x"), "F2",
                          if_else(str_detect(pop.id, "x"), "F1",
                                  "Parent"
                          ))) %>% 
  mutate(Surv=if_else(!is.na(bud.date), 1, 
                      if_else(is.na(death.date), 1, 0))) #1 = surv; 0 = mort

xtabs(~Pop.Type, data=mort_pheno_2025_pops_2025plants)
```

```
## Pop.Type
##     F1     F2 Parent 
##    112    379    193
```

``` r
xtabs(~Pop.Type+Surv, data=mort_pheno_2025_pops_2025plants)
```

```
##         Surv
## Pop.Type   0   1
##   F1      32  80
##   F2      88 291
##   Parent  69 124
```

``` r
#80/112 F1 survived
#291/379 F2s survived
#124/193 Parents survived 

#mort_pheno_2025_pops_2025plants %>% filter(Pop.Type=="F1") %>% distinct(pop.id) #15 F1 types 
mort_pheno_2025_pops_2025plants %>% filter(!is.na(bud.date)) %>% filter(!is.na(death.date)) #7 plants reproduced and died in 2025
```

```
## # A tibble: 7 × 15
##   bed     row col   Unique.ID bud.date death.date status    block pop.id   mf   
##   <chr> <dbl> <chr> <chr>     <chr>    <chr>      <chr>     <chr> <chr>    <chr>
## 1 C        29 D     2269      6/27/25  8/14/25    available B     (WL2 x … 2_1-3
## 2 D        46 B     2238      6/27/25  7/25/25    available F     (WL2 x … 2-3_…
## 3 D        31 D     2647      6/17/25  6/27/25    available F     (WL2 x … 2_1-3
## 4 E        26 A     1713      7/10/25  9/4/25     available I     TM2      1    
## 5 E        28 C     2637      6/27/25  9/24/25    available I     TM2      9    
## 6 E        51 C     1715      6/17/25  9/4/25     available J     TM2      2    
## 7 F        11 A     2288      17-Jun   9/24/25    available J     (WL2 x … 2_1-3
## # ℹ 5 more variables: dame_mf <chr>, sire_mf <chr>, rep <dbl>, Pop.Type <chr>,
## #   Surv <dbl>
```

## Plot by pop type 

``` r
mort_pheno_2025_pops_2025plants %>% 
  group_by(Pop.Type) %>% 
  summarise(meanSurv=mean(Surv), semSurv=sem(Surv)) %>% 
  ggplot(aes(x=Pop.Type, y=meanSurv)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0.00)) +
  labs(y="Year 1 Survival", x="Population Type") +
  theme(text=element_text(size=25))
```

![](WL2_2025_Y1Survival_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/WL2_Y1Surv_2025Plants_PopType.png", width = 10, height = 8, units = "in")
```

## Means by pop

``` r
by_pop_Surv <- mort_pheno_2025_pops_2025plants %>% 
  group_by(pop.id, Pop.Type) %>% 
  summarise(N_Surv = sum(!is.na(Surv)), mean_Surv = mean(Surv,na.rm=(TRUE)), 
            sem_Surv=sem(Surv, na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'pop.id'. You can override using the
## `.groups` argument.
```

## Plot by pops 

### Parents

``` r
by_pop_parents_Surv <- by_pop_Surv %>% 
  filter(Pop.Type == "Parent") %>% 
  left_join(elev_info_yo)
```

```
## Joining with `by = join_by(pop.id)`
```

``` r
head(by_pop_parents_Surv)
```

```
## # A tibble: 6 × 8
## # Groups:   pop.id [6]
##   pop.id Pop.Type N_Surv mean_Surv sem_Surv Lat      Long       elev_m
##   <chr>  <chr>     <int>     <dbl>    <dbl> <chr>    <chr>       <dbl>
## 1 BH     Parent       11     0.545   0.157  37.40985 -119.96458   511.
## 2 CC     Parent        4     0.75    0.25   39.58597 -121.43311   313 
## 3 DPR    Parent       14     0.286   0.125  39.22846 -120.81518  1019.
## 4 LV1    Parent        3     0.333   0.333  40.47471 -121.50486  2593.
## 5 SQ3    Parent       18     0.667   0.114  36.72109 -118.84933  2373.
## 6 TM2    Parent       48     0.667   0.0688 39.59255 -121.55072   379.
```

``` r
by_pop_parents_Surv %>% 
  filter(N_Surv>1) %>% 
  ggplot(aes(x=fct_reorder(pop.id, mean_Surv), y=mean_Surv, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=mean_Surv-sem_Surv,ymax=mean_Surv+sem_Surv),width=.2, position = 
                  position_dodge(0.75)) +
  labs(x="Parent Population", y="Year 1 Survival", fill="Elevation (m)", title="2025 Parents") +
  theme_classic() + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45, hjust = 1))
```

![](WL2_2025_Y1Survival_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/WL2_Y1Surv_2025Plants_Parents.png", width = 14, height = 8, units = "in")
```

### F1s

``` r
by_pop_Surv_F1 <- by_pop_Surv %>% 
  filter(Pop.Type == "F1") %>% 
  separate(pop.id, c("dame_pop",NA, "sire_pop"), remove = FALSE) %>% 
  left_join(elev_info_yo, by=join_by(dame_pop==pop.id)) %>% 
  rename(dame_elev=elev_m, dame_Lat=Lat, dame_Long=Long) %>% 
  left_join(elev_info_yo, by=join_by(sire_pop==pop.id)) %>% 
  rename(sire_elev=elev_m, sire_Lat=Lat, sire_Long=Long) %>% 
  mutate(meanElev=(dame_elev+sire_elev)/2)
head(by_pop_Surv_F1)
```

```
## # A tibble: 6 × 14
## # Groups:   pop.id [6]
##   pop.id dame_pop sire_pop Pop.Type N_Surv mean_Surv sem_Surv dame_Lat dame_Long
##   <chr>  <chr>    <chr>    <chr>     <int>     <dbl>    <dbl> <chr>    <chr>    
## 1 BH x … BH       WL2      F1           14     0.643    0.133 37.40985 -119.964…
## 2 DPR x… DPR      WL2      F1           10     0.6      0.163 39.22846 -120.815…
## 3 LV1 x… LV1      WL2      F1            9     0.667    0.167 40.47471 -121.504…
## 4 SQ3 x… SQ3      WL2      F1            7     0.714    0.184 36.72109 -118.849…
## 5 TM2 x… TM2      WL2      F1           14     0.714    0.125 39.59255 -121.550…
## 6 WL1 x… WL1      WL2      F1           13     0.692    0.133 38.78608 -120.2143
## # ℹ 5 more variables: dame_elev <dbl>, sire_Lat <chr>, sire_Long <chr>,
## #   sire_elev <dbl>, meanElev <dbl>
```

``` r
by_pop_Surv_F1 %>% 
  filter(N_Surv>1) %>% 
  ggplot(aes(x=fct_reorder(pop.id, mean_Surv), y=mean_Surv, fill=dame_elev)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=mean_Surv-sem_Surv,ymax=mean_Surv+sem_Surv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x="F1 Population", y="Year 1 Survival") +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45, hjust = 1))
```

![](WL2_2025_Y1Survival_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

``` r
by_pop_Surv_F1 %>% 
  filter(N_Surv>1) %>% 
  ggplot(aes(x=fct_reorder(pop.id, mean_Surv), y=mean_Surv, fill=meanElev)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=mean_Surv-sem_Surv,ymax=mean_Surv+sem_Surv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x="F1 Population", y="Year 1 Survival") +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45, hjust = 1))
```

![](WL2_2025_Y1Survival_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/WL2_Y1Surv_2025Plants_F1s.png", width = 14, height = 8, units = "in")
```

### F1s + Parents

``` r
parents_F1s_combined <- by_pop_parents_Surv %>% 
  rename(meanElev=elev_m) %>% 
  bind_rows(by_pop_Surv_F1)

parents_F1s_combined %>%  
  filter(N_Surv>1) %>% 
  ggplot(aes(x=fct_reorder(pop.id, mean_Surv), y=mean_Surv, fill=meanElev)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=mean_Surv-sem_Surv,ymax=mean_Surv+sem_Surv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x="Population", y="Year 1 Survival", fill="Elevation (m)") +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45, hjust = 1))
```

![](WL2_2025_Y1Survival_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/WL2_Y1Surv_2025Plants_F1s_Parents.png", width = 14, height = 8, units = "in")
```

## Fitness Relative to WL2 parent

``` r
WL2_crosses_2025 <- by_pop_Surv %>% 
  filter(Pop.Type!="F2") %>% #remove F2s for now
  filter(str_detect(pop.id, "WL2")) %>% #keep only F1s with WL2 involved 
  separate(pop.id, c("dame_pop",NA, "sire_pop"), remove = FALSE) %>%  #define pops for F1s
  mutate(sire_pop=if_else(pop.id=="WL2", "WL2", sire_pop)) %>% #to help with merges
  left_join(elev_info_yo, by=join_by(dame_pop==pop.id)) %>% 
  rename(dame_elev=elev_m, dame_Lat=Lat, dame_Long=Long) %>% 
  left_join(elev_info_yo, by=join_by(sire_pop==pop.id)) %>% 
  rename(sire_elev=elev_m, sire_Lat=Lat, sire_Long=Long) %>% 
  mutate(other_Parent_elev=if_else(dame_pop=="WL2", sire_elev, dame_elev)) %>% 
  mutate(sire_pop=if_else(pop.id=="WL2", NA, sire_pop)) #change sire back to NA for WL2
```

```
## Warning: Expected 3 pieces. Missing pieces filled with `NA` in 1 rows [7].
```

``` r
WL2_crosses_2025
```

```
## # A tibble: 16 × 14
## # Groups:   pop.id [16]
##    pop.id     dame_pop sire_pop Pop.Type N_Surv mean_Surv sem_Surv dame_Lat
##    <chr>      <chr>    <chr>    <chr>     <int>     <dbl>    <dbl> <chr>   
##  1 BH x WL2   BH       WL2      F1           14     0.643   0.133  37.40985
##  2 DPR x WL2  DPR      WL2      F1           10     0.6     0.163  39.22846
##  3 LV1 x WL2  LV1      WL2      F1            9     0.667   0.167  40.47471
##  4 SQ3 x WL2  SQ3      WL2      F1            7     0.714   0.184  36.72109
##  5 TM2 x WL2  TM2      WL2      F1           14     0.714   0.125  39.59255
##  6 WL1 x WL2  WL1      WL2      F1           13     0.692   0.133  38.78608
##  7 WL2        WL2      <NA>     Parent       54     0.648   0.0656 38.8263 
##  8 WL2 x CC   WL2      CC       F1            2     0.5     0.5    38.8263 
##  9 WL2 x DPR  WL2      DPR      F1            7     0.714   0.184  38.8263 
## 10 WL2 x LV1  WL2      LV1      F1           11     0.636   0.152  38.8263 
## 11 WL2 x SQ3  WL2      SQ3      F1            1     1      NA      38.8263 
## 12 WL2 x TM2  WL2      TM2      F1            2     1       0      38.8263 
## 13 WL2 x WL1  WL2      WL1      F1            2     1       0      38.8263 
## 14 WL2 x WV   WL2      WV       F1            5     0.8     0.2    38.8263 
## 15 WL2 x YO11 WL2      YO11     F1           10     0.9     0.1    38.8263 
## 16 YO11 x WL2 YO11     WL2      F1            5     0.8     0.2    37.93844
## # ℹ 6 more variables: dame_Long <chr>, dame_elev <dbl>, sire_Lat <chr>,
## #   sire_Long <chr>, sire_elev <dbl>, other_Parent_elev <dbl>
```


``` r
WL2_crosses_2025 %>% 
  ggplot(aes(x=fct_reorder(pop.id, mean_Surv), y=mean_Surv, fill=other_Parent_elev)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=mean_Surv-sem_Surv,ymax=mean_Surv+sem_Surv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  annotate("text", x = 5, y= 0.76, label = "WL2", 
           colour = "purple", fontface="bold", size = 22 / .pt) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x="Population", y="Y1 Surv", fill="Elevation \n of Donor (m)") +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1)) 
```

![](WL2_2025_Y1Survival_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/WL2_2025Plants_Y1Surv_F1sWL2.png", width = 12, height = 6, units = "in")
```


## Mid Parent 

### Prep

``` r
parent_F1s_surv <- mort_pheno_2025_pops_2025plants %>% 
  filter(Pop.Type!="F2") %>% #remove F2s
  select(bed:Unique.ID, Pop.Type, pop.id, rep, Surv) %>% 
  separate(pop.id, c("dame_pop",NA, "sire_pop"), remove = FALSE) %>% #define pops for crosses
  mutate(sire_pop=if_else(Pop.Type=="Parent", dame_pop, sire_pop)) %>% 
  #add clim and elev info for dames and sires:
  left_join(elev_info_yo, by=join_by(dame_pop==pop.id)) %>% 
  rename(dame_elev=elev_m, dame_Lat=Lat, dame_Long=Long) %>% 
  left_join(elev_info_yo, by=join_by(sire_pop==pop.id)) %>% 
  rename(sire_elev=elev_m, sire_Lat=Lat, sire_Long=Long) %>% 
  mutate(meanElev=(dame_elev+sire_elev)/2) #means for parents are just the parent's actual value since I set that pop as dame and sire 
```

```
## Warning: Expected 3 pieces. Missing pieces filled with `NA` in 193 rows [1, 2, 3, 4, 7,
## 8, 9, 10, 13, 14, 15, 16, 18, 21, 22, 24, 25, 27, 28, 30, ...].
```

``` r
parent_F1s_surv_summary <- parent_F1s_surv %>% 
  group_by(Pop.Type, pop.id, dame_pop, sire_pop, 
           dame_elev, meanElev) %>% 
  summarise(n=n(), 
            mean_Y1Surv=mean(Surv, na.rm=TRUE), 
            stdev_Y1Surv=sd(Surv, na.rm=TRUE)) 
```

```
## `summarise()` has grouped output by 'Pop.Type', 'pop.id', 'dame_pop',
## 'sire_pop', 'dame_elev'. You can override using the `.groups` argument.
```

``` r
parent_F1s_surv_summary
```

```
## # A tibble: 25 × 9
## # Groups:   Pop.Type, pop.id, dame_pop, sire_pop, dame_elev [25]
##    Pop.Type pop.id    dame_pop sire_pop dame_elev meanElev     n mean_Y1Surv
##    <chr>    <chr>     <chr>    <chr>        <dbl>    <dbl> <int>       <dbl>
##  1 F1       BH x WL2  BH       WL2           511.    1266.    14       0.643
##  2 F1       DPR x WL2 DPR      WL2          1019.    1519.    10       0.6  
##  3 F1       LV1 x WL2 LV1      WL2          2593.    2307.     9       0.667
##  4 F1       SQ3 x WL2 SQ3      WL2          2373.    2197.     7       0.714
##  5 F1       TM2 x WL2 TM2      WL2           379.    1200.    14       0.714
##  6 F1       WL1 x WL2 WL1      WL2          1614.    1817.    13       0.692
##  7 F1       WL2 x CC  WL2      CC           2020.    1167.     2       0.5  
##  8 F1       WL2 x DPR WL2      DPR          2020.    1519.     7       0.714
##  9 F1       WL2 x LV1 WL2      LV1          2020.    2307.    11       0.636
## 10 F1       WL2 x SQ3 WL2      SQ3          2020.    2197.     1       1    
## # ℹ 15 more rows
## # ℹ 1 more variable: stdev_Y1Surv <dbl>
```

### Calculating mid-parent values

``` r
F1_info <- parent_F1s_surv_summary %>% ungroup() %>% filter(Pop.Type=="F1") %>% select(pop.id, meanElev)

parent_prep_y1surv <- parent_F1s_surv_summary %>% 
  filter(Pop.Type=="Parent") %>% 
  ungroup() %>% 
  select(pop.id, mean_Y1Surv:stdev_Y1Surv) %>% 
  pivot_wider(names_from = pop.id, 
              values_from = c(mean_Y1Surv, stdev_Y1Surv)) %>% 
  mutate("BH x WL2-mean_Y1Surv"=(mean_Y1Surv_BH+mean_Y1Surv_WL2)/2, 
         "DPR x WL2-mean_Y1Surv"=(mean_Y1Surv_DPR+mean_Y1Surv_WL2)/2,
         "LV1 x WL2-mean_Y1Surv"=(mean_Y1Surv_LV1+mean_Y1Surv_WL2)/2, 
         "SQ3 x WL2-mean_Y1Surv"=(mean_Y1Surv_SQ3+mean_Y1Surv_WL2)/2,
         "TM2 x WL2-mean_Y1Surv"=(mean_Y1Surv_TM2+mean_Y1Surv_WL2)/2, 
         "WL1 x WL2-mean_Y1Surv"=(mean_Y1Surv_WL1+mean_Y1Surv_WL2)/2,
         "WL2 x CC-mean_Y1Surv"=(mean_Y1Surv_WL2+mean_Y1Surv_CC)/2, 
         "WL2 x DPR-mean_Y1Surv"=(mean_Y1Surv_WL2+mean_Y1Surv_DPR)/2,
         "WL2 x LV1-mean_Y1Surv"=(mean_Y1Surv_WL2+mean_Y1Surv_LV1)/2, 
         "WL2 x SQ3-mean_Y1Surv"=(mean_Y1Surv_WL2+mean_Y1Surv_SQ3)/2,
         "WL2 x TM2-mean_Y1Surv"=(mean_Y1Surv_WL2+mean_Y1Surv_TM2)/2, 
         "WL2 x WL1-mean_Y1Surv"=(mean_Y1Surv_WL2+mean_Y1Surv_WL1)/2,
         "WL2 x WV-mean_Y1Surv"=(mean_Y1Surv_WL2+mean_Y1Surv_WV)/2,
         "WL2 x YO11-mean_Y1Surv"=(mean_Y1Surv_WL2+mean_Y1Surv_YO11)/2,
         "YO11 x WL2-mean_Y1Surv"=(mean_Y1Surv_YO11+mean_Y1Surv_WL2)/2) %>% 
  mutate("BH x WL2-stdev_Y1Surv"=(stdev_Y1Surv_BH+stdev_Y1Surv_WL2)/2, 
         "DPR x WL2-stdev_Y1Surv"=(stdev_Y1Surv_DPR+stdev_Y1Surv_WL2)/2,
         "LV1 x WL2-stdev_Y1Surv"=(stdev_Y1Surv_LV1+stdev_Y1Surv_WL2)/2, 
         "SQ3 x WL2-stdev_Y1Surv"=(stdev_Y1Surv_SQ3+stdev_Y1Surv_WL2)/2,
         "TM2 x WL2-stdev_Y1Surv"=(stdev_Y1Surv_TM2+stdev_Y1Surv_WL2)/2, 
         "WL1 x WL2-stdev_Y1Surv"=(stdev_Y1Surv_WL1+stdev_Y1Surv_WL2)/2,
         "WL2 x CC-stdev_Y1Surv"=(stdev_Y1Surv_WL2+stdev_Y1Surv_CC)/2, 
         "WL2 x DPR-stdev_Y1Surv"=(stdev_Y1Surv_WL2+stdev_Y1Surv_DPR)/2,
         "WL2 x LV1-stdev_Y1Surv"=(stdev_Y1Surv_WL2+stdev_Y1Surv_LV1)/2, 
         "WL2 x SQ3-stdev_Y1Surv"=(stdev_Y1Surv_WL2+stdev_Y1Surv_SQ3)/2,
         "WL2 x TM2-stdev_Y1Surv"=(stdev_Y1Surv_WL2+stdev_Y1Surv_TM2)/2, 
         "WL2 x WL1-stdev_Y1Surv"=(stdev_Y1Surv_WL2+stdev_Y1Surv_WL1)/2,
         "WL2 x WV-stdev_Y1Surv"=(stdev_Y1Surv_WL2+stdev_Y1Surv_WV)/2,
         "WL2 x YO11-stdev_Y1Surv"=(stdev_Y1Surv_WL2+stdev_Y1Surv_YO11)/2,
         "YO11 x WL2-stdev_Y1Surv"=(stdev_Y1Surv_YO11+stdev_Y1Surv_WL2)/2) %>% 
  select(`BH x WL2-mean_Y1Surv`:`YO11 x WL2-stdev_Y1Surv`) %>% 
  pivot_longer(cols = everything(), names_to = c("pop.id", "measurement"), names_sep = "-", values_to = "value") %>%
  pivot_wider(names_from = measurement, values_from = value) %>% 
  left_join(F1_info) %>% 
  mutate(pop.id = paste0(pop.id, "_midParent"), Pop.Type="midParent")
```

```
## Joining with `by = join_by(pop.id)`
```

``` r
mid_parent_F1s_y1surv <- parent_F1s_surv_summary %>% 
   filter(Pop.Type=="F1") %>% 
  bind_rows(parent_prep_y1surv) %>% 
  mutate(elevation.group=if_else(meanElev>2000, "All High", "Mixed"))
mid_parent_F1s_y1surv
```

```
## # A tibble: 30 × 10
## # Groups:   Pop.Type, pop.id, dame_pop, sire_pop, dame_elev [30]
##    Pop.Type pop.id    dame_pop sire_pop dame_elev meanElev     n mean_Y1Surv
##    <chr>    <chr>     <chr>    <chr>        <dbl>    <dbl> <int>       <dbl>
##  1 F1       BH x WL2  BH       WL2           511.    1266.    14       0.643
##  2 F1       DPR x WL2 DPR      WL2          1019.    1519.    10       0.6  
##  3 F1       LV1 x WL2 LV1      WL2          2593.    2307.     9       0.667
##  4 F1       SQ3 x WL2 SQ3      WL2          2373.    2197.     7       0.714
##  5 F1       TM2 x WL2 TM2      WL2           379.    1200.    14       0.714
##  6 F1       WL1 x WL2 WL1      WL2          1614.    1817.    13       0.692
##  7 F1       WL2 x CC  WL2      CC           2020.    1167.     2       0.5  
##  8 F1       WL2 x DPR WL2      DPR          2020.    1519.     7       0.714
##  9 F1       WL2 x LV1 WL2      LV1          2020.    2307.    11       0.636
## 10 F1       WL2 x SQ3 WL2      SQ3          2020.    2197.     1       1    
## # ℹ 20 more rows
## # ℹ 2 more variables: stdev_Y1Surv <dbl>, elevation.group <chr>
```

### Figures

``` r
mid_parent_F1s_y1surv %>% 
  ggplot(aes(x=pop.id, y=mean_Y1Surv, fill=meanElev, colour=meanElev)) +
  geom_errorbar(aes(ymin=mean_Y1Surv-0.01,ymax=mean_Y1Surv+stdev_Y1Surv),width=.2, position = 
                  position_dodge(0.75)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_fill_gradient(low = "#b46ca4", high = "#0043F0") +
  scale_colour_gradient(low = "#b46ca4", high = "#0043F0") +
  theme(text=element_text(size=25),axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Avg Y1 Survival + Stdev", x="Population", fill="Avg Elevation (m)", color="Avg Elevation (m)") + 
  facet_wrap(vars(elevation.group), scales="free")
```

![](WL2_2025_Y1Survival_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/WL2_2025Plants_Y1Surv.png", width = 20, height = 8, units = "in")
```


``` r
parent_F1s_surv_summary %>% 
  filter(Pop.Type=="Parent") %>% ggplot(aes(x=fct_reorder(pop.id, meanElev), y=mean_Y1Surv, fill=meanElev, colour=meanElev)) +
  geom_errorbar(aes(ymin=mean_Y1Surv-0.01,ymax=mean_Y1Surv+stdev_Y1Surv),width=.2, position = 
                  position_dodge(0.75)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  theme(text=element_text(size=25),axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Avg Y1 Survival + Stdev", x="Population", fill="Elevation (m)", color="Elevation (m)")
```

![](WL2_2025_Y1Survival_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/WL2_2025Plants_Y1Surv_Parents.png", width = 14, height = 8, units = "in")
```

