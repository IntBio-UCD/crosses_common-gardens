---
title: "WL2_2024Plants_SurvtoRep-2025"
author: "Brandie QC"
date: "2025-09-22"
output: 
  html_document: 
    keep_md: true
---



# Survival to Rep of plants planted in 2024 

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
aug_mort <- read_csv("../input/WL2_2025_Data/CorrectedCSVs/WL2_mort_pheno_20250814_corrected.csv")
```

```
## Rows: 972 Columns: 12
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (11): bed, col, Unique.ID, bud.date, flower.date, fruit.date, last.FL.da...
## dbl  (1): row
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
aug_mort %>% filter(!is.na(Unique.ID), Unique.ID!="buffer") %>% filter(is.na(as.numeric(Unique.ID))) #only non-numeric ID is yose plant from 2023
```

```
## Warning: There was 1 warning in `filter()`.
## ℹ In argument: `is.na(as.numeric(Unique.ID))`.
## Caused by warning:
## ! NAs introduced by coercion
```

```
## # A tibble: 1 × 12
##   bed     row col   Unique.ID bud.date flower.date fruit.date last.FL.date
##   <chr> <dbl> <chr> <chr>     <chr>    <chr>       <chr>      <chr>       
## 1 G         5 D     YO7_5_7   6/27/25  7/3/25      7/18/25    7/25/25     
## # ℹ 4 more variables: last.FR.date <chr>, death.date <chr>,
## #   round2.rep.dates <chr>, survey.notes <chr>
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

### Elevation Info

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
elev_info_yo <- elev_info %>% mutate(pop = str_replace(`Site code`, "YOSE(\\d+)", "YO\\1")) %>% select(Lat, Long, elev_m=`Elevation (m)`, pop)
unique(elev_info_yo$pop)
```

```
##  [1] "BH"    "BB"    "CC"    "CP1"   "CP2"   "CP3"   "DP"    "DPR"   "FR"   
## [10] NA      "HH"    "IH"    "KC1"   "KC2"   "KC3"   "LV1"   "LV2"   "LV3"  
## [19] "LVTR1" "LVTR2" "LVTR3" "SQ1"   "SQ2"   "SQ3"   "SHA"   "SC"    "TM1"  
## [28] "TM2"   "WR"    "WV"    "WL1"   "WL2"   "WL3"   "WL4"   "YO1"   "YO10" 
## [37] "YO11"  "YO12"  "YO13"  "YO2"   "YO3"   "YO4"   "YO5"   "YO6"   "YO7"  
## [46] "YO8"   "YO9"
```

## Merge

``` r
aug_mort_pops <- aug_mort %>% 
  select(bed:Unique.ID, bud.date, death.date) %>% 
  left_join(pop_info_2025) %>% 
  filter(Unique.ID!="buffer", !is.na(Unique.ID))
```

```
## Joining with `by = join_by(bed, row, col, Unique.ID)`
```

## 2024 Plants Only

``` r
aug_mort_pops_2024plants <- aug_mort_pops %>% 
  filter(status=="2024-survivor") %>% 
  mutate(Pop.Type=if_else(str_detect(pop.id, "\\) x"), "F2",
                          if_else(str_detect(pop.id, "x"), "F1",
                                  "Parent"
                          )))

aug_mort_pops_2024plants %>% filter(is.na(bud.date)) #only 5 plants have not initiated rep 
```

```
## # A tibble: 5 × 14
##   bed     row col   Unique.ID bud.date death.date status      block pop.id mf   
##   <chr> <dbl> <chr> <chr>     <chr>    <chr>      <chr>       <chr> <chr>  <chr>
## 1 C        32 A     939       <NA>     6/9/25     2024-survi… <NA>  (YO11… <NA> 
## 2 C        58 A     395       <NA>     7/3/25     2024-survi… <NA>  BH     <NA> 
## 3 D        17 B     403       <NA>     7/18/25    2024-survi… <NA>  BH     <NA> 
## 4 D        35 C     204       <NA>     6/27/25    2024-survi… <NA>  WV x … <NA> 
## 5 E        43 A     369       <NA>     6/3/25     2024-survi… <NA>  (TM2 … <NA> 
## # ℹ 4 more variables: dame_mf <chr>, sire_mf <chr>, rep <dbl>, Pop.Type <chr>
```

``` r
#2 BH  
#2 WL2 BC1s (with TM2 and YO11)
#1 F1 (WV x TM2)
```
