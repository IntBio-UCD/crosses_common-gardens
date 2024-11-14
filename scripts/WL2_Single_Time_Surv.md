---
title: "WL2_Single_Time_Surv"
author: "Brandie QC"
date: "2024-11-13"
output: 
  html_document: 
    keep_md: true
---



# Analysis of Survival at WL2 2024 Garden to Plant for BC2 crosses

To Do:
-   See Julin's code for poperly calculating sem for surv

## Relevant Libraries and Functions

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
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
## ✔ broom        1.0.7     ✔ rsample      1.2.1
## ✔ dials        1.3.0     ✔ tune         1.2.1
## ✔ infer        1.0.7     ✔ workflows    1.1.4
## ✔ modeldata    1.4.0     ✔ workflowsets 1.1.0
## ✔ parsnip      1.2.1     ✔ yardstick    1.3.1
## ✔ recipes      1.1.0     
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ scales::discard() masks purrr::discard()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ recipes::fixed()  masks stringr::fixed()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ yardstick::spec() masks readr::spec()
## ✖ recipes::step()   masks stats::step()
## • Search for functions across packages at https://www.tidymodels.org/find/
```

``` r
tidymodels_prefer()
library(lmerTest) #for mixed effect models
```

```
## Loading required package: lme4
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

``` r
conflicted::conflicts_prefer(lmerTest::lmer)
```

```
## [conflicted] Will prefer lmerTest::lmer over any other package.
```

``` r
library(broom.mixed) #tidy method for lmerTest
library(emmeans) #for post-hoc pairwise comparisons 
```

```
## Welcome to emmeans.
## Caution: You lose important information if you filter this package's results.
## See '? untidy'
```

``` r
library(naniar) #replaces values with NA
library(brms)
```

```
## Loading required package: Rcpp
## 
## Attaching package: 'Rcpp'
## 
## The following object is masked from 'package:rsample':
## 
##     populate
## 
## Loading 'brms' package (version 2.22.0). Useful instructions
## can be found by typing help('brms'). A more detailed introduction
## to the package is available through vignette('brms_overview').
```

``` r
sem <- function(x, na.rm=FALSE) {           #for caclulating standard error
  sd(x,na.rm=na.rm)/sqrt(length(na.omit(x)))
} 

elev_three_palette <- c("#0043F0", "#C9727F", "#F5A540") #colors from Gremer et al 2019
elev_order <- c("High", "Mid", "Low") #for proper arrangement in figures 
```

## Load pop and location data

``` r
plant_info <- read_csv("../input/WL2_2024_Data/WL2_Final_2023_2024_Pop_Loc_Info.csv")
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
head(plant_info)
```

```
## # A tibble: 6 × 15
##   bed.block.order bed.order AB.CD.order column.order Pop.Type status block loc  
##             <dbl>     <dbl>       <dbl>        <dbl> <chr>    <chr>  <chr> <chr>
## 1               1         1           1            1 2023-TM… 2023-… <NA>  A_1_A
## 2              22        12           1            1 2023-su… 2023-… <NA>  A_6_B
## 3              62        32           1            1 2023-su… 2023-… <NA>  A_16…
## 4              65        33           1            1 2023-su… 2023-… <NA>  A_17…
## 5              69        35           1            1 2023-su… 2023-… <NA>  A_18…
## 6              93        47           1            1 2023-su… 2023-… <NA>  A_24…
## # ℹ 7 more variables: bed <chr>, bedrow <dbl>, bedcol <chr>, pop <chr>,
## #   mf <dbl>, rep <dbl>, unique.ID <chr>
```

``` r
unique(plant_info$Pop.Type) #info about whether the plant is from 2023, an F1, F2, or parent 
```

```
## [1] "2023-TM2-fruit" "2023-survivor"  NA               "F2"            
## [5] "Parent"         "F1"
```

``` r
unique(plant_info$status) #status info for planting (i.e. available spot to plant or not)
```

```
## [1] "2023-TM2-fruit" "2023-survivor"  "buffer"         "available"     
## [5] NA
```

``` r
plant_info_to_merge <- plant_info %>% 
  select(Pop.Type, block, parent.pop=pop,mf:unique.ID) %>% 
  filter(!is.na(parent.pop))

pop_loc <- read_csv("../input/Strep_tort_locs.csv")
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
head(pop_loc)
```

```
## # A tibble: 6 × 7
##   `Species epithet` `Species Code` Site  `Site code` Lat   Long  `Elevation (m)`
##   <chr>             <chr>          <chr> <chr>       <chr> <chr>           <dbl>
## 1 Streptanthus tor… STTO           Ben … BH          37.4… -119…            511.
## 2 Streptanthus tor… STTO           Bidw… BB          39.5… -121…            283.
## 3 Streptanthus tor… STTO           Cany… CC          39.5… -121…            313 
## 4 Streptanthus tor… STTO           Cars… CP1         38.6… -120…           2422.
## 5 Streptanthus tor… STTO           Cars… CP2         38.6… -120…           2244.
## 6 Streptanthus tor… STTO           Cars… CP3         38.7… -120…           2266.
```

``` r
unique(pop_loc$`Site code`)
```

```
##  [1] "BH"     "BB"     "CC"     "CP1"    "CP2"    "CP3"    "DP"     "DPR"   
##  [9] "FR"     NA       "HH"     "IH"     "KC1"    "KC2"    "KC3"    "LV1"   
## [17] "LV2"    "LV3"    "LVTR1"  "LVTR2"  "LVTR3"  "SQ1"    "SQ2"    "SQ3"   
## [25] "SHA"    "SC"     "TM1"    "TM2"    "WR"     "WV"     "WL1"    "WL2"   
## [33] "WL3"    "WL4"    "YOSE1"  "YOSE10" "YOSE11" "YOSE12" "YOSE13" "YOSE2" 
## [41] "YOSE3"  "YOSE4"  "YOSE5"  "YOSE6"  "YOSE7"  "YOSE8"  "YOSE9"
```

``` r
unique(plant_info_to_merge$parent.pop)
```

```
##  [1] "TM2"                        "CC"                        
##  [3] "BH"                         "WL2"                       
##  [5] "IH"                         "SC"                        
##  [7] "YO7"                        "SQ1"                       
##  [9] "(WV x WL2) x (WV)"          "(LV1 x WL2) x (TM2 x WL2)" 
## [11] "(TM2 x WL2) x (SQ3 x WL2)"  "(WL2 x BH) x (SQ3 x WL2)"  
## [13] "(TM2 x WL2) x (TM2 x WL2)"  "(LV1 x WL2) x (WL2 x DPR)" 
## [15] "(WL1 x WL2) x (BH x WL2)"   "(TM2 x WL2) x (YO11 x WL2)"
## [17] "(WL1 x WL2) x (WL2 x TM2)"  "(WL2 x TM2) x (CC x TM2)"  
## [19] "WV x TM2"                   "(WL2 x DPR) x (WL2)"       
## [21] "(TM2 x WL2) x (TM2)"        "(SQ3 x WL2) x (YO11 x WL2)"
## [23] "(SQ3 x WL2) x (SQ3 x WL2)"  "(YO11 x WL2) x (SQ3 x WL2)"
## [25] "(WL1 x WL2) x (WL2 x CC)"   "(LV1 x WL2) x (WL2)"       
## [27] "(TM2 x BH) x (TM2 x BH)"    "(LV1 x WL2) x (SQ3 x WL2)" 
## [29] "(DPR x WL2) x (YO11 x WL2)" "WV"                        
## [31] "(DPR x WL2) x (WV x WL2)"   "LV1 x WL2"                 
## [33] "(WV) x (WV x WL2)"          "LV1"                       
## [35] "LV1 x TM2"                  "(YO11 x WL2) x (WL2)"      
## [37] "(TM2 x BH) x (TM2)"         "TM2 x WL2"                 
## [39] "(YO11 x WL2) x (DPR x WL2)" "CC x TM2"                  
## [41] "(DPR) x (WL2 x DPR)"        "(WL2 x DPR) x (TM2 x WL2)" 
## [43] "(SQ3 x WL2) x (TM2 x WL2)"  "(TM2 x WL2) x (WL2)"       
## [45] "(WL2 x BH) x (WL2 x TM2)"   "(WL2 x CC) x (WL2 x TM2)"  
## [47] "(WL2) x (DPR x WL2)"        "WV x WL2"                  
## [49] "SQ3"                        "(SQ3 x WL2) x (WL2)"       
## [51] "(LV1 x WL2) x (YO11 x WL2)" "(BH) x (TM2 x BH)"         
## [53] "(TM2 x BH) x (BH)"          "WL1"                       
## [55] "(WL2) x (WV x WL2)"         "BH x TM2"                  
## [57] "(YO11 x WL2) x (WL2 x TM2)" "(WL2 x CC) x (SQ3 x WL2)"  
## [59] "(DPR) x (DPR x WL2)"        "(SQ3 x WL2) x (LV1 x WL2)" 
## [61] "WL2 x BH"                   "(TM2 x BH) x (TM2 x WL2)"  
## [63] "DPR"                        "(WL1 x TM2) x (WL2 x TM2)" 
## [65] "(YO11 x WL2) x (WV x WL2)"  "(SQ3 x WL2) x (DPR x WL2)" 
## [67] "DPR x WL2"                  "(LV1 x WL2) x (LV1 x WL2)" 
## [69] "BH x WL2"                   "(WV x WL2) x (WL2 x DPR)"  
## [71] "(WL2 x DPR) x (YO11 x WL2)" "YO11"                      
## [73] "(WL2) x (TM2 x WL2)"        "(TM2) x (TM2 x WL2)"       
## [75] "SQ3 x WL2"                  "(CC x TM2) x (WL2 x TM2)"  
## [77] "(TM2 x WL2) x (LV1 x WL2)"  "(DPR x WL2) x (SQ3 x WL2)" 
## [79] "WR"                         "(TM2 x WL2) x (DPR x WL2)" 
## [81] "(WL2 x DPR) x (DPR)"        "Buffer"                    
## [83] "(YO11 x WL2) x (TM2 x WL2)" "(DPR x WL2) x (DPR x WL2)" 
## [85] "(DPR x WL2) x (TM2 x WL2)"  "TM2 x YO11"                
## [87] "buffer"                     "(TM2) x (TM2 x BH)"        
## [89] "(TM2 x WL2) x (TM2 x BH)"   "(WL2 x TM2) x (WL2)"       
## [91] "(TM2 x WL2) x (WV x WL2)"
```

``` r
#need to change YOSE to YO
pop_loc_yo <- pop_loc %>% mutate(parent.pop = str_replace(`Site code`, "YOSE(\\d+)", "YO\\1")) %>% select(Lat, Long, elev_m=`Elevation (m)`, parent.pop)
unique(pop_loc_yo$parent.pop)
```

```
##  [1] "BH"    "BB"    "CC"    "CP1"   "CP2"   "CP3"   "DP"    "DPR"   "FR"   
## [10] NA      "HH"    "IH"    "KC1"   "KC2"   "KC3"   "LV1"   "LV2"   "LV3"  
## [19] "LVTR1" "LVTR2" "LVTR3" "SQ1"   "SQ2"   "SQ3"   "SHA"   "SC"    "TM1"  
## [28] "TM2"   "WR"    "WV"    "WL1"   "WL2"   "WL3"   "WL4"   "YO1"   "YO10" 
## [37] "YO11"  "YO12"  "YO13"  "YO2"   "YO3"   "YO4"   "YO5"   "YO6"   "YO7"  
## [46] "YO8"   "YO9"
```

``` r
pop_elev <- left_join(plant_info_to_merge, pop_loc_yo)
```

```
## Joining with `by = join_by(parent.pop)`
```

``` r
head(pop_elev)
```

```
## # A tibble: 6 × 9
##   Pop.Type       block parent.pop    mf   rep unique.ID Lat      Long     elev_m
##   <chr>          <chr> <chr>      <dbl> <dbl> <chr>     <chr>    <chr>     <dbl>
## 1 2023-TM2-fruit <NA>  TM2            6    11 TM2_6_11  39.59255 -121.55…   379.
## 2 2023-survivor  <NA>  CC             3     3 CC_3_3    39.58597 -121.43…   313 
## 3 2023-survivor  <NA>  BH             3     3 BH_3_3    37.40985 -119.96…   511.
## 4 2023-survivor  <NA>  BH             7     3 BH_7_3    37.40985 -119.96…   511.
## 5 2023-survivor  <NA>  BH             4     3 BH_4_3    37.40985 -119.96…   511.
## 6 2023-survivor  <NA>  WL2            7     9 WL2_7_9   38.8263  -120.25…  2020.
```

## Load the mort/pheno data

``` r
mort_pheno_1023 <- read_csv("../input/WL2_2024_Data/CorrectedCSVs/WL2_mort_pheno_20241023_corrected.csv", 
                            na = c("", "NA", "-", "N/A")) %>% arrange(death.date)
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
head(mort_pheno_1023)
```

```
## # A tibble: 6 × 13
##   block bed     row col   unique.ID bud.date flower.date fruit.date last.FL.date
##   <chr> <chr> <dbl> <chr> <chr>     <chr>    <chr>       <chr>      <chr>       
## 1 <NA>  A        45 B     IH_2_4    7/2/24   7/16/24     7/23/24    9/3/24      
## 2 B     C        28 C     251       <NA>     <NA>        <NA>       <NA>        
## 3 <NA>  C        30 C     BH_4_7    6/25/24  7/9/24      7/16/24    8/27/24     
## 4 E     D        20 B     141       <NA>     <NA>        <NA>       <NA>        
## 5 G     D        48 A     485       <NA>     <NA>        <NA>       <NA>        
## 6 F     D        37 D     312       <NA>     <NA>        <NA>       <NA>        
## # ℹ 4 more variables: last.FR.date <chr>, death.date <chr>, missing.date <chr>,
## #   survey.notes <chr>
```

``` r
names(mort_pheno_1023)
```

```
##  [1] "block"        "bed"          "row"          "col"          "unique.ID"   
##  [6] "bud.date"     "flower.date"  "fruit.date"   "last.FL.date" "last.FR.date"
## [11] "death.date"   "missing.date" "survey.notes"
```

``` r
unique(mort_pheno_1023$bed) #D and "D."
```

```
##  [1] "A"  "C"  "D"  "E"  "H"  "B"  "J"  "G"  "K"  "F"  "I"  "D."
```

``` r
unique(mort_pheno_1023$death.date)  
```

```
##  [1] "10/16/24" "10/2/24"  "10/23/24" "10/9/24"  "6/18/24"  "6/25/24" 
##  [7] "7/16/24"  "7/2/24"   "7/23/24"  "7/30/24"  "7/9/24"   "8/13/24" 
## [13] "8/20/24"  "8/27/24"  "8/6/24"   "9/10/24"  "9/17/24"  "9/24/24" 
## [19] "9/3/24"   NA
```

``` r
#some dates should be checked: 6/24, 6/27 - both were data verifcation errors, corrected on csv. 

mort_pheno_1023_nobuff <- mort_pheno_1023 %>% 
  filter(unique.ID !="buffer") %>% 
  filter(!is.na(unique.ID))
```

## Merge with location info 

``` r
oct_mort_loc <- left_join(mort_pheno_1023_nobuff, pop_elev) 
```

```
## Joining with `by = join_by(block, unique.ID)`
```

``` r
head(oct_mort_loc)
```

```
## # A tibble: 6 × 20
##   block bed     row col   unique.ID bud.date flower.date fruit.date last.FL.date
##   <chr> <chr> <dbl> <chr> <chr>     <chr>    <chr>       <chr>      <chr>       
## 1 <NA>  A        45 B     IH_2_4    7/2/24   7/16/24     7/23/24    9/3/24      
## 2 B     C        28 C     251       <NA>     <NA>        <NA>       <NA>        
## 3 <NA>  C        30 C     BH_4_7    6/25/24  7/9/24      7/16/24    8/27/24     
## 4 E     D        20 B     141       <NA>     <NA>        <NA>       <NA>        
## 5 G     D        48 A     485       <NA>     <NA>        <NA>       <NA>        
## 6 F     D        37 D     312       <NA>     <NA>        <NA>       <NA>        
## # ℹ 11 more variables: last.FR.date <chr>, death.date <chr>,
## #   missing.date <chr>, survey.notes <chr>, Pop.Type <chr>, parent.pop <chr>,
## #   mf <dbl>, rep <dbl>, Lat <chr>, Long <chr>, elev_m <dbl>
```

``` r
names(oct_mort_loc)
```

```
##  [1] "block"        "bed"          "row"          "col"          "unique.ID"   
##  [6] "bud.date"     "flower.date"  "fruit.date"   "last.FL.date" "last.FR.date"
## [11] "death.date"   "missing.date" "survey.notes" "Pop.Type"     "parent.pop"  
## [16] "mf"           "rep"          "Lat"          "Long"         "elev_m"
```

``` r
oct_mort_loc %>% filter(!is.na(missing.date)) #11 2024 plants with a missing date & 1 2023 plant that went missing 
```

```
## # A tibble: 12 × 20
##    block bed     row col   unique.ID bud.date flower.date fruit.date
##    <chr> <chr> <dbl> <chr> <chr>     <chr>    <chr>       <chr>     
##  1 B     C        31 D     853       <NA>     <NA>        <NA>      
##  2 C     C        46 D     429       <NA>     <NA>        <NA>      
##  3 F     D        42 A     1232      <NA>     <NA>        <NA>      
##  4 G     D        56 A     558       <NA>     <NA>        <NA>      
##  5 F     D        38 C     1269      <NA>     <NA>        <NA>      
##  6 I     E        45 A     1025      <NA>     <NA>        <NA>      
##  7 I     E        25 D     637       <NA>     <NA>        <NA>      
##  8 J     F         8 B     729       <NA>     <NA>        <NA>      
##  9 J     F        14 B     1251      <NA>     <NA>        <NA>      
## 10 J     F        18 B     208       <NA>     <NA>        <NA>      
## 11 L     G         4 A     1238      <NA>     <NA>        <NA>      
## 12 <NA>  H        12 B     SC_2_1    <NA>     <NA>        <NA>      
## # ℹ 12 more variables: last.FL.date <chr>, last.FR.date <chr>,
## #   death.date <chr>, missing.date <chr>, survey.notes <chr>, Pop.Type <chr>,
## #   parent.pop <chr>, mf <dbl>, rep <dbl>, Lat <chr>, Long <chr>, elev_m <dbl>
```

``` r
oct_mort_loc %>% filter(Pop.Type=="2023-survivor") # double checked none of these plants were actually alive (no data)
```

```
## # A tibble: 131 × 20
##    block bed     row col   unique.ID bud.date flower.date fruit.date
##    <chr> <chr> <dbl> <chr> <chr>     <chr>    <chr>       <chr>     
##  1 <NA>  A        45 B     IH_2_4    7/2/24   7/16/24     7/23/24   
##  2 <NA>  C        30 C     BH_4_7    6/25/24  7/9/24      7/16/24   
##  3 <NA>  D        51 C     IH_1_8    6/25/24  7/16/24     7/23/24   
##  4 <NA>  E        46 B     IH_3_11   7/2/24   7/30/24     8/6/24    
##  5 <NA>  A        17 A     BH_7_3    6/18/24  7/2/24      7/9/24    
##  6 <NA>  A        24 A     WL2_7_9   6/18/24  6/18/24     7/2/24    
##  7 <NA>  A        32 B     IH_7_4    6/18/24  7/9/24      7/16/24   
##  8 <NA>  A        35 A     SC_8_4    6/18/24  7/2/24      7/16/24   
##  9 <NA>  A        36 A     BH_3_4    6/18/24  7/9/24      7/16/24   
## 10 <NA>  A        39 B     WL2_7_10  6/18/24  6/18/24     7/2/24    
## # ℹ 121 more rows
## # ℹ 12 more variables: last.FL.date <chr>, last.FR.date <chr>,
## #   death.date <chr>, missing.date <chr>, survey.notes <chr>, Pop.Type <chr>,
## #   parent.pop <chr>, mf <dbl>, rep <dbl>, Lat <chr>, Long <chr>, elev_m <dbl>
```

## Survival 

### Add Surv columns 

``` r
wl2_surv <- oct_mort_loc %>% 
  filter(Pop.Type != "2023-TM2-fruit") %>% #take out 2023 fruiting locations 
  filter(is.na(missing.date)) %>% #take out plants that went missing 
  mutate(Surv_to_Oct = if_else(is.na(death.date), 1, 0), #surv to end of 2024 survey season
         Surv_Post_Transplant = if_else(is.na(death.date), 1,
           if_else(death.date=="6/18/24" | death.date=="6/25/24", 0, 1))) %>% #surv 2 weeks post-transplant
  select(Lat:elev_m, Pop.Type:rep, block:unique.ID, death.date, Surv_to_Oct, Surv_Post_Transplant, missing.date, survey.notes)

head(wl2_surv)
```

```
## # A tibble: 6 × 17
##   Lat      Long   elev_m Pop.Type parent.pop    mf   rep block bed     row col  
##   <chr>    <chr>   <dbl> <chr>    <chr>      <dbl> <dbl> <chr> <chr> <dbl> <chr>
## 1 39.09332 -120.…   454. 2023-su… IH             2     4 <NA>  A        45 B    
## 2 <NA>     <NA>      NA  F2       (WL2 x BH…    NA    16 B     C        28 C    
## 3 37.40985 -119.…   511. 2023-su… BH             4     7 <NA>  C        30 C    
## 4 38.8263  -120.…  2020. Parent   WL2           NA    46 E     D        20 B    
## 5 39.59255 -121.…   379. Parent   TM2           NA    15 G     D        48 A    
## 6 <NA>     <NA>      NA  F2       (TM2 x WL…    NA     8 F     D        37 D    
## # ℹ 6 more variables: unique.ID <chr>, death.date <chr>, Surv_to_Oct <dbl>,
## #   Surv_Post_Transplant <dbl>, missing.date <chr>, survey.notes <chr>
```

``` r
tail(wl2_surv)
```

```
## # A tibble: 6 × 17
##   Lat      Long   elev_m Pop.Type parent.pop    mf   rep block bed     row col  
##   <chr>    <chr>   <dbl> <chr>    <chr>      <dbl> <dbl> <chr> <chr> <dbl> <chr>
## 1 <NA>     <NA>      NA  F1       LV1 x TM2     NA    26 P     I        11 A    
## 2 37.40985 -119.…   511. 2023-su… BH             1     1 <NA>  I        18 A    
## 3 39.22846 -120.…  1019. Parent   DPR           NA    15 P     I        10 D    
## 4 37.40985 -119.…   511. 2023-su… BH             4     2 <NA>  J        10 A    
## 5 37.80903 -119.…  2470. 2023-su… YO7            7    11 <NA>  J        15 A    
## 6 37.40985 -119.…   511. 2023-su… BH             6     2 <NA>  J        13 C    
## # ℹ 6 more variables: unique.ID <chr>, death.date <chr>, Surv_to_Oct <dbl>,
## #   Surv_Post_Transplant <dbl>, missing.date <chr>, survey.notes <chr>
```


``` r
xtabs(~Surv_to_Oct+Pop.Type, data=wl2_surv) #numbers look right 
```

```
##            Pop.Type
## Surv_to_Oct 2023-survivor  F1  F2 Parent
##           0           106  86 371    248
##           1            24  17  64     33
```

``` r
#(830 2024 plants - 11 went missing in the field) - 14% overall survival 
#(131 2023 plants - 1 went missing in the field) - 19% overall survival 

xtabs(~Surv_Post_Transplant+Pop.Type, data=wl2_surv)
```

```
##                     Pop.Type
## Surv_Post_Transplant 2023-survivor  F1  F2 Parent
##                    0            22  23  78     87
##                    1           108  80 357    194
```

``` r
#(830 2024 plants - 3 went missing in the field) - 77% overall survival 
```

## Quick Look at 2024 Parent Pops

``` r
meansurv_2024_parents <- wl2_surv %>% 
  filter(Pop.Type=="Parent") %>% 
  group_by(parent.pop, elev_m) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'parent.pop'. You can override using the
## `.groups` argument.
```

``` r
meansurv_2024_parents #variable planting sample sizes (some as small as 2)
```

```
## # A tibble: 10 × 5
## # Groups:   parent.pop [10]
##    parent.pop elev_m N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##    <chr>       <dbl>  <int>            <dbl>                     <dbl>
##  1 BH           511.     21           0.476                      0.857
##  2 CC           313       2           0                          1    
##  3 DPR         1019.      8           0.25                       0.75 
##  4 LV1         2593.     18           0                          0.389
##  5 SQ3         2373.     11           0                          0.727
##  6 TM2          379.     97           0.113                      0.794
##  7 WL1         1614.     13           0.231                      1    
##  8 WL2         2020.     96           0.0625                     0.562
##  9 WV           749.     13           0                          0.538
## 10 YO11        2872.      2           0.5                        1
```

``` r
meansurv_2024_parents %>% 
  ggplot(aes(x=fct_reorder(parent.pop, mean_Surv_to_Oct), y=mean_Surv_to_Oct, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  labs(x="Parent Population", y="Survival to Oct 2024", fill="Elevation (m)") +
  theme_classic() + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0")
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

``` r
meansurv_2024_parents %>% 
  ggplot(aes(x=fct_reorder(parent.pop, mean_Surv_Post_Transplant), y=mean_Surv_Post_Transplant, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  labs(x="Parent Population", y="Survival Two Weeks Post-Transplant", fill="Elevation (m)") +
  theme_classic() + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0")
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

## F1s


``` r
wl2_surv_F1 <- wl2_surv %>% 
  filter(Pop.Type=="F1") %>% 
  unite(Field_Loc, bed:col, sep="_") %>% 
  select(Pop.Type:parent.pop, Field_Loc, unique.ID:Surv_Post_Transplant, survey.notes) %>% 
  separate_wider_delim(parent.pop, " x ", names = c("maternal.pop", "paternal.pop"), cols_remove = FALSE) %>% 
  mutate(WL2.cross = if_else(maternal.pop=="WL2" | paternal.pop=="WL2", TRUE, FALSE))
wl2_surv_F1
```

```
## # A tibble: 103 × 11
##    Pop.Type maternal.pop paternal.pop parent.pop Field_Loc unique.ID death.date
##    <chr>    <chr>        <chr>        <chr>      <chr>     <chr>     <chr>     
##  1 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  2 F1       BH           TM2          BH x TM2   C_17_D    600       6/18/24   
##  3 F1       LV1          WL2          LV1 x WL2  D_52_C    1271      6/18/24   
##  4 F1       LV1          TM2          LV1 x TM2  H_15_C    1246      6/18/24   
##  5 F1       TM2          WL2          TM2 x WL2  C_32_B    1279      6/25/24   
##  6 F1       WV           TM2          WV x TM2   C_49_B    201       6/25/24   
##  7 F1       LV1          TM2          LV1 x TM2  C_56_B    1241      6/25/24   
##  8 F1       LV1          TM2          LV1 x TM2  C_13_D    1245      6/25/24   
##  9 F1       LV1          TM2          LV1 x TM2  D_23_A    1231      6/25/24   
## 10 F1       TM2          WL2          TM2 x WL2  D_18_C    1281      6/25/24   
## # ℹ 93 more rows
## # ℹ 4 more variables: Surv_to_Oct <dbl>, Surv_Post_Transplant <dbl>,
## #   survey.notes <chr>, WL2.cross <lgl>
```

``` r
xtabs(~Surv_to_Oct+WL2.cross, data=wl2_surv_F1) #slightly higher surv in WL2 crosses 
```

```
##            WL2.cross
## Surv_to_Oct FALSE TRUE
##           0    51   35
##           1     9    8
```

``` r
xtabs(~Surv_Post_Transplant+WL2.cross, data=wl2_surv_F1) #slightly higher surv in TM2 crosses
```

```
##                     WL2.cross
## Surv_Post_Transplant FALSE TRUE
##                    0    10   13
##                    1    50   30
```

Filter to only F1s with WL2 involved 

``` r
wl2_surv_wl2F1s <- wl2_surv_F1 %>% filter(WL2.cross=="TRUE")  
unique(wl2_surv_wl2F1s$parent.pop) #only 7 F1s with WL2 involved
```

```
## [1] "LV1 x WL2" "TM2 x WL2" "WL2 x BH"  "SQ3 x WL2" "DPR x WL2" "WV x WL2" 
## [7] "BH x WL2"
```

``` r
xtabs(~Surv_to_Oct+maternal.pop, data=wl2_surv_wl2F1s) 
```

```
##            maternal.pop
## Surv_to_Oct BH DPR LV1 SQ3 TM2 WL2 WV
##           0  2   4   9   4   7   4  5
##           1  0   0   2   0   2   0  4
```

``` r
xtabs(~Surv_Post_Transplant+maternal.pop, data=wl2_surv_wl2F1s) 
```

```
##                     maternal.pop
## Surv_Post_Transplant BH DPR LV1 SQ3 TM2 WL2 WV
##                    0  0   1   5   1   5   1  0
##                    1  2   3   6   3   4   3  9
```

``` r
xtabs(~Surv_to_Oct+paternal.pop, data=wl2_surv_wl2F1s) 
```

```
##            paternal.pop
## Surv_to_Oct BH WL2
##           0  4  31
##           1  0   8
```

``` r
xtabs(~Surv_Post_Transplant+paternal.pop, data=wl2_surv_wl2F1s) 
```

```
##                     paternal.pop
## Surv_Post_Transplant BH WL2
##                    0  1  12
##                    1  3  27
```

``` r
#sample sizes so low, it's hard to feel confident in any trends 
```

### Calculate proportions of each plant for analysis 

``` r
wl2_surv_F1_props <- wl2_surv_F1 %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  mutate(prop.WL2=str_count(parent.pop, "WL2")/2,
         prop.CC=str_count(parent.pop, "CC")/2,
         prop.BH=str_count(parent.pop, "BH")/2,
         prop.WV=str_count(parent.pop, "WV")/2,
         prop.LV1=str_count(parent.pop, "LV1")/2,
         prop.TM2=str_count(parent.pop, "TM2")/2,
         prop.SQ3=str_count(parent.pop, "SQ3")/2,
         prop.DPR=str_count(parent.pop, "DPR")/2,
         prop.YO11=str_count(parent.pop, "YO11")/2)

summary(wl2_surv_F1_props)
```

```
##    Pop.Type         maternal.pop       paternal.pop        parent.pop       
##  Length:43          Length:43          Length:43          Length:43         
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##   Field_Loc          unique.ID          death.date         Surv_to_Oct   
##  Length:43          Length:43          Length:43          Min.   :0.000  
##  Class :character   Class :character   Class :character   1st Qu.:0.000  
##  Mode  :character   Mode  :character   Mode  :character   Median :0.000  
##                                                           Mean   :0.186  
##                                                           3rd Qu.:0.000  
##                                                           Max.   :1.000  
##  Surv_Post_Transplant survey.notes       WL2.cross         prop.WL2  
##  Min.   :0.0000       Length:43          Mode:logical   Min.   :0.5  
##  1st Qu.:0.0000       Class :character   TRUE:43        1st Qu.:0.5  
##  Median :1.0000       Mode  :character                  Median :0.5  
##  Mean   :0.6977                                         Mean   :0.5  
##  3rd Qu.:1.0000                                         3rd Qu.:0.5  
##  Max.   :1.0000                                         Max.   :0.5  
##     prop.CC     prop.BH           prop.WV          prop.LV1     
##  Min.   :0   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0   Median :0.00000   Median :0.0000   Median :0.0000  
##  Mean   :0   Mean   :0.06977   Mean   :0.1047   Mean   :0.1279  
##  3rd Qu.:0   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.2500  
##  Max.   :0   Max.   :0.50000   Max.   :0.5000   Max.   :0.5000  
##     prop.TM2         prop.SQ3          prop.DPR         prop.YO11
##  Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0  
##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0  
##  Median :0.0000   Median :0.00000   Median :0.00000   Median :0  
##  Mean   :0.1047   Mean   :0.04651   Mean   :0.04651   Mean   :0  
##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0  
##  Max.   :0.5000   Max.   :0.50000   Max.   :0.50000   Max.   :0
```

``` r
xtabs(~Surv_to_Oct+prop.BH, data=wl2_surv_F1_props)
```

```
##            prop.BH
## Surv_to_Oct  0 0.5
##           0 29   6
##           1  8   0
```

``` r
xtabs(~Surv_to_Oct+prop.WV, data=wl2_surv_F1_props)
```

```
##            prop.WV
## Surv_to_Oct  0 0.5
##           0 30   5
##           1  4   4
```

``` r
xtabs(~Surv_to_Oct+prop.LV1, data=wl2_surv_F1_props)
```

```
##            prop.LV1
## Surv_to_Oct  0 0.5
##           0 26   9
##           1  6   2
```

``` r
xtabs(~Surv_to_Oct+prop.TM2, data=wl2_surv_F1_props)
```

```
##            prop.TM2
## Surv_to_Oct  0 0.5
##           0 28   7
##           1  6   2
```

``` r
xtabs(~Surv_to_Oct+prop.SQ3, data=wl2_surv_F1_props)
```

```
##            prop.SQ3
## Surv_to_Oct  0 0.5
##           0 31   4
##           1  8   0
```

``` r
xtabs(~Surv_to_Oct+prop.DPR, data=wl2_surv_F1_props)
```

```
##            prop.DPR
## Surv_to_Oct  0 0.5
##           0 31   4
##           1  8   0
```


### with glm


``` r
m1 <- glm(Surv_to_Oct ~ prop.BH+prop.WV+prop.LV1+prop.TM2+prop.SQ3+prop.DPR, family = binomial, data=wl2_surv_F1_props)

m2 <- glm(Surv_Post_Transplant ~ prop.BH+prop.WV+prop.LV1+prop.TM2+prop.SQ3+prop.DPR, family = binomial, data=wl2_surv_F1_props)

m3 <- glm(Surv_to_Oct ~ maternal.pop, family = binomial, data=wl2_surv_F1_props)

m4 <- glm(Surv_Post_Transplant ~ maternal.pop, family = binomial, data=wl2_surv_F1_props)
```


``` r
summary(m1)
```

```
## 
## Call:
## glm(formula = Surv_to_Oct ~ prop.BH + prop.WV + prop.LV1 + prop.TM2 + 
##     prop.SQ3 + prop.DPR, family = binomial, data = wl2_surv_F1_props)
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept) -1.957e+01  5.377e+03  -0.004    0.997
## prop.BH     -3.169e-08  1.388e+04   0.000    1.000
## prop.WV      3.869e+01  1.075e+04   0.004    0.997
## prop.LV1     3.612e+01  1.075e+04   0.003    0.997
## prop.TM2     3.663e+01  1.075e+04   0.003    0.997
## prop.SQ3    -3.180e-08  1.521e+04   0.000    1.000
## prop.DPR            NA         NA      NA       NA
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 41.318  on 42  degrees of freedom
## Residual deviance: 32.331  on 37  degrees of freedom
## AIC: 44.331
## 
## Number of Fisher Scoring iterations: 18
```

``` r
summary(m2)
```

```
## 
## Call:
## glm(formula = Surv_Post_Transplant ~ prop.BH + prop.WV + prop.LV1 + 
##     prop.TM2 + prop.SQ3 + prop.DPR, family = binomial, data = wl2_surv_F1_props)
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)    1.099      1.155   0.951    0.341
## prop.BH        1.022      3.183   0.321    0.748
## prop.WV       34.935   4348.426   0.008    0.994
## prop.LV1      -1.833      2.608  -0.703    0.482
## prop.TM2      -2.644      2.671  -0.990    0.322
## prop.SQ3       0.000      3.266   0.000    1.000
## prop.DPR          NA         NA      NA       NA
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 52.703  on 42  degrees of freedom
## Residual deviance: 41.928  on 37  degrees of freedom
## AIC: 53.928
## 
## Number of Fisher Scoring iterations: 17
```

``` r
summary(m3)
```

```
## 
## Call:
## glm(formula = Surv_to_Oct ~ maternal.pop, family = binomial, 
##     data = wl2_surv_F1_props)
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)
## (Intercept)     -1.957e+01  7.604e+03  -0.003    0.998
## maternal.popDPR -6.443e-09  9.313e+03   0.000    1.000
## maternal.popLV1  1.806e+01  7.604e+03   0.002    0.998
## maternal.popSQ3 -6.496e-09  9.313e+03   0.000    1.000
## maternal.popTM2  1.831e+01  7.604e+03   0.002    0.998
## maternal.popWL2 -6.502e-09  9.313e+03   0.000    1.000
## maternal.popWV   1.934e+01  7.604e+03   0.003    0.998
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 41.318  on 42  degrees of freedom
## Residual deviance: 32.331  on 36  degrees of freedom
## AIC: 46.331
## 
## Number of Fisher Scoring iterations: 18
```

``` r
summary(m4)
```

```
## 
## Call:
## glm(formula = Surv_Post_Transplant ~ maternal.pop, family = binomial, 
##     data = wl2_surv_F1_props)
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)
## (Intercept)      1.857e+01  4.612e+03   0.004    0.997
## maternal.popDPR -1.747e+01  4.612e+03  -0.004    0.997
## maternal.popLV1 -1.838e+01  4.612e+03  -0.004    0.997
## maternal.popSQ3 -1.747e+01  4.612e+03  -0.004    0.997
## maternal.popTM2 -1.879e+01  4.612e+03  -0.004    0.997
## maternal.popWL2 -1.747e+01  4.612e+03  -0.004    0.997
## maternal.popWV   8.695e-09  5.099e+03   0.000    1.000
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 52.703  on 42  degrees of freedom
## Residual deviance: 41.020  on 36  degrees of freedom
## AIC: 55.02
## 
## Number of Fisher Scoring iterations: 17
```

### have to convert using inv_logit

coefficients

``` r
results1 <- c(coef(m1)[1], #intercept
             coef(m1)[2:7] + coef(m1)[1]) %>% #coefficients + intercept
  inv_logit_scaled()

results1
```

```
##  (Intercept)      prop.BH      prop.WV     prop.LV1     prop.TM2     prop.SQ3 
## 3.181005e-09 3.181005e-09 1.000000e+00 9.999999e-01 1.000000e+00 3.181005e-09 
##     prop.DPR 
##           NA
```

``` r
results2 <- c(coef(m2)[1], #intercept
             coef(m2)[2:7] + coef(m2)[1]) %>% #coefficients + intercept
  inv_logit_scaled()

results2
```

```
## (Intercept)     prop.BH     prop.WV    prop.LV1    prop.TM2    prop.SQ3 
##   0.7500000   0.8928571   1.0000000   0.3243243   0.1758242   0.7500000 
##    prop.DPR 
##          NA
```

``` r
results3 <- c(coef(m3)[1], #intercept
             coef(m3)[2:7] + coef(m3)[1]) %>% #coefficients + intercept
  inv_logit_scaled()

results3
```

```
##     (Intercept) maternal.popDPR maternal.popLV1 maternal.popSQ3 maternal.popTM2 
##    3.181005e-09    3.181005e-09    1.818182e-01    3.181005e-09    2.222222e-01 
## maternal.popWL2  maternal.popWV 
##    3.181005e-09    4.444444e-01
```

``` r
results4 <- c(coef(m4)[1], #intercept
             coef(m4)[2:7] + coef(m4)[1]) %>% #coefficients + intercept
  inv_logit_scaled()

results4
```

```
##     (Intercept) maternal.popDPR maternal.popLV1 maternal.popSQ3 maternal.popTM2 
##       1.0000000       0.7500000       0.5454545       0.7500000       0.4444444 
## maternal.popWL2  maternal.popWV 
##       0.7500000       1.0000000
```

error

``` r
summary(m1)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)     prop.BH     prop.WV    prop.LV1    prop.TM2    prop.SQ3 
##           1           1           1           1           1           1
```

``` r
summary(m2)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)     prop.BH     prop.WV    prop.LV1    prop.TM2    prop.SQ3 
##   0.7603684   0.9602006   1.0000000   0.9313543   0.9352833   0.9632433
```

``` r
summary(m3)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
##     (Intercept) maternal.popDPR maternal.popLV1 maternal.popSQ3 maternal.popTM2 
##               1               1               1               1               1 
## maternal.popWL2  maternal.popWV 
##               1               1
```

``` r
summary(m4)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
##     (Intercept) maternal.popDPR maternal.popLV1 maternal.popSQ3 maternal.popTM2 
##               1               1               1               1               1 
## maternal.popWL2  maternal.popWV 
##               1               1
```

``` r
#error is really high 
```

## F2s


``` r
wl2_surv_F2 <- wl2_surv %>% 
  filter(Pop.Type=="F2") %>% 
  unite(Field_Loc, bed:col, sep="_") %>% 
  mutate(WL2.cross = if_else(str_detect(parent.pop, "WL2"), TRUE, FALSE)) %>% 
  select(WL2.cross, Pop.Type:parent.pop, Field_Loc, unique.ID:Surv_Post_Transplant, survey.notes) %>% 
  separate_wider_delim(parent.pop, ") x (", names = c("maternal.pops", "paternal.pops"), cols_remove = FALSE) %>%
  mutate(maternal.pops=str_remove(maternal.pops, "\\("), paternal.pops=str_remove(paternal.pops, "\\)")) 
wl2_surv_F2
```

```
## # A tibble: 435 × 11
##    WL2.cross Pop.Type maternal.pops paternal.pops parent.pop Field_Loc unique.ID
##    <lgl>     <chr>    <chr>         <chr>         <chr>      <chr>     <chr>    
##  1 TRUE      F2       WL2 x BH      WL2 x TM2     (WL2 x BH… C_28_C    251      
##  2 TRUE      F2       TM2 x WL2     SQ3 x WL2     (TM2 x WL… D_37_D    312      
##  3 TRUE      F2       WL2 x TM2     CC x TM2      (WL2 x TM… H_30_D    263      
##  4 TRUE      F2       WL2 x TM2     CC x TM2      (WL2 x TM… D_55_A    254      
##  5 TRUE      F2       WL1 x TM2     WL2 x TM2     (WL1 x TM… D_52_B    326      
##  6 TRUE      F2       WL1 x TM2     WL2 x TM2     (WL1 x TM… E_48_C    328      
##  7 TRUE      F2       TM2 x WL2     YO11 x WL2    (TM2 x WL… F_17_B    377      
##  8 TRUE      F2       TM2 x WL2     TM2           (TM2 x WL… C_28_B    347      
##  9 TRUE      F2       WL2 x DPR     TM2 x WL2     (WL2 x DP… C_42_A    1314     
## 10 TRUE      F2       WL2 x BH      SQ3 x WL2     (WL2 x BH… C_7_C     1172     
## # ℹ 425 more rows
## # ℹ 4 more variables: death.date <chr>, Surv_to_Oct <dbl>,
## #   Surv_Post_Transplant <dbl>, survey.notes <chr>
```

BC1s

``` r
wl2_surv_bc1 <- wl2_surv_F2 %>% 
  filter(WL2.cross=="TRUE") %>% 
  filter(paternal.pops=="WL2"|maternal.pops=="WL2")
wl2_surv_bc1
```

```
## # A tibble: 92 × 11
##    WL2.cross Pop.Type maternal.pops paternal.pops parent.pop Field_Loc unique.ID
##    <lgl>     <chr>    <chr>         <chr>         <chr>      <chr>     <chr>    
##  1 TRUE      F2       WL2 x DPR     WL2           (WL2 x DP… E_7_C     1103     
##  2 TRUE      F2       WL2           DPR x WL2     (WL2) x (… F_27_D    908      
##  3 TRUE      F2       SQ3 x WL2     WL2           (SQ3 x WL… C_3_D     87       
##  4 TRUE      F2       YO11 x WL2    WL2           (YO11 x W… C_16_D    932      
##  5 TRUE      F2       SQ3 x WL2     WL2           (SQ3 x WL… C_22_C    88       
##  6 TRUE      F2       YO11 x WL2    WL2           (YO11 x W… D_9_B     937      
##  7 TRUE      F2       WL2           DPR x WL2     (WL2) x (… E_41_D    904      
##  8 TRUE      F2       YO11 x WL2    WL2           (YO11 x W… E_44_D    940      
##  9 TRUE      F2       YO11 x WL2    WL2           (YO11 x W… F_9_C     930      
## 10 TRUE      F2       TM2 x WL2     WL2           (TM2 x WL… G_14_A    365      
## # ℹ 82 more rows
## # ℹ 4 more variables: death.date <chr>, Surv_to_Oct <dbl>,
## #   Surv_Post_Transplant <dbl>, survey.notes <chr>
```

``` r
xtabs(~Surv_to_Oct+maternal.pops, data=wl2_surv_bc1)
```

```
##            maternal.pops
## Surv_to_Oct LV1 x WL2 SQ3 x WL2 TM2 x WL2 WL2 WL2 x DPR WL2 x TM2 YO11 x WL2
##           0        10        14        10  21         6         1         10
##           1         6         2         4   3         3         0          2
```

``` r
xtabs(~Surv_Post_Transplant+maternal.pops, data=wl2_surv_bc1)
```

```
##                     maternal.pops
## Surv_Post_Transplant LV1 x WL2 SQ3 x WL2 TM2 x WL2 WL2 WL2 x DPR WL2 x TM2
##                    0         0         4         1   5         1         0
##                    1        16        12        13  19         8         1
##                     maternal.pops
## Surv_Post_Transplant YO11 x WL2
##                    0          5
##                    1          7
```

``` r
xtabs(~Surv_to_Oct+paternal.pops, data=wl2_surv_bc1)
```

```
##            paternal.pops
## Surv_to_Oct DPR x WL2 TM2 x WL2 WL2 WV x WL2
##           0         9         4  51        8
##           1         2         0  17        1
```

``` r
xtabs(~Surv_Post_Transplant+paternal.pops, data=wl2_surv_bc1)
```

```
##                     paternal.pops
## Surv_Post_Transplant DPR x WL2 TM2 x WL2 WL2 WV x WL2
##                    0         4         0  11        1
##                    1         7         4  57        8
```

### Calculate proportions of each plant for analysis 

``` r
wl2_surv_F2_props <- wl2_surv_F2 %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F2s just # this out 
  #FOR KEEPING TRACK OF MATERNAL AND PATERNAL POPS
  separate_wider_delim(maternal.pops, " x ", names = c("Parent1", "Parent2"), cols_remove = FALSE, too_few = "align_start") %>%
  separate_wider_delim(paternal.pops, " x ", names = c("Parent3", "Parent4"), cols_remove = FALSE, too_few = "align_start") %>%
  mutate(totalParents=rowSums(!is.na(select(., "Parent1", "Parent2", "Parent3", "Parent4")))) %>% 
  mutate(prop.WL2=str_count(parent.pop, "WL2")/totalParents,
        prop.CC=str_count(parent.pop, "CC")/totalParents,
        prop.BH=str_count(parent.pop, "BH")/totalParents,
        prop.WV=str_count(parent.pop, "WV")/totalParents,
        prop.LV1=str_count(parent.pop, "LV1")/totalParents,
        prop.TM2=str_count(parent.pop, "TM2")/totalParents,
        prop.SQ3=str_count(parent.pop, "SQ3")/totalParents,
        prop.DPR=str_count(parent.pop, "DPR")/totalParents,
        prop.YO11=str_count(parent.pop, "YO11")/totalParents)
  #mutate(WL2prop=if_else(Parent1=="WL2" & Parent2=="WL2" & Parent3=="WL2" & Parent4=="WL2", 1, ))
                         #is there an easier way to do this?)) 
wl2_surv_F2_props
```

```
## # A tibble: 398 × 25
##    WL2.cross Pop.Type Parent1 Parent2 maternal.pops Parent3 Parent4
##    <lgl>     <chr>    <chr>   <chr>   <chr>         <chr>   <chr>  
##  1 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  2 TRUE      F2       TM2     WL2     TM2 x WL2     SQ3     WL2    
##  3 TRUE      F2       WL2     TM2     WL2 x TM2     CC      TM2    
##  4 TRUE      F2       WL2     TM2     WL2 x TM2     CC      TM2    
##  5 TRUE      F2       WL1     TM2     WL1 x TM2     WL2     TM2    
##  6 TRUE      F2       WL1     TM2     WL1 x TM2     WL2     TM2    
##  7 TRUE      F2       TM2     WL2     TM2 x WL2     YO11    WL2    
##  8 TRUE      F2       TM2     WL2     TM2 x WL2     TM2     <NA>   
##  9 TRUE      F2       WL2     DPR     WL2 x DPR     TM2     WL2    
## 10 TRUE      F2       WL2     BH      WL2 x BH      SQ3     WL2    
## # ℹ 388 more rows
## # ℹ 18 more variables: paternal.pops <chr>, parent.pop <chr>, Field_Loc <chr>,
## #   unique.ID <chr>, death.date <chr>, Surv_to_Oct <dbl>,
## #   Surv_Post_Transplant <dbl>, survey.notes <chr>, totalParents <dbl>,
## #   prop.WL2 <dbl>, prop.CC <dbl>, prop.BH <dbl>, prop.WV <dbl>,
## #   prop.LV1 <dbl>, prop.TM2 <dbl>, prop.SQ3 <dbl>, prop.DPR <dbl>,
## #   prop.YO11 <dbl>
```

``` r
summary(wl2_surv_F2_props)
```

```
##  WL2.cross        Pop.Type           Parent1            Parent2         
##  Mode:logical   Length:398         Length:398         Length:398        
##  TRUE:398       Class :character   Class :character   Class :character  
##                 Mode  :character   Mode  :character   Mode  :character  
##                                                                         
##                                                                         
##                                                                         
##  maternal.pops        Parent3            Parent4          paternal.pops     
##  Length:398         Length:398         Length:398         Length:398        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##   parent.pop         Field_Loc          unique.ID          death.date       
##  Length:398         Length:398         Length:398         Length:398        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##   Surv_to_Oct     Surv_Post_Transplant survey.notes        totalParents  
##  Min.   :0.0000   Min.   :0.0000       Length:398         Min.   :3.000  
##  1st Qu.:0.0000   1st Qu.:1.0000       Class :character   1st Qu.:3.000  
##  Median :0.0000   Median :1.0000       Mode  :character   Median :4.000  
##  Mean   :0.1583   Mean   :0.8216                          Mean   :3.588  
##  3rd Qu.:0.0000   3rd Qu.:1.0000                          3rd Qu.:4.000  
##  Max.   :1.0000   Max.   :1.0000                          Max.   :4.000  
##     prop.WL2         prop.CC           prop.BH           prop.WV       
##  Min.   :0.2500   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:0.3333   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
##  Median :0.5000   Median :0.00000   Median :0.00000   Median :0.00000  
##  Mean   :0.4883   Mean   :0.03078   Mean   :0.02136   Mean   :0.06805  
##  3rd Qu.:0.5000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
##  Max.   :0.6667   Max.   :0.25000   Max.   :0.25000   Max.   :0.66667  
##     prop.LV1         prop.TM2         prop.SQ3          prop.DPR      
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000  
##  Median :0.0000   Median :0.0000   Median :0.00000   Median :0.00000  
##  Mean   :0.0379   Mean   :0.1348   Mean   :0.05297   Mean   :0.09862  
##  3rd Qu.:0.0000   3rd Qu.:0.2500   3rd Qu.:0.00000   3rd Qu.:0.25000  
##  Max.   :0.5000   Max.   :0.6667   Max.   :0.50000   Max.   :0.66667  
##    prop.YO11      
##  Min.   :0.00000  
##  1st Qu.:0.00000  
##  Median :0.00000  
##  Mean   :0.04397  
##  3rd Qu.:0.00000  
##  Max.   :0.33333
```

``` r
xtabs(~Surv_to_Oct+prop.WL2, data=wl2_surv_F2_props)
```

```
##            prop.WL2
## Surv_to_Oct 0.25 0.333333333333333 0.5 0.666666666666667
##           0   27                62 174                72
##           1    5                10  28                20
```

### Plot with props

``` r
meansurv_wl2prop <- wl2_surv_F2_props %>% 
  group_by(prop.WL2) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_wl2prop 
```

```
## # A tibble: 4 × 4
##   prop.WL2 N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##      <dbl>  <int>            <dbl>                     <dbl>
## 1    0.25      32            0.156                     0.844
## 2    0.333     72            0.139                     0.889
## 3    0.5      202            0.139                     0.792
## 4    0.667     92            0.217                     0.826
```

``` r
meansurv_wl2prop %>% 
  ggplot(aes(x=prop.WL2, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

``` r
meansurv_CCprop <- wl2_surv_F2_props %>% 
  group_by(prop.CC) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_CCprop 
```

```
## # A tibble: 2 × 4
##   prop.CC N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##     <dbl>  <int>            <dbl>                     <dbl>
## 1    0       349            0.163                     0.834
## 2    0.25     49            0.122                     0.735
```

``` r
meansurv_CCprop %>% 
  ggplot(aes(x=prop.CC, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

``` r
meansurv_BHprop <- wl2_surv_F2_props %>% 
  group_by(prop.BH) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_BHprop 
```

```
## # A tibble: 2 × 4
##   prop.BH N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##     <dbl>  <int>            <dbl>                     <dbl>
## 1    0       364            0.159                     0.819
## 2    0.25     34            0.147                     0.853
```

``` r
meansurv_BHprop %>% 
  ggplot(aes(x=prop.BH, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-3.png)<!-- -->

``` r
meansurv_WVprop <- wl2_surv_F2_props %>% 
  group_by(prop.WV) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_WVprop 
```

```
## # A tibble: 4 × 4
##   prop.WV N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##     <dbl>  <int>            <dbl>                     <dbl>
## 1   0        346           0.153                      0.815
## 2   0.25      11           0.545                      0.727
## 3   0.333      9           0.111                      0.889
## 4   0.667     32           0.0938                     0.906
```

``` r
meansurv_WVprop %>% 
  ggplot(aes(x=prop.WV, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-4.png)<!-- -->

``` r
meansurv_LV1prop <- wl2_surv_F2_props %>% 
  group_by(prop.LV1) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_LV1prop 
```

```
## # A tibble: 4 × 4
##   prop.LV1 N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##      <dbl>  <int>            <dbl>                     <dbl>
## 1    0        345           0.162                      0.820
## 2    0.25      35           0.0286                     0.8  
## 3    0.333     16           0.375                      1    
## 4    0.5        2           0                          0
```

``` r
meansurv_LV1prop %>% 
  ggplot(aes(x=prop.LV1, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-5.png)<!-- -->

``` r
meansurv_TM2prop <- wl2_surv_F2_props %>% 
  group_by(prop.TM2) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_TM2prop 
```

```
## # A tibble: 5 × 4
##   prop.TM2 N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##      <dbl>  <int>            <dbl>                     <dbl>
## 1    0        244           0.156                      0.791
## 2    0.25      88           0.159                      0.864
## 3    0.333     19           0.211                      0.947
## 4    0.5       36           0.167                      0.861
## 5    0.667     11           0.0909                     0.818
```

``` r
meansurv_TM2prop %>% 
  ggplot(aes(x=prop.TM2, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-6.png)<!-- -->

``` r
meansurv_SQ3prop <- wl2_surv_F2_props %>% 
  group_by(prop.SQ3) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_SQ3prop 
```

```
## # A tibble: 4 × 4
##   prop.SQ3 N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##      <dbl>  <int>            <dbl>                     <dbl>
## 1    0        321            0.171                     0.822
## 2    0.25      59            0.102                     0.831
## 3    0.333     16            0.125                     0.75 
## 4    0.5        2            0                         1
```

``` r
meansurv_SQ3prop %>% 
  ggplot(aes(x=prop.SQ3, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-7.png)<!-- -->

``` r
meansurv_DPRprop <- wl2_surv_F2_props %>% 
  group_by(prop.DPR) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_DPRprop 
```

```
## # A tibble: 5 × 4
##   prop.DPR N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##      <dbl>  <int>            <dbl>                     <dbl>
## 1    0        297            0.135                     0.818
## 2    0.25      51            0.235                     0.824
## 3    0.333     20            0.25                      0.75 
## 4    0.5        1            0                         1    
## 5    0.667     29            0.207                     0.897
```

``` r
meansurv_DPRprop %>% 
  ggplot(aes(x=prop.DPR, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-8.png)<!-- -->

``` r
meansurv_YO11prop <- wl2_surv_F2_props %>% 
  group_by(prop.YO11) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
meansurv_YO11prop 
```

```
## # A tibble: 3 × 4
##   prop.YO11 N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##       <dbl>  <int>            <dbl>                     <dbl>
## 1     0        332            0.166                     0.843
## 2     0.25      54            0.111                     0.741
## 3     0.333     12            0.167                     0.583
```

``` r
meansurv_YO11prop %>% 
  ggplot(aes(x=prop.YO11, y=mean_Surv_to_Oct)) + 
  geom_point() +
  labs(y="Survival to Oct 2024") +
  theme_classic() 
```

![](WL2_Single_Time_Surv_files/figure-html/unnamed-chunk-18-9.png)<!-- -->


### with glm


``` r
m5 <- glm(Surv_to_Oct ~ prop.WL2+prop.CC+prop.BH+prop.WV+prop.LV1+prop.TM2+prop.SQ3+prop.DPR+prop.YO11, family = binomial, data=wl2_surv_F2_props)
summary(m5)
```

```
## 
## Call:
## glm(formula = Surv_to_Oct ~ prop.WL2 + prop.CC + prop.BH + prop.WV + 
##     prop.LV1 + prop.TM2 + prop.SQ3 + prop.DPR + prop.YO11, family = binomial, 
##     data = wl2_surv_F2_props)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  -6.1874     2.6421  -2.342   0.0192 *
## prop.WL2      5.8235     2.8664   2.032   0.0422 *
## prop.CC       3.0614     3.6179   0.846   0.3974  
## prop.BH       4.2449     3.6180   1.173   0.2407  
## prop.WV       3.4020     2.6676   1.275   0.2022  
## prop.LV1      2.5130     2.8869   0.870   0.3840  
## prop.TM2      3.8949     2.6369   1.477   0.1397  
## prop.SQ3      0.8332     2.8522   0.292   0.7702  
## prop.DPR      4.4499     2.6180   1.700   0.0892 .
## prop.YO11     1.7066     2.9220   0.584   0.5592  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 347.71  on 397  degrees of freedom
## Residual deviance: 335.87  on 388  degrees of freedom
## AIC: 355.87
## 
## Number of Fisher Scoring iterations: 5
```

``` r
results5 <- c(coef(m5)[1], #intercept
             coef(m5)[2:7] + coef(m5)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results5
```

```
## (Intercept)    prop.WL2     prop.CC     prop.BH     prop.WV    prop.LV1 
## 0.002050902 0.410022215 0.042048276 0.125371791 0.058116859 0.024737022 
##    prop.TM2 
## 0.091748120
```

``` r
summary(m5)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)    prop.WL2     prop.CC     prop.BH     prop.WV    prop.LV1 
##   0.9335213   0.9461627   0.9738630   0.9738652   0.9350875   0.9471974 
##    prop.TM2    prop.SQ3    prop.DPR   prop.YO11 
##   0.9332008   0.9454311   0.9320106   0.9489233
```


``` r
m6 <- glm(Surv_Post_Transplant ~ prop.WL2+prop.CC+prop.BH+prop.WV+prop.LV1+prop.TM2+prop.SQ3+prop.DPR+prop.YO11, family = binomial, data=wl2_surv_F2_props)
summary(m6)
```

```
## 
## Call:
## glm(formula = Surv_Post_Transplant ~ prop.WL2 + prop.CC + prop.BH + 
##     prop.WV + prop.LV1 + prop.TM2 + prop.SQ3 + prop.DPR + prop.YO11, 
##     family = binomial, data = wl2_surv_F2_props)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)  -1.9157     1.8464  -1.038  0.29949   
## prop.WL2      3.6671     2.2285   1.646  0.09986 . 
## prop.CC       0.8512     2.7850   0.306  0.75986   
## prop.BH       4.1822     3.0629   1.365  0.17210   
## prop.WV       4.2136     1.8857   2.235  0.02545 * 
## prop.LV1      3.4397     2.1116   1.629  0.10332   
## prop.TM2      4.9369     1.8753   2.633  0.00847 **
## prop.SQ3      3.0325     1.9450   1.559  0.11897   
## prop.DPR      3.8242     1.8219   2.099  0.03581 * 
## prop.YO11     0.2854     1.9921   0.143  0.88609   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 373.28  on 397  degrees of freedom
## Residual deviance: 353.80  on 388  degrees of freedom
## AIC: 373.8
## 
## Number of Fisher Scoring iterations: 4
```

``` r
results6 <- c(coef(m6)[1], #intercept
             coef(m6)[2:7] + coef(m6)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results6
```

```
## (Intercept)    prop.WL2     prop.CC     prop.BH     prop.WV    prop.LV1 
##   0.1283401   0.8521263   0.2564566   0.9060669   0.9087017   0.8211205 
##    prop.TM2 
##   0.9535240
```

``` r
summary(m6)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)    prop.WL2     prop.CC     prop.BH     prop.WV    prop.LV1 
##   0.8637070   0.9027811   0.9418576   0.9553343   0.8682640   0.8920225 
##    prop.TM2    prop.SQ3    prop.DPR   prop.YO11 
##   0.8670654   0.8749033   0.8607947   0.8799626
```


``` r
m7 <- glm(Surv_to_Oct ~ Parent1, family = binomial, data=wl2_surv_F2_props)
summary(m7)
```

```
## 
## Call:
## glm(formula = Surv_to_Oct ~ Parent1, family = binomial, data = wl2_surv_F2_props)
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -3.182e-14  1.414e+00   0.000    1.000  
## Parent1DPR  -1.046e+00  1.450e+00  -0.721    0.471  
## Parent1LV1  -1.946e+00  1.480e+00  -1.315    0.189  
## Parent1SQ3  -1.705e+00  1.482e+00  -1.150    0.250  
## Parent1TM2  -1.285e+00  1.449e+00  -0.887    0.375  
## Parent1WL1  -2.428e+00  1.537e+00  -1.579    0.114  
## Parent1WL2  -1.780e+00  1.443e+00  -1.233    0.218  
## Parent1WV   -1.758e+00  1.495e+00  -1.176    0.240  
## Parent1YO11 -2.674e+00  1.592e+00  -1.680    0.093 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 347.71  on 397  degrees of freedom
## Residual deviance: 336.58  on 389  degrees of freedom
## AIC: 354.58
## 
## Number of Fisher Scoring iterations: 5
```

``` r
results7 <- c(coef(m7)[1], #intercept
             coef(m7)[2:7] + coef(m7)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results7
```

```
## (Intercept)  Parent1DPR  Parent1LV1  Parent1SQ3  Parent1TM2  Parent1WL1 
##  0.50000000  0.26000000  0.12500000  0.15384615  0.21666667  0.08108108 
##  Parent1WL2 
##  0.14432990
```

``` r
summary(m7)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)  Parent1DPR  Parent1LV1  Parent1SQ3  Parent1TM2  Parent1WL1 
##   0.8044297   0.8100754   0.8145765   0.8149073   0.8097700   0.8230460 
##  Parent1WL2   Parent1WV Parent1YO11 
##   0.8089845   0.8168004   0.8308979
```


``` r
m8 <- glm(Surv_to_Oct ~ Parent2, family = binomial, data=wl2_surv_F2_props)
summary(m8)
```

```
## 
## Call:
## glm(formula = Surv_to_Oct ~ Parent2, family = binomial, data = wl2_surv_F2_props)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.03688    0.61384  -3.318 0.000906 ***
## Parent2CC    0.02198    0.97132   0.023 0.981947    
## Parent2DPR   0.65059    0.83024   0.784 0.433267    
## Parent2TM2   0.37865    0.82123   0.461 0.644740    
## Parent2WL2   0.36126    0.63878   0.566 0.571701    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 283.80  on 328  degrees of freedom
## Residual deviance: 282.97  on 324  degrees of freedom
##   (69 observations deleted due to missingness)
## AIC: 292.97
## 
## Number of Fisher Scoring iterations: 4
```

``` r
results8 <- c(coef(m8)[1], #intercept
             coef(m8)[2:7] + coef(m8)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results8
```

```
## (Intercept)   Parent2CC  Parent2DPR  Parent2TM2  Parent2WL2        <NA> 
##   0.1153846   0.1176471   0.2000000   0.1600000   0.1576763          NA 
##        <NA> 
##          NA
```

``` r
summary(m8)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)   Parent2CC  Parent2DPR  Parent2TM2  Parent2WL2 
##   0.6488165   0.7253818   0.6964059   0.6944975   0.6544781
```


``` r
m9 <- glm(Surv_to_Oct ~ Parent3, family = binomial, data=wl2_surv_F2_props)
summary(m9)
```

```
## 
## Call:
## glm(formula = Surv_to_Oct ~ Parent3, family = binomial, data = wl2_surv_F2_props)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  -0.9163     0.8367  -1.095    0.273  
## Parent3CC    -0.5500     1.0537  -0.522    0.602  
## Parent3DPR   -0.4336     0.9380  -0.462    0.644  
## Parent3LV1   -0.8755     1.3663  -0.641    0.522  
## Parent3SQ3   -2.0281     1.1074  -1.832    0.067 .
## Parent3TM2   -1.1632     0.9421  -1.235    0.217  
## Parent3WL2   -0.5957     0.8623  -0.691    0.490  
## Parent3WV    -0.7419     0.9213  -0.805    0.421  
## Parent3YO11  -0.6592     0.9493  -0.694    0.487  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 347.71  on 397  degrees of freedom
## Residual deviance: 340.16  on 389  degrees of freedom
## AIC: 358.16
## 
## Number of Fisher Scoring iterations: 5
```

``` r
results9 <- c(coef(m9)[1], #intercept
             coef(m9)[2:7] + coef(m9)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results9
```

```
## (Intercept)   Parent3CC  Parent3DPR  Parent3LV1  Parent3SQ3  Parent3TM2 
##   0.2857143   0.1875000   0.2058824   0.1428571   0.0500000   0.1111111 
##  Parent3WL2 
##   0.1806452
```

``` r
summary(m9)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)   Parent3CC  Parent3DPR  Parent3LV1  Parent3SQ3  Parent3TM2 
##   0.6977613   0.7414823   0.7187009   0.7967752   0.7516379   0.7195180 
##  Parent3WL2   Parent3WV Parent3YO11 
##   0.7031442   0.7153087   0.7209721
```


``` r
m10 <- glm(Surv_to_Oct ~ Parent4, family = binomial, data=wl2_surv_F2_props)
summary(m10)
```

```
## 
## Call:
## glm(formula = Surv_to_Oct ~ Parent4, family = binomial, data = wl2_surv_F2_props)
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept) -1.757e+01  3.956e+03  -0.004    0.996
## Parent4CC   -4.442e-08  4.095e+03   0.000    1.000
## Parent4DPR   1.608e+01  3.956e+03   0.004    0.997
## Parent4TM2   1.579e+01  3.956e+03   0.004    0.997
## Parent4WL2   1.588e+01  3.956e+03   0.004    0.997
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 254.60  on 302  degrees of freedom
## Residual deviance: 249.41  on 298  degrees of freedom
##   (95 observations deleted due to missingness)
## AIC: 259.41
## 
## Number of Fisher Scoring iterations: 16
```

``` r
results10 <- c(coef(m10)[1], #intercept
             coef(m10)[2:7] + coef(m10)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results10
```

```
##  (Intercept)    Parent4CC   Parent4DPR   Parent4TM2   Parent4WL2         <NA> 
## 2.350463e-08 2.350463e-08 1.851852e-01 1.451613e-01 1.557789e-01           NA 
##         <NA> 
##           NA
```

``` r
summary(m10)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)   Parent4CC  Parent4DPR  Parent4TM2  Parent4WL2 
##           1           1           1           1           1
```


``` r
m11 <- glm(Surv_Post_Transplant ~ Parent1, family = binomial, data=wl2_surv_F2_props)
summary(m11)
```

```
## 
## Call:
## glm(formula = Surv_Post_Transplant ~ Parent1, family = binomial, 
##     data = wl2_surv_F2_props)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)    14.57     624.19   0.023    0.981
## Parent1DPR    -12.75     624.19  -0.020    0.984
## Parent1LV1    -12.96     624.19  -0.021    0.983
## Parent1SQ3    -13.36     624.19  -0.021    0.983
## Parent1TM2    -12.17     624.19  -0.019    0.984
## Parent1WL1    -13.83     624.19  -0.022    0.982
## Parent1WL2    -13.02     624.19  -0.021    0.983
## Parent1WV     -12.23     624.19  -0.020    0.984
## Parent1YO11   -13.82     624.19  -0.022    0.982
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 373.28  on 397  degrees of freedom
## Residual deviance: 356.25  on 389  degrees of freedom
## AIC: 374.25
## 
## Number of Fisher Scoring iterations: 13
```

``` r
results11 <- c(coef(m11)[1], #intercept
             coef(m11)[2:7] + coef(m11)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results11
```

```
## (Intercept)  Parent1DPR  Parent1LV1  Parent1SQ3  Parent1TM2  Parent1WL1 
##   0.9999995   0.8600000   0.8333333   0.7692308   0.9166667   0.6756757 
##  Parent1WL2 
##   0.8247423
```

``` r
summary(m11)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)  Parent1DPR  Parent1LV1  Parent1SQ3  Parent1TM2  Parent1WL1 
##           1           1           1           1           1           1 
##  Parent1WL2   Parent1WV Parent1YO11 
##           1           1           1
```


``` r
m12 <- glm(Surv_Post_Transplant ~ Parent2, family = binomial, data=wl2_surv_F2_props)
summary(m12)
```

```
## 
## Call:
## glm(formula = Surv_Post_Transplant ~ Parent2, family = binomial, 
##     data = wl2_surv_F2_props)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)   1.7047     0.5436   3.136  0.00171 **
## Parent2CC    -0.1643     0.8368  -0.196  0.84434   
## Parent2DPR   -0.3185     0.7797  -0.408  0.68296   
## Parent2TM2    0.2877     0.8211   0.350  0.72607   
## Parent2WL2   -0.3391     0.5666  -0.598  0.54957   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 321.35  on 328  degrees of freedom
## Residual deviance: 319.93  on 324  degrees of freedom
##   (69 observations deleted due to missingness)
## AIC: 329.93
## 
## Number of Fisher Scoring iterations: 4
```

``` r
results12 <- c(coef(m12)[1], #intercept
             coef(m12)[2:7] + coef(m12)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results12
```

```
## (Intercept)   Parent2CC  Parent2DPR  Parent2TM2  Parent2WL2        <NA> 
##   0.8461538   0.8235294   0.8000000   0.8800000   0.7966805          NA 
##        <NA> 
##          NA
```

``` r
summary(m12)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)   Parent2CC  Parent2DPR  Parent2TM2  Parent2WL2 
##   0.6326395   0.6977886   0.6856185   0.6944738   0.6379855
```


``` r
m13 <- glm(Surv_Post_Transplant ~ Parent3, family = binomial, data=wl2_surv_F2_props)
summary(m13)
```

```
## 
## Call:
## glm(formula = Surv_Post_Transplant ~ Parent3, family = binomial, 
##     data = wl2_surv_F2_props)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)    16.57     906.94   0.018    0.985
## Parent3CC     -14.62     906.94  -0.016    0.987
## Parent3DPR    -15.54     906.94  -0.017    0.986
## Parent3LV1    -16.28     906.94  -0.018    0.986
## Parent3SQ3    -14.62     906.94  -0.016    0.987
## Parent3TM2    -15.08     906.94  -0.017    0.987
## Parent3WL2    -14.96     906.94  -0.016    0.987
## Parent3WV     -14.75     906.94  -0.016    0.987
## Parent3YO11   -15.51     906.94  -0.017    0.986
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 373.28  on 397  degrees of freedom
## Residual deviance: 363.42  on 389  degrees of freedom
## AIC: 381.42
## 
## Number of Fisher Scoring iterations: 15
```

``` r
results13 <- c(coef(m13)[1], #intercept
             coef(m13)[2:7] + coef(m13)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results13
```

```
## (Intercept)   Parent3CC  Parent3DPR  Parent3LV1  Parent3SQ3  Parent3TM2 
##   0.9999999   0.8750000   0.7352941   0.5714286   0.8750000   0.8148148 
##  Parent3WL2 
##   0.8322581
```

``` r
summary(m13)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)   Parent3CC  Parent3DPR  Parent3LV1  Parent3SQ3  Parent3TM2 
##           1           1           1           1           1           1 
##  Parent3WL2   Parent3WV Parent3YO11 
##           1           1           1
```


``` r
m14 <- glm(Surv_Post_Transplant ~ Parent4, family = binomial, data=wl2_surv_F2_props)
summary(m14)
```

```
## 
## Call:
## glm(formula = Surv_Post_Transplant ~ Parent4, family = binomial, 
##     data = wl2_surv_F2_props)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)   -17.57    3956.18  -0.004    0.996
## Parent4CC      17.28    3956.18   0.004    0.997
## Parent4DPR     35.13    4028.78   0.009    0.993
## Parent4TM2     19.34    3956.18   0.005    0.996
## Parent4WL2     19.04    3956.18   0.005    0.996
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 287.05  on 302  degrees of freedom
## Residual deviance: 261.63  on 298  degrees of freedom
##   (95 observations deleted due to missingness)
## AIC: 271.63
## 
## Number of Fisher Scoring iterations: 16
```

``` r
results14 <- c(coef(m14)[1], #intercept
             coef(m14)[2:7] + coef(m14)[1]) %>% #coefficients + intercept
  inv_logit_scaled()
results14
```

```
##  (Intercept)    Parent4CC   Parent4DPR   Parent4TM2   Parent4WL2         <NA> 
## 2.350463e-08 4.285714e-01 1.000000e+00 8.548387e-01 8.140704e-01           NA 
##         <NA> 
##           NA
```

``` r
summary(m14)$coefficients[,2] %>% # get the error from the summary table
  inv_logit_scaled()
```

```
## (Intercept)   Parent4CC  Parent4DPR  Parent4TM2  Parent4WL2 
##           1           1           1           1           1
```



