---
title: "WL2_Single_Time_Surv"
author: "Brandie QC"
date: "2024-12-04"
output: 
  html_document: 
    keep_md: true
---



# Analysis of Survival at WL2 2024 Garden to Plant for BC2 crosses

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
#library(tidymodels)
#library(lmerTest) #for mixed effect models
#conflicted::conflicts_prefer(lmerTest::lmer)
#library(broom.mixed) #tidy method for lmerTest
#library(emmeans) #for post-hoc pairwise comparisons 
#library(naniar) #replaces values with NA
#tidymodels_prefer()
library(brms)
```

```
## Loading required package: Rcpp
## Loading 'brms' package (version 2.22.0). Useful instructions
## can be found by typing help('brms'). A more detailed introduction
## to the package is available through vignette('brms_overview').
## 
## Attaching package: 'brms'
## 
## The following object is masked from 'package:stats':
## 
##     ar
```

``` r
library(tidybayes) #for extracting and visiaulizing brms model output 
```

```
## 
## Attaching package: 'tidybayes'
## 
## The following objects are masked from 'package:brms':
## 
##     dstudent_t, pstudent_t, qstudent_t, rstudent_t
```

``` r
library(modelr) #for data grid

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

## Add Surv columns 

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

### Maternal vs. Paternal Surv

``` r
wl2_surv_F1_meansmat <- wl2_surv_F1 %>% 
  filter(WL2.cross=="TRUE") %>% 
    filter(maternal.pop != "SQ3", maternal.pop != "LV1") %>% #FOR BC2 PREP
  group_by(WL2.cross, maternal.pop) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'WL2.cross'. You can override using the
## `.groups` argument.
```

``` r
wl2_surv_F1_meansmat
```

```
## # A tibble: 5 × 5
## # Groups:   WL2.cross [1]
##   WL2.cross maternal.pop N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##   <lgl>     <chr>         <int>            <dbl>                     <dbl>
## 1 TRUE      BH                2            0                         1    
## 2 TRUE      DPR               4            0                         0.75 
## 3 TRUE      TM2               9            0.222                     0.444
## 4 TRUE      WL2               4            0                         0.75 
## 5 TRUE      WV                9            0.444                     1
```

``` r
wl2_surv_F1_meansmat %>% 
  ggplot(aes(x=fct_reorder(maternal.pop, mean_Surv_to_Oct), y=mean_Surv_to_Oct)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F1s", x="Maternal Populations", y="Survival to Oct 2024") +
  coord_cartesian(ylim = c(0, 1)) +
  geom_text(data = wl2_surv_F1_meansmat, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F1s_SurvtoOct_MatPops_BC2PREP.png", width = 8, height = 6, units = "in")

wl2_surv_F1_meansmat %>% 
  ggplot(aes(x=fct_reorder(maternal.pop, mean_Surv_Post_Transplant), y=mean_Surv_Post_Transplant)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F1s", x="Maternal Populations", y="Survival Two Weeks \n  Post-Transplant") +
  coord_cartesian(ylim = c(0, 1.25)) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00, 1.25)) +
  geom_text(data = wl2_surv_F1_meansmat, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F1s_SurvPostTransplant_MatPops_BC2PREP.png", width = 8, height = 6, units = "in")
```


``` r
wl2_surv_F1_meanspat <- wl2_surv_F1 %>% 
  filter(WL2.cross=="TRUE") %>% 
  group_by(WL2.cross, paternal.pop) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'WL2.cross'. You can override using the
## `.groups` argument.
```

``` r
wl2_surv_F1_meanspat
```

```
## # A tibble: 2 × 5
## # Groups:   WL2.cross [1]
##   WL2.cross paternal.pop N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##   <lgl>     <chr>         <int>            <dbl>                     <dbl>
## 1 TRUE      BH                4            0                         0.75 
## 2 TRUE      WL2              39            0.205                     0.692
```

``` r
wl2_surv_F1_meanspat %>% 
  ggplot(aes(x=fct_reorder(paternal.pop, mean_Surv_to_Oct), y=mean_Surv_to_Oct)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F1s", x="Paternal Populations", y="Survival to Oct 2024") +
  coord_cartesian(ylim = c(0, 1)) +
  geom_text(data = wl2_surv_F1_meanspat, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F1s_SurvtoOct_patPops.png", width = 6, height = 6, units = "in")

wl2_surv_F1_meanspat %>% 
  ggplot(aes(x=fct_reorder(paternal.pop, mean_Surv_Post_Transplant), y=mean_Surv_Post_Transplant)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F1s", x="Paternal Populations", y="Survival Two Weeks \n  Post-Transplant") +
  coord_cartesian(ylim = c(0, 1.25)) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00, 1.25)) +
  geom_text(data = wl2_surv_F1_meanspat, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F1s_SurvPostTransplant_PatPops.png", width = 6, height = 6, units = "in")
```

### Calculate Yes/No Parents

``` r
wl2_surv_F1_binary <- wl2_surv_F1 %>% 
 # filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  mutate(WL2=if_else(str_detect(parent.pop, "WL2"), 1, 0),
         CC=if_else(str_detect(parent.pop, "CC"), 1, 0),
         BH=if_else(str_detect(parent.pop, "BH"), 1, 0),
         WV=if_else(str_detect(parent.pop, "WV"), 1, 0),
         LV1=if_else(str_detect(parent.pop, "LV1"), 1, 0),
         TM2=if_else(str_detect(parent.pop, "TM2"), 1, 0),
         SQ3=if_else(str_detect(parent.pop, "SQ3"), 1, 0),
         DPR=if_else(str_detect(parent.pop, "DPR"), 1, 0),
         YO11=if_else(str_detect(parent.pop, "YO11"), 1, 0)) 
head(wl2_surv_F1_binary, 20)
```

```
## # A tibble: 20 × 20
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
## 11 F1       LV1          TM2          LV1 x TM2  D_45_D    1233      6/25/24   
## 12 F1       LV1          WL2          LV1 x WL2  E_43_C    1259      6/25/24   
## 13 F1       BH           TM2          BH x TM2   E_46_C    594       6/25/24   
## 14 F1       LV1          WL2          LV1 x WL2  F_21_A    1270      6/25/24   
## 15 F1       WL2          BH           WL2 x BH   G_18_B    833       6/25/24   
## 16 F1       TM2          WL2          TM2 x WL2  G_24_A    1277      6/25/24   
## 17 F1       LV1          WL2          LV1 x WL2  G_32_A    1254      6/25/24   
## 18 F1       SQ3          WL2          SQ3 x WL2  G_15_D    644       6/25/24   
## 19 F1       LV1          WL2          LV1 x WL2  H_15_B    1263      6/25/24   
## 20 F1       BH           TM2          BH x TM2   H_30_B    587       6/25/24   
## # ℹ 13 more variables: Surv_to_Oct <dbl>, Surv_Post_Transplant <dbl>,
## #   survey.notes <chr>, WL2.cross <lgl>, WL2 <dbl>, CC <dbl>, BH <dbl>,
## #   WV <dbl>, LV1 <dbl>, TM2 <dbl>, SQ3 <dbl>, DPR <dbl>, YO11 <dbl>
```

``` r
summary(wl2_surv_F1_binary)
```

```
##    Pop.Type         maternal.pop       paternal.pop        parent.pop       
##  Length:103         Length:103         Length:103         Length:103        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##   Field_Loc          unique.ID          death.date         Surv_to_Oct   
##  Length:103         Length:103         Length:103         Min.   :0.000  
##  Class :character   Class :character   Class :character   1st Qu.:0.000  
##  Mode  :character   Mode  :character   Mode  :character   Median :0.000  
##                                                           Mean   :0.165  
##                                                           3rd Qu.:0.000  
##                                                           Max.   :1.000  
##  Surv_Post_Transplant survey.notes       WL2.cross            WL2        
##  Min.   :0.0000       Length:103         Mode :logical   Min.   :0.0000  
##  1st Qu.:1.0000       Class :character   FALSE:60        1st Qu.:0.0000  
##  Median :1.0000       Mode  :character   TRUE :43        Median :0.0000  
##  Mean   :0.7767                                          Mean   :0.4175  
##  3rd Qu.:1.0000                                          3rd Qu.:1.0000  
##  Max.   :1.0000                                          Max.   :1.0000  
##        CC                BH               WV              LV1        
##  Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.00000   Median :0.0000   Median :0.0000   Median :0.0000  
##  Mean   :0.01942   Mean   :0.1359   Mean   :0.3301   Mean   :0.3398  
##  3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
##  Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##       TM2              SQ3               DPR               YO11         
##  Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0.000000  
##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.000000  
##  Median :1.0000   Median :0.00000   Median :0.00000   Median :0.000000  
##  Mean   :0.6699   Mean   :0.03884   Mean   :0.03884   Mean   :0.009709  
##  3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.000000  
##  Max.   :1.0000   Max.   :1.00000   Max.   :1.00000   Max.   :1.000000
```

``` r
#switch to long format?
#could try switching to character?

xtabs(~Surv_to_Oct+WL2, data=wl2_surv_F1_binary)
```

```
##            WL2
## Surv_to_Oct  0  1
##           0 51 35
##           1  9  8
```

``` r
xtabs(~Surv_to_Oct+CC, data=wl2_surv_F1_binary)
```

```
##            CC
## Surv_to_Oct  0  1
##           0 84  2
##           1 17  0
```

``` r
xtabs(~Surv_to_Oct+BH, data=wl2_surv_F1_binary)
```

```
##            BH
## Surv_to_Oct  0  1
##           0 73 13
##           1 16  1
```

``` r
xtabs(~Surv_to_Oct+WV, data=wl2_surv_F1_binary)
```

```
##            WV
## Surv_to_Oct  0  1
##           0 60 26
##           1  9  8
```

``` r
xtabs(~Surv_to_Oct+LV1, data=wl2_surv_F1_binary)
```

```
##            LV1
## Surv_to_Oct  0  1
##           0 57 29
##           1 11  6
```

``` r
xtabs(~Surv_to_Oct+TM2, data=wl2_surv_F1_binary)
```

```
##            TM2
## Surv_to_Oct  0  1
##           0 28 58
##           1  6 11
```

``` r
xtabs(~Surv_to_Oct+SQ3, data=wl2_surv_F1_binary)
```

```
##            SQ3
## Surv_to_Oct  0  1
##           0 82  4
##           1 17  0
```

``` r
xtabs(~Surv_to_Oct+DPR, data=wl2_surv_F1_binary)
```

```
##            DPR
## Surv_to_Oct  0  1
##           0 82  4
##           1 17  0
```

``` r
xtabs(~Surv_to_Oct+YO11, data=wl2_surv_F1_binary)
```

```
##            YO11
## Surv_to_Oct  0  1
##           0 85  1
##           1 17  0
```

### Plot the data

``` r
wl2_surv_F1_binary_long <- wl2_surv_F1_binary %>% 
  pivot_longer(WL2:YO11, names_to = "pop", values_to = "Presence")
head(wl2_surv_F1_binary_long, 15)
```

```
## # A tibble: 15 × 13
##    Pop.Type maternal.pop paternal.pop parent.pop Field_Loc unique.ID death.date
##    <chr>    <chr>        <chr>        <chr>      <chr>     <chr>     <chr>     
##  1 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  2 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  3 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  4 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  5 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  6 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  7 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  8 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
##  9 F1       CC           TM2          CC x TM2   C_38_A    1362      6/18/24   
## 10 F1       BH           TM2          BH x TM2   C_17_D    600       6/18/24   
## 11 F1       BH           TM2          BH x TM2   C_17_D    600       6/18/24   
## 12 F1       BH           TM2          BH x TM2   C_17_D    600       6/18/24   
## 13 F1       BH           TM2          BH x TM2   C_17_D    600       6/18/24   
## 14 F1       BH           TM2          BH x TM2   C_17_D    600       6/18/24   
## 15 F1       BH           TM2          BH x TM2   C_17_D    600       6/18/24   
## # ℹ 6 more variables: Surv_to_Oct <dbl>, Surv_Post_Transplant <dbl>,
## #   survey.notes <chr>, WL2.cross <lgl>, pop <chr>, Presence <dbl>
```

``` r
wl2_surv_F1_binary_long_means <- wl2_surv_F1_binary_long %>% 
  group_by(WL2.cross, pop, Presence) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'WL2.cross', 'pop'. You can override using
## the `.groups` argument.
```

``` r
wl2_surv_F1_binary_long_means
```

```
## # A tibble: 29 × 6
## # Groups:   WL2.cross, pop [18]
##    WL2.cross pop   Presence N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##    <lgl>     <chr>    <dbl>  <int>            <dbl>                     <dbl>
##  1 FALSE     BH           0     52            0.154                     0.865
##  2 FALSE     BH           1      8            0.125                     0.625
##  3 FALSE     CC           0     58            0.155                     0.845
##  4 FALSE     CC           1      2            0                         0.5  
##  5 FALSE     DPR          0     60            0.150                     0.833
##  6 FALSE     LV1          0     36            0.139                     0.861
##  7 FALSE     LV1          1     24            0.167                     0.792
##  8 FALSE     SQ3          0     60            0.150                     0.833
##  9 FALSE     TM2          1     60            0.150                     0.833
## 10 FALSE     WL2          0     60            0.150                     0.833
## # ℹ 19 more rows
```

``` r
wl2_surv_F1_binary_long_means %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  ggplot(aes(x=Presence, y=mean_Surv_to_Oct, fill=Presence)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  #ylim(-0.05, 0.3) +
  labs(title="Survival to October - WL2 F1s") +
  #theme_classic() +
  theme(text=element_text(size=25)) + 
  facet_wrap(~pop)
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F1s_SurvtoOct.png", width = 12, height = 6, units = "in")

wl2_surv_F1_binary_long_means %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  ggplot(aes(x=Presence, y=mean_Surv_Post_Transplant, fill=Presence)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="Survival to Two Weeks Post-Transplant  - WL2 F1s") +
  #theme_classic() +
  theme(text=element_text(size=25)) + 
  facet_wrap(~pop)
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F1s_SurvPostTransplant.png", width = 12, height = 6, units = "in")
```


### Bayesian random
Filter to WL2 F1s Only 
Logit scale reminder: logit(x) = ln(x / (1-x));

``` r
wl2_surv_wl2F1s <- wl2_surv_F1_binary %>% filter(WL2.cross=="TRUE") %>% 
  mutate(Other_Parent=if_else(maternal.pop!="WL2", maternal.pop, paternal.pop))
head(wl2_surv_wl2F1s)
```

```
## # A tibble: 6 × 21
##   Pop.Type maternal.pop paternal.pop parent.pop Field_Loc unique.ID death.date
##   <chr>    <chr>        <chr>        <chr>      <chr>     <chr>     <chr>     
## 1 F1       LV1          WL2          LV1 x WL2  D_52_C    1271      6/18/24   
## 2 F1       TM2          WL2          TM2 x WL2  C_32_B    1279      6/25/24   
## 3 F1       TM2          WL2          TM2 x WL2  D_18_C    1281      6/25/24   
## 4 F1       LV1          WL2          LV1 x WL2  E_43_C    1259      6/25/24   
## 5 F1       LV1          WL2          LV1 x WL2  F_21_A    1270      6/25/24   
## 6 F1       WL2          BH           WL2 x BH   G_18_B    833       6/25/24   
## # ℹ 14 more variables: Surv_to_Oct <dbl>, Surv_Post_Transplant <dbl>,
## #   survey.notes <chr>, WL2.cross <lgl>, WL2 <dbl>, CC <dbl>, BH <dbl>,
## #   WV <dbl>, LV1 <dbl>, TM2 <dbl>, SQ3 <dbl>, DPR <dbl>, YO11 <dbl>,
## #   Other_Parent <chr>
```

``` r
summary(wl2_surv_wl2F1s)
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
##  Surv_Post_Transplant survey.notes       WL2.cross           WL2          CC   
##  Min.   :0.0000       Length:43          Mode:logical   Min.   :1   Min.   :0  
##  1st Qu.:0.0000       Class :character   TRUE:43        1st Qu.:1   1st Qu.:0  
##  Median :1.0000       Mode  :character                  Median :1   Median :0  
##  Mean   :0.6977                                         Mean   :1   Mean   :0  
##  3rd Qu.:1.0000                                         3rd Qu.:1   3rd Qu.:0  
##  Max.   :1.0000                                         Max.   :1   Max.   :0  
##        BH               WV              LV1              TM2        
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
##  Mean   :0.1395   Mean   :0.2093   Mean   :0.2558   Mean   :0.2093  
##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.5000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##       SQ3               DPR               YO11   Other_Parent      
##  Min.   :0.00000   Min.   :0.00000   Min.   :0   Length:43         
##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0   Class :character  
##  Median :0.00000   Median :0.00000   Median :0   Mode  :character  
##  Mean   :0.09302   Mean   :0.09302   Mean   :0                     
##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0                     
##  Max.   :1.00000   Max.   :1.00000   Max.   :0
```

``` r
xtabs(~Surv_to_Oct+CC, data=wl2_surv_wl2F1s) #no CC x WL2 or vice versa 
```

```
##            CC
## Surv_to_Oct  0
##           0 35
##           1  8
```

``` r
xtabs(~Surv_to_Oct+BH, data=wl2_surv_wl2F1s)
```

```
##            BH
## Surv_to_Oct  0  1
##           0 29  6
##           1  8  0
```

``` r
xtabs(~Surv_to_Oct+WV, data=wl2_surv_wl2F1s)
```

```
##            WV
## Surv_to_Oct  0  1
##           0 30  5
##           1  4  4
```

``` r
xtabs(~Surv_to_Oct+LV1, data=wl2_surv_wl2F1s)
```

```
##            LV1
## Surv_to_Oct  0  1
##           0 26  9
##           1  6  2
```

``` r
xtabs(~Surv_to_Oct+TM2, data=wl2_surv_wl2F1s)
```

```
##            TM2
## Surv_to_Oct  0  1
##           0 28  7
##           1  6  2
```

``` r
xtabs(~Surv_to_Oct+SQ3, data=wl2_surv_wl2F1s)
```

```
##            SQ3
## Surv_to_Oct  0  1
##           0 31  4
##           1  8  0
```

``` r
xtabs(~Surv_to_Oct+DPR, data=wl2_surv_wl2F1s)
```

```
##            DPR
## Surv_to_Oct  0  1
##           0 31  4
##           1  8  0
```

``` r
xtabs(~Surv_to_Oct+YO11, data=wl2_surv_wl2F1s) #no YO11 x WL2 or vice versa 
```

```
##            YO11
## Surv_to_Oct  0
##           0 35
##           1  8
```

``` r
xtabs(~Surv_to_Oct+Other_Parent, data=wl2_surv_wl2F1s)
```

```
##            Other_Parent
## Surv_to_Oct BH DPR LV1 SQ3 TM2 WV
##           0  6   4   9   4   7  5
##           1  0   0   2   0   2  4
```

#### Surv to Oct

``` r
surv_parent_binary_bf1 <- brmsformula(Surv_to_Oct ~ (1|Other_Parent))

get_prior(surv_parent_binary_bf1, family = "bernoulli", data = wl2_surv_wl2F1s)
```

```
##                 prior     class      coef        group resp dpar nlpar lb ub
##  student_t(3, 0, 2.5) Intercept                                             
##  student_t(3, 0, 2.5)        sd                                         0   
##  student_t(3, 0, 2.5)        sd           Other_Parent                  0   
##  student_t(3, 0, 2.5)        sd Intercept Other_Parent                  0   
##        source
##       default
##       default
##  (vectorized)
##  (vectorized)
```

``` r
prior1 <- c(set_prior(prior = 'normal(0, 1)', class='Intercept'),
            set_prior(prior = 'normal(0, 1)', class='sd'))
```


``` r
surv_parent_binary_m1 <- brm(surv_parent_binary_bf1, 
                             family = "bernoulli",
                             data = wl2_surv_wl2F1s,
                             cores=4,
                             iter = 5000, #increased iterations b/c complex model
                             sample_prior = "yes", # needed for hypothesis testing
                             control = list(adapt_delta = 0.9),
                             prior=prior1) #increased adapt_delta to help with divergent transitions
```

```
## Compiling Stan program...
```

```
## Trying to compile a simple C file
```

```
## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
## using C compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.3)’
## using SDK: ‘MacOSX15.1.sdk’
## clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Users/bqc/Library/R/arm64/4.4/library/Rcpp/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/unsupported"  -I"/Users/bqc/Library/R/arm64/4.4/library/BH/include" -I"/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/src/"  -I"/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppParallel/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c foo.c -o foo.o
## In file included from <built-in>:1:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/Dense:1:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/Core:19:
## /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: 'cmath' file not found
##   679 | #include <cmath>
##       |          ^~~~~~~
## 1 error generated.
## make: *** [foo.o] Error 1
```

```
## Start sampling
```



``` r
prior_summary(surv_parent_binary_m1)
```

```
##         prior     class      coef        group resp dpar nlpar lb ub
##  normal(0, 1) Intercept                                             
##  normal(0, 1)        sd                                         0   
##  normal(0, 1)        sd           Other_Parent                  0   
##  normal(0, 1)        sd Intercept Other_Parent                  0   
##        source
##          user
##          user
##  (vectorized)
##  (vectorized)
```

``` r
summary(surv_parent_binary_m1)
```

```
##  Family: bernoulli 
##   Links: mu = logit 
## Formula: Surv_to_Oct ~ (1 | Other_Parent) 
##    Data: wl2_surv_wl2F1s (Number of observations: 43) 
##   Draws: 4 chains, each with iter = 5000; warmup = 2500; thin = 1;
##          total post-warmup draws = 10000
## 
## Multilevel Hyperparameters:
## ~Other_Parent (Number of levels: 6) 
##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sd(Intercept)     0.69      0.51     0.03     1.91 1.00     4067     4254
## 
## Regression Coefficients:
##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    -1.35      0.46    -2.26    -0.46 1.00     5941     5022
## 
## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

``` r
#Rhat <1.05 (good!)
#ESS > 1000 (good!)
```


``` r
plot(surv_parent_binary_m1,  nvariables = 3, ask=FALSE) #plots look a little better with prior distribution adjustments 
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

``` r
#pairs(surv_parent_binary_m1)

pp_check(surv_parent_binary_m1)  # posterior predictive checks
```

```
## Using 10 posterior draws for ppc type 'dens_overlay' by default.
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

``` r
#The main use of this function is to check if you model predicts your data accurately (using the estimates). If it does, then you can use that model to generate new data and make accurate predictions.
#light blue = 10 random draws or distributions created by the model
#dark blue = posterior distribution
#some diffs between draws 
```

To calcualte the stats we need to extract the posterior samples, and add the Intercept to each pop random effect, and then compute the stats.


``` r
intercept <- as_draws_df(surv_parent_binary_m1, variable = "b_Intercept") %>% as_tibble() %>% select(starts_with("b"))

r_pops_raw <- as_draws_df(surv_parent_binary_m1, variable = "*r_", regex = TRUE) %>% as_tibble() %>% select(starts_with("r"))

r_pops <- r_pops_raw %>% mutate(across(everything(), ~ .x + intercept$b_Intercept))
```

##### Hypothesis Testing 


``` r
hypothesis(surv_parent_binary_m1,
           class = NULL,
           hypothesis = "r_Other_Parent[LV1,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[L... > 0     -0.1      0.56     -1.1     0.76       0.79
##   Post.Prob Star
## 1      0.44     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m1,
           class = NULL,
           hypothesis = "r_Other_Parent[TM2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[T... > 0    -0.01      0.57       -1     0.92       1.02
##   Post.Prob Star
## 1      0.51     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m1,
           class = NULL,
           hypothesis = "r_Other_Parent[WV,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[W... > 0     0.46      0.61     -0.3     1.61       3.48
##   Post.Prob Star
## 1      0.78     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
This shows that the posterior probability that LV1 increases survivability is 0.43 
Same prob for TM2 is 0.5
Same prob for WV = 0.76

test whether WV increases survival probability *more* than other pops

``` r
hypothesis(surv_parent_binary_m1,
           class = NULL,
           hypothesis = "r_Other_Parent[WV,Intercept] > r_Other_Parent[TM2,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[W... > 0     0.46      0.76    -0.55     1.91        2.6
##   Post.Prob Star
## 1      0.72     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m1,
           class = NULL,
           hypothesis = "r_Other_Parent[WV,Intercept] > r_Other_Parent[LV1,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[W... > 0     0.56      0.77    -0.42     2.05       3.32
##   Post.Prob Star
## 1      0.77     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m1,
           class = NULL,
           hypothesis = "r_Other_Parent[WV,Intercept] > r_Other_Parent[BH,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[W... > 0     0.96      1.14    -0.26     3.22       4.82
##   Post.Prob Star
## 1      0.83     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
This shows that the posterior probability that WV is better than TM2 is 0.71
This shows that the posterior probability that WV is better than LV1 is 0.76
This shows that the posterior probability that WV is better than BH is 0.83

##### Hand calculated model output 

``` r
posterior::summarize_draws(r_pops) %>%
  mutate(across(mean:q95, inv_logit_scaled))
```

```
## # A tibble: 6 × 10
##   variable          mean median    sd   mad     q5   q95  rhat ess_bulk ess_tail
##   <chr>            <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
## 1 r_Other_Parent[… 0.137  0.157 0.710 0.669 0.0277 0.318  1.00    7585.    6707.
## 2 r_Other_Parent[… 0.149  0.167 0.703 0.664 0.0330 0.345  1.00    7896.    7503.
## 3 r_Other_Parent[… 0.190  0.196 0.637 0.624 0.0794 0.352  1.00   13202.    8423.
## 4 r_Other_Parent[… 0.149  0.170 0.705 0.664 0.0326 0.344  1.00    8717.    7479.
## 5 r_Other_Parent[… 0.205  0.209 0.641 0.628 0.0875 0.392  1.00   12945.    8534.
## 6 r_Other_Parent[… 0.291  0.281 0.647 0.643 0.141  0.543  1.00    7657.    8416.
```

``` r
#estimates seem to be a little off...
```

##### Tidybayes tables 

``` r
tibble(Other_Parent = "BH") %>%
  add_epred_draws(surv_parent_binary_m1) %>%
  median_qi(.epred) #median matches above chunk 
```

```
## # A tibble: 1 × 8
##   Other_Parent  .row .epred .lower .upper .width .point .interval
##   <chr>        <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
## 1 BH               1  0.157 0.0169  0.358   0.95 median qi
```

``` r
tibble(Other_Parent = "BH") %>%
  add_epred_draws(surv_parent_binary_m1) %>%
  mean_qi(.epred) #mean does not match above chunk 
```

```
## # A tibble: 1 × 8
##   Other_Parent  .row .epred .lower .upper .width .point .interval
##   <chr>        <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
## 1 BH               1  0.164 0.0169  0.358   0.95 mean   qi
```

``` r
tibble(Other_Parent = "WV") %>%
  add_epred_draws(surv_parent_binary_m1) %>%
  mean_qi(.epred) #mean does not match above chunk 
```

```
## # A tibble: 1 × 8
##   Other_Parent  .row .epred .lower .upper .width .point .interval
##   <chr>        <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
## 1 WV               1  0.305  0.124  0.605   0.95 mean   qi
```

##### Tidy Plot 

``` r
wl2_surv_wl2F1s %>% 
  data_grid(Other_Parent) %>%
  add_epred_draws(surv_parent_binary_m1) %>%
  ggplot(aes(y = Other_Parent, x = .epred)) +
  stat_slab() +
  theme_classic() +
  labs(x="Posterior Survival Predictions", y="Non-WL2 Parent", title="F1 Survival to October") +
  theme(text=element_text(size=25)) 
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F1s_SurvtoOct_PostProb.png", width = 8, height = 6, units = "in")
```

#### Surv Post Transplant

``` r
surv_parent_binary_bf4 <- brmsformula(Surv_Post_Transplant ~ (1|Other_Parent))

get_prior(surv_parent_binary_bf4, family = "bernoulli", data = wl2_surv_wl2F1s)
```

```
##                 prior     class      coef        group resp dpar nlpar lb ub
##  student_t(3, 0, 2.5) Intercept                                             
##  student_t(3, 0, 2.5)        sd                                         0   
##  student_t(3, 0, 2.5)        sd           Other_Parent                  0   
##  student_t(3, 0, 2.5)        sd Intercept Other_Parent                  0   
##        source
##       default
##       default
##  (vectorized)
##  (vectorized)
```

``` r
prior1 <- c(set_prior(prior = 'normal(0, 5)', class='Intercept'),
            set_prior(prior = 'normal(0, 5)', class='sd'))
```


``` r
surv_parent_binary_m4 <- brm(surv_parent_binary_bf4, 
                             family = "bernoulli",
                             data = wl2_surv_wl2F1s,
                             cores=4,
                             iter = 5000, #increased iterations b/c complex model
                             control = list(adapt_delta = 0.9),
                             sample_prior = "yes", # needed for hypothesis testing
                             prior=prior1) #increased adapt_delta to help with divergent transitions
```

```
## Compiling Stan program...
```

```
## Trying to compile a simple C file
```

```
## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
## using C compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.3)’
## using SDK: ‘MacOSX15.1.sdk’
## clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Users/bqc/Library/R/arm64/4.4/library/Rcpp/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/unsupported"  -I"/Users/bqc/Library/R/arm64/4.4/library/BH/include" -I"/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/src/"  -I"/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppParallel/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c foo.c -o foo.o
## In file included from <built-in>:1:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/Dense:1:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/Core:19:
## /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: 'cmath' file not found
##   679 | #include <cmath>
##       |          ^~~~~~~
## 1 error generated.
## make: *** [foo.o] Error 1
```

```
## Start sampling
```

``` r
#Warning: There were 2 divergent transitions after warmup. 
```



``` r
prior_summary(surv_parent_binary_m4)
```

```
##         prior     class      coef        group resp dpar nlpar lb ub
##  normal(0, 5) Intercept                                             
##  normal(0, 5)        sd                                         0   
##  normal(0, 5)        sd           Other_Parent                  0   
##  normal(0, 5)        sd Intercept Other_Parent                  0   
##        source
##          user
##          user
##  (vectorized)
##  (vectorized)
```

``` r
summary(surv_parent_binary_m4)
```

```
##  Family: bernoulli 
##   Links: mu = logit 
## Formula: Surv_Post_Transplant ~ (1 | Other_Parent) 
##    Data: wl2_surv_wl2F1s (Number of observations: 43) 
##   Draws: 4 chains, each with iter = 5000; warmup = 2500; thin = 1;
##          total post-warmup draws = 10000
## 
## Multilevel Hyperparameters:
## ~Other_Parent (Number of levels: 6) 
##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sd(Intercept)     1.49      1.05     0.11     4.26 1.00     2204     3016
## 
## Regression Coefficients:
##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept     1.17      0.86    -0.38     3.11 1.00     2390     1768
## 
## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

``` r
#Rhat <1.05 (good!)
#ESS > 1000 (good!)
```


``` r
plot(surv_parent_binary_m4,  nvariables = 3, ask=FALSE) #plots look a little better with prior distribution adjustments 
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

``` r
#pairs(surv_parent_binary_m4)

pp_check(surv_parent_binary_m4)  # posterior predictive checks
```

```
## Using 10 posterior draws for ppc type 'dens_overlay' by default.
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-25-2.png)<!-- -->

``` r
#The main use of this function is to check if you model predicts your data accurately (using the estimates). If it does, then you can use that model to generate new data and make accurate predictions.
#light blue = 10 random draws or distributions created by the model
#dark blue = posterior distribution
#some diffs between draws 
```

To calculate the stats we need to extract the posterior samples, and add the Intercept to each pop random effect, and then compute the stats.


``` r
intercept <- as_draws_df(surv_parent_binary_m4, variable = "b_Intercept") %>% as_tibble() %>% select(starts_with("b"))

r_pops_raw <- as_draws_df(surv_parent_binary_m4, variable = "*r_", regex = TRUE) %>% as_tibble() %>% select(starts_with("r"))

r_pops <- r_pops_raw %>% mutate(across(everything(), ~ .x + intercept$b_Intercept))
```


##### Hypothesis Testing 


``` r
hypothesis(surv_parent_binary_m4,
           class = NULL,
           hypothesis = "r_Other_Parent[BH,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[B... > 0     0.34      1.07    -1.27     2.18       1.73
##   Post.Prob Star
## 1      0.63     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m4,
           class = NULL,
           hypothesis = "r_Other_Parent[DPR,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[D... > 0     0.04       1.1    -1.73     1.86       1.07
##   Post.Prob Star
## 1      0.52     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m4,
           class = NULL,
           hypothesis = "r_Other_Parent[SQ3,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[S... > 0     0.03      1.08    -1.72      1.8       1.03
##   Post.Prob Star
## 1      0.51     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m4,
           class = NULL,
           hypothesis = "r_Other_Parent[WV,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[W... > 0     1.52      1.62    -0.19     4.55       8.84
##   Post.Prob Star
## 1       0.9     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
This shows that the posterior probability that BH increases survivability is 0.63
Same prob for DPR is 0.51
Same prob for SQ3 is 0.52
Same prob for WV is 0.91

Alternative approach.  Same result, less detail

``` r
sum(r_pops_raw$`r_Other_Parent[BH,Intercept]` > 0) / nrow(r_pops_raw) # posterior probability of BH intercept being greater than 0
```

```
## [1] 0.6343
```

test whether WV increases survival probability *more* than BH

``` r
hypothesis(surv_parent_binary_m4,
           class = NULL,
           hypothesis = "r_Other_Parent[WV,Intercept] > r_Other_Parent[BH,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_Other_Parent[W... > 0     1.18      1.71    -0.86     4.31       3.53
##   Post.Prob Star
## 1      0.78     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
This shows that the posterior probability that WV is better than BH is 0.79


Alternative approach.  Same results, less detail


``` r
sum(r_pops_raw$`r_Other_Parent[WV,Intercept]` > r_pops_raw$`r_Other_Parent[BH,Intercept]`) / nrow(r_pops_raw) # posterior probability of BH intercept being greater than 0
```

```
## [1] 0.7794
```

##### Hand calculated model output 

``` r
posterior::summarize_draws(r_pops) %>%
  mutate(across(mean:q95, inv_logit_scaled))
```

```
## # A tibble: 6 × 10
##   variable           mean median    sd   mad    q5   q95  rhat ess_bulk ess_tail
##   <chr>             <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
## 1 r_Other_Parent[B… 0.819  0.799 0.723 0.701 0.549 0.962  1.00    9149.    6885.
## 2 r_Other_Parent[D… 0.769  0.753 0.727 0.700 0.435 0.949  1.00    9760.    7014.
## 3 r_Other_Parent[L… 0.605  0.610 0.642 0.639 0.364 0.795  1.00    8827.    8596.
## 4 r_Other_Parent[S… 0.767  0.750 0.720 0.695 0.449 0.947  1.00    9022.    7243.
## 5 r_Other_Parent[T… 0.538  0.546 0.662 0.663 0.268 0.768  1.00    6970.    7831.
## 6 r_Other_Parent[W… 0.936  0.911 0.847 0.795 0.681 0.997  1.00    3916.    4771.
```

``` r
#estimates seem to be a little off...
```

##### Tidybayes tables 

``` r
tibble(Other_Parent = "BH") %>%
  add_epred_draws(surv_parent_binary_m4) %>%
  median_qi(.epred) #median matches above chunk 
```

```
## # A tibble: 1 × 8
##   Other_Parent  .row .epred .lower .upper .width .point .interval
##   <chr>        <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
## 1 BH               1  0.799  0.490  0.976   0.95 median qi
```

``` r
tibble(Other_Parent = "BH") %>%
  add_epred_draws(surv_parent_binary_m4) %>%
  mean_qi(.epred) #mean does not match above chunk 
```

```
## # A tibble: 1 × 8
##   Other_Parent  .row .epred .lower .upper .width .point .interval
##   <chr>        <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
## 1 BH               1  0.783  0.490  0.976   0.95 mean   qi
```

``` r
tibble(Other_Parent = "WV") %>%
  add_epred_draws(surv_parent_binary_m4) %>%
  mean_qi(.epred) #mean does not match above chunk 
```

```
## # A tibble: 1 × 8
##   Other_Parent  .row .epred .lower .upper .width .point .interval
##   <chr>        <int>  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
## 1 WV               1  0.883  0.640  0.999   0.95 mean   qi
```

##### Tidy Plot 

``` r
wl2_surv_wl2F1s %>% 
  data_grid(Other_Parent) %>%
  add_epred_draws(surv_parent_binary_m4) %>%
  ggplot(aes(y = Other_Parent, x = .epred)) +
  stat_slab() +
  theme_classic() +
  labs(x="Posterior Survival Predictions", y="Non-WL2 Parent", title="F1 Survival Post-Transplant") +
  theme(text=element_text(size=25)) 
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F1s_SurvPostTrans_PostProb.png", width = 8, height = 6, units = "in")
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

### Calculate yes/no for each parent 

``` r
wl2_surv_F2_binary <- wl2_surv_F2 %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  mutate(WL2=if_else(str_detect(parent.pop, "WL2"), 1, 0),
         CC=if_else(str_detect(parent.pop, "CC"), 1, 0),
         BH=if_else(str_detect(parent.pop, "BH"), 1, 0),
         WV=if_else(str_detect(parent.pop, "WV"), 1, 0),
         LV1=if_else(str_detect(parent.pop, "LV1"), 1, 0),
         TM2=if_else(str_detect(parent.pop, "TM2"), 1, 0),
         SQ3=if_else(str_detect(parent.pop, "SQ3"), 1, 0),
         DPR=if_else(str_detect(parent.pop, "DPR"), 1, 0),
         YO11=if_else(str_detect(parent.pop, "YO11"), 1, 0),
         maternal.WL2=if_else(str_detect(maternal.pops, "WL2"), 1, 0),
         maternal.CC=if_else(str_detect(maternal.pops, "CC"), 1, 0),
         maternal.BH=if_else(str_detect(maternal.pops, "BH"), 1, 0),
         maternal.WV=if_else(str_detect(maternal.pops, "WV"), 1, 0),
         maternal.LV1=if_else(str_detect(maternal.pops, "LV1"), 1, 0),
         maternal.TM2=if_else(str_detect(maternal.pops, "TM2"), 1, 0),
         maternal.SQ3=if_else(str_detect(maternal.pops, "SQ3"), 1, 0),
         maternal.DPR=if_else(str_detect(maternal.pops, "DPR"), 1, 0),
         maternal.YO11=if_else(str_detect(maternal.pops, "YO11"), 1, 0)) %>%
  separate_wider_delim(maternal.pops, " x ", names = c("Parent1", "Parent2"), cols_remove = FALSE, too_few = "align_start") %>%
  separate_wider_delim(paternal.pops, " x ", names = c("Parent3", "Parent4"), cols_remove = FALSE, too_few = "align_start")
head(wl2_surv_F2_binary, 20)
```

```
## # A tibble: 20 × 33
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
## 11 TRUE      F2       WL1     WL2     WL1 x WL2     WL2     CC     
## 12 TRUE      F2       LV1     WL2     LV1 x WL2     YO11    WL2    
## 13 TRUE      F2       YO11    WL2     YO11 x WL2    WV      WL2    
## 14 TRUE      F2       WV      WL2     WV x WL2      WV      <NA>   
## 15 TRUE      F2       YO11    WL2     YO11 x WL2    SQ3     WL2    
## 16 TRUE      F2       DPR     WL2     DPR x WL2     YO11    WL2    
## 17 TRUE      F2       WL2     DPR     WL2 x DPR     WL2     <NA>   
## 18 TRUE      F2       DPR     WL2     DPR x WL2     SQ3     WL2    
## 19 TRUE      F2       WL2     <NA>    WL2           DPR     WL2    
## 20 TRUE      F2       YO11    WL2     YO11 x WL2    WV      WL2    
## # ℹ 26 more variables: paternal.pops <chr>, parent.pop <chr>, Field_Loc <chr>,
## #   unique.ID <chr>, death.date <chr>, Surv_to_Oct <dbl>,
## #   Surv_Post_Transplant <dbl>, survey.notes <chr>, WL2 <dbl>, CC <dbl>,
## #   BH <dbl>, WV <dbl>, LV1 <dbl>, TM2 <dbl>, SQ3 <dbl>, DPR <dbl>, YO11 <dbl>,
## #   maternal.WL2 <dbl>, maternal.CC <dbl>, maternal.BH <dbl>,
## #   maternal.WV <dbl>, maternal.LV1 <dbl>, maternal.TM2 <dbl>,
## #   maternal.SQ3 <dbl>, maternal.DPR <dbl>, maternal.YO11 <dbl>
```

``` r
summary(wl2_surv_F2_binary)
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
##   Surv_to_Oct     Surv_Post_Transplant survey.notes            WL2   
##  Min.   :0.0000   Min.   :0.0000       Length:398         Min.   :1  
##  1st Qu.:0.0000   1st Qu.:1.0000       Class :character   1st Qu.:1  
##  Median :0.0000   Median :1.0000       Mode  :character   Median :1  
##  Mean   :0.1583   Mean   :0.8216                          Mean   :1  
##  3rd Qu.:0.0000   3rd Qu.:1.0000                          3rd Qu.:1  
##  Max.   :1.0000   Max.   :1.0000                          Max.   :1  
##        CC               BH                WV              LV1        
##  Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.00000   Median :0.0000   Median :0.0000  
##  Mean   :0.1231   Mean   :0.08543   Mean   :0.1307   Mean   :0.1332  
##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000  
##       TM2              SQ3              DPR              YO11       
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
##  Mean   :0.3869   Mean   :0.1935   Mean   :0.2538   Mean   :0.1658  
##  3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##   maternal.WL2     maternal.CC       maternal.BH       maternal.WV     
##  Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:1.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
##  Median :1.0000   Median :0.00000   Median :0.00000   Median :0.00000  
##  Mean   :0.8492   Mean   :0.04774   Mean   :0.06533   Mean   :0.08543  
##  3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
##  Max.   :1.0000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
##   maternal.LV1     maternal.TM2     maternal.SQ3      maternal.DPR   
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000   Median :0.00000   Median :0.0000  
##  Mean   :0.1206   Mean   :0.2136   Mean   :0.09799   Mean   :0.1759  
##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000  
##  maternal.YO11    
##  Min.   :0.00000  
##  1st Qu.:0.00000  
##  Median :0.00000  
##  Mean   :0.07789  
##  3rd Qu.:0.00000  
##  Max.   :1.00000
```

``` r
xtabs(~Surv_to_Oct+WL2, data=wl2_surv_F2_binary)
```

```
##            WL2
## Surv_to_Oct   1
##           0 335
##           1  63
```

``` r
xtabs(~Surv_to_Oct+CC, data=wl2_surv_F2_binary)
```

```
##            CC
## Surv_to_Oct   0   1
##           0 292  43
##           1  57   6
```

``` r
xtabs(~Surv_to_Oct+BH, data=wl2_surv_F2_binary)
```

```
##            BH
## Surv_to_Oct   0   1
##           0 306  29
##           1  58   5
```

``` r
xtabs(~Surv_to_Oct+WV, data=wl2_surv_F2_binary)
```

```
##            WV
## Surv_to_Oct   0   1
##           0 293  42
##           1  53  10
```

``` r
xtabs(~Surv_to_Oct+LV1, data=wl2_surv_F2_binary)
```

```
##            LV1
## Surv_to_Oct   0   1
##           0 289  46
##           1  56   7
```

``` r
xtabs(~Surv_to_Oct+TM2, data=wl2_surv_F2_binary)
```

```
##            TM2
## Surv_to_Oct   0   1
##           0 206 129
##           1  38  25
```

``` r
xtabs(~Surv_to_Oct+SQ3, data=wl2_surv_F2_binary)
```

```
##            SQ3
## Surv_to_Oct   0   1
##           0 266  69
##           1  55   8
```

``` r
xtabs(~Surv_to_Oct+DPR, data=wl2_surv_F2_binary)
```

```
##            DPR
## Surv_to_Oct   0   1
##           0 257  78
##           1  40  23
```

``` r
xtabs(~Surv_to_Oct+YO11, data=wl2_surv_F2_binary)
```

```
##            YO11
## Surv_to_Oct   0   1
##           0 277  58
##           1  55   8
```

``` r
xtabs(~Surv_to_Oct+WL2, data=wl2_surv_F2_binary)
```

```
##            WL2
## Surv_to_Oct   1
##           0 335
##           1  63
```

``` r
xtabs(~Surv_to_Oct+CC, data=wl2_surv_F2_binary)
```

```
##            CC
## Surv_to_Oct   0   1
##           0 292  43
##           1  57   6
```

``` r
xtabs(~Surv_to_Oct+BH, data=wl2_surv_F2_binary)
```

```
##            BH
## Surv_to_Oct   0   1
##           0 306  29
##           1  58   5
```

``` r
xtabs(~Surv_to_Oct+WV, data=wl2_surv_F2_binary)
```

```
##            WV
## Surv_to_Oct   0   1
##           0 293  42
##           1  53  10
```

``` r
xtabs(~Surv_to_Oct+LV1, data=wl2_surv_F2_binary)
```

```
##            LV1
## Surv_to_Oct   0   1
##           0 289  46
##           1  56   7
```

``` r
xtabs(~Surv_to_Oct+TM2, data=wl2_surv_F2_binary)
```

```
##            TM2
## Surv_to_Oct   0   1
##           0 206 129
##           1  38  25
```

``` r
xtabs(~Surv_to_Oct+SQ3, data=wl2_surv_F2_binary)
```

```
##            SQ3
## Surv_to_Oct   0   1
##           0 266  69
##           1  55   8
```

``` r
xtabs(~Surv_to_Oct+DPR, data=wl2_surv_F2_binary)
```

```
##            DPR
## Surv_to_Oct   0   1
##           0 257  78
##           1  40  23
```

``` r
xtabs(~Surv_to_Oct+YO11, data=wl2_surv_F2_binary)
```

```
##            YO11
## Surv_to_Oct   0   1
##           0 277  58
##           1  55   8
```

### Plot the data

``` r
wl2_surv_F2_binary_long <- wl2_surv_F2_binary %>% 
  select(WL2.cross:YO11) %>% 
  pivot_longer(WL2:YO11, names_to = "pop", values_to = "Presence")
  #pivot_longer(maternal.WL2:maternal.YO11, names_to = "maternal_pop", values_to = "Maternal_Presence") 
head(wl2_surv_F2_binary_long, 15)
```

```
## # A tibble: 15 × 17
##    WL2.cross Pop.Type Parent1 Parent2 maternal.pops Parent3 Parent4
##    <lgl>     <chr>    <chr>   <chr>   <chr>         <chr>   <chr>  
##  1 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  2 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  3 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  4 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  5 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  6 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  7 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  8 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
##  9 TRUE      F2       WL2     BH      WL2 x BH      WL2     TM2    
## 10 TRUE      F2       TM2     WL2     TM2 x WL2     SQ3     WL2    
## 11 TRUE      F2       TM2     WL2     TM2 x WL2     SQ3     WL2    
## 12 TRUE      F2       TM2     WL2     TM2 x WL2     SQ3     WL2    
## 13 TRUE      F2       TM2     WL2     TM2 x WL2     SQ3     WL2    
## 14 TRUE      F2       TM2     WL2     TM2 x WL2     SQ3     WL2    
## 15 TRUE      F2       TM2     WL2     TM2 x WL2     SQ3     WL2    
## # ℹ 10 more variables: paternal.pops <chr>, parent.pop <chr>, Field_Loc <chr>,
## #   unique.ID <chr>, death.date <chr>, Surv_to_Oct <dbl>,
## #   Surv_Post_Transplant <dbl>, survey.notes <chr>, pop <chr>, Presence <dbl>
```

``` r
wl2_surv_F2_binary_long_means <- wl2_surv_F2_binary_long %>% 
  group_by(WL2.cross, pop, Presence) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'WL2.cross', 'pop'. You can override using
## the `.groups` argument.
```

``` r
wl2_surv_F2_binary_long_means
```

```
## # A tibble: 17 × 6
## # Groups:   WL2.cross, pop [9]
##    WL2.cross pop   Presence N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##    <lgl>     <chr>    <dbl>  <int>            <dbl>                     <dbl>
##  1 TRUE      BH           0    364            0.159                     0.819
##  2 TRUE      BH           1     34            0.147                     0.853
##  3 TRUE      CC           0    349            0.163                     0.834
##  4 TRUE      CC           1     49            0.122                     0.735
##  5 TRUE      DPR          0    297            0.135                     0.818
##  6 TRUE      DPR          1    101            0.228                     0.832
##  7 TRUE      LV1          0    345            0.162                     0.820
##  8 TRUE      LV1          1     53            0.132                     0.830
##  9 TRUE      SQ3          0    321            0.171                     0.822
## 10 TRUE      SQ3          1     77            0.104                     0.818
## 11 TRUE      TM2          0    244            0.156                     0.791
## 12 TRUE      TM2          1    154            0.162                     0.870
## 13 TRUE      WL2          1    398            0.158                     0.822
## 14 TRUE      WV           0    346            0.153                     0.815
## 15 TRUE      WV           1     52            0.192                     0.865
## 16 TRUE      YO11         0    332            0.166                     0.843
## 17 TRUE      YO11         1     66            0.121                     0.712
```

``` r
wl2_surv_F2_binary_long_means %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  ggplot(aes(x=Presence, y=mean_Surv_to_Oct, fill=Presence)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  #ylim(-0.05, 0.3) +
  labs(title="Survival to October - WL2 F2s") +
  #theme_classic() +
  theme(text=element_text(size=25)) + 
  facet_wrap(~pop)
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvtoOct.png", width = 12, height = 6, units = "in")

wl2_surv_F2_binary_long_means %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  ggplot(aes(x=Presence, y=mean_Surv_Post_Transplant, fill=Presence)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="Survival to Two Weeks Post-Transplant  - WL2 F2s") +
  #theme_classic() +
  theme(text=element_text(size=25)) + 
  facet_wrap(~pop)
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-36-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvPostTransplant.png", width = 12, height = 6, units = "in")
```

Just look at maternal and paternal pops 

``` r
unique(wl2_surv_F2_binary$parent.pop)
```

```
##  [1] "(WL2 x BH) x (WL2 x TM2)"   "(TM2 x WL2) x (SQ3 x WL2)" 
##  [3] "(WL2 x TM2) x (CC x TM2)"   "(WL1 x TM2) x (WL2 x TM2)" 
##  [5] "(TM2 x WL2) x (YO11 x WL2)" "(TM2 x WL2) x (TM2)"       
##  [7] "(WL2 x DPR) x (TM2 x WL2)"  "(WL2 x BH) x (SQ3 x WL2)"  
##  [9] "(WL1 x WL2) x (WL2 x CC)"   "(LV1 x WL2) x (YO11 x WL2)"
## [11] "(YO11 x WL2) x (WV x WL2)"  "(WV x WL2) x (WV)"         
## [13] "(YO11 x WL2) x (SQ3 x WL2)" "(DPR x WL2) x (YO11 x WL2)"
## [15] "(WL2 x DPR) x (WL2)"        "(DPR x WL2) x (SQ3 x WL2)" 
## [17] "(WL2) x (DPR x WL2)"        "(DPR) x (DPR x WL2)"       
## [19] "(SQ3 x WL2) x (YO11 x WL2)" "(YO11 x WL2) x (DPR x WL2)"
## [21] "(SQ3 x WL2) x (WL2)"        "(YO11 x WL2) x (WL2)"      
## [23] "(WL2 x CC) x (WL2 x TM2)"   "(LV1 x WL2) x (LV1 x WL2)" 
## [25] "(WL1 x WL2) x (WL2 x TM2)"  "(WV) x (WV x WL2)"         
## [27] "(SQ3 x WL2) x (TM2 x WL2)"  "(SQ3 x WL2) x (LV1 x WL2)" 
## [29] "(TM2 x BH) x (TM2 x WL2)"   "(DPR x WL2) x (TM2 x WL2)" 
## [31] "(DPR x WL2) x (WV x WL2)"   "(TM2 x WL2) x (WL2)"       
## [33] "(WL2) x (WV x WL2)"         "(WL2 x DPR) x (DPR)"       
## [35] "(TM2 x WL2) x (TM2 x BH)"   "(WL1 x WL2) x (BH x WL2)"  
## [37] "(SQ3 x WL2) x (SQ3 x WL2)"  "(LV1 x WL2) x (WL2)"       
## [39] "(DPR) x (WL2 x DPR)"        "(CC x TM2) x (WL2 x TM2)"  
## [41] "(TM2 x WL2) x (TM2 x WL2)"  "(LV1 x WL2) x (WL2 x DPR)" 
## [43] "(LV1 x WL2) x (SQ3 x WL2)"  "(LV1 x WL2) x (TM2 x WL2)" 
## [45] "(SQ3 x WL2) x (DPR x WL2)"  "(WL2 x DPR) x (YO11 x WL2)"
## [47] "(YO11 x WL2) x (WL2 x TM2)" "(WL2) x (TM2 x WL2)"       
## [49] "(TM2) x (TM2 x WL2)"        "(YO11 x WL2) x (TM2 x WL2)"
## [51] "(TM2 x WL2) x (WV x WL2)"   "(DPR x WL2) x (DPR x WL2)" 
## [53] "(WL2 x CC) x (SQ3 x WL2)"   "(TM2 x WL2) x (LV1 x WL2)" 
## [55] "(WL2 x TM2) x (WL2)"        "(WV x WL2) x (WL2 x DPR)"  
## [57] "(TM2 x WL2) x (DPR x WL2)"
```

``` r
unique(wl2_surv_F2_binary$maternal.pops)
```

```
##  [1] "WL2 x BH"   "TM2 x WL2"  "WL2 x TM2"  "WL1 x TM2"  "WL2 x DPR" 
##  [6] "WL1 x WL2"  "LV1 x WL2"  "YO11 x WL2" "WV x WL2"   "DPR x WL2" 
## [11] "WL2"        "DPR"        "SQ3 x WL2"  "WL2 x CC"   "WV"        
## [16] "TM2 x BH"   "CC x TM2"   "TM2"
```

``` r
xtabs(~Surv_to_Oct+maternal.pops, data=wl2_surv_F2_binary)
```

```
##            maternal.pops
## Surv_to_Oct CC x TM2 DPR DPR x WL2 LV1 x WL2 SQ3 x WL2 TM2 TM2 x BH TM2 x WL2
##           0        1  20        17        42        33   2        6        39
##           1        1   6         7         6         6   1        1        11
##            maternal.pops
## Surv_to_Oct WL1 x TM2 WL1 x WL2 WL2 WL2 x BH WL2 x CC WL2 x DPR WL2 x TM2 WV
##           0         6        28  21       17       15        16        14 14
##           1         0         3   3        2        2         4         3  2
##            maternal.pops
## Surv_to_Oct WV x WL2 YO11 x WL2
##           0       15         29
##           1        3          2
```

``` r
unique(wl2_surv_F2_binary$paternal.pops)
```

```
##  [1] "WL2 x TM2"  "SQ3 x WL2"  "CC x TM2"   "YO11 x WL2" "TM2"       
##  [6] "TM2 x WL2"  "WL2 x CC"   "WV x WL2"   "WV"         "WL2"       
## [11] "DPR x WL2"  "LV1 x WL2"  "DPR"        "TM2 x BH"   "BH x WL2"  
## [16] "WL2 x DPR"
```

``` r
xtabs(~Surv_to_Oct+paternal.pops, data=wl2_surv_F2_binary)
```

```
##            paternal.pops
## Surv_to_Oct BH x WL2 CC x TM2 DPR DPR x WL2 LV1 x WL2 SQ3 x WL2 TM2 TM2 x BH
##           0        5       13   3        24         6        38   8        1
##           1        2        3   0         7         1         2   0        0
##            paternal.pops
## Surv_to_Oct TM2 x WL2 WL2 WL2 x CC WL2 x DPR WL2 x TM2 WV WV x WL2 YO11 x WL2
##           0        39  51       14        22        40 15       27         29
##           1         6  17        0         5         6  1        7          6
```

``` r
wl2_surv_F2_binary_for_model <- wl2_surv_F2_binary %>% 
  filter(paternal.pops!="TM2 x BH")
unique(wl2_surv_F2_binary_for_model$paternal.pops)
```

```
##  [1] "WL2 x TM2"  "SQ3 x WL2"  "CC x TM2"   "YO11 x WL2" "TM2"       
##  [6] "TM2 x WL2"  "WL2 x CC"   "WV x WL2"   "WV"         "WL2"       
## [11] "DPR x WL2"  "LV1 x WL2"  "DPR"        "BH x WL2"   "WL2 x DPR"
```

``` r
unique(wl2_surv_F2_binary_for_model$maternal.pops)
```

```
##  [1] "WL2 x BH"   "TM2 x WL2"  "WL2 x TM2"  "WL1 x TM2"  "WL2 x DPR" 
##  [6] "WL1 x WL2"  "LV1 x WL2"  "YO11 x WL2" "WV x WL2"   "DPR x WL2" 
## [11] "WL2"        "DPR"        "SQ3 x WL2"  "WL2 x CC"   "WV"        
## [16] "TM2 x BH"   "CC x TM2"   "TM2"
```

``` r
xtabs(~Surv_to_Oct+paternal.pops, data=wl2_surv_F2_binary_for_model)
```

```
##            paternal.pops
## Surv_to_Oct BH x WL2 CC x TM2 DPR DPR x WL2 LV1 x WL2 SQ3 x WL2 TM2 TM2 x WL2
##           0        5       13   3        24         6        38   8        39
##           1        2        3   0         7         1         2   0         6
##            paternal.pops
## Surv_to_Oct WL2 WL2 x CC WL2 x DPR WL2 x TM2 WV WV x WL2 YO11 x WL2
##           0  51       14        22        40 15       27         29
##           1  17        0         5         6  1        7          6
```

Maternal Pops Plots 

``` r
wl2_surv_F2_binary_for_model_meansmat <- wl2_surv_F2_binary_for_model %>% 
  group_by(WL2.cross, maternal.pops) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'WL2.cross'. You can override using the
## `.groups` argument.
```

``` r
wl2_surv_F2_binary_for_model_meansmat
```

```
## # A tibble: 18 × 5
## # Groups:   WL2.cross [1]
##    WL2.cross maternal.pops N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##    <lgl>     <chr>          <int>            <dbl>                     <dbl>
##  1 TRUE      CC x TM2           2           0.5                        1    
##  2 TRUE      DPR               26           0.231                      0.923
##  3 TRUE      DPR x WL2         24           0.292                      0.792
##  4 TRUE      LV1 x WL2         48           0.125                      0.833
##  5 TRUE      SQ3 x WL2         39           0.154                      0.769
##  6 TRUE      TM2                3           0.333                      1    
##  7 TRUE      TM2 x BH           7           0.143                      0.857
##  8 TRUE      TM2 x WL2         49           0.224                      0.939
##  9 TRUE      WL1 x TM2          6           0                          0.833
## 10 TRUE      WL1 x WL2         31           0.0968                     0.645
## 11 TRUE      WL2               24           0.125                      0.792
## 12 TRUE      WL2 x BH          19           0.105                      0.842
## 13 TRUE      WL2 x CC          17           0.118                      0.824
## 14 TRUE      WL2 x DPR         20           0.2                        0.8  
## 15 TRUE      WL2 x TM2         17           0.176                      0.882
## 16 TRUE      WV                16           0.125                      0.938
## 17 TRUE      WV x WL2          18           0.167                      0.889
## 18 TRUE      YO11 x WL2        31           0.0645                     0.677
```

``` r
wl2_surv_F2_binary_for_model_meansmat %>% 
  ggplot(aes(x=fct_reorder(maternal.pops, mean_Surv_to_Oct), y=mean_Surv_to_Oct)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F2s", x="Maternal Populations", y="Survival to Oct 2024") +
  coord_cartesian(ylim = c(0, 1)) +
  geom_text(data = wl2_surv_F2_binary_for_model_meansmat, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvtoOct_MatPops.png", width = 12, height = 6, units = "in")

wl2_surv_F2_binary_for_model_meansmat %>% 
  ggplot(aes(x=fct_reorder(maternal.pops, mean_Surv_Post_Transplant), y=mean_Surv_Post_Transplant)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F2s", x="Maternal Populations", y="Survival Two Weeks \n  Post-Transplant") +
  coord_cartesian(ylim = c(0, 1.25)) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00, 1.25)) +
  geom_text(data = wl2_surv_F2_binary_for_model_meansmat, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-38-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvPostTransplant_MatPops.png", width = 12, height = 6, units = "in")
```

Paternal Pops Plots

``` r
wl2_surv_F2_binary_for_model_meanspat <- wl2_surv_F2_binary_for_model %>% 
  group_by(WL2.cross, paternal.pops) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'WL2.cross'. You can override using the
## `.groups` argument.
```

``` r
wl2_surv_F2_binary_for_model_meanspat
```

```
## # A tibble: 15 × 5
## # Groups:   WL2.cross [1]
##    WL2.cross paternal.pops N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##    <lgl>     <chr>          <int>            <dbl>                     <dbl>
##  1 TRUE      BH x WL2           7           0.286                      1    
##  2 TRUE      CC x TM2          16           0.188                      0.875
##  3 TRUE      DPR                3           0                          0.667
##  4 TRUE      DPR x WL2         31           0.226                      0.742
##  5 TRUE      LV1 x WL2          7           0.143                      0.571
##  6 TRUE      SQ3 x WL2         40           0.050                      0.875
##  7 TRUE      TM2                8           0                          0.75 
##  8 TRUE      TM2 x WL2         45           0.133                      0.844
##  9 TRUE      WL2               68           0.25                       0.838
## 10 TRUE      WL2 x CC          14           0                          0.429
## 11 TRUE      WL2 x DPR         27           0.185                      1    
## 12 TRUE      WL2 x TM2         46           0.130                      0.848
## 13 TRUE      WV                16           0.0625                     0.875
## 14 TRUE      WV x WL2          34           0.206                      0.853
## 15 TRUE      YO11 x WL2        35           0.171                      0.743
```

``` r
wl2_surv_F2_binary_for_model_meanspat %>% 
  ggplot(aes(x=fct_reorder(paternal.pops, mean_Surv_to_Oct), y=mean_Surv_to_Oct)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F2s", x="Paternal Populations", y="Survival to Oct 2024") +
  coord_cartesian(ylim = c(0, 1)) +
  geom_text(data = wl2_surv_F2_binary_for_model_meanspat, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvtoOct_PatPops.png", width = 12, height = 6, units = "in")

wl2_surv_F2_binary_for_model_meanspat %>% 
  ggplot(aes(x=fct_reorder(paternal.pops, mean_Surv_Post_Transplant), y=mean_Surv_Post_Transplant)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F2s", x="Paternal Populations", y="Survival Two Weeks \n  Post-Transplant") +
  coord_cartesian(ylim = c(0, 1.25)) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00, 1.25)) +
  geom_text(data = wl2_surv_F2_binary_for_model_meanspat, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-39-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvPostTransplant_PatPops.png", width = 12, height = 6, units = "in")
```

Maternal Pops Plots  - BC2 prep

``` r
wl2_surv_F2_binary_for_model_meansmat_BC2PREP <- wl2_surv_F2_binary_for_model %>% 
  group_by(WL2.cross, maternal.pops) %>% 
  filter(maternal.pops == "WL1 x WL2" | maternal.pops == "WL2 x BH" | maternal.pops == "WL2" | 
           maternal.pops == "WV" | maternal.pops == "WV x WL2" | maternal.pops == "WL2 x TM2" |
           maternal.pops == "WL2 x DPR" | maternal.pops == "TM2 x WL2" | maternal.pops == "DPR" |
           maternal.pops == "DPR x WL2" | maternal.pops == "TM2") %>% #FOR BC2 PREP
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'WL2.cross'. You can override using the
## `.groups` argument.
```

``` r
wl2_surv_F2_binary_for_model_meansmat_BC2PREP
```

```
## # A tibble: 11 × 5
## # Groups:   WL2.cross [1]
##    WL2.cross maternal.pops N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##    <lgl>     <chr>          <int>            <dbl>                     <dbl>
##  1 TRUE      DPR               26           0.231                      0.923
##  2 TRUE      DPR x WL2         24           0.292                      0.792
##  3 TRUE      TM2                3           0.333                      1    
##  4 TRUE      TM2 x WL2         49           0.224                      0.939
##  5 TRUE      WL1 x WL2         31           0.0968                     0.645
##  6 TRUE      WL2               24           0.125                      0.792
##  7 TRUE      WL2 x BH          19           0.105                      0.842
##  8 TRUE      WL2 x DPR         20           0.2                        0.8  
##  9 TRUE      WL2 x TM2         17           0.176                      0.882
## 10 TRUE      WV                16           0.125                      0.938
## 11 TRUE      WV x WL2          18           0.167                      0.889
```

``` r
wl2_surv_F2_binary_for_model_meansmat_BC2PREP %>% 
  ggplot(aes(x=fct_reorder(maternal.pops, mean_Surv_to_Oct), y=mean_Surv_to_Oct)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F2s", x="Maternal Populations", y="Survival to Oct 2024") +
  coord_cartesian(ylim = c(0, 1)) +
  geom_text(data = wl2_surv_F2_binary_for_model_meansmat_BC2PREP, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvtoOct_MatPops_BC2PREP.png", width = 8, height = 6, units = "in")

wl2_surv_F2_binary_for_model_meansmat_BC2PREP %>% 
  ggplot(aes(x=fct_reorder(maternal.pops, mean_Surv_Post_Transplant), y=mean_Surv_Post_Transplant)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F2s", x="Maternal Populations", y="Survival Two Weeks \n  Post-Transplant") +
  coord_cartesian(ylim = c(0, 1.25)) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00, 1.25)) +
  geom_text(data = wl2_surv_F2_binary_for_model_meansmat_BC2PREP, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-40-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvPostTransplant_MatPops_BC2PREP.png", width = 8, height = 6, units = "in")
```

Paternal Pops Plots - BC2 prep

``` r
wl2_surv_F2_binary_for_model_meanspat_BC2PREP <- wl2_surv_F2_binary_for_model %>% 
  group_by(WL2.cross, paternal.pops) %>% 
  filter(paternal.pops == "BH x WL2" | paternal.pops == "WL2" | 
           paternal.pops == "WV" | paternal.pops == "WV x WL2" | paternal.pops == "WL2 x TM2" |
           paternal.pops == "WL2 x DPR" | paternal.pops == "TM2 x WL2" | paternal.pops == "DPR" |
           paternal.pops == "DPR x WL2" | paternal.pops == "TM2") %>% #FOR BC2 PREP
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
```

```
## `summarise()` has grouped output by 'WL2.cross'. You can override using the
## `.groups` argument.
```

``` r
wl2_surv_F2_binary_for_model_meanspat_BC2PREP
```

```
## # A tibble: 10 × 5
## # Groups:   WL2.cross [1]
##    WL2.cross paternal.pops N_Surv mean_Surv_to_Oct mean_Surv_Post_Transplant
##    <lgl>     <chr>          <int>            <dbl>                     <dbl>
##  1 TRUE      BH x WL2           7           0.286                      1    
##  2 TRUE      DPR                3           0                          0.667
##  3 TRUE      DPR x WL2         31           0.226                      0.742
##  4 TRUE      TM2                8           0                          0.75 
##  5 TRUE      TM2 x WL2         45           0.133                      0.844
##  6 TRUE      WL2               68           0.25                       0.838
##  7 TRUE      WL2 x DPR         27           0.185                      1    
##  8 TRUE      WL2 x TM2         46           0.130                      0.848
##  9 TRUE      WV                16           0.0625                     0.875
## 10 TRUE      WV x WL2          34           0.206                      0.853
```

``` r
wl2_surv_F2_binary_for_model_meanspat_BC2PREP %>% 
  ggplot(aes(x=fct_reorder(paternal.pops, mean_Surv_to_Oct), y=mean_Surv_to_Oct)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F2s", x="Paternal Populations", y="Survival to Oct 2024") +
  coord_cartesian(ylim = c(0, 1)) +
  geom_text(data = wl2_surv_F2_binary_for_model_meanspat_BC2PREP, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvtoOct_PatPops_BC2PREP.png", width = 12, height = 6, units = "in")

wl2_surv_F2_binary_for_model_meanspat_BC2PREP %>% 
  ggplot(aes(x=fct_reorder(paternal.pops, mean_Surv_Post_Transplant), y=mean_Surv_Post_Transplant)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="WL2 F2s", x="Paternal Populations", y="Survival Two Weeks \n  Post-Transplant") +
  coord_cartesian(ylim = c(0, 1.25)) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00, 1.25)) +
  geom_text(data = wl2_surv_F2_binary_for_model_meanspat_BC2PREP, aes(label = N_Surv), vjust = -1) +
  #theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-41-2.png)<!-- -->

``` r
ggsave("../output/WL2_Traits/F2s_SurvPostTransplant_PatPops_BC2PREP.png", width = 12, height = 6, units = "in")
```

### Bayesian random - maternal and paternal pops 

#### Surv to Oct

``` r
surv_parent_binary_bf2 <- brmsformula(Surv_to_Oct ~ (1|maternal.pops)+(1|paternal.pops))

get_prior(surv_parent_binary_bf2, family = "bernoulli", data = wl2_surv_F2_binary_for_model)
```

```
##                 prior     class      coef         group resp dpar nlpar lb ub
##  student_t(3, 0, 2.5) Intercept                                              
##  student_t(3, 0, 2.5)        sd                                          0   
##  student_t(3, 0, 2.5)        sd           maternal.pops                  0   
##  student_t(3, 0, 2.5)        sd Intercept maternal.pops                  0   
##  student_t(3, 0, 2.5)        sd           paternal.pops                  0   
##  student_t(3, 0, 2.5)        sd Intercept paternal.pops                  0   
##        source
##       default
##       default
##  (vectorized)
##  (vectorized)
##  (vectorized)
##  (vectorized)
```


``` r
surv_parent_binary_m2 <- brm(surv_parent_binary_bf2, 
                             family = "bernoulli",
                             data = wl2_surv_F2_binary_for_model,
                             cores=4,
                             iter = 4000, #increased iterations b/c complex model
                             control = list(adapt_delta = 0.9),
                             sample_prior = "yes", # needed for hypothesis testing
                             prior=prior1) #increased adapt_delta to help with divergent transitions
```

```
## Compiling Stan program...
```

```
## Trying to compile a simple C file
```

```
## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
## using C compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.3)’
## using SDK: ‘MacOSX15.1.sdk’
## clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Users/bqc/Library/R/arm64/4.4/library/Rcpp/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/unsupported"  -I"/Users/bqc/Library/R/arm64/4.4/library/BH/include" -I"/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/src/"  -I"/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppParallel/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c foo.c -o foo.o
## In file included from <built-in>:1:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/Dense:1:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/Core:19:
## /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: 'cmath' file not found
##   679 | #include <cmath>
##       |          ^~~~~~~
## 1 error generated.
## make: *** [foo.o] Error 1
```

```
## Start sampling
```

```
## Warning: There were 1 divergent transitions after warmup. See
## https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
## to find out why this is a problem and how to eliminate them.
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```


``` r
prior_summary(surv_parent_binary_m2)
```

```
##         prior     class      coef         group resp dpar nlpar lb ub
##  normal(0, 5) Intercept                                              
##  normal(0, 5)        sd                                          0   
##  normal(0, 5)        sd           maternal.pops                  0   
##  normal(0, 5)        sd Intercept maternal.pops                  0   
##  normal(0, 5)        sd           paternal.pops                  0   
##  normal(0, 5)        sd Intercept paternal.pops                  0   
##        source
##          user
##          user
##  (vectorized)
##  (vectorized)
##  (vectorized)
##  (vectorized)
```

``` r
summary(surv_parent_binary_m2)
```

```
## Warning: There were 1 divergent transitions after warmup. Increasing
## adapt_delta above 0.9 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
##  Family: bernoulli 
##   Links: mu = logit 
## Formula: Surv_to_Oct ~ (1 | maternal.pops) + (1 | paternal.pops) 
##    Data: wl2_surv_F2_binary_for_model (Number of observations: 397) 
##   Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup draws = 8000
## 
## Multilevel Hyperparameters:
## ~maternal.pops (Number of levels: 18) 
##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sd(Intercept)     0.29      0.21     0.01     0.77 1.00     2860     4176
## 
## ~paternal.pops (Number of levels: 15) 
##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sd(Intercept)     0.47      0.29     0.04     1.17 1.00     1876     2995
## 
## Regression Coefficients:
##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    -1.81      0.25    -2.37    -1.37 1.00     3902     2963
## 
## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

``` r
#Rhat <1.05 (good!)
#ESS > 1000 (good!)
```


``` r
plot(surv_parent_binary_m2,  nvariables = 3, ask=FALSE) #plots look better
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

``` r
#pairs(surv_parent_binary_m2)

pp_check(surv_parent_binary_m2)  # posterior predictive checks
```

```
## Using 10 posterior draws for ppc type 'dens_overlay' by default.
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-45-2.png)<!-- -->

``` r
#some draws differ from posterior distribution 
```

To calcualte the stats we need to extract the posterior samples, and add the Intercept to each pop random effect, and then compute the stats.


``` r
intercept <- as_draws_df(surv_parent_binary_m2, variable = "b_Intercept") %>% as_tibble() %>% select(starts_with("b"))

r_pops_raw <- as_draws_df(surv_parent_binary_m2, variable = "*r_", regex = TRUE) %>% as_tibble() %>% select(starts_with("r"))

r_pops <- r_pops_raw %>% mutate(across(everything(), ~ .x + intercept$b_Intercept))
```

##### Hypothesis Testing 


``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_maternal.pops[CC.x.TM2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.08      0.36    -0.43     0.72       1.38
##   Post.Prob Star
## 1      0.58     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_maternal.pops[DPR.x.WL2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.23      0.32    -0.15     0.87       3.09
##   Post.Prob Star
## 1      0.76     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_maternal.pops[TM2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.07      0.33    -0.41     0.68        1.3
##   Post.Prob Star
## 1      0.56     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
This shows that the posterior probability that CC x TM2 (mom) increases survivability is 0.58 
Same prob for DPR x WL2 (MOM) is 0.76
Same prob for TM2 (mom) = 0.57


``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_paternal.pops[BH.x.WL2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0      0.2      0.49    -0.48     1.08       1.89
##   Post.Prob Star
## 1      0.65     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_paternal.pops[DPR.x.WL2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.27       0.4    -0.26     1.03       3.07
##   Post.Prob Star
## 1      0.75     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_paternal.pops[WL2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.45      0.38    -0.04     1.13       9.74
##   Post.Prob Star
## 1      0.91     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
posterior probability that CC x TM2 (dad) increases survivability is 0.65
Same prob for DPR x WL2 (dad) is 0.76
Same prob for TM2 (dad) = 0.92


test whether DPR x WL2 increases survival probability *more* than other pops

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_maternal.pops[DPR.x.WL2,Intercept] > r_maternal.pops[WL2,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.31      0.46    -0.22     1.24       2.95
##   Post.Prob Star
## 1      0.75     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_maternal.pops[DPR.x.WL2,Intercept] > r_maternal.pops[DPR,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.13      0.39    -0.41     0.87       1.63
##   Post.Prob Star
## 1      0.62     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
This shows that the posterior probability that DPR x WL2 is better than WL2 is 0.75 (MOMS)
posterior probability that DPR x WL2 is better than DPR is 0.61 (MOMS)

test whether DPR x WL2 increases survival probability *more* than other pops

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_paternal.pops[DPR.x.WL2,Intercept] > r_paternal.pops[WV,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.51      0.66     -0.3     1.75       3.89
##   Post.Prob Star
## 1       0.8     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_paternal.pops[WL2,Intercept] > r_paternal.pops[DPR.x.WL2,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.18      0.42    -0.49      0.9       2.03
##   Post.Prob Star
## 1      0.67     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
posterior probability that DPR x WL2 is better than WV is 0.8 (DADS)
posterior probability that WL2 is better than DPR x WL2 is 0.67 (DADS)

Maternal vs. paternal 

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_maternal.pops[DPR,Intercept] > r_paternal.pops[DPR,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.21      0.58    -0.65     1.24       1.81
##   Post.Prob Star
## 1      0.64     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_maternal.pops[TM2,Intercept] > r_paternal.pops[TM2,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.34      0.64     -0.5     1.53       2.37
##   Post.Prob Star
## 1       0.7     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_maternal.pops[WV,Intercept] > r_paternal.pops[WV,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.17      0.54    -0.61     1.12       1.61
##   Post.Prob Star
## 1      0.62     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m2,
           class = NULL,
           hypothesis = "r_paternal.pops[WL2,Intercept] > r_maternal.pops[WL2,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.54       0.5    -0.14     1.42       7.54
##   Post.Prob Star
## 1      0.88     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

##### Hand calculated model output 

``` r
posterior::summarize_draws(r_pops) %>%
  mutate(across(mean:q95, inv_logit_scaled))
```

```
## # A tibble: 33 × 10
##    variable         mean median    sd   mad     q5   q95  rhat ess_bulk ess_tail
##    <chr>           <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
##  1 r_maternal.pop… 0.151  0.150 0.606 0.582 0.0822 0.264  1.00    5928.    5220.
##  2 r_maternal.pop… 0.152  0.153 0.587 0.574 0.0917 0.240  1.00    5535.    4378.
##  3 r_maternal.pop… 0.170  0.166 0.591 0.581 0.106  0.282  1.00    4872.    4477.
##  4 r_maternal.pop… 0.128  0.133 0.586 0.577 0.0717 0.190  1.00    4499.    4031.
##  5 r_maternal.pop… 0.136  0.140 0.584 0.574 0.0783 0.204  1.00    4740.    3785.
##  6 r_maternal.pop… 0.149  0.149 0.599 0.580 0.0843 0.252  1.00    5422.    4479.
##  7 r_maternal.pop… 0.141  0.144 0.601 0.582 0.0755 0.234  1.00    6503.    4804.
##  8 r_maternal.pop… 0.164  0.162 0.579 0.572 0.105  0.253  1.00    4903.    5379.
##  9 r_maternal.pop… 0.130  0.137 0.605 0.583 0.0653 0.208  1.00    5886.    4000.
## 10 r_maternal.pop… 0.130  0.135 0.590 0.578 0.0711 0.199  1.00    6301.    5021.
## # ℹ 23 more rows
```

``` r
#estimates seem to be a little off...
```

##### Tidybayes tables 

``` r
#tibble(maternal.pops = "CC.x.TM2") %>%
 # add_epred_draws(surv_parent_binary_m2) %>%
  #median_qi(.epred) 
#CAN'T USE THIS CODE FOR F2s because it wants info about maternal and paternal pops (pairs)
```

##### Tidy Plot 
NEED TO FIGURE OUT HOW TO MAKE THIS PLOT WHEN TO RANDOM EFFECTS IN MODEL 

``` r
wl2_surv_F2_binary_for_model %>% 
  data_grid(maternal.pops, paternal.pops) %>%
  add_epred_draws(surv_parent_binary_m2) %>%
  ggplot(aes(y = maternal.pops, x = .epred)) +
  stat_slab() +
  theme_classic() +
  labs(x="Posterior Survival Predictions", y="Maternal Parent", title="F1 Survival to October") +
  theme(text=element_text(size=25)) 
ggsave("../output/WL2_Traits/F2s_SurvtoOct_PostProb.png", width = 8, height = 6, units = "in")
```

#### Surv Post Transplant

``` r
surv_parent_binary_bf5 <- brmsformula(Surv_Post_Transplant ~ (1|maternal.pops)+(1|paternal.pops))

get_prior(surv_parent_binary_bf5, family = "bernoulli", data = wl2_surv_F2_binary_for_model)
```

```
##                 prior     class      coef         group resp dpar nlpar lb ub
##  student_t(3, 0, 2.5) Intercept                                              
##  student_t(3, 0, 2.5)        sd                                          0   
##  student_t(3, 0, 2.5)        sd           maternal.pops                  0   
##  student_t(3, 0, 2.5)        sd Intercept maternal.pops                  0   
##  student_t(3, 0, 2.5)        sd           paternal.pops                  0   
##  student_t(3, 0, 2.5)        sd Intercept paternal.pops                  0   
##        source
##       default
##       default
##  (vectorized)
##  (vectorized)
##  (vectorized)
##  (vectorized)
```

``` r
#excluded plants with only 3 parents 
```


``` r
surv_parent_binary_m5 <- brm(surv_parent_binary_bf5, 
                             family = "bernoulli",
                             data = wl2_surv_F2_binary_for_model,
                             cores=4,
                             iter = 5000, #increased iterations b/c complex model
                             control = list(adapt_delta = 0.9),
                             sample_prior = "yes", # needed for hypothesis testing
                             prior=prior1) #increased adapt_delta to help with divergent transitions
```

```
## Compiling Stan program...
```

```
## Trying to compile a simple C file
```

```
## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
## using C compiler: ‘Apple clang version 16.0.0 (clang-1600.0.26.3)’
## using SDK: ‘MacOSX15.1.sdk’
## clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Users/bqc/Library/R/arm64/4.4/library/Rcpp/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/unsupported"  -I"/Users/bqc/Library/R/arm64/4.4/library/BH/include" -I"/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/src/"  -I"/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/RcppParallel/include/"  -I"/Users/bqc/Library/R/arm64/4.4/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c foo.c -o foo.o
## In file included from <built-in>:1:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/Dense:1:
## In file included from /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/Core:19:
## /Users/bqc/Library/R/arm64/4.4/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: 'cmath' file not found
##   679 | #include <cmath>
##       |          ^~~~~~~
## 1 error generated.
## make: *** [foo.o] Error 1
```

```
## Start sampling
```

```
## Warning: There were 1 divergent transitions after warmup. See
## https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
## to find out why this is a problem and how to eliminate them.
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```


``` r
prior_summary(surv_parent_binary_m5)
```

```
##         prior     class      coef         group resp dpar nlpar lb ub
##  normal(0, 5) Intercept                                              
##  normal(0, 5)        sd                                          0   
##  normal(0, 5)        sd           maternal.pops                  0   
##  normal(0, 5)        sd Intercept maternal.pops                  0   
##  normal(0, 5)        sd           paternal.pops                  0   
##  normal(0, 5)        sd Intercept paternal.pops                  0   
##        source
##          user
##          user
##  (vectorized)
##  (vectorized)
##  (vectorized)
##  (vectorized)
```

``` r
summary(surv_parent_binary_m5)
```

```
## Warning: There were 1 divergent transitions after warmup. Increasing
## adapt_delta above 0.9 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
##  Family: bernoulli 
##   Links: mu = logit 
## Formula: Surv_Post_Transplant ~ (1 | maternal.pops) + (1 | paternal.pops) 
##    Data: wl2_surv_F2_binary_for_model (Number of observations: 397) 
##   Draws: 4 chains, each with iter = 5000; warmup = 2500; thin = 1;
##          total post-warmup draws = 10000
## 
## Multilevel Hyperparameters:
## ~maternal.pops (Number of levels: 18) 
##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sd(Intercept)     0.44      0.24     0.04     0.96 1.00     3150     3455
## 
## ~paternal.pops (Number of levels: 15) 
##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sd(Intercept)     0.66      0.35     0.07     1.47 1.00     2036     2419
## 
## Regression Coefficients:
##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept     1.62      0.29     1.06     2.21 1.00     5702     4760
## 
## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

``` r
#Rhat <1.05 (good!)
#ESS > 1000 (good!)
```


``` r
plot(surv_parent_binary_m5,  nvariables = 3, ask=FALSE) #plots look better
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-58-1.png)<!-- -->

``` r
#pairs(surv_parent_binary_m5)

pp_check(surv_parent_binary_m5)  # posterior predictive checks
```

```
## Using 10 posterior draws for ppc type 'dens_overlay' by default.
```

![](WL2_Single_Time_Surv_BayesRandom_files/figure-html/unnamed-chunk-58-2.png)<!-- -->

``` r
#some draws differ from posterior distribution 
```

To calculate the stats we need to extract the posterior samples, and add the Intercept to each pop random effect, and then compute the stats.


``` r
intercept <- as_draws_df(surv_parent_binary_m5, variable = "b_Intercept") %>% as_tibble() %>% select(starts_with("b"))

r_pops_raw <- as_draws_df(surv_parent_binary_m5, variable = "*r_", regex = TRUE) %>% as_tibble() %>% select(starts_with("r"))

r_pops <- r_pops_raw %>% mutate(across(everything(), ~ .x + intercept$b_Intercept))
```

##### Hypothesis Testing 


``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_maternal.pops[TM2.x.WL2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.49      0.43    -0.06      1.3       8.93
##   Post.Prob Star
## 1       0.9     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_maternal.pops[CC.x.TM2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.06      0.48     -0.7     0.88       1.21
##   Post.Prob Star
## 1      0.55     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_maternal.pops[TM2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0      0.1      0.49    -0.63     0.96       1.37
##   Post.Prob Star
## 1      0.58     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_maternal.pops[WV,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.23      0.45    -0.39     1.09       2.19
##   Post.Prob Star
## 1      0.69     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_maternal.pops[DPR,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.22      0.41    -0.35     0.97       2.36
##   Post.Prob Star
## 1       0.7     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
posterior probability that TM2 x WL2 (MOM) increases survivability is 0.9
Same prob for CC x TM2 (MOM) is 0.54
Same prob for TM2 (MOM) is 0.59
Same prob for WV (MOM) is 0.7
Same prob for DPR (MOM) = 0.7


``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_paternal.pops[WL2.x.DPR,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.88      0.75    -0.04     2.28      13.08
##   Post.Prob Star
## 1      0.93     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_paternal.pops[BH.x.WL2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.52      0.71    -0.36     1.85       3.51
##   Post.Prob Star
## 1      0.78     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_paternal.pops[WV,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.14      0.55     -0.7     1.09       1.43
##   Post.Prob Star
## 1      0.59     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_paternal.pops[WL2,Intercept] > 0",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0      0.1      0.37    -0.49     0.73       1.53
##   Post.Prob Star
## 1       0.6     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
posterior probability that WL2 x DPR (DAD) increases survivability is 0.93
Same prob for BH x WL2 (DAD) is 0.79
Same prob for WV (DAD) is 0.6
Same prob for WL2 (DAD) = 0.61

test whether TM2 x WL2 increases survival probability *more* than other pops

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_maternal.pops[TM2.x.WL2,Intercept] > r_maternal.pops[WL2,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.55      0.56    -0.18     1.57       5.77
##   Post.Prob Star
## 1      0.85     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_maternal.pops[TM2.x.WL2,Intercept] > r_maternal.pops[WV,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_maternal.pops[... > 0     0.26      0.54    -0.56     1.22       2.21
##   Post.Prob Star
## 1      0.69     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
This shows that the posterior probability that TM2 x WL2 is better than WL2 is 0.85 (MOMS)
posterior probability that TM2 x WL2 is better than WV is 0.7 (MOMS)


``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_paternal.pops[WL2.x.DPR,Intercept] > r_paternal.pops[WL2,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.78      0.79    -0.23     2.21       6.51
##   Post.Prob Star
## 1      0.87     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```

``` r
hypothesis(surv_parent_binary_m5,
           class = NULL,
           hypothesis = "r_paternal.pops[WL2.x.DPR,Intercept] > r_paternal.pops[BH.x.WL2,Intercept]",
           scope = "standard")
```

```
## Hypothesis Tests for class :
##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
## 1 (r_paternal.pops[... > 0     0.36      0.82    -0.88     1.77       2.14
##   Post.Prob Star
## 1      0.68     
## ---
## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
## Posterior probabilities of point hypotheses assume equal prior probabilities.
```
This shows that the posterior probability that WL2 x DPR is better than WL2 is 0.87 (DADS)
posterior probability that WL2 x DPR is better than BH X WL2 is 0.68 (DADS)

##### Hand calculated model output 

``` r
posterior::summarize_draws(r_pops) %>%
  mutate(across(mean:q95, inv_logit_scaled))
```

```
## # A tibble: 33 × 10
##    variable          mean median    sd   mad    q5   q95  rhat ess_bulk ess_tail
##    <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
##  1 r_maternal.pops… 0.843  0.839 0.637 0.615 0.694 0.935  1.00    9617.    6542.
##  2 r_maternal.pops… 0.863  0.859 0.620 0.610 0.755 0.937  1.00    7413.    7422.
##  3 r_maternal.pops… 0.826  0.826 0.602 0.596 0.708 0.904  1.00    9140.    7187.
##  4 r_maternal.pops… 0.834  0.832 0.592 0.587 0.734 0.904  1.00    8404.    7489.
##  5 r_maternal.pops… 0.810  0.811 0.594 0.589 0.696 0.889  1.00    8082.    6601.
##  6 r_maternal.pops… 0.849  0.843 0.638 0.616 0.710 0.938  1.00    9368.    7047.
##  7 r_maternal.pops… 0.839  0.836 0.623 0.609 0.703 0.924  1.00    9451.    7486.
##  8 r_maternal.pops… 0.892  0.887 0.623 0.619 0.801 0.953  1.00    5318.    7006.
##  9 r_maternal.pops… 0.831  0.830 0.624 0.610 0.686 0.921  1.00    9935.    7134.
## 10 r_maternal.pops… 0.775  0.779 0.610 0.607 0.615 0.873  1.00    5873.    7303.
## # ℹ 23 more rows
```

``` r
#estimates seem to be a little off...
```

##### Tidybayes tables 

``` r
#tibble(Other_Parent = "BH") %>%
 # add_epred_draws(surv_parent_binary_m5) %>%
  #median_qi(.epred) 
#CAN'T USE THIS CODE FOR F2s because it wants info about maternal and paternal pops (pairs)
```

NEED TO FIGURE OUT HOW TO MAKE THIS PLOT WHEN TO RANDOM EFFECTS IN MODEL 

``` r
wl2_surv_F2_binary_for_model %>% 
  data_grid(Other_Parent) %>%
  add_epred_draws(surv_parent_binary_m5) %>%
  ggplot(aes(y = Other_Parent, x = .epred)) +
  stat_slab() +
  theme_classic() +
  labs(x="Posterior Survival Predictions", y="Non-WL2 Parent", title="F1 Survival to October") +
  theme(text=element_text(size=25)) 
ggsave("../output/WL2_Traits/F2s_SurvtoOct_PostProb.png", width = 8, height = 6, units = "in")
```

### MATERNAL POPS (YES/NO) - Plot

``` r
wl2_surv_F2_MAT_binary_long <- wl2_surv_F2_binary %>% 
  select(WL2.cross:survey.notes, maternal.WL2:maternal.YO11) %>% 
  pivot_longer(maternal.WL2:maternal.YO11, names_to = "maternal_pop", values_to = "Maternal_Presence") 
head(wl2_surv_F2_MAT_binary_long, 15)

wl2_surv_F2_MAT_binary_long_means <- wl2_surv_F2_MAT_binary_long %>% 
  group_by(WL2.cross, maternal_pop, Maternal_Presence) %>% 
  summarise(N_Surv = sum(!is.na(Surv_to_Oct)), 
            mean_Surv_to_Oct = mean(Surv_to_Oct,na.rm=(TRUE)), 
            mean_Surv_Post_Transplant = mean(Surv_Post_Transplant,na.rm=(TRUE)))
wl2_surv_F2_MAT_binary_long_means

wl2_surv_F2_MAT_binary_long_means %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  ggplot(aes(x=Maternal_Presence, y=mean_Surv_to_Oct, fill=Maternal_Presence)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  #ylim(-0.05, 0.3) +
  labs(title="Survival to October - WL2 F2ss - Maternal Pops") +
  #theme_classic() +
  facet_wrap(~maternal_pop)

wl2_surv_F2_MAT_binary_long_means %>% 
  filter(WL2.cross=="TRUE") %>% #if want TM2 F1s just # this out 
  ggplot(aes(x=Maternal_Presence, y=mean_Surv_Post_Transplant, fill=Maternal_Presence)) +
  geom_col(width = 0.7,position = position_dodge(0.75), colour="black") +
  labs(title="Survival to Two Weeks Post-Transplant  - WL2 F2s - Maternal Pops") +
  #theme_classic() +
  facet_wrap(~maternal_pop)
```
