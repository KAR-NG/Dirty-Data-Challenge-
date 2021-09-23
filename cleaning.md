Dirty Data Cleaning and Transformation
================
Kar Ng
2021

-   [1 R PACKAGES](#1-r-packages)
-   [2 INTRODUCTION](#2-introduction)
-   [3 DATA IMPORT](#3-data-import)
-   [4 DATA CLEANING](#4-data-cleaning)
    -   [4.1 Cleaning table 1](#41-cleaning-table-1)

------------------------------------------------------------------------

------------------------------------------------------------------------

## 1 R PACKAGES

Following codes load required R packages for this project.

``` r
library(tidyverse)
library(skimr)
library(agridat)
```

## 2 INTRODUCTION

Data cleaning, manipulation and transformation are very important in
data science. They process datasets and convert them into a format of
analysis-ready for later analysis such as visualisation or creating
predictive models.

This is a side project to demonstrate data cleaning skills. I will clean
a public dataset from “agridate” R package, named “bridges.cucumber”.
This dataset has actually been cleaned but I downloaded the cleaned
format, devastate, ruin and mess it. The single cleaned table has been
spitted into 4 tables and with a variability of cleaning tasks.

![](https://raw.githubusercontent.com/KAR-NG/cleaning/main/pic_4Tables.JPG)

How the original format is like?

``` r
data("bridges.cucumber", package = "agridat")
bridges.cucumber
```

    ##        loc      gen row col   yield
    ## 1  Clemson   Dasher   1   3 44.2000
    ## 2  Clemson   Dasher   2   4 54.1000
    ## 3  Clemson   Dasher   3   2 47.2000
    ## 4  Clemson   Dasher   4   1 36.7000
    ## 5  Clemson Guardian   1   4 33.0000
    ## 6  Clemson Guardian   2   2 13.6000
    ## 7  Clemson Guardian   3   1 44.1000
    ## 8  Clemson Guardian   4   3 35.8000
    ## 9  Clemson Poinsett   1   1 11.5000
    ## 10 Clemson Poinsett   2   3 22.4000
    ## 11 Clemson Poinsett   3   4 30.3000
    ## 12 Clemson Poinsett   4   2 21.5000
    ## 13 Clemson   Sprint   1   2 15.1000
    ## 14 Clemson   Sprint   2   1 20.3000
    ## 15 Clemson   Sprint   3   3 41.3000
    ## 16 Clemson   Sprint   4   4 27.1000
    ## 17  Tifton   Dasher   1   3 53.5463
    ## 18  Tifton   Dasher   2   4 37.5220
    ## 19  Tifton   Dasher   3   2 49.3943
    ## 20  Tifton   Dasher   4   1 61.4758
    ## 21  Tifton Guardian   1   4 34.7026
    ## 22  Tifton Guardian   2   2 29.1300
    ## 23  Tifton Guardian   3   1 40.2423
    ## 24  Tifton Guardian   4   3 50.7930
    ## 25  Tifton Poinsett   1   1 36.5749
    ## 26  Tifton Poinsett   2   3 24.6696
    ## 27  Tifton Poinsett   3   4 30.7489
    ## 28  Tifton Poinsett   4   2 40.0661
    ## 29  Tifton   Sprint   1   2 35.0771
    ## 30  Tifton   Sprint   2   1 43.3040
    ## 31  Tifton   Sprint   3   3 38.4251
    ## 32  Tifton   Sprint   4   4 39.9119

It is a dataset that record the experimental result of a cucumber with
variables of loc (location), gen (genotype), row (row position of the
trial block), col (column position of the trial block) and lastly, the
yield.

This purpose of this project is to show the R codes used to convert the
4 messy tables into this final analysis-ready format.

## 3 DATA IMPORT

Following codes import the 4 tables.

``` r
table1 <- read.csv("cucum1.csv", fileEncoding = "UTF-8-BOM")

table2 <- read.csv("cucum2.csv", fileEncoding = "UTF-8-BOM")

table3 <- read.csv("cucum3.csv", fileEncoding = "UTF-8-BOM")

table4 <- read.csv("cucum4.csv", fileEncoding = "UTF-8-BOM")
```

## 4 DATA CLEANING

### 4.1 Cleaning table 1

Tasks identify:

-   Rename the column names
-   Split the first column into two
-   Strings manipulation in the first column
-   Fill up the missing values of the first column
-   Convert the *4000* in the “row” into 4, according to adjacent values
    of this column.
-   Convert the *1000* in the “column” into 1, according to adjacent
    values of this column.

``` r
table1 
```

    ##    Llocation.genotype rowrow column yield.g
    ## 1      Clemson-Dasher      1      3    44.2
    ## 2      Clemson-Dasher      2      4    54.1
    ## 3                   -      3      2    47.2
    ## 4      Clemson-Dasher   4000      1    36.7
    ## 5    Clemson-Guardian      1      4    33.0
    ## 6    CLEMSON-Guardian      2      2    13.6
    ## 7    Clemson-Guardian      3      1    44.1
    ## 8    Clemson-Guardian      4      3    35.8
    ## 9   Clem_son-Poinsett      1   1000    11.5
    ## 10   Clemson-Poinsett      2      3    22.4
    ## 11   Clemson-Poinsett      3      4    30.3
    ## 12   CLEMSON-poinsett      4      2    21.5
    ## 13        Clem-Sprint      1      2    15.1
    ## 14     Clemson-Sprint      2      1    20.3
    ## 15          Clemson-s      3      3    41.3
    ## 16          Clemson-s      4      4    27.1

``` r
tbl1 <- table1 %>%
  separate("Llocation.genotype", into = c("loc", "gen"), sep = "-") %>% 
  rename(row = rowrow,
         col = column,
         yield = yield.g) %>% 
  mutate(loc = as.factor(loc),
         gen = as.factor(gen))
  

summary(tbl1)
```

    ##        loc           gen         row               col              yield      
    ##          : 1           :1   Min.   :   1.00   Min.   :   1.00   Min.   :11.50  
    ##  Clem    : 1   Dasher  :3   1st Qu.:   1.75   1st Qu.:   2.00   1st Qu.:21.20  
    ##  Clem_son: 1   Guardian:4   Median :   2.50   Median :   3.00   Median :31.65  
    ##  Clemson :11   poinsett:1   Mean   : 252.25   Mean   :  64.94   Mean   :31.14  
    ##  CLEMSON : 2   Poinsett:3   3rd Qu.:   3.25   3rd Qu.:   4.00   3rd Qu.:42.00  
    ##                s       :2   Max.   :4000.00   Max.   :1000.00   Max.   :54.10  
    ##                Sprint  :2

``` r
tbl1 %>% 
  mutate(loc = replace(loc, loc == "Clem", "Hi"))
```

    ## Warning in `[<-.factor`(`*tmp*`, list, value = "Hi"): invalid factor level, NA
    ## generated

    ##         loc      gen  row  col yield
    ## 1   Clemson   Dasher    1    3  44.2
    ## 2   Clemson   Dasher    2    4  54.1
    ## 3                       3    2  47.2
    ## 4   Clemson   Dasher 4000    1  36.7
    ## 5   Clemson Guardian    1    4  33.0
    ## 6   CLEMSON Guardian    2    2  13.6
    ## 7   Clemson Guardian    3    1  44.1
    ## 8   Clemson Guardian    4    3  35.8
    ## 9  Clem_son Poinsett    1 1000  11.5
    ## 10  Clemson Poinsett    2    3  22.4
    ## 11  Clemson Poinsett    3    4  30.3
    ## 12  CLEMSON poinsett    4    2  21.5
    ## 13     <NA>   Sprint    1    2  15.1
    ## 14  Clemson   Sprint    2    1  20.3
    ## 15  Clemson        s    3    3  41.3
    ## 16  Clemson        s    4    4  27.1
