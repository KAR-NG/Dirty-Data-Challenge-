Dirty Data Cleaning and Transformation
================
Kar Ng
2021

-   [R PACKAGES](#r-packages)
-   [INTRODUCTION](#introduction)
-   [DATA IMPORT](#data-import)

------------------------------------------------------------------------

------------------------------------------------------------------------

## R PACKAGES

Following codes load required R packages for this project.

``` r
library(tidyverse)
library(skimr)
library(agridat)
```

## INTRODUCTION

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

## DATA IMPORT
