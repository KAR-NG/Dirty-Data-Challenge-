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

## DATA IMPORT
