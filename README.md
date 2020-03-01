README
================

# South Sudan PRM Analysis/Tutorial

This project was developed in January 2020 to automate South Sudan PRM
data collation and analysis. This document serves as a tutorial for the
the next Analyst. As this Github will no longer be maintaine after March
6 2020 it is recommended that this github be forked by the responsible
GIS/Data Unit Manager and the fork be maintained and updated.

## Load packages

You will need the packages below. This analysis is dependent on the
butteR package which can be downloaded from github (link below).

``` r
# install.packages("devtools")
devtools::install_github("zackarno/butteR")
```

``` r
library(srvyr)
library(dplyr)
library(sf)
library(readr)
library(tidyr)
source("util_functions/recoding_functions.R")
```
