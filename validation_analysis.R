## check analysis
rm(list=ls())

## library
library(tidyverse)
library(arsenal)

## load data
crossborder_team <- read.csv("./outputs/20210609_May_DATA_reach_ssd_prm_cross_border_analyzed_data_inclRenkabsolutevalues.csv", stringsAsFactors = FALSE)
crossborder_hq <- read.csv("./outputs/20210618_May_DATA_reach_ssd_prm_cross_border_analyzed_data_hq.csv", stringsAsFactors = FALSE)

comparedf(crossborder_team, crossborder_hq)


analysis_team <- read.csv("./outputs/20210609_May_DATA_reach_ssd_prm_internal_analyzed_data.csv", stringsAsFactors = FALSE)
analysis_hq <- read.csv("./outputs/20210618_May_DATA_reach_ssd_prm_internal_analyzed_data_hq.csv", stringsAsFactors = FALSE)

comparedf(analysis_hq, analysis_team)
