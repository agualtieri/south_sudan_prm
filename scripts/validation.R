### validation

library(srvyr)
library(dplyr)
library(sf)
library(readr)
library(tidyr)
library(butteR)
library(compareDF)
library(lubridate)
source("util_functions/recoding_functions.R")

all_data<-butteR::read_all_csvs_in_folder("inputs/2021_01_data/")
akobo <- all_data[[1]]
kapoeta <- all_data[[2]]
nyal <- all_data[[3]]
yambo <- all_data[[4]]


all_data<-butteR::read_all_csvs_in_folder("./inputs/2021_01_data/_archive/")
akobo_team <- all_data[[1]]
kapoeta_team <- all_data[[2]]
nyal_team <- all_data[[3]]
yambo_team <- all_data[[4]]


akobo_check <- compare_df(akobo, akobo_team, "X_uuid") ### ok ok
kapoeta_check <- compare_df(kapoeta, kapoeta_team, "X_uuid") ### why???
kapoeta_check[["comparison_table_diff"]]

nyal_check <- compare_df(nyal, nyal_team, "X_uuid") ### why again??
nyal_check[["comparison_table_diff"]]

yambo_check <- compare_df(yambo, yambo_team, "X_uuid") ### why....
yambo_check[["comparison_table_diff"]]


## Check results
all_analysis<-butteR::read_all_csvs_in_folder("outputs/")
cross_border_team <- all_analysis[[1]]
cross_border_team$id <- paste(cross_border_team$A.base, "_", cross_border_team$i.flow_type)

cross_border_hq <- all_analysis[[3]]
cross_border_hq$id <- paste(cross_border_hq$A.base, "_", cross_border_hq$i.flow_type)

cross_check <- compare_df(cross_border_hq, cross_border_team, "id") ### ok

internal_team <- all_analysis[[2]]
internal_team$id <- paste(internal_team$A.base,"_",internal_team$A.movement_type)

internal_hq <- all_analysis[[4]]
internal_hq$id <- paste(internal_team$A.base,"_",internal_hq$A.movement_type)
internal_check <- compare_df(internal_team, internal_hq, "id") ### ok

