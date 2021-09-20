
library(srvyr)
library(dplyr)
library(sf)
library(readr)
library(tidyr)
library(lubridate)
source("util_functions/recoding_functions.R")


# SCRIPT INPUTS -----------------------------------------------------------

# DEFINE MONTH
month_to_analyze<-"2021-05-01"

# ISO FORMAT DATE (USED LATER FOR NAMING OUTPUT)
iso_date<-stringr::str_replace_all(Sys.Date(),"-","")

# AO SAW INITIAL OUTPUT AND GAVE ME A LIST OF COLUMNS TO REMOVE AT THE END TO SIMPLYIFY
# cols_to_remove_df<-readr::read_csv("inputs/reach_ssd_prm_analyzed_data_draft_20200124_dropcolumns_RD.csv")
cols_to_remove_df<-readr::read_csv("inputs/prm_cols_to_remove_after_analysis.csv")


# SUPPLEMENTARY CORRECT INFO ABOUT EACH BASE
prm_base_info<- readr::read_csv('inputs/prm_bases.csv') %>% select(aux_settlement=NAME, aux_state= STATEJOIN, aux_county= COUNTYJOIN)

#DEFINE THE FOLDER WITH ALL CSVS AND READ EACH ONE AS A DATA FRAME AND ADD TO LIST
all_data<-butteR::read_all_csvs_in_folder("inputs/2021_05_data/")
dir("inputs/2021_05_data/")


#BIND ALL CSVS INTO ONE DF
df<- data.table::rbindlist(all_data,fill = TRUE)



# ADD SOME COLUMNS TO AID ANALYSES AND OUTPUT -----------------------------

# DEFINE MONTH TO ANALYZE AS MONTH
df<-df %>% mutate(month=month_to_analyze)

# ADD AUXILLIARY INFO
df<-df %>% left_join(prm_base_info %>% mutate(aux_settlement= aux_settlement %>% tolower(),
                                              aux_settlement_extra=aux_settlement), by=c("A.base"= "aux_settlement"))



# THIS SHOULD PROBABLY BE BUILT INTO THE FORM AS CALCULATIONS
df2<- fill_missing_kobo_calculations(df)

# A COUPLE OF COMPOSITE INIDCATORS TO ANALYZE
df3 <- add_composite_indicators(df2)


#ONLY SELECT CURRENT MONTH - DOESNT MATTER THIS ROUND BECAUSE WE HAVE ONLY RECIEVED ONE MONTH OF DATA FROM EACH BASE
dfc<-df3 %>%
  filter(month==month_to_analyze)

# I KNOW THERE IS A BETTER WAY TO DO THIS-- BUT THIS WORKS
is_not_empty<-function(x){ all(is.na(x))==FALSE}

# SELECT COLUMNS FOR ANALYISIS --------------------------------------------

# JUST A CHEAP TRICK TO GET THE RIGHT COLUMNS IN THE RIGHT FORMAT TO ADD TO VECTORS
dfc %>% select(-ends_with("_other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% colnames() %>% dput()

# dfc<-dfc %>% purrr::map_df(trimws)
# SLIGHT DIFFERENCES BETWEEN CROSS BORDER AND INTERNAL COLUMNS TO ANALYZE - SEPARATE HERE

#cols to analyze out of whole survey - do not subset NA
dfc %>% select(contains("next")) %>% colnames() %>% dput()

do_not_subset_NAs<-c("D.prev_country", "D.prev_camp_yn", "D.d1.prev_camp", "D.d1.prev_camp_site", "D.d2.prev_region", "D.d2.prev_region_sub", "D.d2.prev_settlement",
                     "E.next_country", "E.next_camp_yn", "E.e1.next_camp","E.e1.next_camp_site", "E.e2.next_region", "E.e2.next_region_sub")

cross_border_cols_to_analyze<-c("A.port_type", "A.movement_type",
                                 "B.hohh_gender", "B.hohh_age", "B.origin_country", "B.origin_region",
                                 "B.origin_region_sub", "H.refugee_status",
                                 "H.J.IDP", "I.origin_predisplacement", "I.habitual_residence",
                                 "I.habitual_region_sub", "I.habitual_settlement", "C.m_0_4",
                                 "C.f_0_4", "C.m_5_17", "C.f_5_17", "C.m_18_59", "C.f_18_59",
                                 "C.m_above60", "C.f_above60", "C.total_males", "C.total_females",
                                 "C.total_household", "D.prev_country", "D.prev_camp_yn", "D.d1.prev_camp",
                                 "D.d1.prev_camp_site", "D.d1.prev_camp_site_duration", "D.d2.prev_region",
                                 "D.d2.prev_region_sub", "D.d2.prev_settlement", "D.d2.prev_camp_site_duration_001",
                                 "E.next_country", "E.next_camp_yn", "E.e1.next_camp", "E.e1.next_camp_site",
                                 "E.e2.next_region", "E.e2.next_region_sub",
                                 "F.stay_duration", "F.family_composition", "F.push_factors",
                                 "F.pull_factors", "F.transport_type", "F.money_source", "F.vulnerabilities",
                                 "F.vulnerabilities.breastfeeding", "F.vulnerabilities.unaccompanied_minor",
                                 "F.vulnerabilities.critically_ill", "F.vulnerabilities.no_vulnerable",
                                 "F.vulnerabilities.seperated_child", "F.vulnerabilities.elderly",
                                 "F.vulnerabilities.malnourished", "F.vulnerabilities.physically_disabled",
                                 "F.vulnerabilities.mentally_disabled", "F.vulnerabilities.pregnant_women",
                                 "F.vulnerabilities.single_parent",
                                 "i.push_factors", "i.pull_factors", "i.pull_fators2", "i.stay_duration", "sep_child", "unaccomp_minor", "separatedunaccompanied_composite")
cross_border_cols_to_analyze<-cross_border_cols_to_analyze[!cross_border_cols_to_analyze %in% do_not_subset_NAs]

internal_cols_to_analyze<-c("A.port_type", "A.movement_type",
                            "B.hohh_gender", "B.hohh_age", "B.origin_country", "B.origin_region",
                            "B.origin_region_sub", "H.refugee_status",
                            "H.J.IDP", "I.origin_predisplacement", "I.habitual_residence",
                            "I.habitual_region_sub", "I.habitual_settlement", "C.m_0_4",
                            "C.f_0_4", "C.m_5_17", "C.f_5_17", "C.m_18_59", "C.f_18_59",
                            "C.m_above60", "C.f_above60", "C.total_males", "C.total_females",
                            "C.total_household", "D.prev_camp_yn", "D.d1.prev_camp",
                            "D.d1.prev_camp_site_duration", "D.d2.prev_region",
                            "D.d2.prev_region_sub", "D.d2.prev_settlement", "D.d2.prev_camp_site_duration_001",
                            "E.next_camp_yn", "E.e1.next_camp",
                            "E.e2.next_region", "E.e2.next_region_sub",
                            "F.stay_duration", "F.family_composition", "F.push_factors",
                            "F.pull_factors", "F.transport_type", "F.money_source", "F.vulnerabilities",
                            "F.vulnerabilities.breastfeeding", "F.vulnerabilities.unaccompanied_minor",
                            "F.vulnerabilities.critically_ill", "F.vulnerabilities.no_vulnerable",
                            "F.vulnerabilities.seperated_child", "F.vulnerabilities.elderly",
                            "F.vulnerabilities.malnourished", "F.vulnerabilities.physically_disabled",
                            "F.vulnerabilities.mentally_disabled", "F.vulnerabilities.pregnant_women",
                            "F.vulnerabilities.single_parent",
                            "i.push_factors", "i.pull_factors", "i.pull_fators2", "i.stay_duration", "sep_child", "unaccomp_minor", "separatedunaccompanied_composite")
internal_cols_to_analyze<-internal_cols_to_analyze[!internal_cols_to_analyze %in% do_not_subset_NAs]



# THIS ALLOWS THE SCRIPT TO BE RUN BEFORE ALL DATA FROM ALL BASES IS AVAILABLE - DOESNT EFFECT FINALIZED CLEAN DATA
dfc<-dfc %>% filter(!is.na(A.base))

# SPLIT DATA SETS TO PREPARE FOR SEPARATE INTERNAL/CROSS BORDER ANALYSES
df_internal<- dfc %>% filter(A.base %in% c("nyal", "yambio"))
df_crossborder<- dfc %>% filter(A.base %in% c("kapoeta", "akobo", "renk"))


internal_pop_numbers<-df_internal %>% group_by(A.base,A.movement_type) %>%
  summarise(total_num_HH=n(),
            total_num_indiv= sum(C.total_household))

cross_border_pop_numbers<-df_crossborder %>% group_by(A.base,i.flow_type) %>%
  summarise(total_num_HH=n(),
            total_num_indiv= sum(C.total_household))






# MAKE SURVEY OBJECTS AN APPLY BUTTER -------------------------------------

dfsvy_internal<-srvyr::as_survey(df_internal, strata=A.base)
dfsvy_crossborder<-srvyr::as_survey(df_crossborder, strata=A.base)


#creating composite indicator
dfsvy_crossborder_composite <- dfsvy_crossborder$variables %>%
  mutate(
    sep_child = ifelse(F.vulnerabilities.seperated_child == 1, 1,0),
    unaccomp_minor = ifelse(F.vulnerabilities.unaccompanied_minor == 1, 1,0),
    separatedunaccompanied_composite = ifelse(F.vulnerabilities.seperated_child | F.vulnerabilities.unaccompanied_minor == 1, 1,0)
  )


## as the input adapts each month, this continually needs to change. This is needed to remove columns which are not compatible with the mean proportion tables
## in the next section of the code. When running the mean proportion tables, it will tell you want columns are failing, therefore just add those to list below.
dfsvy_crossborder_composite <- select(dfsvy_crossborder_composite, -c(B.origin_country, H.J.IDP, I.origin_predisplacement, I.habitual_residence, I.habitual_region_sub,
                                                  I.habitual_settlement, D.d1.prev_camp_site_duration, D.d2.prev_camp_site_duration_001, B.origin_region))

# trying to icnlude these: F.stay_duration, i.stay_duration


cross_border_analysis<-butteR::mean_prop_working(design = dfsvy_crossborder_composite,
                                                     list_of_variables = cross_border_cols_to_analyze,
                                                     aggregation_level = c("A.base","i.flow_type"),
                                                     round_to = 2,
                                                     return_confidence = FALSE,
                                                     na_replace = FALSE)

cross_border_analysis_no_NA_subsetting<- butteR::mean_prop_working(design = dfsvy_crossborder_composite,
                                                                       list_of_variables = do_not_subset_NAs,
                                                                       aggregation_level = c("A.base","i.flow_type"),
                                                                       round_to = 2,
                                                                       return_confidence = FALSE,
                                                                       na_replace = T)


cross_border_analysis<-left_join(cross_border_analysis,cross_border_analysis_no_NA_subsetting)

cross_border_analysis$B.origin_region_sub.Ayod


dfsvy_interval_composite <- dfsvy_internal$variables %>%
  mutate(
    sep_child = ifelse(F.vulnerabilities.seperated_child == 1, 1,0),
    unaccomp_minor = ifelse(F.vulnerabilities.unaccompanied_minor == 1, 1,0),
    separatedunaccompanied_composite = ifelse(F.vulnerabilities.seperated_child | F.vulnerabilities.unaccompanied_minor == 1, 1,0)
  )


## This is D.prev_camp_yn, D.d1.prev_camp
dfsvy_interval_composite <- select(dfsvy_interval_composite, -c(D.prev_country, E.next_country, D.d1.prev_camp_site, E.e1.next_camp_site))

internal_movement_analysis<-butteR::mean_prop_working(design = dfsvy_interval_composite,
                                                          list_of_variables = internal_cols_to_analyze,
                                                          aggregation_level = c("A.base","A.movement_type"),
                                                          round_to = 2,
                                                          return_confidence = FALSE,
                                                          na_replace = FALSE)

internal_movement_analysis_no_NA_subsetting<-butteR::mean_prop_working(design = dfsvy_interval_composite,
                                                                           list_of_variables = do_not_subset_NAs,
                                                                           aggregation_level = c("A.base","A.movement_type"),
                                                                           round_to = 2,
                                                                           return_confidence = FALSE,
                                                                           na_replace = TRUE)
internal_movement_analysis<-left_join(internal_movement_analysis,internal_movement_analysis_no_NA_subsetting)




cross_border_analysis_cols_remove<- colnames(cross_border_analysis)[colnames(cross_border_analysis) %in%cols_to_remove_df$cols_to_remove]
internal_analysis_cols_remove<- colnames(internal_movement_analysis)[colnames(internal_movement_analysis) %in%
                                                                       cols_to_remove_df$cols_to_remove]



#REMOVE COLUMNS TO SIMPLIFY OUTPUT
cross_border_analysis<-cross_border_analysis %>% select(-c(cross_border_analysis_cols_remove))
internal_movement_analysis<-internal_movement_analysis %>% select(-c(internal_analysis_cols_remove))


## ---- using debugonce is a really good trick to seeing in to where a fucntion is failing
#debugonce(prm_demographic_analysis)
#prm_demographic_analysis(df_crossborder,strata2 = "i.flow_type")



#this has been included as a bug-fix, because the dataset was being read as a data table, not data frame, which was braking the following function
class(df_internal)
df_crossborder<- as.data.frame(df_crossborder)
df_internal<- as.data.frame(df_internal)


#DEMOGRAPHIC ANALAYSIS NEEDED
demog_analysis_crossborder <- prm_demographic_analysis(df_crossborder,strata2 = "i.flow_type")
demog_analysis_internal <- prm_demographic_analysis(df_internal,strata2 = "A.movement_type")


#JOIN MAIN ANALYSES DATASETS WITH THE SEPARATE DEMOGRAPHIC ANALYSES
cross_border_analysis<- cross_border_analysis %>%
  left_join(demog_analysis_crossborder, by= c("A.base"= "A.base", "i.flow_type"="i.flow_type")) %>%
  left_join(cross_border_pop_numbers, by= c("A.base"= "A.base", "i.flow_type"="i.flow_type"))

internal_analysis<- internal_movement_analysis %>%
  left_join(demog_analysis_internal, by= c("A.base"= "A.base", "A.movement_type"="A.movement_type"))%>%
  left_join(internal_pop_numbers, by= c("A.base"= "A.base", "A.movement_type"="A.movement_type"))



#WRITE FULL ANALYSES TO OUTPUT FOLDER (CROSS BORDER SEPARATE FROM INTERNAL)
write.csv(cross_border_analysis,paste0("outputs/",iso_date,"_",month(month_to_analyze,label = T),"_DATA_reach_ssd_prm_cross_border_analyzed_data_hq.csv"), row.names = FALSE)
write.csv(internal_analysis,paste0("outputs/",iso_date,"_",month(month_to_analyze,label = T),"_DATA_reach_ssd_prm_internal_analyzed_data_hq.csv"), row.names = FALSE)





