library(srvyr)
library(dplyr)
library(sf)
library(readr)
library(tidyr)

source("util_functions/recoding_functions.R")


# SCRIPT INPUTS -----------------------------------------------------------

# DEFINE MONTH
month_to_analyze<-"2020-01-01"
# ISO FORMAT DATE (USED LATER FOR NAMING OUTPUT)
iso_date<-stringr::str_replace_all(Sys.Date(),"-","")

# AO SAW INITIAL OUTPUT AND GAVE ME A LIST OF COLUMNS TO REMOVE AT THE END TO SIMPLYIFY
cols_to_remove_df<-readr::read_csv("inputs/reach_ssd_prm_analyzed_data_draft_20200124_dropcolumns_RD.csv")

# SUPPLEMENTARY CORRECT INFO ABOUT EACH BASE
prm_base_info<- readr::read_csv('inputs/prm_bases.csv') %>% select(aux_settlement=NAME, aux_state= STATEJOIN, aux_county= COUNTYJOIN)

#DEFINE THE FOLDER WITH ALL CSVS AND READ EACH ONE AS A DATA FRAME AND ADD TO LIST
jan_data_list<-butteR::read_all_csvs_in_folder("inputs/january2020")

#BIND ALL CSVS INTO ONE DF
df<- data.table::rbindlist(jan_data_list,fill = TRUE)


# ADD SOME COLUMNS TO AID ANALYSES AND OUTPUT -----------------------------

# DEFINE MONTH TO ANALYZE AS MONTH
df<-df %>% mutate(month=month_to_analyze)

# ADD AUXILLIARY INFO
df<-df %>% left_join(prm_base_info, by=c("A.base"= "aux_settlement"))

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

# SLIGHT DIFFERENCES BETWEEN CROSS BORDER AND INTERNAL COLUMNS TO ANALYZE - SEPARATE HERE
cross_boarder_cols_to_analyze<-c("A.port_type", "A.movement_type",
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
                                 "i.push_factors", "i.pull_factors", "i.pull_fators2", "i.stay_duration")

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
                            "i.push_factors", "i.pull_factors", "i.pull_fators2", "i.stay_duration")


#JUST  A PRECAUTION DURING DATA ANALYSIS- DOESNT EFFECT FINALIZED CLEAN DATA
dfc<-dfc %>% filter(!is.na(A.base))

# SPLIT DATA SETS TO PREPARE FOR SEPARATE INTERNAL/CROSS BORDER ANALYSES
df_internal<- dfc %>% filter(A.base %in% c("nyal", "yambio"))
df_crossborder<- dfc %>% filter(A.base %in% c("akobo", "kapoeta", "renk"))

internal_pop_numbers<-df_internal %>% group_by(A.base,A.movement_type) %>%
  summarise(total_num_HH=n(),
            total_num_indiv= sum(C.total_household))

cross_border_pop_numbers<-df_crossborder %>% group_by(A.base,i.flow_type) %>%
  summarise(total_num_HH=n(),
            total_num_indiv= sum(C.total_household))



# MAKE SURVEY OBJECTS AN APPLY BUTTER -------------------------------------

dfsvy_internal<-srvyr::as_survey(df_internal, strata=A.base)
dfsvy_crossborder<-srvyr::as_survey(df_crossborder, strata=A.base)

cross_border_basic_analysis_strat_na_replace_false<-butteR::mean_proportion_table(design = dfsvy_crossborder,
                                                                                  list_of_variables = cross_boarder_cols_to_analyze,
                                                                                  aggregation_level = c("A.base","i.flow_type"),
                                                                                  round_to = 2,
                                                                                  return_confidence = FALSE,
                                                                                  na_replace = FALSE)

internal_basic_analysis_strat_na_replace_false<-butteR::mean_proportion_table(design = dfsvy_internal,
                                                                              list_of_variables = internal_cols_to_analyze,
                                                                              aggregation_level = c("A.base","A.movement_type"),
                                                                              round_to = 2,
                                                                              return_confidence = FALSE,
                                                                              na_replace = FALSE)

cross_border_analysis_cols_remove<- colnames(cross_border_basic_analysis_strat_na_replace_false)[colnames(cross_border_basic_analysis_strat_na_replace_false) %in%colnames(cols_to_remove_df)]
internal_analysis_cols_remove<- colnames(internal_basic_analysis_strat_na_replace_false)[colnames(internal_basic_analysis_strat_na_replace_false) %in%
                                                                                           colnames(cols_to_remove_df)]



#REMOVE COLUMNS TO SIMPLIFY OUTPUT
cross_border_basic_analysis_strat_na_replace_false<-cross_border_basic_analysis_strat_na_replace_false %>% select(-c(cross_border_analysis_cols_remove))
internal_basic_analysis_strat_na_replace_false<-internal_basic_analysis_strat_na_replace_false %>% select(-c(internal_analysis_cols_remove))



#DEMOGRAPHIC ANALAYSIS NEEDED
demog_analysis_crossborder <- prm_demographic_analysis(df_crossborder,strata2 = "i.flow_type")
demog_analysis_internal <- prm_demographic_analysis(df_internal,strata2 = "A.movement_type")


#JOIN MAIN ANALYSES DATASETS WITH THE SEPARATE DEMOGRAPHIC ANALYSES
cross_border_analysis<- cross_border_basic_analysis_strat_na_replace_false %>%
  left_join(demog_analysis_crossborder, by= c("A.base"= "A.base", "i.flow_type"="i.flow_type")) %>%
  left_join(cross_border_pop_numbers, by= c("A.base"= "A.base", "i.flow_type"="i.flow_type"))

internal_analysis<- internal_basic_analysis_strat_na_replace_false %>%
  left_join(demog_analysis_internal, by= c("A.base"= "A.base", "A.movement_type"="A.movement_type"))%>%
  left_join(internal_pop_numbers, by= c("A.base"= "A.base", "A.movement_type"="A.movement_type"))


#WRITE FULL ANALYSES TO OUTPUT FOLDER (CROSS BORDER SEPARATE FROM INTERNAL)
write.csv(cross_border_analysis,paste0("outputs/",iso_date,"_reach_ssd_prm_cross_border_analyzed_data.csv"))
write.csv(internal_analysis,paste0("outputs/",iso_date,"_reach_ssd_prm_internal_analyzed_data.csv"))





