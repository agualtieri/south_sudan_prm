

fill_missing_kobo_calculations<-function(df){df %>%
  mutate(
    D.prev_country=ifelse(A.movement_type=="exit","SouthSudan", D.prev_country),
    D.d2.prev_region=ifelse(A.movement_type=="exit", aux_state, D.d2.prev_region),
    D.d2.prev_region_sub=ifelse(A.movement_type=="exit", aux_county,D.d2.prev_region_sub),
    D.d2.prev_settlement=ifelse(A.movement_type=="exit", aux_settlement_extra,D.d2.prev_settlement),
    E.next_country = ifelse(A.movement_type=="entry", "SouthSudan", E.next_country),
    E.e2.next_region = ifelse(A.movement_type=="entry", aux_state,E.e2.next_region),
    E.e2.next_region_sub = ifelse(A.movement_type=="entry", aux_county, E.e2.next_region_sub),
    E.e2.next_settlement =ifelse(A.movement_type=="entry", aux_settlement_extra, E.e2.next_region_sub)
  )}

add_composite_indicators<- function(df){df %>%
  mutate(
    i.cross_border_vs_internal=ifelse((D.prev_country!="SouthSudan"|
                                         E.next_country!="SouthSudan"),
                                      "cross_border",
                                      "internal" ),
    i.cross_border_class=ifelse((D.prev_country!="SouthSudan"&
                                   E.next_country!="SouthSudan"),
                                "cross_border_transit",
                                i.cross_border_vs_internal),

    i.flow_type= ifelse(i.cross_border_class== "cross_border" &
                          D.prev_country != "SouthSudan" &
                          A.movement_type %in% c("entry", "transit"),
                        "cross_border_inbound",
                        ifelse(i.cross_border_class== "cross_border" &
                                 E.next_country!="SouthSudan" &
                                 A.movement_type %in% c("exit", "transit"),
                               "cross_border_outbout", i.cross_border_class)),
    i.push_factors=ifelse(F.push_factors %in% c("far_family","far_from_home"),"Proximity to family/home",F.push_factors),
    i.pull_factors=ifelse(F.pull_factors %in% c("family","want_to_be_home"), "Proximity to family/home", F.pull_factors),
    i.pull_fators2= ifelse(F.pull_factors %in% c("food_dist","local_food", "plant_crops"), "Perceived availability of food",F.pull_factors),
    i.stay_duration= ifelse(F.stay_duration %in% c("6_months" , "permanently") , "More than 6 months or permanently", F.stay_duration)
  )}



prm_demographic_analysis <- function(dataset,strata2){
  child_demographic=c("C.m_0_4", "C.f_0_4", "C.m_5_17", "C.f_5_17")
  adult_male_demographic=c("C.m_18_59", "C.m_above60")
  adult_female_demographic<- c("C.f_18_59", "C.f_above60")
  df_resp_level<-dataset %>%
    mutate(
      int.num_child=rowSums(.[child_demographic], na.rm=TRUE),
      int.num_male_adult=rowSums(.[adult_male_demographic], na.rm=TRUE),
      int.num_female_adult=rowSums(.[adult_female_demographic], na.rm=TRUE),

    )


  df_base_level<-df_resp_level %>%
    group_by(A.base,!!sym(strata2)) %>%
    summarise(

      i.demog.perc_child=sum(int.num_child)/sum(C.total_household),
      i.demog.perc_adult_male=sum(int.num_male_adult)/sum(C.total_household),
      i.demog.perc_adult_female=sum(int.num_female_adult)/sum(C.total_household),

    ) %>%
    select(A.base,strata2,starts_with("i.demog")) %>% ungroup()

}




