
female <- readRDS(paste0(path_response_folder,"/working/iair_clean.RDS")) %>% 
  mutate(
    age_category = cut(ha1,breaks=c(15,25,35,45,55),include.lowest=TRUE,right=FALSE),
    schooling = case_when(hv106 == 0 ~ "1, No education",
                          hv106 == 1 ~ "2, Primary",
                          hv106 == 2 ~ "3, Secondary",
                          hv106 == 3 ~ "4, Higher",
                          hv106 == 8 ~ "1, No education",
                          TRUE ~ NA_character_),
    caste = case_when(sh36 == 1 ~ "Scheduled Caste",
                      sh36 == 2 ~ "Scheduled Tribe",
                      sh36 == 3 ~ "OBC",
                      sh36 %in% c(4,8,9) | is.na(sh36) ~ "General or Dont know/Missing",
                      TRUE ~ NA_character_),
    religion = case_when(sh34 == 1 ~ "Hindu",
                         sh34 == 2 ~ "Muslim",
                         TRUE ~ "Other"),
    
    wealth = factor(hv270,levels=c(1:5),labels=paste0("Q",c(1:5))),
    rural = case_when(hv025 == 1 ~ 0,
                      hv025 == 2 ~ 1,
                      TRUE ~ NA_real_),
    
    insurance = case_when(sh54 == 1 ~ 1,
                          sh54 == 0 ~ 0,
                          sh54 == 8 ~ 0, # Dont know
                          TRUE ~ NA_real_),
    
    married = case_when(hv115 == 1 ~ 1,
                        hv115 %in% c(0,2,3,4,5,8,9) ~ 0,
                        TRUE ~ NA_real_),
    pregnant = case_when(ha54 == 1 ~ 1,
                         ha54 == 0 ~ 0,
                         ha54 == 9 ~ 0,
                         TRUE ~ NA_real_),
    nonna_weight = case_when(is.na(ha2) ~ 0,
                             TRUE ~ 1),
    present_weight = case_when(is.na(ha2) ~ NA_real_,
                               ha2 == 9994 ~ 0,
                               TRUE ~ 1),
    consented_weight = case_when(is.na(ha2) ~ NA_real_,
                                 ha2 == 9994 ~ NA_real_,
                                 ha2 == 9995 ~ 0,
                                 TRUE ~ 1),
    valid_weight = case_when(is.na(ha2) ~ NA_real_,
                             ha2 %in% c(9996,9999) ~ 0,
                             ha2 %in% c(9994,9995) ~ NA_real_,
                             ha2 %in% c(150:2000) ~ 1,
                             TRUE ~ 0),
    nonna_height = case_when(is.na(ha3) ~ 0,
                             TRUE ~ 1),
    present_height = case_when(is.na(ha3) ~ NA_real_,
                               ha3 == 9994 ~ 0,
                               TRUE ~ 1),
    consented_height = case_when(is.na(ha3) ~ NA_real_,
                                 ha3 == 9994 ~ NA_real_,
                                 ha3 == 9995 ~ 0,
                                 TRUE ~ 1),
    valid_height = case_when(is.na(ha3) ~ NA_real_,
                             ha3 %in% c(9996,9999) ~ 0,
                             ha3 %in% c(9994,9995) ~ NA_real_,
                             ha3 %in% c(500:2200) ~ 1,
                             TRUE ~ 0),
    # hb -------
    nonna_hb = case_when(is.na(ha55) ~ 0,
                         TRUE ~ 1),
    present_hb = case_when(is.na(ha55) ~ NA_real_,
                           ha55 == 3 ~ 0,
                           TRUE ~ 1),
    consented_hb = case_when(ha55 == 3 ~ NA_real_,
                             ha55 == 4 ~ 0,
                             TRUE ~ 1),
    reported_hb = case_when(is.na(ha55) ~ NA_real_,
                            ha55 == 3 ~ NA_real_,
                            ha55 == 4 ~ NA_real_,
                            ha55 %in% c(6,9) ~ 0,
                            ha55 == 0 | ha53 %in% c(10:990) ~ 1,
                            TRUE ~ 0),
    valid_hb = case_when(
                         is.na(ha55) ~ NA_real_,
                         ha55 == 3 ~ NA_real_,
                         ha55 == 4 ~ NA_real_,
                         ha55 %in% c(6,9) ~ NA_real_,
                         ha53 %in% c(30:250) | ha56 %in% c(30:250) ~ 1,
                         TRUE ~ 0),
    
    # BP -----------
    selected_biomarker = case_when(shbsel == 1 ~ 1,
                                   shbsel == 0 ~ 0,
                                   TRUE ~ NA_real_),
    nonna_bp = case_when(is.na(shb10) ~ 0,
                         TRUE ~ 1),
    consented_bp1 = case_when(is.na(shb10) ~ NA_real_,
                              shb10 == 1 ~ 1,
                              shb10 %in% c(2,3) ~ 0,
                              TRUE ~ NA_real_),
    consented_bp2 = case_when(is.na(shb10) ~ NA_real_,
                              shb21 == 1 ~ 1,
                              shb21 %in% c(0,9) ~ 0,
                              TRUE ~ NA_real_),
    consented_bp3 = case_when(is.na(shb10) ~ NA_real_,
                              shb25 == 1 ~ 1,
                              shb25 %in% c(0,9) ~ 0,
                              TRUE ~ 0),
    
    consented_bpall = case_when(is.na(shb10) ~ NA_real_,
                                shb10 == 1 & shb21 == 1 & shb25 == 1 ~ 1,
                                shb10 %in% c(2,3) ~ 0,
                                is.na(shb10) | shb21 %in% c(0,9) ~ 0,
                                is.na(shb21) | shb25 %in% c(0,9) ~ 0, 
                                TRUE ~ NA_real_),
    
    # Glucose -----
    nonna_glucose = case_when(is.na(shb20) ~ 0,
                              TRUE ~ 1),
    consented_glucose = case_when(is.na(shb20) ~ NA_real_,
                                  shb20 == 1 ~ 1,
                                  shb20 %in% c(2,3) ~ 0,
                                  TRUE ~ NA_real_),
    reported_glucose = case_when(
                              # shb20 %in% c(2,3) ~ NA_real_,
                              shb70 %in% c(20:499) ~ 1,
                              # shb20 == 1 ~ 0,
                              TRUE ~ 0),
    valid_glucose = case_when(is.na(shb20) ~ NA_real_,
                                 shb20 %in% c(2,3) ~ NA_real_,
                                 shb20 == 1 & shb70 %in% c(20:499) ~ 1,
                                 shb20 == 1 ~ 0,
                                 TRUE ~ 0),
    # HIV --------
    nonna_hiv = case_when(is.na(ha61) ~ 0,
                          TRUE ~ 1),
    present_hiv = case_when(is.na(ha61) ~ NA_real_,
                            ha61 %in% c(1:4) ~ 1,
                            TRUE ~ 0),
    consented_hiv = case_when(is.na(ha61) ~ NA_real_,
                              ha61 == 1 ~ 1,
                              ha61 %in% c(2,3) ~ 0,
                              TRUE ~ NA_real_),
    valid_hiv = case_when(is.na(ha61) ~ NA_real_,
                          ha61 == 1 ~ 1,
                          ha61 %in% c(4) ~ 0,
                          TRUE ~ NA_real_)
    
    
  ) %>% 
  # Blood pressure cleaning -------
mutate_at(vars(
  shb16s,shb16d,
  shb23s,shb23d,
  shb27s,shb27d),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                       TRUE ~ as.numeric(x))) %>% 
  mutate(reported_sbp_count = rowSums(!is.na(.[,c("shb16s","shb23s","shb27s")])),
         
         reported_dbp_count = rowSums(!is.na(.[,c("shb16d","shb23d","shb27d")])),
         reported_sbp = case_when(reported_sbp_count == 3 ~ 1,
                                  reported_sbp_count %in% c(1,2) ~ 0,
                                  reported_sbp_count == 0 ~ 0),
         reported_dbp = case_when(reported_dbp_count == 3 ~ 1,
                                  reported_dbp_count %in% c(1,2) ~ 0,
                                  reported_dbp_count == 0 ~ 0),
         reported_sbp_atleast1 = case_when(reported_sbp_count >= 1 ~ 1,
                                           reported_sbp_count == 0 ~ 0),
         reported_dbp_atleast1 = case_when(reported_dbp_count >=1 ~ 1,
                                           reported_dbp_count == 0 ~ 0)
  ) %>% 
  
  mutate(shb16s = case_when(shb10 == 1 ~ shb16s,
                            TRUE ~ NA_real_),
         shb16d = case_when(shb10 == 1 ~ shb16d,
                            TRUE ~ NA_real_),
         shb23s = case_when(shb21 == 1 ~ shb23s,
                            TRUE ~ NA_real_),
         shb23d = case_when(shb21 == 1 ~ shb23d,
                            TRUE ~ NA_real_),
         shb27s = case_when(shb25 == 1 ~ shb27s,
                            TRUE ~ NA_real_),
         shb27d = case_when(shb25 == 1 ~ shb27d,
                            TRUE ~ NA_real_)) %>% 
  
  mutate(valid_sbp_count = rowSums(!is.na(.[,c("shb16s","shb23s","shb27s")])),
         
         valid_dbp_count = rowSums(!is.na(.[,c("shb16d","shb23d","shb27d")])),
         
         value_weight = case_when(is.na(ha2) ~ NA_real_,
                                  ha2 %in% c(9996,9999) ~ NA_real_,
                                  ha2 %in% c(9994,9995) ~ NA_real_,
                                  ha2 %in% c(150:2000) ~ ha2 %>% as.numeric(.),
                                  TRUE ~ 0),
         value_height = case_when(is.na(ha3) ~ NA_real_,
                                  ha3 %in% c(9996,9999) ~ NA_real_,
                                  ha3 %in% c(9994,9995) ~ NA_real_,
                                  ha3 %in% c(500:2200) ~ ha3 %>% as.numeric(.),
                                  TRUE ~ NA_real_),
         value_hb = case_when(is.na(ha55) ~ NA_real_,
                              ha55 %in% c(3,4) ~ NA_real_,
                              ha55 %in% c(0,6,9) ~ NA_real_,
                              ha53 %in% c(30:250) | ha56 %in% c(30:250) ~ ha56 %>% as.numeric(.),
                              TRUE ~ NA_real_),
         
         value_sbp_atleast1 = rowMeans(.[,c("shb16s","shb23s","shb27s")],na.rm=TRUE),
         
         value_dbp_atleast1 = rowMeans(.[,c("shb16d","shb23d","shb27d")],na.rm=TRUE),
         
         value_sbp = rowMeans(.[,c("shb16s","shb23s","shb27s")]),
         
         value_dbp = rowMeans(.[,c("shb16d","shb23d","shb27d")]),
         
         value_glucose = case_when(is.na(shb20) ~ NA_real_,
                                   shb20 %in% c(2,3) ~ NA_real_,
                                   shb70 %in% c(20:499) ~ shb70 %>% as.numeric(.),
                                   TRUE ~ NA_real_)
         ) %>% 
  # For many participants, all 3 values are there, but consent (shb10) is 'respondent refused' or missing
  # Hence we don't use the consent variable while calculating valid estimates
  mutate(valid_sbp = case_when(#is.na(shb10) ~ NA_real_,
                               valid_sbp_count == 3 ~ 1,
                               valid_sbp_count %in% c(1,2) ~ 0,
                               valid_sbp_count == 0 ~ 0),
         valid_dbp = case_when(#is.na(shb10) ~ NA_real_,
                               valid_dbp_count == 3 ~ 1,
                               valid_dbp_count %in% c(1,2) ~ 0,
                               valid_dbp_count == 0 ~ 0),
         valid_sbp_atleast1 = case_when(#is.na(shb10) ~ NA_real_,
                                        valid_sbp_count >= 1 ~ 1,
                                        valid_sbp_count == 0 ~ 0),
         valid_dbp_atleast1 = case_when(#is.na(shb10) ~ NA_real_,
                                        valid_dbp_count >=1 ~ 1,
                                        valid_dbp_count == 0 ~ 0)) %>% 
  mutate(hv024 = factor(hv024,levels=c(1:36),labels=attr(hv024,"labels") %>% attr(.,"names"))) %>% 
  mutate(hweight = hv005/(10^6),
         weight = v005/(10^6),
         state = case_when(shdistri %in% c(494,495,496) ~ "daman and diu",
                           shdistri %in% c(3,4) ~ "ladakh",
                           TRUE ~ as.character(hv024))) 


saveRDS(female,paste0(path_response_folder,"/working/female cleaned.RDS"))

