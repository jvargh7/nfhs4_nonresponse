male <- readRDS(paste0(path_response_folder,"/working/iamr_clean.RDS")) %>% 
  mutate(
    age_category = cut(hb1,breaks=c(15,25,35,45,55),include.lowest=TRUE,right=FALSE),
    schooling = case_when(hb66 == 0 ~ "No education",
                          hb66 == 1 ~ "Primary",
                          hb66 == 2 ~ "Secondary",
                          hb66 == 3 ~ "Higher",
                          hb66 == 8 ~ "Don't know",
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
    
    # married = case_when()
    nonna_weight = case_when(is.na(hb2) ~ 0,
                             TRUE ~ 1),
    present_weight = case_when(is.na(hb2) ~ NA_real_,
                               hb2 == 9994 ~ 0,
                               TRUE ~ 1),
    consented_weight = case_when(is.na(hb2) ~ NA_real_,
                                 hb2 == 9994 ~ NA_real_,
                                 hb2 == 9995 ~ 0,
                                 TRUE ~ 1),
    valid_weight = case_when(is.na(hb2) ~ NA_real_,
                             hb2 %in% c(9996,9999) ~ 0,
                             hb2 %in% c(9994,9995) ~ NA_real_,
                             hb2 %in% c(150:2500) ~ 1,
                             TRUE ~ 0),
    nonna_height = case_when(is.na(hb3) ~ 0,
                             TRUE ~ 1),
    present_height = case_when(is.na(hb3) ~ NA_real_,
                               hb3 == 9994 ~ 0,
                               TRUE ~ 1),
    consented_height = case_when(is.na(hb3) ~ NA_real_,
                                 hb3 == 9994 ~ NA_real_,
                                 hb3 == 9995 ~ 0,
                                 TRUE ~ 1),
    valid_height = case_when(is.na(hb3) ~ NA_real_,
                             hb3 %in% c(9996,9999) ~ 0,
                             hb3 %in% c(9994,9995) ~ NA_real_,
                             hb3 %in% c(200:2500) ~ 1,
                             TRUE ~ 0),
    # hb -------
    nonna_hb = case_when(is.na(hb55) ~ 0,
                             TRUE ~ 1),
    present_hb = case_when(is.na(hb55) ~ NA_real_,
                           hb55 == 3 ~ 0,
                           TRUE ~ 1),
    consented_hb = case_when(hb55 == 3 ~ NA_real_,
                             hb55 == 4 ~ 0,
                             TRUE ~ 1),
    valid_hb = case_when(is.na(hb55) ~ NA_real_,
                         hb55 %in% c(3,4) ~ NA_real_,
                         hb53 %in% c(10:990) ~ 1,
                         hb55 %in% c(0,6,9) ~ 0,
                         TRUE ~ NA_real_),
    
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
    valid_glucose = case_when(is.na(shb20) ~ NA_real_,
                              shb20 %in% c(2,3) ~ NA_real_,
                              shb70 %in% c(20:499) ~ 1,
                              shb20 == 1 ~ 0,
                              TRUE ~ 0),
    
    # HIV --------
    nonna_hiv = case_when(is.na(hb61) ~ 0,
                              TRUE ~ 1),
    present_hiv = case_when(is.na(hb61) ~ NA_real_,
                            hb61 %in% c(1:4) ~ 1,
                            TRUE ~ 0),
    consented_hiv = case_when(is.na(hb61) ~ NA_real_,
                              hb61 == 1 ~ 1,
                              hb61 %in% c(2,3) ~ 0,
                              TRUE ~ NA_real_),
    valid_hiv = case_when(is.na(hb61) ~ NA_real_,
                          hb61 == 1 ~ 1,
                          hb61 %in% c(4) ~ 0,
                          TRUE ~ NA_real_)
    
    
  ) %>% 
  # Blood pressure cleaning -------
 mutate_at(vars(
  shb16s,shb16d,
  shb23s,shb23d,
  shb27s,shb27d),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                       TRUE ~ as.numeric(x))) %>% 
  mutate(valid_sbp_count = rowSums(!is.na(.[,c("shb16s","shb23s","shb27s")])),
          
          valid_dbp_count = rowSums(!is.na(.[,c("shb16d","shb23d","shb27d")])),
         
         value_weight = case_when(is.na(hb2) ~ NA_real_,
                                  hb2 %in% c(9996,9999) ~ NA_real_,
                                  hb2 %in% c(9994,9995) ~ NA_real_,
                                  hb2 %in% c(150:2500) ~ hb2 %>% as.numeric(.),
                                  TRUE ~ 0),
         value_height = case_when(is.na(hb3) ~ NA_real_,
                                  hb3 %in% c(9996,9999) ~ NA_real_,
                                  hb3 %in% c(9994,9995) ~ NA_real_,
                                  hb3 %in% c(200:2500) ~ hb3 %>% as.numeric(.),
                                  TRUE ~ NA_real_),
         value_hb = case_when(is.na(hb55) ~ NA_real_,
                              hb55 %in% c(3,4) ~ NA_real_,
                              hb53 %in% c(10:990) ~ hb53 %>% as.numeric(.),
                              hb55 %in% c(0,6,9) ~ NA_real_,
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
                               valid_dbp_count == 0 ~ 0)
         )  %>% 
  mutate(hv024 = factor(hv024,levels=c(1:36),labels=attr(hv024,"labels") %>% attr(.,"names"))) %>% 
  mutate(weight = hv005/(10^6),
         state = case_when(shdistri %in% c(494,495,496) ~ "daman and diu",
                           shdistri %in% c(3,4) ~ "ladakh",
                           TRUE ~ as.character(hv024))) 


saveRDS(male,paste0(path_response_folder,"/working/male cleaned.RDS"))

