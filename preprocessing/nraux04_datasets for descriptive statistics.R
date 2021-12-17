male <- readRDS(paste0(path_response_folder,"/working/male cleaned.RDS")) %>% 
  dplyr::select(hhid,hvidx,hv001,
                hv002,hv006,hv007,hv008,
                hv010,hv011,hv011,
                hv021,
                state,hv024,shdistri,
                weight,
                
                hv035, hv041,hv042,
                
                age_category,schooling, caste,married,
                religion, wealth, rural, insurance,
                contains("_height"),contains("_weight"),
                contains("_hb"),contains("_bp"),contains("_sbp"),contains("_dbp"),
                contains("_hiv"),contains("_glucose")) %>% 
  mutate_at(vars(matches("(nonna|consented|reported|valid|present)_(height|weight|hb|bp|sbp|dbp|glucose)")),
            function(x) case_when(is.na(x) ~ 0,
                                  TRUE ~ x)) %>% 
  rename(
         nadults = hv041,
         nchildren = hv035,
         hb_select = hv042) 
female <- readRDS(paste0(path_response_folder,"/working/female cleaned.RDS")) %>% 
  dplyr::select(v001,
                v002,v003,
                hv021,
                state,hv024,shdistri,
                weight,hv006,hv007,hv008,
                hv010,hv011,hv011,
                hv035, hv041,hv042,
                
                age_category,schooling, caste,
                religion, wealth, rural, insurance,married,pregnant,
                contains("_height"),contains("_weight"),
                contains("_hb"),contains("_bp"),contains("_sbp"),contains("_dbp"),
                contains("_hiv"),contains("_glucose")) %>% 
  rename(hv001 = v001,
         hv002 = v002,
         hvidx = v003,
         nadults = hv041,
         nchildren = hv035,
         hb_select = hv042) %>% 
  mutate_at(vars(matches("(nonna|consented|reported|valid|present)_(height|weight|hb|bp|sbp|dbp|glucose)")),
            function(x) case_when(is.na(x) ~ 0,
                                  TRUE ~ x))
child <- readRDS(paste0(path_response_folder,"/working/child cleaned.RDS"))%>% 
  dplyr::select(hhid,hvidx,hv001,
                hv002,hv006,hv007,hv008,
                hv010,hv011,hv011,
                hv021,
                state,hv024,shdistri,
                weight,
                
                hv035, hv041,hv042,
                
                age_category,schooling,caste,
                religion, wealth, rural, insurance,
                contains("_height"),contains("_weight"),
                contains("_hb"))%>% 
  mutate_at(vars(matches("(nonna|consented|reported|valid|present)_(height|weight)")),~case_when(is.na(.) ~ 0,
                                                                                        TRUE ~ .)) %>% 
  # Only for those older than 6 mo
  mutate_at(vars(contains("_hb")),function(x) case_when(.$age_category == "[0,6)" ~ NA_real_,
                                                        is.na(x) ~ 0,
                                                        TRUE ~ x))  %>% 
  rename(
    nadults = hv041,
    nchildren = hv035,
    hb_select = hv042) %>% 
  mutate(schooling_imp = case_when(is.na(schooling) ~ "1, No education",
                                   TRUE ~ schooling))


# SURVEY DESIGN ------------
male_surveydesign <- male  %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") 

male_surveydesign_hiv <- male %>% 
  dplyr::filter(!is.na(consented_hiv)) %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

female_surveydesign <- female %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") 
female_surveydesign_hiv <- female %>% 
  dplyr::filter(!is.na(consented_hiv)) %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


child_surveydesign <- child  %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") 

child_surveydesign_hb <- child  %>% 
  dplyr::filter(age_category != "[0,6)") %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") 
