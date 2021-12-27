
source("preprocessing/nraux04_datasets for descriptive statistics.R")

female_weights <- readRDS(paste0(path_response_folder,"/working/female weights.RDS"))



female_df <-  readRDS(paste0(path_ecological_analysis,"/working/iair74_clean.RDS")) %>% 
  left_join(female_weights %>% 
              dplyr::select(-hv021,-hv024),
            by=c("v001"="hv001",
                 "v002" = "hv002",
                 "v003" = "hvidx")) %>% 
  left_join(female %>% 
              dplyr::select(hv001,hv002,hvidx,strata,
                            wealth,schooling,caste,religion),
            by = c("v001"="hv001",
                   "v002" = "hv002",
                   "v003" = "hvidx")) %>% 

  mutate(
    S14 = case_when(v133 %in% c(9:20) | v155 %in% c(1,2) ~ 1,
                    TRUE ~ 0),
    S16 = case_when(v133 %in% c(10:20) ~ 1,
                    v133 %in% c(0:9) ~ 0,
                    v133 >= 97 ~ NA_real_,
                    TRUE ~ NA_real_),
    
    S20 = case_when(
      v511 == 99 ~ NA_real_,
      v012 < 20 | v012 > 24 ~ NA_real_,
      v012 %in% c(20:24) & is.na(v511) ~ 0,
      v012 %in% c(20:24) & v511 %in% c(0:18) ~ 0,
      v012 %in% c(20:24) & v511 %in% c(19:24) ~ 1,
      TRUE ~ NA_real_
    ),
    
    S86 = case_when(v454 == 1 ~ NA_real_,
                    v445 > 6000 ~ NA_real_,
                    v445 < 1850 ~ 1,
                    v445 >= 1850 ~ 0,
                    TRUE ~ NA_real_),
    
    S88 = case_when(v454 == 1 ~ NA_real_,
                    v445 > 6000 ~ NA_real_,
                    v445 >= 2500  ~ 1,
                    v445 < 2500 ~ 0,
                    TRUE ~ NA_real_),
    
    overweight = case_when(v454 == 1 ~ NA_real_,
                           v445 > 6000 ~ NA_real_,
                           v445 >= 2500 & v445 < 3000 ~ 1,
                           v445 < 2500 | v445 >= 3000 ~ 0,
                           TRUE ~ NA_real_),
    
    
    obese = case_when(v454 == 1 ~ NA_real_,
                      v445 > 6000 ~ NA_real_,
                      v445 >= 3000 ~ 1,
                      v445 < 3000 ~ 0,
                      TRUE ~ NA_real_),
    
    S95 = f_hemo_anemia,
    
    S99 = case_when(is.na(sb70) | sb70 > 498 ~ NA_real_,
                    sb70 >= 140 & sb70 <= 160 ~ 1,
                    sb70 < 140 | sb70 > 160 ~ 0,
                    TRUE ~ NA_real_
    ),
    S100 = case_when(is.na(sb70) | sb70 > 498 ~ NA_real_,
                     sb70 > 160 ~ 1,
                     sb70 <= 160 ~ 0,
                     TRUE ~ NA_real_),
    
    S101 = case_when(s723ab == 1 ~ 1,
                     is.na(sb70) | sb70 > 498 ~ NA_real_,
                     sb70 >= 140 ~ 1,
                     sb70 < 140 ~ 0,
                     TRUE ~ NA_real_),
    
    S105 = case_when(is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                     f_sbp >= 140 & f_sbp < 160 ~ 1,
                     f_dbp >= 90 & f_dbp < 100 ~ 1,
                     f_sbp < 140 | f_sbp >= 160 ~ 0,
                     f_dbp < 90 | f_dbp >= 100 ~ 0,
                     TRUE ~ NA_real_
    ),
    S106 = case_when(is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                     f_sbp >= 160 ~ 1,
                     f_dbp >= 100  ~ 1,
                     f_sbp < 160  ~ 0,
                     f_dbp < 100 ~ 0,
                     TRUE ~ NA_real_
    ),
    S107 = case_when(sb19 == 1 ~ 1,
                     is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                     f_sbp >= 140 ~ 1,
                     f_dbp >= 90  ~ 1,
                     f_sbp < 140  ~ 0,
                     f_dbp < 90 ~ 0,
                     TRUE ~ NA_real_
    ),
    
    S128 = case_when(v463z == 1 ~ 0,
                     v463z == 0 ~ 1,
                     TRUE ~ NA_real_),
    S130 = case_when(s716 == 0 ~ 0,
                     s716 == 1 ~ 1,
                     TRUE ~ NA_real_)
    
    
  ) %>% 
  mutate(v024 = factor(v024,levels=c(1:36),labels=attr(v024,"labels") %>% attr(.,"names"))) %>% 
  mutate(weight = v005/(10^6),
         state = case_when(sdistri %in% c(494,495,496) ~ "daman and diu",
                           sdistri %in% c(3,4) ~ "ladakh",
                           TRUE ~ as.character(v024))) 


# STATE ------------

indicators <- c("S86","S88","S95","S100","S106")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

female_indicators <- map2_dfr(indicators,weight_vars,
                            function(x,y){
                              print(x);
                              female_df$ipw = female_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                              female_df$valid = female_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();

                              ipw = female_df %>%
                                mutate(new_weight = weight*ipw) %>%
                                dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = v001,strata = state,
                                                 weight = new_weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"state")) %>%
                                group_by(state) %>%
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>%
                                mutate(state = str_replace(state,"&","and"));

                              unadjusted = female_df %>%
                                dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = v001,strata = state,
                                                 weight = weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"state")) %>%
                                group_by(state) %>%
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>%
                                mutate(state = str_replace(state,"&","and"));

                              original = female_df %>%
                                # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = v001,strata = state,
                                                 weight = weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"state")) %>%
                                group_by(state) %>%
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>%
                                mutate(state = str_replace(state,"&","and"));

                              female_df$ipw = NULL;
                              female_df$valid = NULL;

                              left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                        unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                        by="state") %>%
                                left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                          by = "state") %>%
                                mutate(indicator = x,
                                       ipw = y) %>%
                                return(.)


                            })

write_csv(female_indicators,paste0(path_response_folder,"/working/ipw weighted female estimates at state level.csv"))


# WEALTH ------------

indicators <- c("S86","S88","S95","S100","S106")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

female_indicators_wealth <- map2_dfr(indicators,weight_vars,
                              function(x,y){
                                print(x);
                                female_df$ipw = female_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                                female_df$valid = female_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();

                                ipw = female_df %>%
                                  mutate(new_weight = weight*ipw) %>%
                                  dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                  as_survey_design(ids = v001,strata = state,
                                                   weight = new_weight,
                                                   nest = TRUE,
                                                   variance = "YG",pps = "brewer",
                                                   variables = c(x,"wealth")) %>%
                                  group_by(wealth) %>%
                                  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));

                                unadjusted = female_df %>%
                                  dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                  as_survey_design(ids = v001,strata = state,
                                                   weight = weight,
                                                   nest = TRUE,
                                                   variance = "YG",pps = "brewer",
                                                   variables = c(x,"wealth")) %>%
                                  group_by(wealth) %>%
                                  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));

                                original = female_df %>%
                                  # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                  as_survey_design(ids = v001,strata = state,
                                                   weight = weight,
                                                   nest = TRUE,
                                                   variance = "YG",pps = "brewer",
                                                   variables = c(x,"wealth")) %>%
                                  group_by(wealth) %>%
                                  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));

                                female_df$ipw = NULL;
                                female_df$valid = NULL;

                                left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                          unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                          by="wealth") %>%
                                  left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                            by = "wealth") %>%

                                  mutate(indicator = x,
                                         ipw = y) %>%
                                  return(.)


                              })

write_csv(female_indicators_wealth,paste0(path_response_folder,"/working/ipw weighted female estimates at wealth level.csv"))

# DISTRICT ------------

indicators <- c("S86","S88","S95","S100","S106")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

female_indicators_district <- map2_dfr(indicators,weight_vars,
                              function(x,y){
                                print(x);
                                female_df$ipw = female_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                                female_df$valid = female_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                                
                                ipw = female_df %>% 
                                  mutate(new_weight = weight*ipw) %>% 
                                  dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                  as_survey_design(ids = v001,strata = state,
                                                   weight = new_weight,
                                                   nest = TRUE,
                                                   variance = "YG",pps = "brewer",
                                                   variables = c(x,"sdistri")) %>% 
                                  group_by(sdistri) %>% 
                                  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                
                                unadjusted = female_df %>% 
                                  dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                  as_survey_design(ids = v001,strata = state,
                                                   weight = weight,
                                                   nest = TRUE,
                                                   variance = "YG",pps = "brewer",
                                                   variables = c(x,"sdistri")) %>% 
                                  group_by(sdistri) %>% 
                                  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                
                                original = female_df %>% 
                                  # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                  as_survey_design(ids = v001,strata = state,
                                                   weight = weight,
                                                   nest = TRUE,
                                                   variance = "YG",pps = "brewer",
                                                   variables = c(x,"sdistri")) %>% 
                                  group_by(sdistri) %>% 
                                  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                
                                female_df$ipw = NULL;
                                female_df$valid = NULL;
                                
                                left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                          unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                          by="sdistri") %>% 
                                  left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                            by = "sdistri") %>% 
                                  mutate(indicator = x,
                                         ipw = y) %>% 
                                  return(.)
                                
                                
                              })

write_csv(female_indicators_district,paste0(path_response_folder,"/working/ipw weighted female estimates at district level.csv"))  


# SCHOOLING ------------

indicators <- c("S86","S88","S95","S100","S106")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

female_indicators_schooling <- map2_dfr(indicators,weight_vars,
                                     function(x,y){
                                       print(x);
                                       female_df$ipw = female_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                                       female_df$valid = female_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                                       
                                       ipw = female_df %>% 
                                         mutate(new_weight = weight*ipw) %>% 
                                         dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                         as_survey_design(ids = v001,strata = state,
                                                          weight = new_weight,
                                                          nest = TRUE,
                                                          variance = "YG",pps = "brewer",
                                                          variables = c(x,"schooling")) %>% 
                                         group_by(schooling) %>% 
                                         summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                       
                                       unadjusted = female_df %>% 
                                         dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                         as_survey_design(ids = v001,strata = state,
                                                          weight = weight,
                                                          nest = TRUE,
                                                          variance = "YG",pps = "brewer",
                                                          variables = c(x,"schooling")) %>% 
                                         group_by(schooling) %>% 
                                         summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                       
                                       original = female_df %>% 
                                         # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                         as_survey_design(ids = v001,strata = state,
                                                          weight = weight,
                                                          nest = TRUE,
                                                          variance = "YG",pps = "brewer",
                                                          variables = c(x,"schooling")) %>% 
                                         group_by(schooling) %>% 
                                         summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                       
                                       female_df$ipw = NULL;
                                       female_df$valid = NULL;
                                       
                                       left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                                 unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                                 by="schooling") %>% 
                                         left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                                   by = "schooling") %>% 
                                         
                                         mutate(indicator = x,
                                                ipw = y) %>% 
                                         return(.)
                                       
                                       
                                     })

write_csv(female_indicators_schooling,paste0(path_response_folder,"/working/ipw weighted female estimates at schooling level.csv"))  

# CASTE ------------

indicators <- c("S86","S88","S95","S100","S106")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

female_indicators_caste <- map2_dfr(indicators,weight_vars,
                                       function(x,y){
                                         print(x);
                                         female_df$ipw = female_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                                         female_df$valid = female_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                                         
                                         ipw = female_df %>% 
                                           mutate(new_weight = weight*ipw) %>% 
                                           dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                           as_survey_design(ids = v001,strata = state,
                                                            weight = new_weight,
                                                            nest = TRUE,
                                                            variance = "YG",pps = "brewer",
                                                            variables = c(x,"caste")) %>% 
                                           group_by(caste) %>% 
                                           summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                         
                                         unadjusted = female_df %>% 
                                           dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                           as_survey_design(ids = v001,strata = state,
                                                            weight = weight,
                                                            nest = TRUE,
                                                            variance = "YG",pps = "brewer",
                                                            variables = c(x,"caste")) %>% 
                                           group_by(caste) %>% 
                                           summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                         
                                         original = female_df %>% 
                                           # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                           as_survey_design(ids = v001,strata = state,
                                                            weight = weight,
                                                            nest = TRUE,
                                                            variance = "YG",pps = "brewer",
                                                            variables = c(x,"caste")) %>% 
                                           group_by(caste) %>% 
                                           summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                         
                                         female_df$ipw = NULL;
                                         female_df$valid = NULL;
                                         
                                         left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                                   unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                                   by="caste") %>% 
                                           left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                                     by = "caste") %>% 
                                           
                                           mutate(indicator = x,
                                                  ipw = y) %>% 
                                           return(.)
                                         
                                         
                                       })

write_csv(female_indicators_caste,paste0(path_response_folder,"/working/ipw weighted female estimates at caste level.csv"))  


# RELIGION ------------

indicators <- c("S86","S88","S95","S100","S106")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

female_indicators_religion <- map2_dfr(indicators,weight_vars,
                                        function(x,y){
                                          print(x);
                                          female_df$ipw = female_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                                          female_df$valid = female_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                                          
                                          ipw = female_df %>% 
                                            mutate(new_weight = weight*ipw) %>% 
                                            dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                            as_survey_design(ids = v001,strata = state,
                                                             weight = new_weight,
                                                             nest = TRUE,
                                                             variance = "YG",pps = "brewer",
                                                             variables = c(x,"religion")) %>% 
                                            group_by(religion) %>% 
                                            summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                          
                                          unadjusted = female_df %>% 
                                            dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                            as_survey_design(ids = v001,strata = state,
                                                             weight = weight,
                                                             nest = TRUE,
                                                             variance = "YG",pps = "brewer",
                                                             variables = c(x,"religion")) %>% 
                                            group_by(religion) %>% 
                                            summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                          
                                          original = female_df %>% 
                                            # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                            as_survey_design(ids = v001,strata = state,
                                                             weight = weight,
                                                             nest = TRUE,
                                                             variance = "YG",pps = "brewer",
                                                             variables = c(x,"religion")) %>% 
                                            group_by(religion) %>% 
                                            summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                          
                                          female_df$ipw = NULL;
                                          female_df$valid = NULL;
                                          
                                          left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                                    unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                                    by="religion") %>% 
                                            left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                                      by = "religion") %>% 
                                            
                                            mutate(indicator = x,
                                                   ipw = y) %>% 
                                            return(.)
                                          
                                          
                                        })

write_csv(female_indicators_religion,paste0(path_response_folder,"/working/ipw weighted female estimates at religion level.csv"))  
