source("preprocessing/nraux04_datasets for descriptive statistics.R")
male_weights <- readRDS(paste0(path_response_folder,"/male weights.RDS"))

# From ~/nfhs5_analysis/nfhs4 estimates/male_indicators.R ----------------

male_df <- readRDS(paste0(path_ecological_analysis,"/working/iamr74_clean.RDS")) %>% 
  left_join(male_weights %>% 
              dplyr::select(-hv021,-hv024,-hhid),
            by=c("mv001"="hv001",
                 "mv002" = "hv002",
                 "mv003" = "hvidx")) %>% 
  left_join(male %>% 
              dplyr::select(hv001,hv002,hvidx,strata,
                            wealth,schooling,caste,religion),
            by = c("mv001"="hv001",
                   "mv002" = "hv002",
                   "mv003" = "hvidx")) %>% 
  mutate(
    
    S15 = case_when(mv133 %in% c(9:20) | mv155 %in% c(1,2)~ 1,
                    TRUE ~ 0),
    S17 = case_when(mv133 %in% c(10:20) ~ 1,
                    mv133 %in% c(0:9) ~ 0,
                    mv133 >= 97 ~ NA_real_,
                    TRUE ~ NA_real_),
    S87 = case_when(hb40 > 6000 ~ NA_real_,
                    hb40 < 1850 ~ 1,
                    hb40 >= 1850 ~ 0,
                    TRUE ~ NA_real_),
    
    
    S89 = case_when(hb40 > 6000 ~ NA_real_,
                    hb40 >= 2500 ~ 1,
                    hb40 < 2500 ~ 0,
                    TRUE ~ NA_real_),
    
    S97 = m_hemo_anemia,
    
    overweight = case_when(hb40 > 6000 ~ NA_real_,
                           hb40 >= 2500 & hb40 < 3000 ~ 1,
                           hb40 < 2500 | hb40 >= 3000 ~ 0,
                           TRUE ~ NA_real_),
    
    obese = case_when(hb40 > 6000 ~ NA_real_,
                      hb40 >= 3000 ~ 1,
                      hb40 < 3000 ~ 0,
                      TRUE ~ NA_real_),
    
    S102 = case_when(is.na(smb70) | smb70 > 498 ~ NA_real_,
                     smb70 >= 140 & smb70 <= 160 ~ 1,
                     smb70 < 140 | smb70 > 160 ~ 0,
                     TRUE ~ NA_real_ ),
    
    S103 = case_when(is.na(smb70) | smb70 > 498 ~ NA_real_,
                     smb70 > 160 ~ 1,
                     smb70 <= 160 ~ 0,
                     TRUE ~ NA_real_),
    
    S104 = case_when(sm622ab == 1 ~ 1,
                     is.na(smb70) | smb70 > 498 ~ NA_real_,
                     smb70 >= 140 ~ 1,
                     smb70 < 140 ~ 0,
                     TRUE ~ NA_real_),
    
    S108 = case_when(is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                     m_sbp >= 140 & m_sbp < 160 ~ 1,
                     m_dbp >= 90 & m_dbp < 100 ~ 1,
                     m_sbp < 140 | m_sbp >= 160 ~ 0,
                     m_dbp < 90 | m_dbp >= 100 ~ 0,
                     TRUE ~ NA_real_
    ),
    S109 = case_when(is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                     m_sbp >= 160 ~ 1,
                     m_dbp >= 100  ~ 1,
                     m_sbp < 160  ~ 0,
                     m_dbp < 100 ~ 0,
                     TRUE ~ NA_real_
    ),
    S110 = case_when(smb19 == 1 ~ 1,
                     is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                     m_sbp >= 140 ~ 1,
                     m_dbp >= 90  ~ 1,
                     m_sbp < 140  ~ 0,
                     m_dbp < 90 ~ 0,
                     TRUE ~ NA_real_
    ),
    S129 = case_when(mv463z == 1 ~ 0,
                     mv463z == 0 ~ 1,
                     TRUE ~ NA_real_),
    S131 = case_when(sm615 == 0 ~ 0,
                     sm615 == 1 ~ 1,
                     TRUE ~ NA_real_)
  ) %>% 
  mutate(mv024 = factor(mv024,levels=c(1:36),labels=attr(mv024,"labels") %>% attr(.,"names"))) %>% 
  mutate(weight = mv005/(10^6),
         state = case_when(smdistri %in% c(494,495,496) ~ "daman and diu",
                           smdistri %in% c(3,4) ~ "ladakh",
                           TRUE ~ as.character(mv024)),
         smdistri = as.numeric(smdistri)) 


# STATE ------------

indicators <- c("S87","S89","S97","S103","S109")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

male_indicators <- map2_dfr(indicators,weight_vars,
                        function(x,y){
                          print(x);
                          male_df$ipw = male_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                          male_df$valid = male_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();

                          ipw = male_df %>%
                            mutate(new_weight = weight*ipw) %>%
                            dplyr::filter(valid == 1 & !is.na(valid)) %>%
                            as_survey_design(ids = mv001,strata = state,
                                             weight = new_weight,
                                             nest = TRUE,
                                             variance = "YG",pps = "brewer",
                                             variables = c(x,"state")) %>%
                            group_by(state) %>%
                            summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>%
                            mutate(state = str_replace(state,"&","and"));

                          unadjusted = male_df %>%
                            dplyr::filter(valid == 1 & !is.na(valid)) %>%
                            as_survey_design(ids = mv001,strata = state,
                                             weight = weight,
                                             nest = TRUE,
                                             variance = "YG",pps = "brewer",
                                             variables = c(x,"state")) %>%
                            group_by(state) %>%
                            summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>%
                            mutate(state = str_replace(state,"&","and"));

                          original = male_df %>%
                            # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                            as_survey_design(ids = mv001,strata = state,
                                             weight = weight,
                                             nest = TRUE,
                                             variance = "YG",pps = "brewer",
                                             variables = c(x,"state")) %>%
                            group_by(state) %>%
                            summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>%
                            mutate(state = str_replace(state,"&","and"));


                          male_df$ipw = NULL;
                          male_df$valid = NULL;

                          left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                    unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                    by="state") %>%
                            left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                      by = "state") %>%
                            mutate(indicator = x,
                                   ipw = y) %>%
                            return(.)


                        })


write_csv(male_indicators,paste0(path_response_folder,"/working/ipw weighted male estimates at state level.csv"))

# WEALTH ------------

indicators <- c("S87","S89","S97","S103","S109")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

male_indicators_wealth <- map2_dfr(indicators,weight_vars,
                            function(x,y){
                              print(x);
                              male_df$ipw = male_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                              male_df$valid = male_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();

                              ipw = male_df %>%
                                mutate(new_weight = weight*ipw) %>%
                                dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = mv001,strata = state,
                                                 weight = new_weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"wealth")) %>%
                                group_by(wealth) %>%
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));

                              unadjusted = male_df %>%
                                dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = mv001,strata = state,
                                                 weight = weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"wealth")) %>%
                                group_by(wealth) %>%
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));

                              original = male_df %>%
                                # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = mv001,strata = state,
                                                 weight = weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"wealth")) %>%
                                group_by(wealth) %>%
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));

                              male_df$ipw = NULL;
                              male_df$valid = NULL;

                              left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                        unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                        by="wealth") %>%
                                left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                          by = "wealth") %>%

                                mutate(indicator = x,
                                       ipw = y) %>%
                                return(.)


                            })


write_csv(male_indicators_wealth,paste0(path_response_folder,"/working/ipw weighted male estimates at wealth level.csv"))

# SCHOOLING ------------

indicators <- c("S87","S89","S97","S103","S109")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

male_indicators_schooling <- map2_dfr(indicators,weight_vars,
                                   function(x,y){
                                     print(x);
                                     male_df$ipw = male_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                                     male_df$valid = male_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                                     
                                     ipw = male_df %>% 
                                       mutate(new_weight = weight*ipw) %>% 
                                       dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                       as_survey_design(ids = mv001,strata = state,
                                                        weight = new_weight,
                                                        nest = TRUE,
                                                        variance = "YG",pps = "brewer",
                                                        variables = c(x,"schooling")) %>% 
                                       group_by(schooling) %>% 
                                       summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                     
                                     unadjusted = male_df %>% 
                                       dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                       as_survey_design(ids = mv001,strata = state,
                                                        weight = weight,
                                                        nest = TRUE,
                                                        variance = "YG",pps = "brewer",
                                                        variables = c(x,"schooling")) %>% 
                                       group_by(schooling) %>% 
                                       summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                     
                                     original = male_df %>% 
                                       # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                       as_survey_design(ids = mv001,strata = state,
                                                        weight = weight,
                                                        nest = TRUE,
                                                        variance = "YG",pps = "brewer",
                                                        variables = c(x,"schooling")) %>% 
                                       group_by(schooling) %>% 
                                       summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                     
                                     male_df$ipw = NULL;
                                     male_df$valid = NULL;
                                     
                                     left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                               unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                               by="schooling") %>% 
                                       left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                                 by = "schooling") %>% 
                                       
                                       mutate(indicator = x,
                                              ipw = y) %>% 
                                       return(.)
                                     
                                     
                                   })


write_csv(male_indicators_schooling,paste0(path_response_folder,"/working/ipw weighted male estimates at schooling level.csv"))  

# CASTE ------------

indicators <- c("S87","S89","S97","S103","S109")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

male_indicators_caste <- map2_dfr(indicators,weight_vars,
                                      function(x,y){
                                        print(x);
                                        male_df$ipw = male_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                                        male_df$valid = male_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                                        
                                        ipw = male_df %>% 
                                          mutate(new_weight = weight*ipw) %>% 
                                          dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                          as_survey_design(ids = mv001,strata = state,
                                                           weight = new_weight,
                                                           nest = TRUE,
                                                           variance = "YG",pps = "brewer",
                                                           variables = c(x,"caste")) %>% 
                                          group_by(caste) %>% 
                                          summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                        
                                        unadjusted = male_df %>% 
                                          dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                          as_survey_design(ids = mv001,strata = state,
                                                           weight = weight,
                                                           nest = TRUE,
                                                           variance = "YG",pps = "brewer",
                                                           variables = c(x,"caste")) %>% 
                                          group_by(caste) %>% 
                                          summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                        
                                        original = male_df %>% 
                                          # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                          as_survey_design(ids = mv001,strata = state,
                                                           weight = weight,
                                                           nest = TRUE,
                                                           variance = "YG",pps = "brewer",
                                                           variables = c(x,"caste")) %>% 
                                          group_by(caste) %>% 
                                          summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                        
                                        male_df$ipw = NULL;
                                        male_df$valid = NULL;
                                        
                                        left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                                  unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                                  by="caste") %>% 
                                          left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                                    by = "caste") %>% 
                                          
                                          mutate(indicator = x,
                                                 ipw = y) %>% 
                                          return(.)
                                        
                                        
                                      })


write_csv(male_indicators_caste,paste0(path_response_folder,"/working/ipw weighted male estimates at caste level.csv"))  

# RELIGION ------------

indicators <- c("S87","S89","S97","S103","S109")
weight_vars <- c("_weight","_weight","_hb","_glucose","_sbp_atleast1")

male_indicators_religion <- map2_dfr(indicators,weight_vars,
                                  function(x,y){
                                    print(x);
                                    male_df$ipw = male_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                                    male_df$valid = male_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                                    
                                    ipw = male_df %>% 
                                      mutate(new_weight = weight*ipw) %>% 
                                      dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                      as_survey_design(ids = mv001,strata = state,
                                                       weight = new_weight,
                                                       nest = TRUE,
                                                       variance = "YG",pps = "brewer",
                                                       variables = c(x,"religion")) %>% 
                                      group_by(religion) %>% 
                                      summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                    
                                    unadjusted = male_df %>% 
                                      dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                      as_survey_design(ids = mv001,strata = state,
                                                       weight = weight,
                                                       nest = TRUE,
                                                       variance = "YG",pps = "brewer",
                                                       variables = c(x,"religion")) %>% 
                                      group_by(religion) %>% 
                                      summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                    
                                    original = male_df %>% 
                                      # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                      as_survey_design(ids = mv001,strata = state,
                                                       weight = weight,
                                                       nest = TRUE,
                                                       variance = "YG",pps = "brewer",
                                                       variables = c(x,"religion")) %>% 
                                      group_by(religion) %>% 
                                      summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                                    
                                    male_df$ipw = NULL;
                                    male_df$valid = NULL;
                                    
                                    left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                              unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                              by="religion") %>% 
                                      left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                                by = "religion") %>% 
                                      
                                      mutate(indicator = x,
                                             ipw = y) %>% 
                                      return(.)
                                    
                                    
                                  })


write_csv(male_indicators_religion,paste0(path_response_folder,"/working/ipw weighted male estimates at religion level.csv"))  

