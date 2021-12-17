source("preprocessing/nraux04_datasets for descriptive statistics.R")

child_weights <- readRDS(paste0(path_response_folder,"/child weights.RDS"))

# From ~/nfhs5_analysis/nfhs4 estimates/child_indicators.R ----------------
child_df <- readRDS(paste0(path_ecological_analysis,"/working/iapr74_child.RDS")) %>% 
  left_join(child_weights %>% 
              dplyr::select(-hv021,-hv024,-hhid,-shdistri),
            by=c("hv001",
                 "hv002",
                 "hvidx")) %>% 
  left_join(child %>% 
              dplyr::select(hv001,hv002,hvidx,
                            wealth,schooling,caste,religion),
            by = c("hv001",
                   "hv002",
                   "hvidx"))
# STATE ---------

indicators <- c("S81","S84","S92")
weight_vars <- c("_height","_weight","_hb")

child_indicators <- map2_dfr(indicators,weight_vars,
                            function(x,y){
                              print(x);
                              child_df$ipw = child_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                              child_df$valid = child_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                              
                              ipw = child_df %>% 
                                mutate(new_weight = weight*ipw) %>% 
                                dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = hv001,strata = state,
                                                 weight = new_weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"state")) %>% 
                                group_by(state) %>% 
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
                                mutate(state = str_replace(state,"&","and"));
                              
                              unadjusted = child_df %>% 
                                dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = hv001,strata = state,
                                                 weight = weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"state")) %>% 
                                group_by(state) %>% 
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
                                mutate(state = str_replace(state,"&","and"));
                              
                              original = child_df %>% 
                                # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                as_survey_design(ids = hv001,strata = state,
                                                 weight = weight,
                                                 nest = TRUE,
                                                 variance = "YG",pps = "brewer",
                                                 variables = c(x,"state")) %>% 
                                group_by(state) %>% 
                                summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
                                mutate(state = str_replace(state,"&","and"));
                              
                              child_df$ipw = NULL;
                              child_df$valid = NULL;
                              
                              left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                        unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                        by="state") %>% 
                                left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                          by = "state") %>% 
                                mutate(indicator = x,
                                       ipw = y) %>% 
                                return(.)
                              
                              
                            })
write_csv(child_indicators,paste0(path_response_folder,"/working/ipw weighted child estimates at state level.csv"))  


# WEALTH -------------
indicators <- c("S81","S84","S92")
weight_vars <- c("_height","_weight","_hb")
child_indicators_wealth <- map2_dfr(indicators,weight_vars,
                             function(x,y){
                               print(x);
                               child_df$ipw = child_df %>% dplyr::select(one_of(paste0("ipw",y))) %>% pull();
                               child_df$valid = child_df %>% dplyr::select(one_of(paste0("valid",y))) %>% pull();
                               
                               ipw = child_df %>% 
                                 mutate(new_weight = weight*ipw) %>% 
                                 dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                 as_survey_design(ids = hv001,strata = state,
                                                  weight = new_weight,
                                                  nest = TRUE,
                                                  variance = "YG",pps = "brewer",
                                                  variables = c(x,"wealth")) %>% 
                                 group_by(wealth) %>% 
                                 summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                               
                               unadjusted = child_df %>% 
                                 dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                 as_survey_design(ids = hv001,strata = state,
                                                  weight = weight,
                                                  nest = TRUE,
                                                  variance = "YG",pps = "brewer",
                                                  variables = c(x,"wealth")) %>% 
                                 group_by(wealth) %>% 
                                 summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                               
                               original = child_df %>% 
                                 # dplyr::filter(valid == 1 & !is.na(valid)) %>%
                                 as_survey_design(ids = hv001,strata = state,
                                                  weight = weight,
                                                  nest = TRUE,
                                                  variance = "YG",pps = "brewer",
                                                  variables = c(x,"wealth")) %>% 
                                 group_by(wealth) %>% 
                                 summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE));
                               
                               child_df$ipw = NULL;
                               child_df$valid = NULL;
                               
                               left_join(ipw %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_ipw")),
                                         unadjusted %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_unadj")),
                                         by="wealth") %>% 
                                 left_join(original %>% rename_at(vars(starts_with(x)),~paste0(str_replace(.,x,"proportion"),"_orig")),
                                           by = "wealth") %>% 
                                 
                                 mutate(indicator = x,
                                        ipw = y) %>% 
                                 return(.)
                               
                               
                             })


write_csv(child_indicators_wealth,paste0(path_response_folder,"/working/ipw weighted child estimates at wealth level.csv"))  

