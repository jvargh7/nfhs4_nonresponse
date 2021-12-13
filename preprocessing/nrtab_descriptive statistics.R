source("preprocessing/nraux04_datasets for descriptive statistics.R")


# Group variables -----------
all_variables = unique(c(names(male),names(child),names(female)))
id_variables = c("hv001","hv021","state","hv024","shdistri","weight","hv006","hv007","hv008","hv010","hv011")

group_variables = c("age_category","schooling","caste","religion","wealth")
other_variables = all_variables[!all_variables %in% c(id_variables,group_variables)]

female_summary_groups <- map_dfr(group_variables,
                                 function(g){
                                   
                                   female_surveydesign %>% 
                                     group_by_at(g) %>% 
                                     summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci"),
                                               count = unweighted(n())) %>% 
                                     rename(group = g) %>% 
                                     mutate(variable = g)
                                   
                                 }) %>% 
  dplyr::filter(!is.na(proportion)) %>% 
  group_by(variable) %>% 
  mutate(N = sum(count)) %>% 
  ungroup()

male_summary_groups <- map_dfr(group_variables,
                                 function(g){
                                   
                                   male_surveydesign %>% 
                                     group_by_at(g) %>% 
                                     summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci"),
                                               count = unweighted(n())) %>% 
                                     rename(group = g) %>% 
                                     mutate(variable = g)
                                   
                                 }) %>% 
  dplyr::filter(!is.na(proportion))%>% 
  group_by(variable) %>% 
  mutate(N = sum(count)) %>% 
  ungroup()

child_summary_groups <- map_dfr(group_variables,
                               function(g){
                                 
                                 child_surveydesign %>% 
                                   group_by_at(g) %>% 
                                   summarize(proportion = survey_mean(na.rm = TRUE,vartype="ci"),
                                             count = unweighted(n())) %>% 
                                   rename(group = g) %>% 
                                   mutate(variable = g)
                                 
                               }) %>% 
  dplyr::filter(!is.na(proportion))%>% 
  group_by(variable) %>% 
  mutate(N = sum(count)) %>% 
  ungroup()


# Means ---------

male_summary_means <- male_surveydesign %>% 
  summarize_at(vars(one_of(other_variables)),.funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=everything()) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)


female_summary_means <- female_surveydesign %>% 
  summarize_at(vars(one_of(other_variables)),.funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=everything()) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)

child_summary_means <- child_surveydesign %>% 
  summarize_at(vars(one_of(other_variables)),.funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=everything()) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)



summary_table <- bind_rows(child_summary_groups %>%
                             mutate(sex = "Child"),
                           child_summary_means %>%
                             mutate(sex = "Child"),
                           
                           female_summary_groups %>% 
                             mutate(sex = "Female"),
                           female_summary_means %>%
                             mutate(sex = "Female"),
                           
                           male_summary_groups %>% 
                             mutate(sex = "Male"),
                           male_summary_means %>%
                             mutate(sex = "Male")
                           ) %>% 

  mutate(coef_ci = case_when(str_detect(variable,"value_") ~ paste0(round(proportion,1)," (",
                                                                    round(proportion_low,1),", ",
                                                                    round(proportion_upp,1),")"),
                             TRUE ~ paste0(round(proportion*100,1)," (",
                                           round(proportion_low*100,1),", ",
                                           round(proportion_upp*100,1),")"))) 

summary_table %>% 
    dplyr::select(sex,variable,group,coef_ci) %>% 
  pivot_wider(names_from=sex,values_from=coef_ci) %>% 
  dplyr::select(variable,group,Female,Male,Child) %>% 
write_csv(.,paste0(path_response_folder,"/working/table 1 descriptives.csv"))
