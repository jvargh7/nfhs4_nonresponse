source("preprocessing/nraux04_datasets for descriptive statistics.R")


# Group variables -----------
all_variables = unique(c(names(male),names(child),names(female)))

valid_variables <- c(all_variables[str_detect(all_variables,"valid")],"consented_hiv")


male_summary_means <- male_surveydesign %>% 
  group_by(state) %>% 
  summarize_at(vars(one_of(valid_variables)),.funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=-state) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)


female_summary_means <- female_surveydesign %>% 
  group_by(state) %>% 
  summarize_at(vars(one_of(valid_variables)),.funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=-state) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)

child_summary_means <- child_surveydesign %>% 
  group_by(state) %>% 
  summarize_at(vars(one_of(valid_variables)),.funs = list(mean = ~survey_mean(.,na.rm=TRUE,vartype="ci"))) %>% 
  pivot_longer(.,names_to=c("variable","est"),names_sep ="_mean",cols=-state) %>% 
  mutate(est = case_when(est == "_low" ~ "proportion_low",
                         est == "_upp" ~ "proportion_upp",
                         TRUE ~ "proportion")) %>% 
  pivot_wider(names_from=est,values_from=value)



# SUMMARY -------

nfhs5_factsheet_map <- readxl::read_excel(paste0(path_mapping_folder,"/mapping.xlsx"),sheet="nfhs5_factsheet_map")

summary_table <- bind_rows(
                           child_summary_means %>%
                             mutate(sex = "Child"),
                           
                           
                           female_summary_means %>%
                             mutate(sex = "Female"),
                           
                           
                           male_summary_means %>%
                             mutate(sex = "Male")
)%>% 
  left_join(nfhs5_factsheet_map,
            by=c("state" = "v024_label")) %>% 
  mutate(variable = case_when(variable == "valid_height" ~ "Height",
                              variable == "valid_weight" ~ "Weight",
                              variable == "valid_sbp" ~ "Systolic BP - all",
                              variable == "valid_dbp" ~ "Diastolic BP - all",
                              variable == "valid_sbp_atleast1" ~ "Systolic BP - atleast 1",
                              variable == "valid_dbp_atleast1" ~ "Diastolic BP - atleast 1",
                              variable == "consented_hiv" ~ "HIV",
                              variable == "valid_hb" ~ "Hemoglobin",
                              variable == "valid_glucose" ~ "Glucose",
                              TRUE ~ NA_character_)) %>% 
  dplyr::filter(!(is.na(variable) | variable %in% c("Diastolic BP - all","Diastolic BP - atleast 1"))) %>% 
  mutate(variable = factor(variable,levels=c("Weight","Height","HIV","Hemoglobin",
                                             "Glucose","Systolic BP - atleast 1",
                                             "Systolic BP - all")))


(summary_table %>% 
    mutate(factsheet_state = case_when(factsheet_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra & Nagar Haveli",
                             TRUE ~ factsheet_state)) %>% 
  ggplot(data=.,aes(x=variable,y=factsheet_state,fill=proportion)) +
  geom_tile() +
  facet_grid(~sex,scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "top")  + 
  scale_fill_gradient2(low = "red", high = "darkgreen", mid = "yellow",
                       midpoint = 0.75, limit = c(0.5,1), space = "Lab",
                       name="") +
  xlab("") + ylab("")) %>% 
  ggsave(.,filename = paste0(path_response_folder,"/figures/heatmap of state summary.png"),height = 6.5,width=5.3,units = "in")
  
