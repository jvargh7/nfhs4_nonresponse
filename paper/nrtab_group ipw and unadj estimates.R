female_indicators_wealth <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at wealth level.csv"))  
male_indicators_wealth <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at wealth level.csv"))  
child_indicators_wealth <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at wealth level.csv"))  
female_indicators_schooling <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at schooling level.csv"))  
male_indicators_schooling <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at schooling level.csv"))  
child_indicators_schooling <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at schooling level.csv"))  
female_indicators_caste <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at caste level.csv"))  
male_indicators_caste <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at caste level.csv"))  
child_indicators_caste <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at caste level.csv"))  
female_indicators_religion <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at religion level.csv"))  
male_indicators_religion <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at religion level.csv"))  
child_indicators_religion <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at religion level.csv"))  


nfhs4_group_estimates <- bind_rows(
  female_indicators_wealth %>% mutate(population = "Women",strata = "4, Wealth") %>% rename(id = wealth),
  child_indicators_wealth %>% mutate(population = "Child",strata = "4, Wealth") %>% rename(id = wealth),
  male_indicators_wealth %>% mutate(population = "Men",strata = "4, Wealth") %>% rename(id = wealth),
  
  female_indicators_schooling %>% mutate(population = "Women",strata = "1, Schooling") %>% rename(id = schooling),
  child_indicators_schooling %>% mutate(population = "Child",strata = "1, Schooling") %>% rename(id = schooling),
  male_indicators_schooling %>% mutate(population = "Men",strata = "1, Schooling") %>% rename(id = schooling),
  
  female_indicators_caste %>% mutate(population = "Women",strata = "2, Caste") %>% rename(id = caste),
  child_indicators_caste %>% mutate(population = "Child",strata = "2, Caste") %>% rename(id = caste),
  male_indicators_caste %>% mutate(population = "Men",strata = "2, Caste") %>% rename(id = caste),
  
  female_indicators_religion %>% mutate(population = "Women",strata = "3, Religion") %>% rename(id = religion),
  child_indicators_religion %>% mutate(population = "Child",strata = "3, Religion") %>% rename(id = religion),
  male_indicators_religion %>% mutate(population = "Men",strata = "3, Religion") %>% rename(id = religion)
  
)  %>% 
  dplyr::select(population,strata,id,indicator,
                proportion_ipw,proportion_low_ipw,proportion_upp_ipw,
                proportion_unadj,proportion_low_unadj,proportion_upp_unadj) %>% 
  mutate_at(vars(contains("proportion_")),function(x) round(x*100,1)) %>% 
  dplyr::mutate(ipw = paste0(proportion_ipw," (",proportion_low_ipw,", ",proportion_upp_ipw,")"),
                unadj = paste0(proportion_unadj," (",proportion_low_unadj,", ",proportion_upp_unadj,")")) %>% 
  dplyr::select(strata,population,id,indicator,ipw,unadj) %>% 
  mutate(indicator = case_when(indicator == "S81" ~ "Stunting",
                               indicator %in% c("S84","S86","S87") ~ "Underweight",
                               indicator %in% c("S92","S97","S95") ~ "Anemia",
                               indicator %in% c("S89","S88") ~ "Overweight",
                               indicator %in% c("S103","S100") ~ "Glucose",
                               indicator %in% c("S109","S106") ~ "BloodPressure",
                               TRUE ~ NA_character_)
  ) %>% 
  mutate_at(vars(contains("bias")),function(x) x*100) %>% 
  mutate(indicator = factor(indicator,levels=c("Stunting",
                                               "Underweight",
                                               "Overweight",
                                               "BloodPressure",
                                               "Anemia",
                                               "Glucose"
                                               ),ordered=TRUE)) %>% 
  pivot_wider(names_from=c(indicator,population),values_from=c(ipw,unadj)) %>% 
  dplyr::select(strata,id,
                contains("Stunting_Child"),
                contains("Underweight_Child"),
                contains("Anemia_Child"),
                contains("Underweight_Women"),
                contains("Overweight_Women"),
                contains("BloodPressure_Women"),
                contains("Anemia_Women"),
                contains("Glucose_Women"),
                contains("Underweight_Men"),
                contains("Overweight_Men"),
                contains("BloodPressure_Men"),
                contains("Anemia_Men"),
                contains("Glucose_Men")
                
                ) %>% 
  arrange(strata)


write_csv(nfhs4_group_estimates,paste0(path_response_folder,"/working/table_group estimates with intervals.csv"))
