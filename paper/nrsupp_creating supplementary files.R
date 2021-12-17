female_indicators_wealth <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at wealth level.csv"))  
male_indicators_wealth <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at wealth level.csv"))  
child_indicators_wealth <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at wealth level.csv"))  
female_indicators_state <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at state level.csv"))  
male_indicators_state <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at state level.csv"))  
child_indicators_state <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at state level.csv"))  


bind_rows(female_indicators_state %>% mutate(population = "Women",strata = "State") %>% rename(id = state),
          female_indicators_wealth %>% mutate(population = "Women",strata = "Wealth") %>% rename(id = wealth),
          child_indicators_state %>% mutate(population = "Child",strata = "State") %>% rename(id = state),
          child_indicators_wealth %>% mutate(population = "Child",strata = "Wealth") %>% rename(id = wealth),
          male_indicators_state %>% mutate(population = "Men",strata = "State") %>% rename(id = state),
          male_indicators_wealth %>% mutate(population = "Men",strata = "Wealth") %>% rename(id = wealth)) %>% 
  mutate(indicator = case_when(indicator == "S81" ~ "Stunting",
                               indicator %in% c("S84","S86","S87") ~ "Underweight",
                               indicator %in% c("S92","S97","S95") ~ "Anemia",
                               indicator %in% c("S89","S88") ~ "Overweight",
                               indicator %in% c("S103","S100") ~ "Very high blood glucose",
                               indicator %in% c("S109","S106") ~ "Moderate/severe blood pressure",
                               TRUE ~ NA_character_)) %>% 
  mutate_at(vars(contains("proportion")),~round(.*100,2)) %>% 
  dplyr::select(population,strata,indicator,id,everything()) %>% 
  writexl::write_xlsx(.,paste0(path_response_folder,"/working/Supplementary File 1.xlsx"))
