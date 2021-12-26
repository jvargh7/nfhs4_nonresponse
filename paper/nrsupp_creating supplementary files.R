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


female_indicators_state <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at state level.csv"))  
male_indicators_state <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at state level.csv"))  
child_indicators_state <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at state level.csv"))  
female_indicators_district <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at district level.csv"))  
# male_indicators_district <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at district level.csv"))  
child_indicators_district <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at district level.csv"))  




bind_rows(female_indicators_state %>% mutate(population = "Women",strata = "State") %>% rename(id = state),
          child_indicators_state %>% mutate(population = "Child",strata = "State") %>% rename(id = state),
          male_indicators_state %>% mutate(population = "Men",strata = "State") %>% rename(id = state)) %>% 
  mutate(indicator = case_when(indicator == "S81" ~ "Stunting",
                               indicator %in% c("S84","S86","S87") ~ "Underweight",
                               indicator %in% c("S92","S97","S95") ~ "Anemia",
                               indicator %in% c("S89","S88") ~ "Overweight",
                               indicator %in% c("S103","S100") ~ "Very high blood glucose",
                               indicator %in% c("S109","S106") ~ "Moderate/severe blood pressure",
                               TRUE ~ NA_character_)) %>% 
  mutate_at(vars(contains("proportion")),~round(.*100,2)) %>% 
  dplyr::select(population,strata,indicator,id,everything()) %>% 
  writexl::write_xlsx(.,paste0(path_response_folder,"/working/Supplementary File 2.xlsx"))


bind_rows(
          female_indicators_wealth %>% mutate(population = "Women",strata = "Wealth") %>% rename(id = wealth),
          child_indicators_wealth %>% mutate(population = "Child",strata = "Wealth") %>% rename(id = wealth),
          male_indicators_wealth %>% mutate(population = "Men",strata = "Wealth") %>% rename(id = wealth),
          
          female_indicators_schooling %>% mutate(population = "Women",strata = "Schooling") %>% rename(id = schooling),
          child_indicators_schooling %>% mutate(population = "Child",strata = "Schooling") %>% rename(id = schooling),
          male_indicators_schooling %>% mutate(population = "Men",strata = "Schooling") %>% rename(id = schooling),
          
          female_indicators_caste %>% mutate(population = "Women",strata = "Caste") %>% rename(id = caste),
          child_indicators_caste %>% mutate(population = "Child",strata = "Caste") %>% rename(id = caste),
          male_indicators_caste %>% mutate(population = "Men",strata = "Caste") %>% rename(id = caste),
          
          female_indicators_religion %>% mutate(population = "Women",strata = "Religion") %>% rename(id = religion),
          child_indicators_religion %>% mutate(population = "Child",strata = "Religion") %>% rename(id = religion),
          male_indicators_religion %>% mutate(population = "Men",strata = "Religion") %>% rename(id = religion)
          
          ) %>% 
  mutate(indicator = case_when(indicator == "S81" ~ "Stunting",
                               indicator %in% c("S84","S86","S87") ~ "Underweight",
                               indicator %in% c("S92","S97","S95") ~ "Anemia",
                               indicator %in% c("S89","S88") ~ "Overweight",
                               indicator %in% c("S103","S100") ~ "Very high blood glucose",
                               indicator %in% c("S109","S106") ~ "Moderate/severe blood pressure",
                               TRUE ~ NA_character_)) %>% 
  mutate_at(vars(contains("proportion")),~round(.*100,2)) %>% 
  dplyr::select(population,strata,indicator,id,everything()) %>% 
  writexl::write_xlsx(.,paste0(path_response_folder,"/working/Supplementary File 3.xlsx"))


bind_rows(female_indicators_district %>% mutate(population = "Women",strata = "District") %>% rename(id = sdistri),
          # male_indicators_district %>% mutate(population = "Men",strata = "District") %>% rename(id = smdistri),
          child_indicators_district %>% mutate(population = "Child",strata = "District") %>% rename(id = shdistri)
          ) %>% 
  mutate(indicator = case_when(indicator == "S81" ~ "Stunting",
                               indicator %in% c("S84","S86","S87") ~ "Underweight",
                               indicator %in% c("S92","S97","S95") ~ "Anemia",
                               indicator %in% c("S89","S88") ~ "Overweight",
                               indicator %in% c("S103","S100") ~ "Very high blood glucose",
                               indicator %in% c("S109","S106") ~ "Moderate/severe blood pressure",
                               TRUE ~ NA_character_)) %>% 
  mutate_at(vars(contains("proportion")),~round(.*100,2)) %>% 
  dplyr::select(population,strata,indicator,id,everything()) %>% 
  writexl::write_xlsx(.,paste0(path_response_folder,"/working/Supplementary File 4.xlsx"))
